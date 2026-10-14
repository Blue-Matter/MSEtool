
.AlignDimsTo <- function(a, b) {
  aDN <- names(dimnames(a))
  bDN <- names(dimnames(b))
  if (is.null(aDN) || is.null(bDN)) return(b)
  extra <- setdiff(bDN, aDN)
  if (length(extra)) b <- DropDimension(b, extra, warn = FALSE)
  b
}


#' Resolve selectivity-at-age arrays for a nominal index
#'
#' Builds a named list of selectivity arrays (one per stock in the complex)
#' for use by [.CalcNomIndex()] from `IndexObs@Selectivity`. See
#' [IndicesObs()] for the accepted forms.
#'
#' @param object A `hist`-like object with an `@OM` slot.
#' @param stocks Integer vector of stock indices in the complex.
#' @param fleet Character fleet / index name.
#' @param IndexObs An [indicesobs-class] object.
#' @param Years Calendar years spanned by the selectivity arrays.
#' @param sim Optional simulation index. When set, arrays are sliced to that
#'   replicate and returned without a `Sim` dimension.
#' @param nArea Number of spatial areas. Defaults to [nArea()] of `object`.
#' @param StockNames Optional character vector of stock names (defaults to
#'   [StockNames()] for `stocks`).
#'
#' @return A list of selectivity arrays, one element per stock in `stocks`.
#' @keywords internal
.ResolveIndexSelectivity <- function(object,
                                     stocks,
                                     fleet,
                                     IndexObs,
                                     Years,
                                     sim = NULL,
                                     nArea = NULL,
                                     StockNames = NULL) {

  if (is.null(nArea)) nArea <- nArea(object)
  Areas <- seq_len(nArea)
  if (is.null(StockNames)) StockNames <- StockNames(object@OM)[stocks]
  spec <- IndexObs@Selectivity

  .flat_sel <- function(AgeClasses) {
    if (is.null(sim)) {
      array(1, dim = c(1, length(AgeClasses), 1, nArea),
            dimnames = list(Sim = 1, Age = AgeClasses, Year = Years[1], Area = Areas))
    } else {
      array(1, dim = c(length(AgeClasses), 1, nArea),
            dimnames = list(Age = AgeClasses, Year = Years[1], Area = Areas))
    }
  }

  .slice_sim <- function(arr) {
    if (is.null(sim)) return(arr)
    dd <- dim(arr)
    if (length(dd) >= 1L && !is.null(dimnames(arr)[['Sim']])) {
      abind::asub(arr, min(sim, dd[1]), 1, drop = FALSE) |> abind::adrop(1)
    } else {
      arr
    }
  }

  .subset_years <- function(arr) {
    if (is.null(dimnames(arr)[['Year']])) return(arr)
    arr |> .ArraySubsetYear(Years)
  }

  .maturity_sel <- function(st) {
    mat <- object@OM@Stock[[st]]@Maturity@MeanAtAge
    if (is.null(sim)) {
      mat |>
        .subset_years() |>
        AddDimension('Area') |>
        ExtendAreas(Areas) |>
        ReduceDims()
    } else {
      dd <- dim(mat)
      mat[min(dd[1], sim), , , drop = FALSE] |>
        .subset_years() |>
        abind::adrop(1) |>
        AddDimension('Area') |>
        ExtendAreas(Areas)
    }
  }

  .user_sel <- function(sel, st, st_idx) {
    arr <- .IndexSelectivityArray(sel, object@OM@Stock[[st]]@Ages@Classes,
                                  Index = fleet, Stock = StockNames[st_idx]) |>
      .subset_years() |>
      AddDimension('Year', val = Years[1])
    if (is.null(sim)) arr <- AddDimension(arr, 'Sim', pos = 1)
    arr <- arr |> .slice_sim() |> AddDimension('Area') |> ExtendAreas(Areas)
    arr <- base::aperm(arr, intersect(c('Sim', 'Age', 'Year', 'Area'), names(dimnames(arr))))
    if (is.null(sim)) ReduceDims(arr) else arr
  }

  if (is.character(spec)) {
    return(purrr::imap(stocks, function(st, st_idx) {
      switch(spec,
             Biomass  = .flat_sel(object@OM@Stock[[st]]@Ages@Classes),
             SBiomass = .maturity_sel(st),
             cli::cli_abort(c("x" = "Unknown {.field Selectivity} {.val {spec}} for index {.val {fleet}}.",
                              "i" = "Use {.val Biomass}, {.val SBiomass}, a vector or array of selectivity-at-age, or a list of these by stock.")))
    }) |> stats::setNames(StockNames))
  }

  if (is.list(spec)) {
    return(purrr::imap(stocks, function(st, st_idx) {
      sel <- if (!is.null(names(spec)) && StockNames[st_idx] %in% names(spec)) {
        spec[[StockNames[st_idx]]]
      } else if (is.null(names(spec)) && length(spec) >= st_idx) {
        spec[[st_idx]]
      }
      if (is.null(sel))
        cli::cli_abort("{.field Selectivity} list for index {.val {fleet}} has no entry for stock {.val {StockNames[st_idx]}}.")
      .user_sel(sel, st, st_idx)
    }) |> stats::setNames(StockNames))
  }

  if (!is.null(spec)) {
    return(purrr::imap(stocks, \(st, st_idx) .user_sel(spec, st, st_idx)) |>
             stats::setNames(StockNames))
  }

  if (!fleet %in% names(object@OM@Fleet[[stocks[1]]]))
    cli::cli_abort(.MsgIndexNotFleet(fleet, names(object@OM@Fleet[[stocks[1]]])))

  purrr::map(object@OM@Fleet[stocks], function(stock_fleets) {
    sel <- stock_fleets[[fleet]]@Selectivity@MeanAtAge
    if (is.null(sim)) {
      sel |> .subset_years() |> ReduceDims()
    } else {
      dd <- dim(sel)
      sel[min(dd[1], sim), , , , drop = FALSE] |>
        .subset_years() |>
        abind::adrop(1)
    }
  })
}

#' Coerce a user-supplied index selectivity to a named array
#'
#' @param sel Numeric vector (named by age, or unnamed of length
#'   `length(AgeClasses)`) or array with named dimnames including `Age`, and
#'   optionally `Sim`, `Year`, and `Area`.
#' @param AgeClasses Age classes of the stock.
#' @param Index,Stock Index and stock names, for error messages.
#'
#' @return An array with an `Age` dimension ordered as `AgeClasses`.
#' @keywords internal
.IndexSelectivityArray <- function(sel, AgeClasses, Index = NULL, Stock = NULL) {
  where <- cli::format_inline("index {.val {Index}}, stock {.val {Stock}}")
  if (!is.numeric(sel))
    cli::cli_abort("{.field Selectivity} for {where} must be numeric, not {.cls {class(sel)}}.")
  if (any(!is.finite(sel)) || any(sel < 0))
    cli::cli_abort("{.field Selectivity} for {where} must be finite and non-negative.")

  Ages <- as.character(AgeClasses)

  if (is.null(dim(sel)) || (length(dim(sel)) == 1L && is.null(names(dimnames(sel))))) {
    nms <- names(sel) %||% dimnames(sel)[[1]]
    if (is.null(nms)) {
      if (length(sel) != length(Ages))
        cli::cli_abort("Unnamed {.field Selectivity} for {where} has length {length(sel)}; needs one value per age class ({length(Ages)}), or names giving the ages.")
      nms <- Ages
    }
    Bad <- setdiff(nms, Ages)
    if (length(Bad))
      cli::cli_abort("{.field Selectivity} for {where} names {cli::qty(length(Bad))}age{?s} {.val {Bad}} not in the stock's age classes ({min(AgeClasses)} to {max(AgeClasses)}).")
    out <- stats::setNames(rep(0, length(Ages)), Ages)
    out[nms] <- as.numeric(sel)
    return(array(out, dim = length(Ages), dimnames = list(Age = Ages)))
  }

  DN <- dimnames(sel)
  if (is.null(names(DN)) || !'Age' %in% names(DN))
    cli::cli_abort("{.field Selectivity} array for {where} needs named dimnames including {.val Age}.")
  Bad <- setdiff(names(DN), c('Sim', 'Age', 'Year', 'Area'))
  if (length(Bad))
    cli::cli_abort("{.field Selectivity} array for {where} has unsupported {cli::qty(length(Bad))}dimension{?s} {.val {Bad}}; allowed: {.val {c('Sim', 'Age', 'Year', 'Area')}}.")
  if (!setequal(DN$Age, Ages))
    cli::cli_abort("{.field Selectivity} array for {where} must cover the stock's age classes exactly ({min(AgeClasses)} to {max(AgeClasses)}).")
  if (identical(DN$Age, Ages)) return(sel)
  abind::asub(sel, match(Ages, DN$Age), match('Age', names(DN)), drop = FALSE)
}

#' Validate an index selectivity specification against the stocks of a complex
#'
#' @param spec An index `Selectivity` specification (see [IndicesObs()]).
#' @param OM An [om-class] object.
#' @param stocks Integer stock indices of the complex.
#' @param Index Index name, for error messages.
#'
#' @return `spec`, invisibly.
#' @keywords internal
.CheckIndexSelectivity <- function(spec, OM, stocks, Index) {
  if (is.null(spec)) return(invisible(spec))
  StockNms <- StockNames(OM)[stocks]
  if (is.character(spec)) {
    if (length(spec) != 1 || !spec %in% c('Biomass', 'SBiomass'))
      cli::cli_abort(c("x" = "Unknown {.field Selectivity} {.val {spec}} for index {.val {Index}}.",
                       "i" = "Use {.val Biomass}, {.val SBiomass}, a vector or array of selectivity-at-age, or a list of these by stock."))
    return(invisible(spec))
  }
  for (i in seq_along(stocks)) {
    sel <- spec
    if (is.list(spec)) {
      sel <- if (!is.null(names(spec))) spec[[StockNms[i]]] else if (length(spec) >= i) spec[[i]]
      if (is.null(sel))
        cli::cli_abort("{.field Selectivity} list for index {.val {Index}} has no entry for stock {.val {StockNms[i]}}.")
    }
    .IndexSelectivityArray(sel, OM@Stock[[stocks[i]]]@Ages@Classes, Index = Index, Stock = StockNms[i])
  }
  invisible(spec)
}

.MsgIndexNotFleet <- function(Index, FleetNames) {
  Closest <- .ClosestName(Index, FleetNames)
  c("x" = cli::format_inline("Index {.val {Index}} is not a fleet in {.code OM@Fleet}, so its {.field Selectivity} must be set."),
    if (!is.null(Closest))
      c("i" = cli::format_inline("If it is the CPUE of fleet {.val {Closest}}, name the index {.val {Closest}} to use that fleet's selectivity.")),
    "i" = cli::format_inline("Otherwise set it with {.code IndexSelectivity(OM, '{Index}') <- 'Biomass'}, or add the index with {.fn AddIndex}. See {.help MSEtool::IndicesObs}."))
}

# closest of `choices` to `x` (ignoring case), or NULL when none is close
.ClosestName <- function(x, choices) {
  if (!length(choices)) return(NULL)
  d <- utils::adist(tolower(x), tolower(choices))[1, ]
  if (min(d) > max(2, nchar(x) %/% 3)) return(NULL)
  choices[which.min(d)]
}


#' Apply index unit conversion to selectivity-weighted numbers
#'
#' @param SimNumberSelectedList List of `[Sim x Age x Year x Area]` or
#'   `[Age x Year x Area]` arrays (one per stock).
#' @param object A `hist`-like object with an `@OM` slot.
#' @param stocks Integer vector of stock indices.
#' @param Units One of `"Biomass"`, `"Number"`, or `"Recruitment"`.
#' @param Years Calendar years.
#' @param sim Optional simulation index.
#' @param nSeasons Number of seasons per year.
#' @param recruitment_years Optional calendar years (names or values) with
#'   observed recruitment index values. When supplied with `nSeasons > 1`, the
#'   first non-zero age class is matched separately in each of these years
#'   (conditioning behaviour). When `NULL` and `nSeasons > 1`, the first
#'   non-zero age class is taken across all years (plot behaviour). When
#'   `nSeasons == 1` or `sim` is set without `recruitment_years`, the minimum
#'   age class is used (historical / projection generation behaviour).
#'
#' @return List of arrays in the same format as the input list.
#' @keywords internal
.ApplyIndexUnits <- function(SimNumberSelectedList,
                             object,
                             stocks,
                             Units,
                             Years,
                             sim = NULL,
                             nSeasons = 1,
                             recruitment_years = NULL) {

  nYear <- length(Years)

  if (Units == 'Number') {
    return(SimNumberSelectedList)
  }

  if (Units == 'Biomass') {
    WeightAtAgeList <- purrr::map(object@OM@Stock[stocks], function(stock) {
      wght <- stock@Weight@MeanAtAge
      if (is.null(sim)) {
        wght |>
          .ArraySubsetYear(Years) |>
          AddDimension('Area') |>
          ExtendAreas(seq_len(dim(SimNumberSelectedList[[1]])[length(dim(SimNumberSelectedList[[1]]))]))
      } else {
        dd <- dim(wght)
        wght[min(dd[1], sim), , , drop = FALSE] |>
          .ArraySubsetYear(Years) |>
          abind::adrop(1) |>
          AddDimension('Area') |>
          ExtendAreas(seq_len(dim(SimNumberSelectedList[[1]])[length(dim(SimNumberSelectedList[[1]]))]))
      }
    })
    return(purrr::map2(SimNumberSelectedList, WeightAtAgeList, function(num, wght) {
      ArrayMultiply(num, .AlignDimsTo(num, wght))
    }))
  }

  if (Units != 'Recruitment') {
    cli::cli_abort(
      'Only {.val Biomass}, {.val Number} and {.val Recruitment} currently supported for {.val Units}',
      .internal = TRUE
    )
  }

  if (nSeasons > 1 && !is.null(recruitment_years)) {
    year_ind <- match(as.character(recruitment_years), as.character(Years))
    nSim_ <- if (is.null(sim)) dim(SimNumberSelectedList[[1]])[1] else 1L
    nArea_ <- dim(SimNumberSelectedList[[1]])[length(dim(SimNumberSelectedList[[1]]))]

    return(purrr::map(SimNumberSelectedList, function(stock) {
      ages <- as.numeric(dimnames(stock)[['Age']])
      if (is.null(sim)) {
        index <- array(NA_real_,
                       dim = c(nSim_, 1L, nYear, nArea_),
                       dimnames = list(Sim = seq_len(nSim_),
                                       Age = min(ages),
                                       Year = Years,
                                       Area = seq_len(nArea_)))
        n <- stock |> .SubsetYear(Years = as.character(recruitment_years))
        first_non_zero_age <- apply(n, 1, function(x) {
          dim(x) <- c(dim(x)[1], prod(dim(x)[-1]))
          which(rowSums(x > 0) > 0)[1]
        })
        for (s in seq_len(nSim_)) {
          index[s, , year_ind, ] <- n[s, first_non_zero_age[s], , , drop = FALSE]
        }
        index
      } else {
        ages <- as.numeric(dimnames(stock)[['Age']])
        stock |> .SubsetYear(Years = as.character(recruitment_years)) |>
          .ArraySubsetAge(min(ages))
      }
    }))
  }

  if (nSeasons > 1 && is.null(sim)) {
    nSim_ <- dim(SimNumberSelectedList[[1]])[1]
    nArea_ <- dim(SimNumberSelectedList[[1]])[length(dim(SimNumberSelectedList[[1]]))]
    return(purrr::map(SimNumberSelectedList, function(stock) {
      ages <- as.numeric(dimnames(stock)[['Age']])
      index <- array(NA_real_,
                     dim = c(nSim_, 1L, nYear, nArea_),
                     dimnames = list(Sim = seq_len(nSim_),
                                     Age = min(ages),
                                     Year = Years,
                                     Area = seq_len(nArea_)))
      first_non_zero_age <- apply(stock, 1, function(x) {
        dim(x) <- c(dim(x)[1], prod(dim(x)[-1]))
        which(rowSums(x > 0) > 0)[1]
      })
      for (s in seq_len(nSim_)) {
        index[s, , , ] <- stock[s, first_non_zero_age[s], , , drop = FALSE]
      }
      index
    }))
  }

  purrr::map(SimNumberSelectedList, function(stock) {
    ages <- as.numeric(dimnames(stock)[['Age']])
    if (length(dim(stock)) == 3L) {
      # Age x Year x Area
      stock[, 1, , drop = FALSE] |> abind::adrop(2)
    } else {
      stock |> .ArraySubsetAge(min(ages))
    }
  })
}


#' Aggregate stock-level index arrays to a nominal index
#'
#' @param SimulatedIndexList List of arrays (one per stock).
#' @param Areas Integer area indices to retain.
#' @param sim Optional simulation index (`NULL` returns an `nSim x nYear` matrix).
#'
#' @return Numeric matrix (`sim = NULL`) or numeric vector (`sim` set).
#' @keywords internal
.AggregateNomIndex <- function(SimulatedIndexList, Areas, sim = NULL) {

  per_stock <- purrr::map(SimulatedIndexList, function(stock) {
    if (length(dim(stock)) == 2L) {
      # Age x Area (single year, single sim)
      stock[, Areas, drop = FALSE] |> SumOverArea() |> SumOverAge()
    } else if (length(dim(stock)) == 3L) {
      # Age x Year x Area
      stock[, , Areas, drop = FALSE] |> SumOverArea() |> SumOverAge()
    } else {
      stock[, , , Areas, drop = FALSE] |> SumOverArea() |> SumOverAge()
    }
  })

  if (is.null(sim)) {
    return(per_stock |>
             List2Array('Stock') |>
             apply(c('Sim', 'Year'), sum))
  }

  Reduce(`+`, per_stock)
}


#' Calculate the nominal (selectivity-weighted) fishery index
#'
#' Shared core for conditioning (`.ConditionObsIndex()`), historical and
#' projected data generation (`.GenHistDataIndices()`, `.GenProjDataIndex()`),
#' and index-fit diagnostics ([PlotIndexFit()]).
#'
#' Computes the selectivity-weighted, area-restricted population index that
#' `Beta` and `Efficiency` are calibrated against:
#' `Observed = Efficiency * NomIndex^Beta * Error`.
#'
#' @param Number_List Named list of numbers-at-age arrays per stock. Each
#'   element is either `[Sim x Age x Year x Area]`, `[Age x Year x Area]`, or
#'   `[Age x Area]` (single year / simulation slice).
#' @param object A `hist`-like object (`Hist`, `Proj`, or `mse`).
#' @param stocks Integer vector of stock indices in the complex.
#' @param fleet Character fleet / index name.
#' @param IndexObs An [indicesobs-class] object.
#' @param Years Numeric calendar year(s) to include.
#' @param sim Optional simulation replicate index.
#' @param timing Numeric fraction of the time step at which the index is
#'   observed, for within-step mortality decay. `NA` skips decay.
#' @param TSIndex Optional integer time-step index(es) matching `Years` in the
#'   population arrays. When `NULL`, matched from array dimnames.
#' @param apply_timing Logical. Apply timing decay? Default `TRUE`.
#' @param Units Index units; defaults to `IndexObs@Units` or `"Biomass"`.
#' @param recruitment_years Optional calendar years for seasonal recruitment
#'   indexing (see [.ApplyIndexUnits()]).
#'
#' @return Numeric matrix (`nSim x nYear`) when `sim` is `NULL`; numeric vector
#'   of length `nYear` (or scalar when `Years` is length 1) when `sim` is set.
#' @keywords internal
.CalcNomIndex <- function(Number_List,
                          object,
                          stocks,
                          fleet,
                          IndexObs,
                          Years,
                          sim = NULL,
                          timing = NA_real_,
                          TSIndex = NULL,
                          apply_timing = TRUE,
                          Units = NULL,
                          recruitment_years = NULL) {

  Years <- as.numeric(Years)
  nArea_ <- nArea(object)
  Areas <- IndexObs@Areas %||NA% seq_len(nArea_)
  nSeasons <- Seasons(object)

  if (is.null(Units)) Units <- IndexObs@Units %||NA% 'Biomass'

  if (is.null(TSIndex)) {
    year_names <- dimnames(Number_List[[1]])[['Year']]
    if (!is.null(year_names)) {
      TSIndex <- match(as.character(Years), year_names)
    } else {
      TSIndex <- seq_along(Years)
    }
  }

  Number_List <- purrr::map(Number_List, function(num) {
    dd <- dim(num)
    if (length(dd) == 4L) {
      num <- num |> .ArraySubsetYear(Years)
      if (!is.null(sim)) {
        dd2 <- dim(num)
        num <- num[min(sim, dd2[1]), , , , drop = FALSE] |> abind::adrop(1)
      }
      num
    } else if (length(dd) == 3L) {
      num |> .ArraySubsetYear(Years)
    } else {
      num
    }
  })

  if (apply_timing && is.finite(timing) && timing > 0) {
    if (is.null(sim)) {
      Number_List <- .DecayNumberListToTiming(Number_List, object, stocks, timing)
    } else {
      Number_List <- purrr::map2(Number_List, stocks, function(num, st) {
        decayed <- .DecayNumbersToTiming(list(num), object, st, sim, TSIndex, timing, Areas)
        decayed[[1]]
      })
    }
  }

  SelectivityAtAgeList <- .ResolveIndexSelectivity(
    object        = object,
    stocks        = stocks,
    fleet         = fleet,
    IndexObs      = IndexObs,
    Years         = Years,
    sim           = sim,
    nArea         = nArea_
  )

  SimNumberSelectedList <- purrr::map2(Number_List, SelectivityAtAgeList, function(num, sel) {
    ArrayMultiply(num, .AlignDimsTo(num, sel))
  })

  SimulatedIndexList <- .ApplyIndexUnits(
    SimNumberSelectedList = SimNumberSelectedList,
    object                = object,
    stocks                = stocks,
    Units                 = Units,
    Years                 = Years,
    sim                   = sim,
    nSeasons              = nSeasons,
    recruitment_years     = recruitment_years
  )

  .AggregateNomIndex(SimulatedIndexList, Areas = Areas, sim = sim)
}
