
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
#' for use by [.CalcNomIndex()]. When `SelectivityAtAge` is `NULL`, fleet
#' selectivity from `object@OM@Fleet` is used.
#'
#' @param object A `hist`-like object with an `@OM` slot.
#' @param stocks Integer vector of stock indices in the complex.
#' @param fleet Character fleet / index name.
#' @param IndexObs An [indicesobs-class] object.
#' @param Years Calendar years spanned by the selectivity arrays.
#' @param SelectivityAtAge Optional selectivity specification. When `NULL`,
#'   fleet selectivity is used. Otherwise may be `character` (`"Biomass"`,
#'   `"SBiomass"`, `"Obs"`), a per-stock `list`, or a single `array`.
#' @param sim Optional simulation index. When set, arrays are sliced to that
#'   replicate and returned without a `Sim` dimension.
#' @param nArea Number of spatial areas. Defaults to [nArea()] of `object`.
#' @param StockNames Optional character vector of stock names (defaults to
#'   [StockNames()] for `stocks`).
#' @param on_missing_obs_sel Optional zero-argument function invoked once per
#'   stock when `SelectivityAtAge = "Obs"` but no selectivity is available on
#'   `IndexObs`.
#'
#' @return A list of selectivity arrays, one element per stock in `stocks`.
#' @keywords internal
.ResolveIndexSelectivity <- function(object,
                                     stocks,
                                     fleet,
                                     IndexObs,
                                     Years,
                                     SelectivityAtAge = NULL,
                                     sim = NULL,
                                     nArea = NULL,
                                     StockNames = NULL,
                                     on_missing_obs_sel = NULL) {

  if (is.null(nArea)) nArea <- nArea(object)
  Areas <- seq_len(nArea)
  if (is.null(StockNames)) StockNames <- StockNames(object@OM)[stocks]
  spec <- SelectivityAtAge

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
      arr[min(sim, dd[1]), , , drop = FALSE] |> abind::adrop(1)
    } else {
      arr
    }
  }

  .subset_years <- function(arr) {
    if (is.null(dimnames(arr)[['Year']])) return(arr)
    arr |> .ArraySubsetYear(Years)
  }

  .with_areas <- function(arr) {
    if (is.null(sim)) {
      arr |>
        AddDimension('Area') |>
        ExtendAreas(Areas) |>
        ReduceDims()
    } else {
      arr |>
        AddDimension('Area') |>
        ExtendAreas(Areas) |>
        DropDimension(c('Sim', 'Year'))
    }
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

  .obs_sel <- function(st_idx, st) {
    sel <- IndexObs@Selectivity
    AgeClasses <- object@OM@Stock[[st]]@Ages@Classes

    if (inherits(sel, 'array')) {
      return(sel |> .subset_years() |> .slice_sim() |> .with_areas())
    }

    if (inherits(sel, 'list')) {
      stock_sel <- sel[[st_idx]] %||% sel[[st]]
      if (is.null(stock_sel)) {
        if (!is.null(on_missing_obs_sel)) on_missing_obs_sel()
        return(.flat_sel(AgeClasses))
      }
      return(stock_sel |> .subset_years() |> .slice_sim() |> .with_areas())
    }

    if (!is.null(on_missing_obs_sel)) on_missing_obs_sel()
    .flat_sel(AgeClasses)
  }

  if (is.list(spec) && !is.character(spec)) {
    return(purrr::imap(spec, function(stock_sel, nm) {
      arr <- stock_sel |>
        .subset_years() |>
        .slice_sim() |>
        AddDimension('Area') |>
        ExtendAreas(Areas)
      if (is.null(sim)) ReduceDims(arr) else DropDimension(arr, c('Sim', 'Year'))
    }))
  }

  if (inherits(spec, 'array')) {
    sel_arr <- spec |> .subset_years() |> .slice_sim() |> .with_areas()
    return(stats::setNames(rep(list(sel_arr), length(stocks)), StockNames))
  }

  if (is.character(spec)) {
    return(purrr::imap(stocks, function(st, st_idx) {
      AgeClasses <- object@OM@Stock[[st]]@Ages@Classes
      switch(spec,
             Biomass  = .flat_sel(AgeClasses),
             SBiomass = .maturity_sel(st),
             Obs      = .obs_sel(st_idx, st),
             cli::cli_abort('Unknown selectivity type: {.val {spec}}', .internal = TRUE))
    }))
  }

  # Fleet selectivity (default)
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
#' @param SelectivityAtAge Optional selectivity specification passed to
#'   [.ResolveIndexSelectivity()]. When `NULL`, fleet selectivity is used.
#' @param sim Optional simulation replicate index.
#' @param timing Numeric fraction of the time step at which the index is
#'   observed, for within-step mortality decay. `NA` skips decay.
#' @param TSIndex Optional integer time-step index(es) matching `Years` in the
#'   population arrays. When `NULL`, matched from array dimnames.
#' @param apply_timing Logical. Apply timing decay? Default `TRUE`.
#' @param Units Index units; defaults to `IndexObs@Units` or `"Biomass"`.
#' @param recruitment_years Optional calendar years for seasonal recruitment
#'   indexing (see [.ApplyIndexUnits()]).
#' @param on_missing_obs_sel Optional callback when `"Obs"` selectivity is missing.
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
                          SelectivityAtAge = NULL,
                          sim = NULL,
                          timing = NA_real_,
                          TSIndex = NULL,
                          apply_timing = TRUE,
                          Units = NULL,
                          recruitment_years = NULL,
                          on_missing_obs_sel = NULL) {

  Years <- as.numeric(Years)
  nArea_ <- nArea(object)
  Areas <- IndexObs@Areas %||% seq_len(nArea_)
  nSeasons <- Seasons(object)

  if (is.null(Units)) Units <- IndexObs@Units %||% 'Biomass'

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
      Number_List <- purrr::imap(Number_List, function(num, idx) {
        st <- stocks[idx]
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
    SelectivityAtAge = SelectivityAtAge,
    sim           = sim,
    nArea         = nArea_,
    on_missing_obs_sel = on_missing_obs_sel
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
