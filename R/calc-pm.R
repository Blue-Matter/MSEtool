#' Performance Metrics
#'
#' `PM_*` functions evaluate a performance metric against an [mse-class]
#' object, or a `list` of [mse-class] objects (combined via [CombineMSE()]
#' before calculation, so simulations from every list element are pooled
#' and treated as one analysis). See [PM-equations] for the mathematical
#' definition of each metric.
#'
#' `PM_Removals`, `PM_Landings` (and `PM_Yield`, which is a thin wrapper
#' around the two), `PM_RelYield`, `PM_AAVY`, and `PM_Stability` sum their
#' series (TAC, removals, or landings) over the stocks in each
#' `OM@Complexes` group by default; pass `Stocks` to override with an
#' explicit set of stock names.
#'
#' `PM_FFMSY` and `PM_Status` are evaluated at the *complex* level
#' (`OM@Complexes`), not per stock, because `FMSY` is a single value
#' optimised jointly across a complex's member stocks (see [F_FMSY()]).
#'
#' `PM_Status` sums the status metric and its MSY reference point across each
#' complex's spawning stock(s) before dividing, so both sides of the joint
#' status test are assessed at the same aggregation level rather than pairing
#' a per-stock ratio with an identical, complex-wide F flag.
#'
#' `PM_Status` and `PM_Safety` accept a `Definition` argument selecting
#' whether stock status is assessed on spawning biomass (`SBiomass()`, the
#' default) or spawning production (`SProduction()`). `PM_SBSBMSY`/`PM_SBSBlim`
#' and `PM_SPSPMSY`/`PM_SPSPlim` are the corresponding single-metric,
#' non-switchable counterparts for spawning biomass and spawning production
#' respectively.
#'
#' @param object An [mse-class] object, or a `list` of [mse-class] objects.
#' @param Ref Numeric, or `NULL`. Reference/threshold value for `PM_FFMSY`/
#'   `PM_SBSBMSY`/`PM_SPSPMSY`'s probability metric (default `1`). Pass
#'   `Ref = NULL` instead to get the mean projected ratio itself (`Stat`
#'   averaged across simulations, on its natural scale) rather than a
#'   probability against a threshold -- `Prob` is left `NA` in that case,
#'   consistent with other natural-scale metrics such as `PM_Yield()`.
#' @param Lim Numeric, or a named numeric vector keyed by stock name. Limit
#'   reference point, expressed as a fraction of `SBMSY` (`PM_SBSBlim`) or
#'   `SPMSY` (`PM_SPSPlim`), or of `SBMSY`/`SPMSY` per `Definition`
#'   (`PM_Safety`) -- e.g. `Lim = 0.5` tests against half of `SBMSY`.
#' @param Definition Character. Which stock-status metric to use in
#'   `PM_Status` and `PM_Safety`: `"SBiomass"` (spawning biomass, default) or
#'   `"SProduction"` (spawning production).
#' @param Type Character. For `PM_Yield`, which catch metric to report:
#'   `"Removals"` (landings + discards, default) or `"Landings"`. For
#'   `PM_AAVY`/`PM_Stability`, which series to assess interval-to-interval
#'   variability in: `"TAC"` (the MP's recommendation, default),
#'   `"Removals"`, or `"Landings"`.
#' @param Year Numeric. The single projection year in which to evaluate
#'   rebuilding, used by `PM_Rebuild`.
#' @param Target Numeric. Rebuilding target, expressed as a multiple of
#'   `SBMSY`, used by `PM_Rebuild`. Default `1`.
#' @param Threshold Numeric. Maximum acceptable interval-to-interval change
#'   in the series selected by `Type`, used by `PM_Stability`.
#' @param Fleets Character vector of fleet names to include in `PM_AAVE`.
#'   Default `NULL` uses all fleets.
#' @param Years Numeric vector of projection years to evaluate over. Default
#'   `NULL` uses all projection years in which the MP was active, i.e.
#'   excluding any "interim" years before `OM@MPStartYear` (see
#'   [om-class]).
#' @param Stocks Character vector of stock names to include. Default `NULL`
#'   uses the automatic complex/spawning-stock grouping described above.
#' @param silent Logical. Suppress the [CombineMSE()] summary message when
#'   `object` is a `list`. Default `TRUE`.
#'
#' @return A [pm-class] object.
#'
#' @seealso [PM-equations] for the mathematical definition of each metric.
#'
#' @name PM
NULL

#' Performance Metric Equations
#'
#' Mathematical definitions of the performance metrics computed by the
#' [PM] functions. Notation: `s` indexes simulation replicates, `y` indexes
#' the projection years in the evaluation window, and each metric is
#' computed per management procedure (MP).
#'
#' @section Status:
#' `PM_FFMSY`: fishing mortality relative to \eqn{F_{MSY}}{FMSY},
#' \deqn{F_{s,y} / F_{MSY,s}}{F[s,y] / FMSY[s]}
#' with the probability metric \eqn{P(F/F_{MSY} < Ref)}{P(F/FMSY < Ref)}.
#'
#' `PM_SBSBMSY` / `PM_SPSPMSY`: spawning biomass or spawning production
#' relative to its value at MSY,
#' \deqn{SB_{s,y} / SB_{MSY,s} \quad\text{or}\quad SP_{s,y} / SP_{MSY,s}}{SB[s,y] / SBMSY[s]  or  SP[s,y] / SPMSY[s]}
#' with the probability metric \eqn{P(SB/SB_{MSY} > Ref)}{P(SB/SBMSY > Ref)}
#' (or the SP equivalent).
#'
#' `PM_Status`: the joint probability that a complex is neither overfished
#' nor experiencing overfishing,
#' \deqn{P\left(\frac{SB_{y}}{SB_{MSY}} > 1 \ \text{and}\ \frac{F_{y}}{F_{MSY}} < 1\right)}{P( SB/SBMSY > 1  and  F/FMSY < 1 )}
#' evaluated at the complex level, where `SB` is spawning biomass or spawning
#' production according to `Definition`.
#'
#' @section Safety:
#' `PM_SBSBlim` / `PM_SPSPlim`: spawning biomass or spawning production
#' relative to MSY, relative in turn to a limit fraction `Lim` of `SBMSY`/
#' `SPMSY`,
#' \deqn{\frac{SB_{s,y}/SB_{MSY,s}}{Lim_s} \quad\text{or}\quad \frac{SP_{s,y}/SP_{MSY,s}}{Lim_s}}{(SB[s,y]/SBMSY[s]) / Lim[s]  or  (SP[s,y]/SPMSY[s]) / Lim[s]}
#' with the probability metric \eqn{P(\frac{SB/SB_{MSY}}{Lim} > 1)}{P((SB/SBMSY)/Lim > 1)}
#' (or the SP equivalent) -- i.e. the probability that `SB/SBMSY` exceeds
#' `Lim`.
#'
#' `PM_Safety`: the probability that the stock-status metric, relative to
#' MSY, never falls below the limit fraction `Lim` at any point during the
#' projection,
#' \deqn{P\left(\min_{y \in Y} \frac{SB_{s,y}}{SB_{MSY,s}} > Lim_s\right)}{P( min over y in Y of SB[s,y]/SBMSY[s] > Lim[s] )}
#' where `SB` is spawning biomass or spawning production according to
#' `Definition`.
#'
#' @section Rebuild:
#' `PM_Rebuild`: for stocks overfished (\eqn{SB/SB_{MSY} < 1}{SB/SBMSY < 1})
#' at the end of the historical period, the probability that the stock has
#' rebuilt to a target multiple of \eqn{SB_{MSY}}{SBMSY} by a target year,
#' \deqn{P\left(\frac{SB_{s,\mathrm{Year}}}{SB_{MSY,s}} > \mathrm{Target}\right)}{P( SB[s,Year] / SBMSY[s] > Target )}
#'
#' @section Yield:
#' `PM_Yield` / `PM_Removals` / `PM_Landings`: mean catch over the evaluation
#' window,
#' \deqn{\frac{1}{|Y|}\sum_{y \in Y} C_{s,y}}{(1/|Y|) * sum over y in Y of C[s,y]}
#' where `C` is removals (landings + discards), or landings only.
#'
#' `PM_RelYield`: mean catch over the evaluation window, expressed relative
#' to MSY yield,
#' \deqn{\frac{1}{|Y|}\sum_{y \in Y} C_{s,y} \Big/ MSY_s}{[(1/|Y|) * sum over y in Y of C[s,y]] / MSY[s]}
#' with the probability metric \eqn{P(\mathrm{RelYield} > Ref)}{P(RelYield > Ref)}
#' when `Ref` is supplied.
#'
#' @section Stability:
#' `PM_AAVY` / `PM_AAVE`: average annual variability in TAC, removals,
#' landings (per `Type`), or effort, across management intervals,
#' \deqn{\frac{1}{|Y|-1}\sum_{y \in Y \setminus \{y_1\}} \frac{|C_{s,y} - C_{s,y-1}|}{C_{s,y-1}}}{(1/(|Y|-1)) * sum over y in Y (excluding the first year) of |C[s,y] - C[s,y-1]| / C[s,y-1]}
#'
#' `PM_Stability`: the probability that the series selected by `Type`
#' changes by less than a threshold amount between one management interval
#' and the next,
#' \deqn{P\left(\frac{|C_{s,y} - C_{s,y-1}|}{C_{s,y-1}} < \mathrm{Threshold}\right)}{P( |C[s,y] - C[s,y-1]| / C[s,y-1] < Threshold )}
#' evaluated per interval `y` and averaged across the evaluation window and
#' simulations.
#'
#' @section Stat, Prob, and Mean:
#' Every `PM_*` function returns a [pm-class] object with three related
#' summaries: `Stat` is the raw metric above, computed per simulation (and
#' averaged over the evaluation window for the Yield-family metrics); `Prob`
#' is the per-simulation probability/indicator of meeting the stated
#' objective (`NA` for metrics with no such objective, e.g. `PM_Yield`);
#' `Mean` is `Prob` averaged across simulations, or — for metrics with no
#' `Prob` — `Stat` averaged across simulations instead.
#'
#' @seealso [PM] for the full list of performance metric functions and their
#'   arguments.
#' @name PM-equations
NULL

# Cache so repeated PM_* calls on the same `MSE_List` don't re-run CombineMSE.
.PMCombineCache <- new.env(parent = emptyenv())

.CoercePMInput <- function(object, silent = TRUE) {
  if (is.list(object) && !isS4(object)) {
    key <- digest::digest(object, algo = "spookyhash")
    if (!identical(.PMCombineCache$key, key)) {
      .PMCombineCache$key   <- key
      .PMCombineCache$value <- CombineMSE(object, silent = silent)
    }
    object <- .PMCombineCache$value
  }
  .CheckClass(object, 'mse', 'object')
  object
}

.PMArray <- function(df, valcol, group_col = 'Stock') {
  agg <- df |>
    dplyr::group_by(.data$Sim, .data[[group_col]], .data$MP) |>
    dplyr::summarise(Value = mean(.data[[valcol]], na.rm = TRUE), .groups = 'drop')

  simN   <- sort(unique(agg$Sim))
  grpN   <- unique(agg[[group_col]])
  mpN    <- unique(agg$MP)

  arr <- array(NA_real_, dim = c(length(simN), length(grpN), length(mpN)))
  dimnames(arr) <- stats::setNames(
    list(as.character(simN), as.character(grpN), as.character(mpN)),
    c('Sim', group_col, 'MP')
  )

  idx <- cbind(match(agg$Sim, simN), match(agg[[group_col]], grpN), match(agg$MP, mpN))
  arr[idx] <- agg$Value
  arr
}

.PMMean <- function(ProbArr) {
  d  <- dim(ProbArr)
  dn <- dimnames(ProbArr)
  out <- apply(ProbArr, c(2, 3), mean, na.rm = TRUE)
  dim(out) <- d[c(2, 3)]
  dimnames(out) <- dn[c(2, 3)]
  out
}

.BuildPM <- function(df, Ref, Years, op, Name, Caption, group_col = 'Stock', OM = NULL) {
  if ('Period' %in% names(df))
    df <- df[df$Period == 'Projection', ]
  if (!is.null(OM))
    df <- .FilterMPActiveYears(df, OM)
  if (!is.null(Years))
    df <- df[df$Year %in% Years, ]
  YearsOut <- if ('Year' %in% names(df)) sort(unique(df$Year)) else numeric(0)

  StatArr <- .PMArray(df, 'Value', group_col)

  if (is.null(op)) {
    ProbArr <- StatArr
    ProbArr[] <- NA_real_
    MeanArr <- .PMMean(StatArr)
  } else {
    df$.Met <- op(df$Value, Ref)
    ProbArr <- .PMArray(df, '.Met', group_col)
    MeanArr <- .PMMean(ProbArr)
  }

  methods::new('pm',
               Name    = Name,
               Caption = Caption,
               Stat    = StatArr,
               Ref     = if (is.null(Ref)) NA_real_ else Ref,
               Prob    = ProbArr,
               Mean    = MeanArr,
               MPs     = sort(unique(df$MP)),
               Years   = YearsOut)
}

.ResolveRefByStock <- function(Ref, stockcol) {
  if (length(Ref) == 1)
    return(rep(unname(Ref), length(stockcol)))
  if (!is.null(names(Ref))) {
    out <- Ref[stockcol]
    if (anyNA(out))
      cli::cli_abort("`Ref` is a named vector but does not cover stock(s): {.val {unique(stockcol[is.na(out)])}}.")
    return(unname(out))
  }
  cli::cli_abort("`Ref` must be length 1 or a named numeric vector keyed by stock name.")
}

# Identify the spawning ("female") stock in each sex-structured complex.
.SpawningStockNames <- function(OM) {
  stockNms <- StockNames(OM)
  if (length(stockNms) <= 1)
    return(stockNms)

  fem <- .FemaleStockNames(OM)
  if (!fem$ambiguous)
    return(fem$stocks)

  complexes <- OM@Complexes
  keep <- character(0)
  for (cx in complexes) {
    nms <- stockNms[cx]
    if (length(nms) == 1) {
      keep <- c(keep, nms)
      next
    }
    resolved <- nms[nms %in% fem$stocks]
    if (length(resolved) == 1) {
      keep <- c(keep, resolved)
      next
    }
    self_ref <- purrr::keep(cx, \(i) {
      spf <- OM@Stock[[i]]@SRR@SPFrom
      length(spf) == 0 ||
        (is.character(spf) && identical(spf, stockNms[i])) ||
        (is.numeric(spf)   && spf == i)
    })
    if (length(self_ref) == 1) {
      keep <- c(keep, stockNms[self_ref])
    } else {
      keep <- c(keep, nms)
    }
  }
  keep
}

.ResolveComplexGroups <- function(OM, Stocks) {
  stockNms <- StockNames(OM)
  if (!is.null(Stocks))
    return(stats::setNames(list(Stocks), paste(Stocks, collapse = ' + ')))

  complexes <- OM@Complexes
  if (!length(complexes))
    return(stats::setNames(as.list(stockNms), stockNms))

  grp <- purrr::imap(complexes, \(idx, nm) stockNms[idx])
  leftover <- setdiff(stockNms, unlist(grp))
  if (length(leftover))
    grp <- c(grp, stats::setNames(as.list(leftover), leftover))
  grp
}


.FilterStockDim <- function(arr, keep) {
  dn        <- dimnames(arr)
  stock_pos <- match('Stock', names(dn))
  idx_list              <- rep(list(quote(expr = )), length(dn))
  idx_list[[stock_pos]] <- which(dn[[stock_pos]] %in% keep)
  do.call('[', c(list(arr), idx_list, list(drop = FALSE)))
}

.FilterMPActiveYears <- function(df, OM) {
  if (is.null(OM@MPStartYear))
    return(df)
  df[floor(df$Year) >= OM@MPStartYear, , drop = FALSE]
}

.FilterManagementYears <- function(df, object) {
  YearsProj <- Years(object@OM, 'Projection')
  mpNames   <- unique(df$MP)
  keep <- lapply(mpNames, function(mp) {
    Interval  <- .ResolveInterval(object@OM@Interval, mp, object@MPs[[mp]])
    ManageYrs <- .CalcManagementYears(YearsProj, Interval)
    if (!is.null(object@OM@MPStartYear))
      ManageYrs <- ManageYrs[floor(ManageYrs) >= object@OM@MPStartYear]
    df$MP == mp & df$Year %in% ManageYrs
  })
  df[Reduce(`|`, keep), ]
}

.GroupedCatch <- function(object, Stocks, ManagementOnly = FALSE, FUN = Removals) {
  df <- FUN(object, df = TRUE, byFleet = FALSE, byAge = FALSE,
            bySize = FALSE, byArea = FALSE, Reduce = FALSE)
  df <- df[df$Period == 'Projection', ]
  df <- .FilterMPActiveYears(df, object@OM)

  if (ManagementOnly)
    df <- .FilterManagementYears(df, object)

  groups <- .ResolveComplexGroups(object@OM, Stocks)
  purrr::imap(groups, \(stk, grpName) {
    df[df$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
      dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = grpName)
  }) |> dplyr::bind_rows()
}

.GroupedTAC <- function(object, Stocks, ManagementOnly = FALSE) {
  df <- TACs(object)
  df <- df[df$Period == 'Projection', ]
  df <- .FilterMPActiveYears(df, object@OM)

  if (ManagementOnly)
    df <- .FilterManagementYears(df, object)

  groups <- .ResolveComplexGroups(object@OM, Stocks)
  purrr::imap(groups, \(stk, grpName) {
    df[df$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
      dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = grpName)
  }) |> dplyr::bind_rows()
}

# Shared dispatch for the stability-family PMs (PM_AAVY, PM_Stability): TAC
# (the recommendation issued by the MP) vs. realised removals/landings.
.StabilityLabel <- c(TAC = 'TAC', Removals = 'removals', Landings = 'landings')

.GroupedStabilitySeries <- function(object, Type, Stocks, ManagementOnly = TRUE) {
  switch(Type,
    TAC      = .GroupedTAC(object, Stocks, ManagementOnly = ManagementOnly),
    Removals = .GroupedCatch(object, Stocks, ManagementOnly = ManagementOnly, FUN = Removals),
    Landings = .GroupedCatch(object, Stocks, ManagementOnly = ManagementOnly, FUN = Landings)
  )
}

.GroupedEffort <- function(object, Fleets, ManagementOnly = FALSE) {
  df <- Effort(object, df = TRUE)
  df <- df[df$Period == 'Projection', ]
  df <- .FilterMPActiveYears(df, object@OM)

  if (ManagementOnly)
    df <- .FilterManagementYears(df, object)
  if (!is.null(Fleets))
    df <- df[df$Fleet %in% Fleets, ]

  df |>
    dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
    dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
    dplyr::mutate(Stock = 'All')
}


.AAV <- function(df, group_col = 'Stock') {
  df |>
    dplyr::arrange(.data$Sim, .data[[group_col]], .data$MP, .data$Year) |>
    dplyr::group_by(.data$Sim, .data[[group_col]], .data$MP) |>
    dplyr::mutate(Prev = dplyr::lag(.data$Value)) |>
    dplyr::filter(!is.na(.data$Prev)) |>
    dplyr::mutate(Value = abs(.data$Value - .data$Prev) / .data$Prev) |>
    dplyr::ungroup() |>
    dplyr::select('Sim', dplyr::all_of(group_col), 'Year', 'MP', 'Value')
}

# ---- Status: F/FMSY, SB/SBMSY, joint status --------------------------------

#' @rdname PM
#' @export
PM_FFMSY <- function(object, Ref = 1, Years = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- F_FMSY(object, df = TRUE, Reduce = FALSE)
  if (is.null(Ref))
    return(.BuildPM(df, Ref = NA_real_, Years = Years, op = NULL,
             Name = 'F_FMSY', Caption = 'Mean projected F/FMSY', OM = object@OM))
  .BuildPM(df, Ref = Ref, Years = Years, op = `<`,
           Name = 'F_FMSY', Caption = paste0('P(F < ', Ref, ' FMSY)'), OM = object@OM)
}
class(PM_FFMSY) <- 'pm'

#' @rdname PM
#' @export
PM_SBSBMSY <- function(object, Ref = 1, Years = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- SB_SBMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Stock %in% .SpawningStockNames(object@OM), ]
  if (is.null(Ref))
    return(.BuildPM(df, Ref = NA_real_, Years = Years, op = NULL,
             Name = 'SB_SBMSY', Caption = 'Mean projected SB/SBMSY', OM = object@OM))
  .BuildPM(df, Ref = Ref, Years = Years, op = `>`,
           Name = 'SB_SBMSY', Caption = paste0('P(SB > ', Ref, ' SBMSY)'), OM = object@OM)
}
class(PM_SBSBMSY) <- 'pm'

#' @rdname PM
#' @export
PM_SPSPMSY <- function(object, Ref = 1, Years = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- SP_SPMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Stock %in% .SpawningStockNames(object@OM), ]
  if (is.null(Ref))
    return(.BuildPM(df, Ref = NA_real_, Years = Years, op = NULL,
             Name = 'SP_SPMSY', Caption = 'Mean projected SP/SPMSY', OM = object@OM))
  .BuildPM(df, Ref = Ref, Years = Years, op = `>`,
           Name = 'SP_SPMSY', Caption = paste0('P(SP > ', Ref, ' SPMSY)'), OM = object@OM)
}
class(PM_SPSPMSY) <- 'pm'


.StockStatusSeries <- function(object, Definition) {
  if (Definition == 'SProduction') {
    list(value = SProduction(object, df = FALSE, Reduce = FALSE),
         msy   = SPMSY(object))
  } else {
    list(value = SBiomass(object, df = FALSE, Reduce = FALSE),
         msy   = SBMSY(object))
  }
}

#' @rdname PM
#' @export
PM_Status <- function(object, Definition = c('SBiomass', 'SProduction'),
                       Years = NULL, silent = TRUE) {
  Definition <- match.arg(Definition)
  object <- .CoercePMInput(object, silent)
  spawn_stocks <- .SpawningStockNames(object@OM)
  series <- .StockStatusSeries(object, Definition)

  sb_arr    <- .FilterStockDim(series$value, spawn_stocks)
  sbmsy_arr <- .FilterStockDim(series$msy, spawn_stocks)

  sb_complex    <- .AggregateStockToComplex(sb_arr, object@OM, sum, strict = FALSE)
  sbmsy_complex <- .AggregateStockToComplex(sbmsy_arr, object@OM, sum, strict = FALSE)

  target_years  <- dimnames(sb_complex)[['Year']]
  sbmsy_aligned <- .AlignDenomYears(sbmsy_complex, target_years) |>
    AddDimension('MP', val = dimnames(sb_complex)[['MP']])

  sb_df <- ArrayDivide(sb_complex, sbmsy_aligned) |>
    Array2DF() |>
    dplyr::rename(Complex = 'Stock', SB = 'Value')

  ff <- F_FMSY(object, df = TRUE, Reduce = FALSE)
  ff <- ff[ff$Period == 'Projection', ] |>
    dplyr::rename(Complex = 'Stock') |>
    dplyr::select('Sim', 'Complex', 'Year', 'MP', F = 'Value')

  sb_df <- .FilterMPActiveYears(sb_df, object@OM)
  ff    <- .FilterMPActiveYears(ff, object@OM)

  if (!is.null(Years)) {
    sb_df <- sb_df[sb_df$Year %in% Years, ]
    ff    <- ff[ff$Year %in% Years, ]
  }

  joined <- dplyr::inner_join(sb_df, ff, by = c('Sim', 'Complex', 'Year', 'MP'))
  joined$Value <- as.numeric(joined$SB > 1 & joined$F < 1)
  joined <- joined |> dplyr::rename(Stock = 'Complex')

  .BuildPM(joined, Ref = 1, Years = NULL, op = \(x, r) x >= r,
           Name = 'Status',
           Caption = paste0('P(', Definition, ' > ', Definition, 'MSY & F < FMSY), complex-level'))
}
class(PM_Status) <- 'pm'

# ---- Safety: limit reference points ----------------------------------------

#' @rdname PM
#' @export
PM_SBSBlim <- function(object, Lim, Years = NULL, silent = TRUE) {
  if (missing(Lim) || is.null(Lim))
    cli::cli_abort("`Lim` must be supplied.")
  object <- .CoercePMInput(object, silent)

  df <- SB_SBMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Stock %in% .SpawningStockNames(object@OM), ]
  df$Value <- df$Value / .ResolveRefByStock(Lim, df$Stock)

  .BuildPM(df, Ref = 1, Years = Years, op = `>`,
           Name = 'SB_SBlim', Caption = 'P(SB/SBMSY > Lim)', OM = object@OM)
}
class(PM_SBSBlim) <- 'pm'

#' @rdname PM
#' @export
PM_SPSPlim <- function(object, Lim, Years = NULL, silent = TRUE) {
  if (missing(Lim) || is.null(Lim))
    cli::cli_abort("`Lim` must be supplied.")
  object <- .CoercePMInput(object, silent)

  df <- SP_SPMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Stock %in% .SpawningStockNames(object@OM), ]
  df$Value <- df$Value / .ResolveRefByStock(Lim, df$Stock)

  .BuildPM(df, Ref = 1, Years = Years, op = `>`,
           Name = 'SP_SPlim', Caption = 'P(SP/SPMSY > Lim)', OM = object@OM)
}
class(PM_SPSPlim) <- 'pm'

#' @rdname PM
#' @export
PM_Safety <- function(object, Lim, Definition = c('SBiomass', 'SProduction'),
                       Years = NULL, silent = TRUE) {
  if (missing(Lim) || is.null(Lim))
    cli::cli_abort("`Lim` must be supplied.")
  Definition <- match.arg(Definition)
  object <- .CoercePMInput(object, silent)

  df <- if (Definition == 'SProduction')
    SP_SPMSY(object, df = TRUE, Reduce = FALSE)
  else
    SB_SBMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Period == 'Projection' & df$Stock %in% .SpawningStockNames(object@OM), ]
  df <- .FilterMPActiveYears(df, object@OM)
  if (!is.null(Years))
    df <- df[df$Year %in% Years, ]
  YearsOut <- sort(unique(df$Year))

  df$Ref   <- .ResolveRefByStock(Lim, df$Stock)
  df$Above <- df$Value > df$Ref

  StatArr <- .PMArray(df, 'Value')
  safe_df <- df |>
    dplyr::group_by(.data$Sim, .data$Stock, .data$MP) |>
    dplyr::summarise(Met = as.numeric(all(.data$Above)), .groups = 'drop')
  ProbArr <- .PMArray(safe_df, 'Met')
  MeanArr <- .PMMean(ProbArr)

  metric <- if (Definition == 'SProduction') 'SP' else 'SB'
  methods::new('pm',
               Name = paste0(metric, '_Safety'),
               Caption = paste0('P(', metric, '/', metric, 'MSY never drops below Lim during the projection)'),
               Stat = StatArr, Ref = NA_real_, Prob = ProbArr, Mean = MeanArr,
               MPs = sort(unique(df$MP)), Years = YearsOut)
}
class(PM_Safety) <- 'pm'

#' @rdname PM
#' @export
PM_Rebuild <- function(object, Year, Target = 1, silent = TRUE) {
  if (missing(Year))
    cli::cli_abort("`Year` (the rebuilding target year) must be supplied.")
  object <- .CoercePMInput(object, silent)

  sb <- SB_SBMSY(object, df = TRUE, Reduce = FALSE)
  sb <- sb[sb$Stock %in% .SpawningStockNames(object@OM), ]

  lastHistYr <- max(Years(object@OM, 'Historical'))
  baseline <- sb[sb$Period == 'Historical' & sb$Year == lastHistYr, ] |>
    dplyr::group_by(.data$Stock) |>
    dplyr::summarise(Overfished = mean(.data$Value) < 1, .groups = 'drop')
  overfished_stocks <- baseline$Stock[baseline$Overfished]

  if (!length(overfished_stocks))
    cli::cli_alert_info("No stocks are overfished (SB < SBMSY) at the end of the historical period.")

  target_df <- sb[sb$Period == 'Projection' & sb$Year == Year &
                    sb$Stock %in% overfished_stocks, ]
  target_df <- .FilterMPActiveYears(target_df, object@OM)
  target_df$Value <- target_df$Value / Target

  .BuildPM(target_df, Ref = 1, Years = Year, op = `>`,
           Name = 'Rebuild', Caption = paste0('P(SB > ', Target, ' SBMSY by ', Year, ')'))
}
class(PM_Rebuild) <- 'pm'

# ---- Yield ------------------------------------------------------------------

.YieldPM <- function(object, Years, Stocks, FUN, Name, Caption, silent) {
  object <- .CoercePMInput(object, silent)
  df <- .GroupedCatch(object, Stocks, FUN = FUN)
  .BuildPM(df, Ref = NA_real_, Years = Years, op = NULL, Name = Name, Caption = Caption)
}

#' @rdname PM
#' @export
PM_Removals <- function(object, Years = NULL, Stocks = NULL, silent = TRUE) {
  .YieldPM(object, Years, Stocks, Removals, 'Removals',
           'Mean projected removals (landings + discards)', silent)
}
class(PM_Removals) <- 'pm'

#' @rdname PM
#' @export
PM_Landings <- function(object, Years = NULL, Stocks = NULL, silent = TRUE) {
  .YieldPM(object, Years, Stocks, Landings, 'Landings',
           'Mean projected landings', silent)
}
class(PM_Landings) <- 'pm'

#' @rdname PM
#' @export
PM_Yield <- function(object, Years = NULL, Stocks = NULL,
                      Type = c('Removals', 'Landings'), silent = TRUE) {
  Type <- match.arg(Type)
  out <- switch(Type,
    Removals = PM_Removals(object, Years, Stocks, silent),
    Landings = PM_Landings(object, Years, Stocks, silent))
  out@Name    <- 'Yield'
  out@Caption <- paste0('Mean projected yield (', Type, ')')
  out
}
class(PM_Yield) <- 'pm'

#' @rdname PM
#' @export
PM_RelYield <- function(object, Ref = NULL, Years = NULL, Stocks = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- .GroupedCatch(object, Stocks)

  msy_df <- Array2DF(MSYLandings(object)) |>
    dplyr::group_by(.data$Sim, .data$Stock) |>
    dplyr::summarise(MSY = mean(.data$Value, na.rm = TRUE), .groups = 'drop')

  groups <- .ResolveComplexGroups(object@OM, Stocks)
  grp_msy <- purrr::imap(groups, \(stk, nm) {
    msy_df[msy_df$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim) |>
      dplyr::summarise(MSY = sum(.data$MSY, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = nm)
  }) |> dplyr::bind_rows()

  df <- dplyr::left_join(df, grp_msy, by = c('Sim', 'Stock'))
  df$Value <- df$Value / df$MSY

  op <- if (is.null(Ref)) NULL else `>`
  .BuildPM(df, Ref = Ref, Years = Years, op = op,
           Name = 'RelYield', Caption = 'Yield relative to MSY yield')
}
class(PM_RelYield) <- 'pm'

# ---- Stability ----------------------------------------------------------------

#' @rdname PM
#' @export
PM_AAVY <- function(object, Type = c('TAC', 'Removals', 'Landings'), Years = NULL,
                     Stocks = NULL, silent = TRUE) {
  Type <- match.arg(Type)
  object <- .CoercePMInput(object, silent)
  ydf <- .GroupedStabilitySeries(object, Type, Stocks, ManagementOnly = TRUE)
  if (!is.null(Years))
    ydf <- ydf[ydf$Year %in% Years, ]
  aav <- .AAV(ydf)
  .BuildPM(aav, Ref = NA_real_, Years = NULL, op = NULL,
           Name = 'AAVY',
           Caption = paste0('Average annual variability in ', .StabilityLabel[[Type]],
                             ' across management intervals'))
}
class(PM_AAVY) <- 'pm'

#' @rdname PM
#' @export
PM_AAVE <- function(object, Years = NULL, Fleets = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  edf <- .GroupedEffort(object, Fleets, ManagementOnly = TRUE)
  if (!is.null(Years))
    edf <- edf[edf$Year %in% Years, ]
  aav <- .AAV(edf)
  .BuildPM(aav, Ref = NA_real_, Years = NULL, op = NULL,
           Name = 'AAVE', Caption = 'Average annual variability in effort across management intervals')
}
class(PM_AAVE) <- 'pm'

#' @rdname PM
#' @export
PM_Stability <- function(object, Threshold, Type = c('TAC', 'Removals', 'Landings'),
                          Years = NULL, Stocks = NULL, silent = TRUE) {
  if (missing(Threshold))
    cli::cli_abort("`Threshold` (maximum acceptable interval-to-interval change) must be supplied.")
  Type <- match.arg(Type)
  object <- .CoercePMInput(object, silent)
  ydf <- .GroupedStabilitySeries(object, Type, Stocks, ManagementOnly = TRUE)
  if (!is.null(Years))
    ydf <- ydf[ydf$Year %in% Years, ]
  aav <- .AAV(ydf)
  .BuildPM(aav, Ref = Threshold, Years = NULL, op = `<=`,
           Name = 'Stability',
           Caption = paste0('P(interval-to-interval ', .StabilityLabel[[Type]],
                             ' change < ', Threshold, ')'))
}
class(PM_Stability) <- 'pm'
