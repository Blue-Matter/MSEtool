#' Performance Metrics
#'
#' `PM_*` functions evaluate a performance metric against an [mse-class]
#' object, or a `list` of [mse-class] objects (combined via [CombineMSE()]
#' before calculation, so simulations from every list element are pooled
#' and treated as one analysis). See [PM-equations] for the mathematical
#' definition of each metric.
#'
#' `PM_Removals`, `PM_Landings` (and `PM_Yield`, which is a thin wrapper
#' around the two), `PM_LogYield`, `PM_RelYield`, `PM_AAVY`, and
#' `PM_Stability` sum their
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
#' `PM_Kobe` is the probability of being in one quadrant of the Kobe plot,
#' evaluated at the complex level in the same way as `PM_Status`. The four
#' quadrants partition every year: `"green"` (`SB > SBMSY` and `F < FMSY`),
#' `"red"` (`SB < SBMSY` and `F > FMSY`), `"yellow"` (`SB <= SBMSY` and
#' `F <= FMSY`), and `"orange"` (all other years: `SB >= SBMSY` and
#' `F >= FMSY`, excluding the green and red quadrants). `PM_Status` is the
#' green quadrant and `PM_Red` the red quadrant.
#'
#' `PM_MinStatus` is the lowest annual stock status in each simulation over
#' `Years`, relative to `SBMSY` (`Reference = "MSY"`) or to the equilibrium
#' unfished spawning biomass `SB0` (`Reference = "Unfished"`), at the complex
#' level. It is a statistic, not a probability (`Prob` is `NA`).
#'
#' `PM_TACLimited` is the proportion of management cycles in each simulation
#' in which the change in the TAC is at the maximum allowed by the MP:
#' increases are compared with `DeltaUp[2]` and decreases with `DeltaDown[2]`
#' (see [ConstrainTAC()]). The limits are the defaults of the `DeltaUp` and
#' `DeltaDown` arguments of each MP (e.g. as set with [SetMPArgs()]) unless
#' given in `DeltaUp`/`DeltaDown`. MPs with no limits, or no TAC, are `NA`.
#'
#' `PM_Status`, `PM_Kobe`, `PM_MinStatus`, and `PM_Safety` accept a
#' `Definition` argument selecting
#' whether stock status is assessed on spawning biomass (`SBiomass()`, the
#' default) or spawning production (`SProduction()`). `PM_SBSBMSY`/`PM_SBSBlim`
#' and `PM_SPSPMSY`/`PM_SPSPlim` are the corresponding single-metric,
#' non-switchable counterparts for spawning biomass and spawning production
#' respectively.
#'
#' In seasonal models (`Seasons > 1`), the metrics based on MSY reference
#' points (`PM_FFMSY`, `PM_SBSBMSY`, `PM_SPSPMSY`, `PM_Status`, `PM_Kobe`,
#' `PM_MinStatus`, `PM_SBSBlim`, `PM_SPSPlim`, `PM_Safety`, and `PM_Rebuild`)
#' are evaluated on annual status per calendar year, as returned by
#' [F_FMSY()], [SB_SBMSY()], and [SP_SPMSY()], and `Years`/`Year` refer to
#' calendar years. `PM_MinStatus` with `Reference = "Unfished"` uses the
#' ratio to unfished spawning biomass in the reference season of each year
#' (see [RefSeason()]), the season used for `SB/SBMSY`. The catch-based
#' metrics (`PM_Yield`, `PM_Removals`, `PM_Landings`, `PM_LogYield`,
#' `PM_RelYield`, and `PM_AAVY`/`PM_Stability` with `Type = "Removals"` or
#' `"Landings"`) use annual catch, summed over the seasons of each complete
#' calendar year. In all metrics, `Years` refers to calendar years; see
#' [PMYears()] for the years in which the MPs are active, or a window of them.
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
#'   `PM_Status`, `PM_Kobe`, `PM_Red`, `PM_MinStatus`, and `PM_Safety`:
#'   `"SBiomass"` (spawning biomass, default) or `"SProduction"` (spawning
#'   production).
#' @param Quadrant Character. The Kobe quadrant of `PM_Kobe`: `"green"`,
#'   `"red"`, `"yellow"`, or `"orange"`. See Details.
#' @param Reference Character. The reference point of `PM_MinStatus`:
#'   `"MSY"` (`SBMSY`, default) or `"Unfished"` (equilibrium unfished
#'   spawning biomass, `SB0`).
#' @param IncludeFirst Logical. In `PM_AAVY`, `PM_Stability`, and
#'   `PM_TACLimited`, include the change from the value in effect before the
#'   first management cycle in `Years` to the first value: for the first
#'   cycle in which the MPs are active, the TAC in the last time step before
#'   `OM@MPStartYear` (e.g. an interim TAC), or, if there is none, the
#'   removals (or landings) in the calendar year before. Default `FALSE`
#'   (`TRUE` for `PM_TACLimited`).
#' @param DeltaUp,DeltaDown `NULL` (default), a number, or a named numeric
#'   vector (one value per MP). The maximum proportional TAC increase and
#'   decrease in `PM_TACLimited`, overriding the `DeltaUp[2]`/`DeltaDown[2]`
#'   of the MPs.
#' @param tol Numeric. Tolerance of `PM_TACLimited`: a change within `tol` of
#'   the limit is at the limit. Default `1e-6`.
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
#' `PM_Kobe`: the probability of being in a quadrant of the Kobe plot, e.g.
#' for the red quadrant,
#' \deqn{P\left(\frac{SB_{y}}{SB_{MSY}} < 1 \ \text{and}\ \frac{F_{y}}{F_{MSY}} > 1\right)}{P( SB/SBMSY < 1  and  F/FMSY > 1 )}
#' (see [PM] for the definition of each quadrant).
#'
#' `PM_MinStatus`: the lowest annual stock status over the evaluation window,
#' \deqn{\min_{y \in Y} \frac{SB_{s,y}}{SB_{ref,s}}}{min over y in Y of SB[s,y] / SBref[s]}
#' where \eqn{SB_{ref}}{SBref} is \eqn{SB_{MSY}}{SBMSY} or \eqn{SB_0}{SB0}
#' according to `Reference`.
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
#' where `C` is annual removals (landings + discards), or landings only,
#' summed over seasons in seasonal models.
#'
#' `PM_LogYield`: mean log catch over the evaluation window, with catch
#' floored at a fraction `Floor` of the simulation's mean historical catch
#' \eqn{\bar{C}^{hist}_s}{Chist[s]} so that a zero catch has a finite log,
#' \deqn{\frac{1}{|Y|}\sum_{y \in Y} \log\left(\max\left(C_{s,y},\ \mathrm{Floor} \cdot \bar{C}^{hist}_s\right)\right)}{(1/|Y|) * sum over y in Y of log(max(C[s,y], Floor * Chist[s]))}
#' where `C` is landings (default) or removals, per `Type`.
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
#' where `Y` is the set of management years in the evaluation window and
#' `y-1` denotes the preceding management year.
#'
#' `PM_Stability`: the probability that the series selected by `Type`
#' changes by no more than a threshold amount between one management interval
#' and the next,
#' \deqn{P\left(\frac{|C_{s,y} - C_{s,y-1}|}{C_{s,y-1}} \le \mathrm{Threshold}\right)}{P( |C[s,y] - C[s,y-1]| / C[s,y-1] <= Threshold )}
#' evaluated per interval `y` and averaged across the evaluation window and
#' simulations.
#'
#' `PM_TACLimited`: the probability that the change in the TAC between one
#' management interval and the next is at the limit of the MP,
#' \deqn{P\left(\frac{TAC_{s,y} - TAC_{s,y-1}}{TAC_{s,y-1}} \ge \Delta^{up} \ \text{or}\ \frac{TAC_{s,y-1} - TAC_{s,y}}{TAC_{s,y-1}} \ge \Delta^{down}\right)}{P( (TAC[s,y] - TAC[s,y-1]) / TAC[s,y-1] >= DeltaUp  or  (TAC[s,y-1] - TAC[s,y]) / TAC[s,y-1] >= DeltaDown )}
#' where \eqn{\Delta^{up}}{DeltaUp} and \eqn{\Delta^{down}}{DeltaDown} are
#' the maximum proportional increase and decrease (`DeltaUp[2]` and
#' `DeltaDown[2]` of the MP).
#'
#' With `IncludeFirst = TRUE`, `y-1` of the first management year in the
#' evaluation window is the value in effect before it (see [PM]).
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
#'   arguments; [NewPM()] to create a custom metric.
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
    self_ref <- purrr::keep(cx, \(i)
      .IsSPFromSelfOnly(OM@Stock[[i]]@SRR@SPFrom, i, stockNms)
    )
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

.FilterManagementYears <- function(df, object, Calendar = FALSE) {
  YearsProj <- Years(object@OM, 'Projection')
  YearsProj <- YearsProj[!YearsProj %in% .InterimTimesteps(object@OM)]
  mpNames   <- unique(df$MP)
  keep <- lapply(mpNames, function(mp) {
    Interval  <- .ResolveInterval(object@OM@Interval, mp, object@MPs[[mp]], object@OM@Seasons)
    ManageYrs <- if (length(YearsProj)) .CalcManagementYears(YearsProj, Interval, object@OM@Seasons) else YearsProj
    if (Calendar)
      return(df$MP == mp & df$Year %in% .CalendarYear(ManageYrs))
    df$MP == mp & df$Year %in% ManageYrs
  })
  df[Reduce(`|`, keep), ]
}

.CalendarYear <- function(Year) floor(Year + 1e-8)

# Sum a seasonal time-step series within each complete calendar year.
.SumCalendarYearDF <- function(df, OM) {
  if (!.IsSeasonal(OM))
    return(df)
  ts  <- sort(unique(df$Year))
  cal <- .CalendarYear(ts)
  complete <- as.numeric(names(which(table(cal) == OM@Seasons)))
  grp <- intersect(c('Sim', 'Stock', 'MP', 'Period'), names(df))
  df |>
    dplyr::mutate(Year = .CalendarYear(.data$Year)) |>
    dplyr::filter(.data$Year %in% complete) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp)), .data$Year) |>
    dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop')
}

.GroupCatchByStocks <- function(df, OM, Stocks) {
  groups <- .ResolveComplexGroups(OM, Stocks)
  purrr::imap(groups, \(stk, grpName) {
    df[df$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
      dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = grpName)
  }) |> dplyr::bind_rows()
}

.GroupedCatch <- function(object, Stocks, ManagementOnly = FALSE, FUN = Removals,
                          IncludeFirst = FALSE) {
  df <- FUN(object, df = TRUE, byFleet = FALSE, byAge = FALSE,
            bySize = FALSE, byArea = FALSE, Reduce = FALSE)
  df <- df[df$Period == 'Projection', ]
  df <- .FilterMPActiveYears(df, object@OM)
  df <- .SumCalendarYearDF(df, object@OM)

  if (ManagementOnly)
    df <- .FilterManagementYears(df, object, Calendar = TRUE)

  out <- .GroupCatchByStocks(df, object@OM, Stocks)
  if (IncludeFirst)
    out <- dplyr::bind_rows(.PreMPCatch(object, Stocks, FUN, out), out)
  out
}

# Calendar-year catch before the first MP year, one row per Sim/MP/Stock of `template`.
.PreMPCatch <- function(object, Stocks, FUN, template) {
  Year0 <- .CalendarYear(.FirstMPTimestep(object@OM)) - 1
  df <- FUN(object, df = TRUE, byFleet = FALSE, byAge = FALSE,
            bySize = FALSE, byArea = FALSE, Reduce = FALSE)
  df <- .SumCalendarYearDF(as.data.frame(df), object@OM)
  df <- df[.CalendarYear(df$Year) == Year0, ]
  catch <- .GroupCatchByStocks(df, object@OM, Stocks)

  keys <- dplyr::distinct(template, .data$Sim, .data$MP, .data$Stock)
  byMP <- dplyr::left_join(keys, catch, by = c('Sim', 'MP', 'Stock'))
  hist <- catch[catch$MP == 'Historical', c('Sim', 'Stock', 'Value')]
  byHist <- dplyr::left_join(keys, hist, by = c('Sim', 'Stock'))
  byMP$Value <- ifelse(is.na(byMP$Value), byHist$Value, byMP$Value)
  byMP$Year <- Year0
  byMP[, c('Sim', 'Year', 'MP', 'Value', 'Stock')]
}

.FirstMPTimestep <- function(OM) {
  YearsProj <- Years(OM, 'Projection')
  YearsProj[!YearsProj %in% .InterimTimesteps(OM)][1]
}

.GroupTACByStocks <- function(df, OM, Stocks) {
  # TAC rows are per complex; a group takes the TAC of any complex it overlaps
  cxStocks <- .ResolveComplexGroups(OM, NULL)
  groups   <- .ResolveComplexGroups(OM, Stocks)
  purrr::imap(groups, \(stk, grpName) {
    cx <- names(cxStocks)[purrr::map_lgl(cxStocks, \(s) any(s %in% stk))]
    df[df$Stock %in% cx, ] |>
      dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
      dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = grpName)
  }) |> dplyr::bind_rows()
}

.GroupedTAC <- function(object, Stocks, ManagementOnly = FALSE, IncludeFirst = FALSE) {
  all_df <- TACs(object)
  all_df <- all_df[all_df$Period == 'Projection', ]
  df <- .FilterMPActiveYears(all_df, object@OM)

  if (ManagementOnly)
    df <- .FilterManagementYears(df, object)

  out <- .GroupTACByStocks(df, object@OM, Stocks)
  if (IncludeFirst)
    out <- dplyr::bind_rows(.PreMPTAC(object, all_df, Stocks, out), out)
  out
}

# TAC in the last time step before the first MP time step, or the catch in the
# calendar year before when there is no TAC (as `LastTAC()`).
.PreMPTAC <- function(object, df, Stocks, template) {
  First <- .FirstMPTimestep(object@OM)
  df <- df[df$Year < First - 1e-8 & !is.na(df$Value), ]
  keys <- dplyr::distinct(template, .data$Sim, .data$MP, .data$Stock)
  if (nrow(df)) {
    df  <- df[df$Year == max(df$Year), ]
    pre <- .GroupTACByStocks(df, object@OM, Stocks)
    out <- dplyr::left_join(keys, pre, by = c('Sim', 'MP', 'Stock'))
  } else {
    out <- dplyr::mutate(keys, Year = NA_real_, Value = NA_real_)
  }
  Missing <- is.na(out$Value)
  if (any(Missing)) {
    Catch <- .PreMPCatch(object, Stocks, Removals, keys[Missing, ])
    out[Missing, c('Year', 'Value')] <- Catch[, c('Year', 'Value')]
  }
  out[, c('Sim', 'Year', 'MP', 'Value', 'Stock')]
}

# Shared dispatch for the stability-family PMs (PM_AAVY, PM_Stability): TAC
# (the recommendation issued by the MP) vs. realised removals/landings.
.StabilityLabel <- c(TAC = 'TAC', Removals = 'removals', Landings = 'landings')

.GroupedStabilitySeries <- function(object, Type, Stocks, ManagementOnly = TRUE,
                                    IncludeFirst = FALSE) {
  switch(Type,
    TAC      = .GroupedTAC(object, Stocks, ManagementOnly, IncludeFirst),
    Removals = .GroupedCatch(object, Stocks, ManagementOnly, FUN = Removals, IncludeFirst),
    Landings = .GroupedCatch(object, Stocks, ManagementOnly, FUN = Landings, IncludeFirst)
  )
}

# Changes between successive values of a series; with `IncludeFirst`, the
# change into the first value in `Years` is kept.
.WindowChanges <- function(df, Years = NULL, IncludeFirst = FALSE, group_col = 'Stock') {
  if (!is.null(Years) && !IncludeFirst)
    df <- df[.CalendarYear(df$Year) %in% Years, ]
  ch <- .Changes(df, group_col)
  if (!is.null(Years) && IncludeFirst)
    ch <- ch[.CalendarYear(ch$Year) %in% Years, ]
  ch
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


.Changes <- function(df, group_col = 'Stock') {
  df |>
    dplyr::arrange(.data$Sim, .data[[group_col]], .data$MP, .data$Year) |>
    dplyr::group_by(.data$Sim, .data[[group_col]], .data$MP) |>
    dplyr::mutate(Prev = dplyr::lag(.data$Value)) |>
    dplyr::filter(!is.na(.data$Prev)) |>
    dplyr::ungroup()
}

.RelChange <- function(ch, group_col = 'Stock') {
  ch |>
    dplyr::mutate(Value = abs(.data$Value - .data$Prev) / .data$Prev) |>
    dplyr::select('Sim', dplyr::all_of(group_col), 'Year', 'MP', 'Value')
}

.AAV <- function(df, group_col = 'Stock') .RelChange(.Changes(df, group_col), group_col)

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

# Annual SB/SBMSY (`SB`) and F/FMSY (`F`) per complex in the projection.
.KobeStatusDF <- function(object, Definition, Years = NULL, ActiveOnly = TRUE) {
  sb_df <- .ComplexStatusSeries(object, Definition) |>
    Array2DF() |>
    dplyr::rename(Complex = 'Stock', SB = 'Value')

  ff <- F_FMSY(object, df = TRUE, Reduce = FALSE)
  ff <- ff[ff$Period == 'Projection', ] |>
    dplyr::rename(Complex = 'Stock') |>
    dplyr::select('Sim', 'Complex', 'Year', 'MP', F = 'Value')

  if (ActiveOnly) {
    sb_df <- .FilterMPActiveYears(sb_df, object@OM)
    ff    <- .FilterMPActiveYears(ff, object@OM)
  }
  if (!is.null(Years)) {
    sb_df <- sb_df[sb_df$Year %in% Years, ]
    ff    <- ff[ff$Year %in% Years, ]
  }

  dplyr::inner_join(sb_df, ff, by = c('Sim', 'Complex', 'Year', 'MP')) |>
    dplyr::rename(Stock = 'Complex')
}

.KobeQuadrant <- function(SB, F) {
  out <- rep('orange', length(SB))
  out[SB <= 1 & F <= 1] <- 'yellow'
  out[SB > 1 & F < 1]   <- 'green'
  out[SB < 1 & F > 1]   <- 'red'
  out[is.na(SB) | is.na(F)] <- NA
  out
}

.KobeCaption <- c(green  = 'SB > SBMSY & F < FMSY',
                  red    = 'SB < SBMSY & F > FMSY',
                  yellow = 'SB <= SBMSY & F <= FMSY',
                  orange = 'SB >= SBMSY & F >= FMSY')

#' @rdname PM
#' @export
PM_Kobe <- function(object, Quadrant = c('green', 'red', 'yellow', 'orange'),
                    Definition = c('SBiomass', 'SProduction'), Years = NULL, silent = TRUE) {
  Quadrant   <- match.arg(Quadrant)
  Definition <- match.arg(Definition)
  object <- .CoercePMInput(object, silent)

  df <- .KobeStatusDF(object, Definition, Years)
  df$Value <- as.numeric(.KobeQuadrant(df$SB, df$F) == Quadrant)

  .BuildPM(df, Ref = 1, Years = NULL, op = \(x, r) x >= r,
           Name = paste0('Kobe', .FirstUp(Quadrant)),
           Caption = paste0('P(Kobe ', Quadrant, ': ', .KobeCaption[[Quadrant]], ')'))
}
class(PM_Kobe) <- 'pm'

#' @rdname PM
#' @export
PM_Status <- function(object, Definition = c('SBiomass', 'SProduction'),
                       Years = NULL, silent = TRUE) {
  Definition <- match.arg(Definition)
  out <- PM_Kobe(object, 'green', Definition, Years, silent)
  out@Name    <- 'Status'
  out@Caption <- paste0('P(', Definition, ' > ', Definition, 'MSY & F < FMSY), complex-level')
  out
}
class(PM_Status) <- 'pm'

#' @rdname PM
#' @export
PM_Red <- function(object, Definition = c('SBiomass', 'SProduction'),
                   Years = NULL, silent = TRUE) {
  PM_Kobe(object, 'red', match.arg(Definition), Years, silent)
}
class(PM_Red) <- 'pm'

#' @rdname PM
#' @export
PM_MinStatus <- function(object, Reference = c('MSY', 'Unfished'),
                         Definition = c('SBiomass', 'SProduction'),
                         Years = NULL, silent = TRUE) {
  Reference  <- match.arg(Reference)
  Definition <- match.arg(Definition)
  object <- .CoercePMInput(object, silent)

  df <- if (Reference == 'MSY') {
    Array2DF(.ComplexStatusSeries(object, Definition))
  } else {
    .ComplexDepletionDF(object, Definition)
  }
  if ('Period' %in% names(df))
    df <- df[df$Period == 'Projection', ]
  df <- .FilterMPActiveYears(df, object@OM)
  if (!is.null(Years))
    df <- df[df$Year %in% Years, ]
  YearsOut <- sort(unique(df$Year))

  df <- df |>
    dplyr::group_by(.data$Sim, .data$Stock, .data$MP) |>
    dplyr::summarise(Value = min(.data$Value), Year = max(.data$Year), .groups = 'drop')

  Metric <- if (Definition == 'SProduction') 'SP' else 'SB'
  RefNm  <- if (Reference == 'MSY') 'MSY' else '0'
  out <- .BuildPM(df, Ref = NA_real_, Years = NULL, op = NULL, Name = 'MinStatus',
                  Caption = paste0('Minimum ', Metric, '/', Metric, RefNm))
  out@Years <- YearsOut
  out
}
class(PM_MinStatus) <- 'pm'

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

  lastHistYr <- max(floor(Years(object@OM, 'Historical')))
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
#' @param Floor Positive number. In `PM_LogYield`, catches are floored at
#'   `Floor` times the simulation's mean historical catch before taking logs,
#'   so a zero catch has a finite log. Default `1e-3`.
#' @export
PM_LogYield <- function(object, Years = NULL, Stocks = NULL,
                        Type = c('Landings', 'Removals'), Floor = 1e-3, silent = TRUE) {
  Type <- match.arg(Type)
  object <- .CoercePMInput(object, silent)
  FUN <- if (Type == 'Landings') Landings else Removals
  df <- .GroupedCatch(object, Stocks, FUN = FUN)

  HistDF <- FUN(object, df = TRUE, byFleet = FALSE, byAge = FALSE, bySize = FALSE,
                byArea = FALSE, Reduce = FALSE)
  HistDF <- HistDF[HistDF$Period == 'Historical', ]
  HistDF <- .SumCalendarYearDF(HistDF, object@OM)
  groups <- .ResolveComplexGroups(object@OM, Stocks)
  RefDF <- purrr::imap(groups, \(stk, grpName) {
    HistDF[HistDF$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim, .data$Year) |>
      dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::group_by(.data$Sim) |>
      dplyr::summarise(HistMean = mean(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = grpName)
  }) |> dplyr::bind_rows()

  df <- dplyr::left_join(df, RefDF, by = c('Sim', 'Stock'))
  Lower <- ifelse(is.finite(df$HistMean) & df$HistMean > 0, Floor * df$HistMean, 0)
  df$Value <- log(pmax(df$Value, Lower))
  .BuildPM(df, Ref = NA_real_, Years = Years, op = NULL, Name = 'LogYield',
           Caption = paste0('Mean log projected ', tolower(Type)))
}
class(PM_LogYield) <- 'pm'

#' Create a Custom Performance Metric
#'
#' Builds a [pm-class] object from a data frame of per-simulation, per-year
#' values, so any metric can be used as a compatible performance metric (PM), e.g. 
#' as the objective or a constraint in [TuneMP()]. 
#' 
#' A custom PM function takes an
#' [mse-class] object, calculates the metric (e.g. from [Landings()] or
#' [SB_SBMSY()] with `df = TRUE`), and returns `NewPM(...)`.
#'
#' @param df A data frame with columns `Sim`, `Stock`, `Year`, `MP`, and
#'   `Value` (and optionally `Period`; only `'Projection'` rows are used).
#' @param Name Character. Short name of the metric.
#' @param Caption Character. Description of the metric. Default `Name`.
#' @param Ref `NULL` (default) or the reference value compared with `Value`
#'   by `Op`.
#' @param Op `NULL` (default) or a comparison function, e.g. `` `>` ``. When
#'   given, `Prob` is, for each simulation, the proportion of years in which
#'   `Op(Value, Ref)` is `TRUE`, and `Mean` is `Prob` averaged over
#'   simulations; otherwise `Prob` is `NA` and `Mean` is `Stat` (the
#'   per-simulation mean `Value`) averaged over simulations.
#' @param Years `NULL` (default, all projection years) or the years to include.
#' @param OM `NULL`, or the [om-class] object, to exclude interim years
#'   before `OM@@MPStartYear`.
#'
#' @return A [pm-class] object.
#'
#' @examples
#' \dontrun{
#' PM_MinSB <- function(object, Years = NULL) {
#'   df <- SB_SBMSY(object, df = TRUE, Reduce = FALSE)
#'   df <- df[df$Period == 'Projection', ]
#'   df <- dplyr::summarise(dplyr::group_by(df, Sim, Stock, MP),
#'                          Value = min(Value), Year = max(Year), .groups = 'drop')
#'   NewPM(df, Name = 'MinSB', Caption = 'P(min SB/SBMSY > 0.5)', Ref = 0.5, Op = `>`)
#' }
#' class(PM_MinSB) <- 'pm'
#' }
#'
#' @seealso [PM], [TuneMP()]
#' @export
NewPM <- function(df, Name, Caption = Name, Ref = NULL, Op = NULL, Years = NULL, OM = NULL) {
  Missing <- setdiff(c('Sim', 'Stock', 'Year', 'MP', 'Value'), names(df))
  if (length(Missing))
    cli::cli_abort("{.arg df} is missing column{?s} {.val {Missing}}.")
  if (!is.null(Op) && is.null(Ref))
    cli::cli_abort("{.arg Ref} is needed when {.arg Op} is given.")
  .BuildPM(as.data.frame(df), Ref = Ref, Years = Years, op = Op, Name = Name,
           Caption = Caption, OM = OM)
}

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
                     Stocks = NULL, IncludeFirst = FALSE, silent = TRUE) {
  Type <- match.arg(Type)
  object <- .CoercePMInput(object, silent)
  ydf <- .GroupedStabilitySeries(object, Type, Stocks, ManagementOnly = TRUE, IncludeFirst)
  aav <- .RelChange(.WindowChanges(ydf, Years, IncludeFirst))
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
    edf <- edf[.CalendarYear(edf$Year) %in% Years, ]
  aav <- .AAV(edf)
  .BuildPM(aav, Ref = NA_real_, Years = NULL, op = NULL,
           Name = 'AAVE', Caption = 'Average annual variability in effort across management intervals')
}
class(PM_AAVE) <- 'pm'

#' @rdname PM
#' @export
PM_Stability <- function(object, Threshold, Type = c('TAC', 'Removals', 'Landings'),
                          Years = NULL, Stocks = NULL, IncludeFirst = FALSE, silent = TRUE) {
  if (missing(Threshold))
    cli::cli_abort("`Threshold` (maximum acceptable interval-to-interval change) must be supplied.")
  Type <- match.arg(Type)
  object <- .CoercePMInput(object, silent)
  ydf <- .GroupedStabilitySeries(object, Type, Stocks, ManagementOnly = TRUE, IncludeFirst)
  aav <- .RelChange(.WindowChanges(ydf, Years, IncludeFirst))
  .BuildPM(aav, Ref = Threshold, Years = NULL, op = \(x, r) x <= r + 1e-4,
           Name = 'Stability',
           Caption = paste0('P(interval-to-interval ', .StabilityLabel[[Type]],
                             ' change < ', Threshold, ')'))
}
class(PM_Stability) <- 'pm'

#' @rdname PM
#' @export
PM_TACLimited <- function(object, DeltaUp = NULL, DeltaDown = NULL, IncludeFirst = TRUE,
                          Years = NULL, Stocks = NULL, tol = 1e-6, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  Up   <- .MPDeltaLimit(object@MPs, 'DeltaUp', DeltaUp)
  Down <- .MPDeltaLimit(object@MPs, 'DeltaDown', DeltaDown)

  ydf <- .GroupedTAC(object, Stocks, ManagementOnly = TRUE, IncludeFirst = IncludeFirst)
  ch  <- .WindowChanges(ydf, Years, IncludeFirst)
  Change <- (ch$Value - ch$Prev) / ch$Prev
  Lim    <- ifelse(Change < 0, Down[ch$MP], Up[ch$MP])
  ch$Value <- as.numeric(abs(Change) >= Lim - tol)
  ch$Value[!is.finite(Change)] <- NA

  out <- .BuildPM(ch[, c('Sim', 'Stock', 'Year', 'MP', 'Value')], Ref = 1, Years = NULL,
                  op = \(x, r) x >= r, Name = 'TACLimited',
                  Caption = 'P(TAC change at the limit of the MP)')
  out@Stat[is.nan(out@Stat)] <- NA
  out@Prob[is.nan(out@Prob)] <- NA
  out@Mean[is.nan(out@Mean)] <- NA
  out
}
class(PM_TACLimited) <- 'pm'

# Maximum proportional TAC change (`Arg[2]`) of each MP, from `Override` or the MP's formals.
.MPDeltaLimit <- function(MPList, Arg, Override = NULL) {
  MPs <- names(MPList)
  Named <- !is.null(Override) && !is.null(names(Override))
  if (!is.null(Override) && !Named && length(Override) != 1)
    cli::cli_abort("{.arg {Arg}} must be a single number or a numeric vector named by MP.")
  vapply(MPs, \(mp) {
    if (Named && mp %in% names(Override)) return(as.numeric(Override[[mp]]))
    if (!is.null(Override) && !Named) return(as.numeric(Override))
    fn <- MPList[[mp]]
    if (is.character(fn)) fn <- get0(fn, mode = 'function')
    if (!is.function(fn) || !Arg %in% names(formals(fn))) return(NA_real_)
    val <- tryCatch(eval(formals(fn)[[Arg]], environment(fn)), error = \(e) NULL)
    if (!is.numeric(val) || !length(val)) return(NA_real_)
    as.numeric(utils::tail(val, 1))
  }, numeric(1))
}
