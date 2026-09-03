#' Empirical Index-Based Management Procedures
#'
#' Management procedures that set a TAC from one or more relative abundance
#' indices (`Survey(Data)` or `CPUE(Data)`, or both).
#'
#' Both optionally smooth the selected index/indices with [stats::loess()]
#' (`Smooth`, `ENPMult`) before use, to reduce sensitivity to observation
#' noise, average that smoothed status over the most recent `RecentYears`,
#' and constrain the resulting TAC change via `DeltaDown`/`DeltaUp`/`TACRange`.
#'
#' - `IndexRate()`: Calibrates a catch-per-index rate from `CalibYears` of
#'   recent history, damps that rate (`catch_per_index x IndexFactor`)
#'   through a hockey-stick harvest control rule
#'   (`HCRControlPointsIndex`/`HCRControlPointsRate`) based on the current
#'   index level relative to `IndexTarget`, then multiplies the adjusted
#'   rate by the current index status to get a trial TAC. `IndexFactor = 1`
#'   (the default) holds the catch-per-index rate observed in `CalibYears`
#'   constant; there is no way to derive a "correct" `IndexFactor` from
#'   `Data` alone (it would require knowing the true current fishing rate
#'   relative to some target), so a user wanting e.g. a 75% FMSY target must
#'   supply that ratio themselves.
#' - `IndexTarget()`: Simpler and does not calibrate against catch. Instead it
#'   moves the TAC up or down from its previous value in proportion to
#'   `(current index / IndexTarget) ^ Responsiveness`.
#'
#' @param Data A [data-class] object.
#' @param Indices Character (matching `Name`) or integer vector selecting
#'   which columns of the chosen `IndexSource` to use. `NULL` (default) uses
#'   every index in `IndexSource`.
#' @param IndexSource Character vector of one or both of `'Survey'`,
#'   `'CPUE'`. Use indices from `Data@Survey` (default, `'Survey'`),
#'   `Data@CPUE`, or both (`c('Survey', 'CPUE')`). When both are used, their
#'   indices are combined into a single selection (`Survey` columns first,
#'   then `CPUE`), and `Indices`, `IndexFreq`, and `IndexWeight` apply across
#'   that combined set.
#' @param IndexFreq Positive integer vector, same length as the selected
#'   indices. How often each index is available in the projection period
#'   (`1` = every year, `2` = every 2 years, `0` = excluded entirely).
#'   `NULL` (default) assumes every year.
#' @param IndexWeight Positive numeric vector, same length as the selected
#'   indices, giving each index's weight when averaging. `NULL` (default)
#'   weights indices equally.
#' @param Smooth Logical. Whether to smooth each selected index with
#'   [stats::loess()] (`ENPMult` controls the degree of smoothing) before
#'   use. `TRUE` (default). Set `FALSE` to use the raw (unsmoothed) index
#'   values directly - useful for a short series where a loess fit is
#'   unstable, or when the raw index should drive the harvest control rule
#'   directly.
#' @param ENPMult Fraction. Smoothing parameter for [stats::loess()]: the
#'   number of effective parameters is `length(index) x ENPMult`. Larger
#'   values mean less smoothing. Ignored when `Smooth = FALSE`.
#' @param RecentYears Positive integer. Number of most recent years (after
#'   smoothing, if `Smooth = TRUE`) averaged, per index, to obtain the
#'   current status estimate used by both MPs. `1` (default) uses only the
#'   terminal year.
#' @param TrendYears `NULL` (default) disables trend projection. Otherwise a
#'   positive integer, `>= 2`, giving the number of most recent smoothed
#'   years (per index) used to estimate a log-linear trend. When set, the
#'   status compared against `IndexTarget` (and, in `IndexRate()`, fed to
#'   the harvest control rule) is that trend projected forward
#'   `TrendHorizon` years, rather than the current status itself - so a
#'   stock at target but declining fast is treated more cautiously than one
#'   at target and stable. The status used to scale the trial TAC in
#'   `IndexRate()` is always the actual (non-projected) current status.
#' @param TrendHorizon Positive number. Number of years the estimated trend
#'   is extrapolated forward; see `TrendYears`. Default `1`. Ignored when
#'   `TrendYears` is `NULL`.
#' @param IndexTarget `NULL` (default), or a positive number (or vector, one
#'   per selected index) giving the index level considered "on target".
#'   Defaults to `Ref` on the selected `IndexSource` object (see
#'   [IndicesData()]); an error is raised if neither is available.
#' @param DeltaDown,DeltaUp Numeric vector, length 2 (`c(min, max)`). Minimum
#'   and maximum allowed fractional TAC change among management cycles,
#'   downward and upward respectively.
#' @param TACRange Numeric vector, length 2 (`c(min, max)`). Absolute bounds
#'   on the TAC. `NULL` (default) effectively imposes no limit
#'   (`c(0, 100 * max(historical removals))`).
#' @param Allocation `NULL` (default), or a positive numeric vector of length
#'   `nFleet` giving the fraction of the TAC allocated to each fleet
#'   (normalised to sum to `1`). `NULL` returns a single stock-wide TAC,
#'   which the framework then splits across fleets using `Allocation(OM)`
#'   (see [Advice()]).
#'
#' @return An [advice-class] object with `TAC` set.
#'
#' @seealso [Advice()], [CheckCatch()], [LastTAC()], [FilterTAC()],
#'   [HockeyStickHCR()], [ConstrainTAC()], [SmoothSeries()], [data-class],
#'   [advice-class]
#' @name IndexMPs
NULL


#' @rdname IndexMPs
#' @param CalibYears Positive integer. Number of recent historical years used
#'   to calculate the catch-per-index calibration ratio. Clamped to the
#'   number of historical years actually available.
#' @param IndexFactor Positive number. Multiplier applied to the
#'   catch-per-index ratio when projecting the trial TAC; see Details.
#'   Default `1`.
#' @param HCRControlPointsIndex Numeric vector, length 2 (`c(Lx, Ux)`). The
#'   lower and upper control points, in units of current-index-over-target,
#'   of a hockey-stick harvest control rule applied to the catch-per-index
#'   rate. Below `Lx` the rate is multiplied by `HCRControlPointsRate[1]`;
#'   above `Ux` by `HCRControlPointsRate[2]`; in between it is linearly
#'   ramped. `c(0, 0)` (default) disables the HCR (constant multiplier of
#'   `1`).
#' @param HCRControlPointsRate Numeric vector, length 2 (`c(Ly, Uy)`). The
#'   rate multipliers corresponding to `HCRControlPointsIndex`. Default
#'   `c(0, 1)`.
#' @param RampType Character. Shape of the harvest control rule's ramp
#'   between control points: `'linear'` (default) or `'smooth'` (a cubic
#'   smoothstep, avoiding the slope discontinuity a linear ramp has at each
#'   control point). `HCRControlPointsIndex`/`HCRControlPointsRate` may have
#'   `2` control points (the classic two-point hockey stick) or more - e.g.
#'   `3`, to cap the rate above a very healthy index level as well as floor
#'   it below a limit. See [HockeyStickHCR()].
#' @param Responsiveness Positive number. Responsiveness of the TAC-change
#'   calculation: `TAC change = exp(log(new_TAC / old_TAC) * Responsiveness)`.
#'   Values below `1` damp the implied change; `1` (default) applies it in
#'   full.
#' @export
IndexRate <- function(Data,
                      Indices               = NULL,
                      IndexSource           = 'Survey',
                      IndexFreq             = NULL,
                      IndexWeight           = NULL,
                      CalibYears            = 2,
                      Smooth                = TRUE,
                      ENPMult               = 0.3,
                      RecentYears           = 1,
                      TrendYears            = NULL,
                      TrendHorizon          = 1,
                      IndexFactor           = 1,
                      IndexTarget           = NULL,
                      HCRControlPointsIndex = c(0, 0),
                      HCRControlPointsRate  = c(0, 1),
                      RampType              = c('linear', 'smooth'),
                      Responsiveness        = 1,
                      DeltaDown             = c(0.01, 0.5),
                      DeltaUp               = c(0.01, 0.5),
                      TACRange              = NULL,
                      Allocation            = NULL) {

  RampType <- match.arg(RampType)

  CheckCatch(Data)
  IndexSource <- match.arg(IndexSource, c('Survey', 'CPUE'), several.ok = TRUE)

  Selected <- .SelectIndices(Data, IndexSource, Indices)
  nSel <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)

  Ref <- .ResolveIndexTarget(Selected$Ref, IndexTarget, nSel, IndexSource)

  LHInd   <- LastHistYearInd(Data)
  YearCur <- max(Data@Years)

  IndexHist <- .ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, YearCur, Data@Years)

  Keep        <- IndexFreq > 0
  Ref         <- Ref[Keep]
  IndexWeight <- IndexWeight[Keep]

  CalibRows   <- seq(max(1, LHInd - CalibYears + 1), LHInd)
  Removals    <- rowSums(Data@Landings@Value, na.rm = TRUE) + rowSums(Data@Discards@Value, na.rm = TRUE)
  CalibCatch  <- mean(Removals[CalibRows], na.rm = TRUE)
  CalibIndex  <- rowMeans(IndexHist[, CalibRows, drop = FALSE], na.rm = TRUE)
  CatchPerIndex <- CalibCatch / CalibIndex

  IndexSmooth <- .SmoothIndices(IndexHist, Smooth, ENPMult)

  Status <- .RecentStatus(IndexSmooth, RecentYears)
  if (all(is.na(Status)))
    cli::cli_abort("All index values are {.val NA} over the {.arg RecentYears} status window.")

  Slope       <- .TrendSlope(IndexSmooth, TrendYears)
  StatusTrend <- Status * exp(Slope * TrendHorizon)

  Est      <- stats::weighted.mean(StatusTrend, IndexWeight, na.rm = TRUE)
  RefLevel <- stats::weighted.mean(Ref, IndexWeight, na.rm = TRUE)

  TrialRate <- CatchPerIndex * IndexFactor
  AdjRate   <- HockeyStickHCR(TrialRate, 
                              Est = Est, 
                              Ref = RefLevel,
                              ControlPointsIndex = HCRControlPointsIndex,
                              ControlPointsRate  = HCRControlPointsRate,
                              RampType           = RampType)

  TrialTAC <- stats::weighted.mean(Status * AdjRate, IndexWeight, na.rm = TRUE)
  PrevTAC  <- LastTAC(Data)
  if (is.na(TrialTAC)) TrialTAC <- PrevTAC
  TrialTAC <- FilterTAC(TrialTAC)

  Mod <- exp(log(TrialTAC / PrevTAC) * Responsiveness)

  if (is.null(TACRange))
    TACRange <- c(0, 100 * max(Removals, na.rm = TRUE))

  TAC <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)
  TAC <- .ApplyAllocation(TAC, Allocation, Data)

  Advice(TAC = TAC, TACType = 'Removals', TACUnit = Data@Landings@Units)
}
class(IndexRate) <- 'mp'


#' @rdname IndexMPs
#' @export
IndexTarget <- function(Data,
                        Indices        = NULL,
                        IndexSource    = 'Survey',
                        IndexFreq      = NULL,
                        IndexWeight    = NULL,
                        Smooth         = TRUE,
                        ENPMult        = 0.3,
                        RecentYears    = 1,
                        TrendYears     = NULL,
                        TrendHorizon   = 1,
                        IndexTarget    = NULL,
                        Responsiveness = 1,
                        DeltaDown      = c(0.01, 0.5),
                        DeltaUp        = c(0.01, 0.5),
                        TACRange       = NULL,
                        Allocation     = NULL) {

  CheckCatch(Data)
  IndexSource <- match.arg(IndexSource, c('Survey', 'CPUE'), several.ok = TRUE)

  Selected <- .SelectIndices(Data, IndexSource, Indices)
  nSel <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)

  Ref <- .ResolveIndexTarget(Selected$Ref, IndexTarget, nSel, IndexSource)

  YearCur   <- max(Data@Years)
  IndexHist <- .ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, YearCur, Data@Years)

  Keep        <- IndexFreq > 0
  Ref         <- Ref[Keep]
  IndexWeight <- IndexWeight[Keep]

  IndexSmooth <- .SmoothIndices(IndexHist, Smooth, ENPMult)

  Status <- .RecentStatus(IndexSmooth, RecentYears)
  if (all(is.na(Status)))
    cli::cli_abort("All index values are {.val NA} over the {.arg RecentYears} status window.")

  Slope       <- .TrendSlope(IndexSmooth, TrendYears)
  StatusTrend <- Status * exp(Slope * TrendHorizon)

  Est      <- stats::weighted.mean(StatusTrend, IndexWeight, na.rm = TRUE)
  RefLevel <- stats::weighted.mean(Ref, IndexWeight, na.rm = TRUE)

  Mod <- exp(log(Est / RefLevel) * Responsiveness)

  PrevTAC  <- LastTAC(Data)
  Removals <- rowSums(Data@Landings@Value, na.rm = TRUE) + rowSums(Data@Discards@Value, na.rm = TRUE)
  if (is.null(TACRange))
    TACRange <- c(0, 100 * max(Removals, na.rm = TRUE))

  TAC <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)
  TAC <- .ApplyAllocation(TAC, Allocation, Data)

  Advice(TAC = TAC, TACType = 'Removals', TACUnit = Data@Landings@Units)
}
class(IndexTarget) <- 'mp'


#' Select and Validate Indices from One or More `indicesdata` Objects
#'
#' @param Data A [data-class] object.
#' @param IndexSource Character vector of one or both of `'Survey'`,
#'   `'CPUE'`; see [IndexRate()]. Indices from each requested source are
#'   combined column-wise, in the order given.
#' @param Indices Character, integer, or `NULL`; see [IndexRate()].
#' @return A list with elements `Value` (`[nYear x nSelected]`) and `Ref`
#'   (length `nSelected`, `NA` where unset).
#' @keywords internal
.SelectIndices <- function(Data, IndexSource, Indices) {
  Sources <- lapply(IndexSource, function(src) {
    IndexData <- slot(Data, src)
    if (is.null(IndexData@Value))
      cli::cli_abort("No index data available in {.code Data@{src}}.")
    nIndex <- ncol(IndexData@Value)
    Name <- IndexData@Name
    if (is.null(Name)) Name <- paste0(src, seq_len(nIndex))
    Ref <- IndexData@Ref
    if (is.null(Ref)) Ref <- rep(NA_real_, nIndex)
    list(Value = IndexData@Value, Name = Name, Ref = Ref)
  })

  Value <- do.call(cbind, lapply(Sources, `[[`, 'Value'))
  Name  <- unlist(lapply(Sources, `[[`, 'Name'),  use.names = FALSE)
  Ref   <- unlist(lapply(Sources, `[[`, 'Ref'),   use.names = FALSE)

  nIndex <- ncol(Value)

  if (is.null(Indices)) {
    Sel <- seq_len(nIndex)
  } else if (is.character(Indices)) {
    Sel <- match(Indices, Name)
    if (anyNA(Sel))
      cli::cli_abort("{.val {Indices[is.na(Sel)]}} not found in {.field Name} of the selected {.arg IndexSource}.")
  } else {
    Sel <- as.integer(Indices)
    if (!all(Sel %in% seq_len(nIndex)))
      cli::cli_abort("{.arg Indices} must be within {.val 1} to {.val {nIndex}}.")
  }

  list(Value = Value[, Sel, drop = FALSE], Ref = Ref[Sel])
}

#' Resolve the Target ("on target") Index Level
#'
#' Uses the user-supplied `IndexTarget`, falling back to `Ref` from the
#' selected [indicesdata-class] object(s). Errors if
#' neither is available.
#' @param Ref Numeric vector from `.SelectIndices()$Ref`.
#' @param IndexTarget User-supplied override; see [IndexRate()].
#' @param nSel Number of selected indices.
#' @param IndexSource Character vector, for the error message.
#' @keywords internal
.ResolveIndexTarget <- function(Ref, IndexTarget, nSel, IndexSource) {
  if (!is.null(IndexTarget))
    Ref <- rep_len(IndexTarget, nSel)

  if (anyNA(Ref)) {
    Slots <- paste(paste0('Data@', IndexSource), collapse = ' or ')
    cli::cli_abort(c(
      "No reference (target) index level available for one or more selected indices.",
      "i" = "Set {.field Ref} on {.code {Slots}}, or supply {.arg IndexTarget}."
    ))
  }
  Ref
}

#' Thin an Index Time Series to a Given Sampling Frequency
#'
#' Simulates irregular index monitoring in the projection period by setting
#' observations to `NA` according to `IndexFreq`. The historical period
#' (`Years <= YearLH`) is always left untouched.
#'
#' @param IndexHist `[nSelected x nYear]` matrix of index values (negative
#'   values are first set to `NA`).
#' @param IndexFreq Positive integer vector, length `nSelected`; see
#'   [IndexRate()]. Indices with `IndexFreq == 0` are dropped from the
#'   returned matrix.
#' @param YearLH,YearCur,Years See [IndexRate()]/`Data@YearLH`, `max(Data@Years)`,
#'   `Data@Years`.
#' @return A `[sum(IndexFreq != 0) x nYear]` matrix.
#' @keywords internal
.ApplyIndexFrequency <- function(IndexHist, IndexFreq, YearLH, YearCur, Years) {
  IndexHist[IndexHist < 0] <- NA
  nIndex <- nrow(IndexHist)
  nYear  <- ncol(IndexHist)
  nKeep  <- sum(IndexFreq != 0)
  IndexKeep <- array(NA, c(nKeep, nYear))
  Selected  <- seq_len(nIndex)[IndexFreq > 0]

  j <- 0
  for (i in Selected) {
    j <- j + 1
    Vec     <- IndexHist[i, ]
    ProjInd <- match((YearLH + 1):YearCur, Years)
    if (!is.na(ProjInd[1])) {
      nProj    <- length(ProjInd)
      DropMask <- rep(c(rep(TRUE, IndexFreq[i] - 1), FALSE), 100)[seq_len(nProj)]
      Vec[ProjInd[DropMask]] <- NA
    }
    IndexKeep[j, ] <- Vec
  }
  IndexKeep
}

#' Optionally Smooth Each Row of an Index Matrix
#'
#' @param IndexHist `[nSelected x nYear]` matrix of index values.
#' @param Smooth Logical; see [IndexRate()].
#' @param ENPMult Passed to [SmoothSeries()]; see [IndexRate()].
#' @return A `[nSelected x nYear]` matrix, smoothed row-wise when
#'   `Smooth = TRUE`, otherwise `IndexHist` unchanged.
#' @keywords internal
.SmoothIndices <- function(IndexHist, Smooth, ENPMult) {
  if (!Smooth) return(IndexHist)
  t(apply(IndexHist, 1, SmoothSeries, ENPMult = ENPMult))
}

#' Average the Most Recent Years of an Index Matrix into a Status Vector
#'
#' @param IndexSmooth `[nSelected x nYear]` matrix of (optionally smoothed)
#'   index values.
#' @param RecentYears Positive integer; see [IndexRate()].
#' @return A numeric vector, length `nSelected`.
#' @keywords internal
.RecentStatus <- function(IndexSmooth, RecentYears) {
  nYear <- ncol(IndexSmooth)
  Cols  <- seq(max(1, nYear - RecentYears + 1), nYear)
  rowMeans(IndexSmooth[, Cols, drop = FALSE], na.rm = TRUE)
}

#' Estimate a Per-Index Log-Linear Trend Slope
#'
#' @param IndexSmooth `[nSelected x nYear]` matrix of (optionally smoothed)
#'   index values.
#' @param TrendYears `NULL` (trend disabled) or a positive integer, `>= 2`;
#'   see [IndexRate()].
#' @return A numeric vector, length `nSelected`, of annual log-scale slopes
#'   (`0` for every index when `TrendYears` is `NULL`, or where an index has
#'   fewer than 2 non-`NA`/positive values in the trend window).
#' @keywords internal
.TrendSlope <- function(IndexSmooth, TrendYears) {
  nIndex <- nrow(IndexSmooth)
  if (is.null(TrendYears)) return(rep(0, nIndex))

  nYear <- ncol(IndexSmooth)
  Cols  <- seq(max(1, nYear - TrendYears + 1), nYear)
  Time  <- Cols

  vapply(seq_len(nIndex), function(i) {
    y  <- IndexSmooth[i, Cols]
    ok <- !is.na(y) & y > 0
    if (sum(ok) < 2) return(0)
    stats::coef(stats::lm(log(y[ok]) ~ Time[ok]))[2]
  }, numeric(1))
}

#' Split a Stock-Wide TAC Across Fleets by a Fixed Allocation
#'
#' @param TAC Single numeric TAC value.
#' @param Allocation `NULL`, or a positive numeric vector of length `nFleet`;
#'   see [IndexRate()].
#' @param Data A [data-class] object, used to determine `nFleet`.
#' @return `TAC` unchanged if `Allocation` is `NULL`, otherwise a numeric
#'   vector of length `nFleet`.
#' @keywords internal
.ApplyAllocation <- function(TAC, Allocation, Data) {
  if (is.null(Allocation)) return(TAC)

  nFleet <- ncol(Data@Landings@Value)
  if (length(Allocation) != nFleet)
    cli::cli_abort("{.arg Allocation} must have length {.val {nFleet}} (one per fleet).")
  if (any(Allocation < 0) || !any(Allocation > 0))
    cli::cli_abort("{.arg Allocation} must be non-negative with at least one positive value.")

  TAC * (Allocation / sum(Allocation))
}
