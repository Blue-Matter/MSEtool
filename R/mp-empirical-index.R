#' Empirical Index-Based Management Procedures
#'
#' Management procedures that set a TAC from one or more relative abundance
#' indices (`Survey(Data)` or `CPUE(Data)`, or both).
#'
#' Both MPs estimate the current status of each selected index as its
#' (optionally loess-smoothed, see `Smooth`) value averaged over the
#' `RecentYears` most recent years. When `TrendYears` is set, that status is
#' projected forward `TrendHorizon` years along a log-linear trend before it
#' is compared with the index target. Per-index values are combined with
#' `IndexWeight`, and the proposed change from the previous TAC
#' ([LastTAC()]) is raised to the power `Responsiveness` and then constrained
#' by `DeltaDown`/`DeltaUp`/`TACRange`.
#'
#' - `IndexRate()`: Calculates a catch-per-index rate as the mean catch of
#'   type `TACType` (summed over fleets) over the last `CalibYears`
#'   historical years divided by the mean unsmoothed index over the same
#'   years, then multiplies it by `IndexFactor` and `tunepar`. That rate is
#'   scaled by a hockey-stick harvest control rule
#'   (`HCRControlPointsIndex`/`HCRControlPointsRate`) evaluated at
#'   `status / IndexTarget`, and the trial TAC is the scaled rate multiplied
#'   by the current (non-projected) status. With the default control points
#'   the harvest control rule is inactive, so the trial TAC is
#'   `rate x current status` and `IndexTarget` has no effect. If the trial
#'   TAC is negative, infinite, or `NA`, the previous TAC is kept.
#' - `IndexTarget()`: Multiplies the previous TAC by
#'   `(status x tunepar / IndexTarget) ^ Responsiveness`, so the TAC
#'   increases while the index is above target and decreases while it is
#'   below.
#'
#' For seasonal data (`Data@@Seasons > 1`), both MPs first aggregate `Data` to
#' calendar years with [AnnualData()]: catches are summed over seasons and
#' each index is averaged over the seasons in `IndexSeasons`. All arguments
#' given in years (`CalibYears`, `RecentYears`, `TrendYears`, `TrendHorizon`,
#' `IndexFreq`) therefore refer to calendar years, and the TAC is annual.
#'
#' @param Data A [data-class] object.
#' @param Indices Character (matching `Name`) or integer vector selecting
#'   which columns of the chosen `IndexSource` to use. `NULL` (default) uses
#'   every index in `IndexSource`.
#' @param IndexSource Character vector of one or both of `'Survey'`,
#'   `'CPUE'`. Use indices from `Data@Survey` (default, `'Survey'`),
#'   `Data@CPUE`, or both (`c('Survey', 'CPUE')`). When both are used, their
#'   indices are combined into a single selection (`Survey` columns first,
#'   then `CPUE`), and `Indices`, `IndexFreq`, `IndexWeight`, `IndexTarget`
#'   and `IndexSeasons` apply across that combined set.
#' @param IndexFreq Non-negative integer vector, one per selected index. How
#'   often each index is available in the projection period (`1` = every
#'   year, `2` = every 2 years, `0` = not used). Historical values are
#'   always used. `NULL` (default) assumes every year.
#' @param IndexWeight Positive numeric vector, one per selected index, giving
#'   each index's weight when combining indices. `NULL` (default) weights
#'   indices equally.
#' @param Smooth Logical. Whether to smooth each selected index with
#'   [SmoothSeries()] before calculating its status and trend. `TRUE`
#'   (default). `FALSE` uses the observed index values, e.g. for a series too
#'   short for a stable loess fit.
#' @param ENPMult Fraction. Smoothing parameter for [SmoothSeries()]: the
#'   number of effective parameters is `length(index) x ENPMult`. Larger
#'   values mean less smoothing. Ignored when `Smooth = FALSE`.
#' @param RecentYears Positive integer. Number of most recent years averaged,
#'   per index, to obtain its current status. Years without an observation
#'   are ignored. `1` (default) uses only the terminal year.
#' @param TrendYears `NULL` (default) disables trend projection. Otherwise an
#'   integer `>= 2` giving the number of most recent years (per index) used
#'   to estimate a log-linear trend. The status is then projected forward
#'   `TrendHorizon` years along that trend before it is compared with
#'   `IndexTarget`, so a declining index is treated more cautiously than a
#'   stable one at the same level. In `IndexRate()` the projected status is
#'   used only in the harvest control rule; the trial TAC is always
#'   calculated from the current status.
#' @param TrendHorizon Positive number. Number of years the estimated trend
#'   is extrapolated forward; see `TrendYears`. Default `1`. Ignored when
#'   `TrendYears` is `NULL`.
#' @param IndexTarget `NULL` (default), or a positive number (or vector, one
#'   per selected index) giving the target index level, in the units of the
#'   (annual) index. Targets of multiple indices are combined with
#'   `IndexWeight`. When `NULL`, `Ref` on the selected `IndexSource` object
#'   (see [IndicesData()]) is used, and an index with no `Ref` uses its
#'   status at the end of the historical period: the historical index,
#'   smoothed as set by `Smooth`/`ENPMult`, averaged over the `RecentYears`
#'   ending in the last historical year with an observation. The target is
#'   fixed across management cycles.
#'   - In `IndexTarget()`, the TAC is adjusted each management cycle to move
#'     the index toward this level.
#'   - In `IndexRate()`, it is only the reference level for the harvest
#'     control rule: `HCRControlPointsIndex` is expressed as a fraction of
#'     `IndexTarget`. It has no effect with the default
#'     `HCRControlPointsIndex = c(0, 0)`.
#' @param Responsiveness Positive number. Exponent applied to the proposed
#'   TAC ratio before `DeltaDown`/`DeltaUp` are applied: in `IndexRate()`,
#'   `(trial TAC / previous TAC) ^ Responsiveness`; in `IndexTarget()`,
#'   `(status / IndexTarget) ^ Responsiveness`. Values below `1` damp the
#'   change; `1` (default) applies it in full.
#' @param DeltaDown,DeltaUp Numeric vector, length 2 (`c(min, max)`). Limits
#'   on the fractional TAC change from the previous TAC, for decreases and
#'   increases respectively. Changes larger than `max` are capped at `max`;
#'   changes smaller than `min` leave the TAC unchanged. See [ConstrainTAC()].
#' @param TACRange Numeric vector, length 2 (`c(min, max)`). Absolute bounds
#'   on the TAC, applied after `DeltaDown`/`DeltaUp`. `NULL` (default) uses
#'   `c(0, 100 x max(annual catch in Data))`, with catch of type `TACType`,
#'   which in practice imposes no limit.
#' @param Allocation `NULL` (default), or a non-negative numeric vector of
#'   length `nFleet` giving the fraction of the TAC allocated to each fleet
#'   (normalised to sum to `1`). `NULL` returns a single stock-wide TAC,
#'   which the framework then splits across fleets using `FleetAllocation(OM)`
#'   (see [Advice()]).
#' @param TACType Character. Whether the TAC applies to `'Removals'`
#'   (default; landings plus discards) or `'Landings'`. Sets the catch used
#'   for the `IndexRate()` calibration, for the previous TAC in the first
#'   management cycle (see [LastTAC()]), and for the default `TACRange`. See
#'   [Advice()].
#' @param IndexSeasons Seasonal data only. `NULL` (default) averages each
#'   index over every season of the year. Otherwise an integer vector of
#'   seasons applied to every selected index, or a list with one element
#'   (integer vector, or `NULL` for every season) per selected index. See
#'   [AnnualData()]. Ignored when `Data@@Seasons = 1`.
#' @param tunepar Positive number used to tune the MP (see [TuneMP()]);
#'   larger values give higher catches. `1` (default) applies the MP as
#'   specified. In `IndexRate()` it multiplies the catch-per-index rate; in
#'   `IndexTarget()` it divides `IndexTarget`.
#'
#' @return An [advice-class] object with `TAC` and `TACType` set, and
#'   `TACUnit` taken from `Data@Landings@Units`.
#'
#' @seealso [Advice()], [CheckCatch()], [LastTAC()], [FilterTAC()],
#'   [HockeyStickHCR()], [ConstrainTAC()], [SmoothSeries()], [data-class],
#'   [advice-class]
#' @name IndexMPs
NULL


#' @rdname IndexMPs
#' @param CalibYears Positive integer. Number of most recent historical years
#'   used to calculate the catch-per-index rate. Default `2`. Clamped to the
#'   number of historical years available.
#' @param IndexFactor Positive number. Multiplier on the calibrated
#'   catch-per-index rate. `1` (default) keeps the rate observed over
#'   `CalibYears`; e.g. `0.75` sets it 25% lower.
#' @param HCRControlPointsIndex Numeric vector of at least 2 non-decreasing
#'   values: the control points of a hockey-stick harvest control rule, as
#'   fractions of `IndexTarget` (i.e. in units of `status / IndexTarget`).
#'   At or below the first control point the catch-per-index rate is
#'   multiplied by the first value of `HCRControlPointsRate`; at or above the
#'   last, by the last value; in between, the multiplier is interpolated
#'   (see `RampType`). E.g. `c(0.2, 0.8)` with `HCRControlPointsRate = c(0, 1)`
#'   closes the fishery below 20% of `IndexTarget` and applies the full rate
#'   above 80%. `c(0, 0)` (default) makes the multiplier `1` for any positive
#'   status, so the harvest control rule and `IndexTarget` have no effect.
#'   See [HockeyStickHCR()].
#' @param HCRControlPointsRate Numeric vector, same length as
#'   `HCRControlPointsIndex`, giving the rate multiplier at each control
#'   point. Default `c(0, 1)`.
#' @param RampType Character. Shape of the harvest control rule between
#'   control points: `'linear'` (default) or `'smooth'` (a cubic smoothstep,
#'   with zero slope at each control point).
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
                      Allocation            = NULL,
                      TACType               = c('Removals', 'Landings'),
                      IndexSeasons          = NULL,
                      tunepar               = 1) {

  RampType <- match.arg(RampType, c('linear', 'smooth'))
  TACType  <- match.arg(TACType, c('Removals', 'Landings'))
  .CheckTunePar(tunepar)

  CheckCatch(Data)
  IndexSource <- match.arg(IndexSource, c('Survey', 'CPUE'), several.ok = TRUE)
  Data <- AnnualData(Data, .IndexSeasonsBySource(Data, IndexSource, Indices, IndexSeasons))

  Selected <- .SelectIndices(Data, IndexSource, Indices)
  nSel <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)

  LHInd   <- LastHistYearInd(Data)
  YearCur <- max(Data@Years)

  IndexHist <- .ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, YearCur, Data@Years)

  Keep        <- IndexFreq > 0
  IndexWeight <- IndexWeight[Keep]
  Ref <- .ResolveIndexTarget(Selected$Ref, IndexTarget, Keep, IndexHist, LHInd,
                             Smooth, ENPMult, RecentYears)

  CalibRows   <- seq(max(1, LHInd - CalibYears + 1), LHInd)
  Catch       <- .AnnualCatchByType(Data, TACType)
  CalibCatch  <- mean(Catch[CalibRows], na.rm = TRUE)
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

  TrialRate <- CatchPerIndex * IndexFactor * tunepar
  AdjRate   <- HockeyStickHCR(TrialRate, 
                              Est = Est, 
                              Ref = RefLevel,
                              ControlPointsIndex = HCRControlPointsIndex,
                              ControlPointsRate  = HCRControlPointsRate,
                              RampType           = RampType)

  TrialTAC <- stats::weighted.mean(Status * AdjRate, IndexWeight, na.rm = TRUE)
  PrevTAC  <- LastTAC(Data, TACType)
  TrialTAC <- FilterTAC(TrialTAC)
  if (is.na(TrialTAC)) TrialTAC <- PrevTAC

  Mod <- exp(log(TrialTAC / PrevTAC) * Responsiveness)

  if (is.null(TACRange))
    TACRange <- c(0, 100 * max(Catch, na.rm = TRUE))

  TAC <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)
  TAC <- .ApplyAllocation(TAC, Allocation, Data)

  Advice(TAC = TAC, TACType = TACType, TACUnit = Data@Landings@Units)
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
                        Allocation     = NULL,
                        TACType        = c('Removals', 'Landings'),
                        IndexSeasons   = NULL,
                        tunepar        = 1) {

  TACType <- match.arg(TACType, c('Removals', 'Landings'))
  .CheckTunePar(tunepar)
  CheckCatch(Data)
  IndexSource <- match.arg(IndexSource, c('Survey', 'CPUE'), several.ok = TRUE)
  Data <- AnnualData(Data, .IndexSeasonsBySource(Data, IndexSource, Indices, IndexSeasons))

  Selected <- .SelectIndices(Data, IndexSource, Indices)
  nSel <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)

  LHInd     <- LastHistYearInd(Data)
  YearCur   <- max(Data@Years)
  IndexHist <- .ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, YearCur, Data@Years)

  Keep        <- IndexFreq > 0
  IndexWeight <- IndexWeight[Keep]
  Ref <- .ResolveIndexTarget(Selected$Ref, IndexTarget, Keep, IndexHist, LHInd,
                             Smooth, ENPMult, RecentYears)

  IndexSmooth <- .SmoothIndices(IndexHist, Smooth, ENPMult)

  Status <- .RecentStatus(IndexSmooth, RecentYears)
  if (all(is.na(Status)))
    cli::cli_abort("All index values are {.val NA} over the {.arg RecentYears} status window.")

  Slope       <- .TrendSlope(IndexSmooth, TrendYears)
  StatusTrend <- Status * exp(Slope * TrendHorizon)

  Est      <- stats::weighted.mean(StatusTrend, IndexWeight, na.rm = TRUE)
  RefLevel <- stats::weighted.mean(Ref, IndexWeight, na.rm = TRUE)

  # a loess-smoothed status can dip below zero
  Mod <- exp(log(max(Est * tunepar / RefLevel, 0)) * Responsiveness)

  PrevTAC <- LastTAC(Data, TACType)
  Catch   <- .AnnualCatchByType(Data, TACType)
  if (is.null(TACRange))
    TACRange <- c(0, 100 * max(Catch, na.rm = TRUE))

  TAC <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)
  TAC <- .ApplyAllocation(TAC, Allocation, Data)

  Advice(TAC = TAC, TACType = TACType, TACUnit = Data@Landings@Units)
}
class(IndexTarget) <- 'mp'


#' Annual Catch Summed Over Fleets
#'
#' @param Data An annual [data-class] object (see [AnnualData()]).
#' @param TACType `'Removals'` (landings plus discards) or `'Landings'`.
#' @return Numeric vector, one value per `Data@@Years`.
#' @keywords internal
.AnnualCatchByType <- function(Data, TACType) {
  Catch <- rowSums(Data@Landings@Value, na.rm = TRUE)
  if (TACType == 'Removals' && !is.null(Data@Discards@Value))
    Catch <- Catch + rowSums(Data@Discards@Value, na.rm = TRUE)
  Catch
}


#' Select and Validate Indices from One or More `indicesdata` Objects
#'
#' @param Data A [data-class] object.
#' @param IndexSource Character vector of one or both of `'Survey'`,
#'   `'CPUE'`; see [IndexRate()]. Indices from each requested source are
#'   combined column-wise, in the order given.
#' @param Indices Character, integer, or `NULL`; see [IndexRate()].
#' @return A list with elements `Value` and `CV` (`[nYear x nSelected]`;
#'   `CV` is `NA` where unset), and, each of length `nSelected`: `Ref` (`NA`
#'   where unset), `Name`, `Timing` (`0` where unset), `Units` (`NA` where
#'   unset), and `Source`/`Column` (the slot and column each selected index
#'   comes from).
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
    CV <- IndexData@CV
    if (is.null(CV) || !identical(dim(CV), dim(IndexData@Value)))
      CV <- array(NA_real_, dim(IndexData@Value))
    Timing <- IndexData@Timing
    if (!length(Timing)) Timing <- 0
    Units <- IndexData@Units
    if (!length(Units)) Units <- NA_character_
    list(Value = IndexData@Value, CV = CV, Name = Name, Ref = Ref,
         Timing = rep_len(Timing, nIndex), Units = rep_len(Units, nIndex),
         Source = rep(src, nIndex), Column = seq_len(nIndex))
  })

  Value  <- do.call(cbind, lapply(Sources, `[[`, 'Value'))
  CV     <- do.call(cbind, lapply(Sources, `[[`, 'CV'))
  Name   <- unlist(lapply(Sources, `[[`, 'Name'),   use.names = FALSE)
  Ref    <- unlist(lapply(Sources, `[[`, 'Ref'),    use.names = FALSE)
  Timing <- unlist(lapply(Sources, `[[`, 'Timing'), use.names = FALSE)
  Units  <- unlist(lapply(Sources, `[[`, 'Units'),  use.names = FALSE)
  Source <- unlist(lapply(Sources, `[[`, 'Source'), use.names = FALSE)
  Column <- unlist(lapply(Sources, `[[`, 'Column'), use.names = FALSE)
  Name   <- make.unique(Name)

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

  list(Value = Value[, Sel, drop = FALSE], CV = CV[, Sel, drop = FALSE],
       Ref = Ref[Sel], Name = Name[Sel], Timing = Timing[Sel], Units = Units[Sel],
       Source = Source[Sel], Column = Column[Sel])
}

#' Map a Per-Selected-Index `IndexSeasons` List to [AnnualData()]'s Per-Slot Form
#'
#' @param Data A [data-class] object (before aggregation).
#' @param IndexSource,Indices See [IndexRate()].
#' @param IndexSeasons `NULL`, an integer vector, or a list with one element
#'   per selected index; see [IndexRate()].
#' @return `IndexSeasons` unchanged unless it is a list, in which case a list
#'   with one element per `IndexSource` slot, each a list with one element per
#'   index in that slot (`NULL` for unselected indices).
#' @keywords internal
.IndexSeasonsBySource <- function(Data, IndexSource, Indices, IndexSeasons) {
  if (!is.list(IndexSeasons))
    return(IndexSeasons)

  Selected <- .SelectIndices(Data, IndexSource, Indices)
  if (length(IndexSeasons) != length(Selected$Source))
    cli::cli_abort("A list {.arg IndexSeasons} must have one element per selected index ({.val {length(Selected$Source)}}).")

  Out <- list()
  for (src in unique(Selected$Source)) {
    BySlot <- vector('list', ncol(slot(Data, src)@Value))
    Ind <- which(Selected$Source == src)
    BySlot[Selected$Column[Ind]] <- IndexSeasons[Ind]
    Out[[src]] <- BySlot
  }
  Out
}

#' Resolve the Target ("on target") Index Level
#'
#' Uses the user-supplied `IndexTarget`, falling back to `Ref` from the
#' selected [indicesdata-class] object(s). Any index still without a target
#' uses its status at the last historical year: the index over the historical
#' years only, smoothed as in the MP (`Smooth`, `ENPMult`) and averaged over
#' the `RecentYears` ending at the last historical year with a non-`NA`
#' value.
#' @param Ref Numeric vector from `.SelectIndices()$Ref`.
#' @param IndexTarget User-supplied override; see [IndexRate()].
#' @param Keep Logical vector, one per selected index; `IndexFreq > 0`.
#' @param IndexHist `[sum(Keep) x nYear]` matrix from `.ApplyIndexFrequency()`.
#' @param LHInd Column of `IndexHist` for the last historical year.
#' @param Smooth,ENPMult,RecentYears See [IndexRate()].
#' @return Numeric vector, length `sum(Keep)`.
#' @keywords internal
.ResolveIndexTarget <- function(Ref, IndexTarget, Keep, IndexHist, LHInd,
                                Smooth, ENPMult, RecentYears) {
  if (!is.null(IndexTarget))
    Ref <- rep_len(IndexTarget, length(Keep))
  Ref <- Ref[Keep]

  Missing <- which(is.na(Ref))
  if (!length(Missing))
    return(Ref)

  Hist <- .SmoothIndices(IndexHist[Missing, seq_len(LHInd), drop = FALSE], Smooth, ENPMult)
  Ref[Missing] <- apply(Hist, 1, function(x) {
    Last <- utils::tail(which(!is.na(x)), 1)
    if (!length(Last))
      cli::cli_abort("No historical index values available to set a default {.arg IndexTarget}.")
    mean(x[seq(max(1, Last - RecentYears + 1), Last)], na.rm = TRUE)
  })
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
