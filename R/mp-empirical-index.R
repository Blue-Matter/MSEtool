#' Empirical Index-Based Management Procedures
#'
#' Management procedures that set a TAC from one or more relative abundance
#' indices (`Survey(Data)` or `CPUE(Data)`). 
#' 
#' Both smooth the selected index/indices with [stats::loess()]
#' before use, to reduce sensitivity to observation noise, and both
#' constrain the resulting TAC change via `DeltaDown`/`DeltaUp`/`TACRange`.
#'
#' - `IndexRate()`: Calibrates a catch-per-index rate from `CalibYears` of
#'   recent history, damps that rate (`catch_per_index x IndexFactor`)
#'   through a hockey-stick harvest control rule
#'   (`HCRControlPointsIndex`/`HCRControlPointsRate`) based on the current
#'   index level relative to `IndexTarget`, then multiplies the adjusted
#'   rate by `smoothed_index[terminal]` to get a trial TAC. `IndexFactor = 1`
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
#' @param IndexSource Character. Use indices from `Data@Survey` (default) or
#'   `Data@CPUE`.
#' @param IndexFreq Positive integer vector, same length as the selected
#'   indices. How often each index is available in the projection period
#'   (`1` = every year, `2` = every 2 years, `0` = excluded entirely).
#'   `NULL` (default) assumes every year.
#' @param IndexWeight Positive numeric vector, same length as the selected
#'   indices, giving each index's weight when averaging. `NULL` (default)
#'   weights indices equally.
#' @param ENPMult Fraction. Smoothing parameter for [stats::loess()]: the
#'   number of effective parameters is `length(index) x ENPMult`. Larger
#'   values mean less smoothing.
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
#'   to calculate the catch-per-index calibration ratio.
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
#' @param Responsiveness Positive number. Responsiveness of the TAC-change
#'   calculation: `TAC change = exp(log(new_TAC / old_TAC) * Responsiveness)`.
#'   Values below `1` damp the implied change; `1` (default) applies it in
#'   full.
#' @export
IndexRate <- function(Data,
                      Indices               = NULL,
                      IndexSource           = c('Survey', 'CPUE'),
                      IndexFreq             = NULL,
                      IndexWeight           = NULL,
                      CalibYears            = 2,
                      ENPMult               = 0.3,
                      IndexFactor           = 1,
                      IndexTarget           = NULL,
                      HCRControlPointsIndex = c(0, 0),
                      HCRControlPointsRate  = c(0, 1),
                      Responsiveness        = 1,
                      DeltaDown             = c(0.01, 0.5),
                      DeltaUp               = c(0.01, 0.5),
                      TACRange              = NULL) {

  CheckCatch(Data)
  IndexSource <- match.arg(IndexSource)

  Selected <- .SelectIndices(slot(Data, IndexSource), Indices)
  nSel <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)

  Ref <- .ResolveIndexTarget(Selected$Ref, IndexTarget, nSel, IndexSource)

  LHInd   <- LastHistYearInd(Data)
  YearCur <- max(Data@Years)

  IndexHist <- .ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, YearCur, Data@Years)

  CalibRows   <- LHInd - (CalibYears - 1):0
  Removals    <- rowSums(Data@Landings@Value, na.rm = TRUE) + rowSums(Data@Discards@Value, na.rm = TRUE)
  CalibCatch  <- mean(Removals[CalibRows], na.rm = TRUE)
  CalibIndex  <- rowMeans(IndexHist[, CalibRows, drop = FALSE], na.rm = TRUE)
  CatchPerIndex <- CalibCatch / CalibIndex

  IndexSmooth <- t(apply(IndexHist, 1, SmoothSeries, ENPMult = ENPMult))

  TerminalIndex <- IndexSmooth[, ncol(IndexSmooth)]
  if (all(is.na(TerminalIndex)))
    cli::cli_abort("All index values are {.val NA} in the terminal year.")

  Est      <- stats::weighted.mean(TerminalIndex, IndexWeight, na.rm = TRUE)
  RefLevel <- stats::weighted.mean(Ref, IndexWeight, na.rm = TRUE)

  TrialRate <- CatchPerIndex * IndexFactor
  AdjRate   <- HockeyStickHCR(TrialRate, Est = Est, Ref = RefLevel,
                              ControlPointsIndex = HCRControlPointsIndex,
                              ControlPointsRate  = HCRControlPointsRate)

  TrialTAC <- mean(TerminalIndex * AdjRate, na.rm = TRUE)
  PrevTAC  <- LastTAC(Data)
  if (is.na(TrialTAC)) TrialTAC <- PrevTAC
  TrialTAC <- FilterTAC(TrialTAC)

  Mod <- exp(log(TrialTAC / PrevTAC) * Responsiveness)

  if (is.null(TACRange))
    TACRange <- c(0, 100 * max(Removals, na.rm = TRUE))

  TAC <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)

  Advice(TAC = TAC, TACType = 'Removals', TACUnit = Data@Landings@Units)
}
class(IndexRate) <- 'mp'


#' @rdname IndexMPs
#' @export
IndexTarget <- function(Data,
                        Indices        = NULL,
                        IndexSource    = c('Survey', 'CPUE'),
                        IndexFreq      = NULL,
                        IndexWeight    = NULL,
                        ENPMult        = 0.3,
                        IndexTarget    = NULL,
                        Responsiveness = 1,
                        DeltaDown      = c(0.01, 0.5),
                        DeltaUp        = c(0.01, 0.5),
                        TACRange       = NULL) {

  CheckCatch(Data)
  IndexSource <- match.arg(IndexSource)

  Selected <- .SelectIndices(slot(Data, IndexSource), Indices)
  nSel <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)

  Ref <- .ResolveIndexTarget(Selected$Ref, IndexTarget, nSel, IndexSource)

  YearCur   <- max(Data@Years)
  IndexHist <- .ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, YearCur, Data@Years)

  IndexSmooth <- t(apply(IndexHist, 1, SmoothSeries, ENPMult = ENPMult))

  TerminalIndex <- IndexSmooth[, ncol(IndexSmooth)]
  if (all(is.na(TerminalIndex)))
    cli::cli_abort("All index values are {.val NA} in the terminal year.")

  Est      <- stats::weighted.mean(TerminalIndex, IndexWeight, na.rm = TRUE)
  RefLevel <- stats::weighted.mean(Ref, IndexWeight, na.rm = TRUE)

  Mod <- exp(log(Est / RefLevel) * Responsiveness)

  PrevTAC  <- LastTAC(Data)
  Removals <- rowSums(Data@Landings@Value, na.rm = TRUE) + rowSums(Data@Discards@Value, na.rm = TRUE)
  if (is.null(TACRange))
    TACRange <- c(0, 100 * max(Removals, na.rm = TRUE))

  TAC <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)

  Advice(TAC = TAC, TACType = 'Removals', TACUnit = Data@Landings@Units)
}
class(IndexTarget) <- 'mp'


#' Select and Validate Indices from an `indicesdata` Object
#'
#' @param IndexData An [indicesdata-class] object (`Data@Survey` or
#'   `Data@CPUE`).
#' @param Indices Character, integer, or `NULL`; see [IndexRate()].
#' @return A list with elements `Value` (`[nYear x nSelected]`) and `Ref`
#'   (length `nSelected`, `NA` where unset).
#' @keywords internal
.SelectIndices <- function(IndexData, Indices) {
  if (is.null(IndexData@Value))
    cli::cli_abort("No index data available in the selected {.arg IndexSource}.")

  nIndex <- ncol(IndexData@Value)

  if (is.null(Indices)) {
    Sel <- seq_len(nIndex)
  } else if (is.character(Indices)) {
    Sel <- match(Indices, IndexData@Name)
    if (anyNA(Sel))
      cli::cli_abort("{.val {Indices[is.na(Sel)]}} not found in {.field Name} of the selected {.arg IndexSource}.")
  } else {
    Sel <- as.integer(Indices)
    if (!all(Sel %in% seq_len(nIndex)))
      cli::cli_abort("{.arg Indices} must be within {.val 1} to {.val {nIndex}}.")
  }

  Ref <- IndexData@Ref
  Ref <- if (is.null(Ref)) rep(NA_real_, nIndex)[Sel] else Ref[Sel]

  list(Value = IndexData@Value[, Sel, drop = FALSE], Ref = Ref)
}

#' Resolve the Target ("on target") Index Level
#'
#' Uses the user-supplied `IndexTarget`, falling back to `Ref` from the
#' selected [indicesdata-class] object (see [.SelectIndices()]). Errors if
#' neither is available.
#' @param Ref Numeric vector from `.SelectIndices()$Ref`.
#' @param IndexTarget User-supplied override; see [IndexRate()].
#' @param nSel Number of selected indices.
#' @param IndexSource Character, for the error message.
#' @keywords internal
.ResolveIndexTarget <- function(Ref, IndexTarget, nSel, IndexSource) {
  if (!is.null(IndexTarget))
    Ref <- rep_len(IndexTarget, nSel)

  if (anyNA(Ref))
    cli::cli_abort(c(
      "No reference (target) index level available for one or more selected indices.",
      "i" = "Set {.field Ref} on {.code Data@{IndexSource}}, or supply {.arg IndexTarget}."
    ))
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

