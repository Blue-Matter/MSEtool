#' Surplus Production Model Management Procedure
#'
#' A model-based management procedure that fits a surplus production model to
#' the catch and one or more relative abundance indices with [FitSP()], sets a
#' fishing mortality from the estimated stock status with a harvest control
#' rule, and projects the fitted model to calculate the TAC (or effort) for
#' the next management interval.
#'
#' ## Model parameters
#'
#' The leading parameters of the surplus production model are:
#'
#' - `FMSY`: the fishing mortality that produces MSY. Always estimated.
#' - `MSY`: the maximum sustainable yield, in catch units. Always estimated.
#' - `Depletion`: biomass at the start of the first fitted year relative to
#'   `K`. Fixed at `Depletion` (default `1`, an unfished stock at the start
#'   of the fitted years), or estimated with `EstDepletion = TRUE`.
#' - `Shape`: the shape parameter `n` of the production function, which sets
#'   `BMSY/K = n^(1/(1-n))`. `n = 2` (the default) is the Schaefer model
#'   (`BMSY/K = 0.5`), and `n = 1` the Fox model (`BMSY/K = exp(-1)`, the
#'   limit as `n` approaches `1`). Fixed at `Shape`, or estimated with
#'   `EstShape = TRUE` with a lognormal prior (median `Shape`, CV `ShapeCV`,
#'   or `Priors$Shape`).
#'
#' The derived quantities are `BMSY = MSY / FMSY` and `K = BMSY / (BMSY/K)`.
#' Lognormal priors can be set on any estimated leading parameter with
#' `Priors`. With `Model = 'internal'`, the catchability of each index is
#' calculated at its maximum likelihood value given the leading parameters,
#' and the observation standard deviation of each index is either calculated
#' the same way (`IndexSD = 'estimate'`) or set from the index `CV`
#' (`IndexSD = 'data'`). See [FitSP()] for the model equations, and
#' [SpictControl()] for how the leading parameters map to the `spict`
#' parameterisation with `Model = 'spict'`, which also estimates process
#' error and catch observation error.
#'
#' ## Procedure
#'
#' 1. The model is fitted to the calendar-year data (see [FitSP()] for the
#'    models and the index selection arguments, and [AnnualData()] for
#'    seasonal data). Each fit starts from the estimates of the previous
#'    management cycle, if these were made with the same `Model` (`WarmStart`
#'    in [SPControl()]/[SpictControl()]).
#' 2. Biomass at the start of `AdviceYear` is calculated by projecting the
#'    fitted model through any years between the last fitted year and
#'    `AdviceYear` (when there is a data lag, or `FitYears` ends before the
#'    last data year), with the catch set by `GapCatch`.
#' 3. The fishing mortality is
#'    `HockeyStickHCR(tunepar * FTarget * FMSY, Est = B/BMSY, Ref = 1)`, where
#'    `B/BMSY` is the projected status at the start of `AdviceYear` and the
#'    control points are `HCRControlPointsBiomass`/`HCRControlPointsRate` (see
#'    [HockeyStickHCR()]). With `FractileB` (`FractileF`), the `FractileB`
#'    (`FractileF`) quantile of `B/BMSY` (`FMSY`) is used instead of the point
#'    estimate, assuming a lognormal distribution with the standard error of
#'    the estimate (for `B/BMSY`, the standard error at the start of the year
#'    after the last fitted year).
#' 4. The model is projected at that fishing mortality for the `Interval`
#'    years of the management cycle. The TAC is the mean (`TACYears = 'mean'`)
#'    or first-year (`'first'`) catch. With `AdviceType = 'Effort'`, the advice
#'    is the effort relative to the last historical year, `F / F(YearLH)`.
#' 5. The TAC is converted to `TACType` using the landed fraction of the
#'    removals over the last `LandedFracYears` data years (when `TACType`
#'    differs from `CatchType`). The change from the previous TAC (or effort)
#'    is scaled by `Responsiveness` and constrained by `DeltaDown` and
#'    `DeltaUp`, and the TAC is bounded by `TACRange` (see [ConstrainTAC()]).
#'    If the previous TAC is zero, the TAC is only bounded by `TACRange`.
#'
#' If the data cannot be prepared, fewer than `MinIndexYears` years have
#' index observations, the model fit fails or does not converge, the
#' standard errors needed by `FractileB`/`FractileF` are not available, or
#' (with `AdviceType = 'Effort'`) the estimated fishing mortality in the last
#' historical year is zero, the advice is set by `OnFail`: `'hold'` keeps
#' the previous TAC (or effort), and `'trend'` changes it by the ratio of the
#' mean index over the last `FallbackYears[1]` fitted years to the mean over
#' the `FallbackYears[2]` years before them (the geometric mean over indices,
#' weighted by `IndexWeight`, and raised to `Responsiveness`). If the data
#' cannot be prepared, `'trend'` keeps the previous advice. The fallback
#' advice is constrained as in
#' step 5. Each such management cycle is recorded as a warning in the `Log`
#' of the returned [advice-class] object.
#'
#' The estimates of each management cycle are stored in
#' `Advice@@Misc$SurplusProduction` (see `Diagnostics`); [SPEstimates()]
#' extracts them from an [mse-class] object.
#'
#' `tunepar = 1` applies the MP as specified; larger values increase the
#' target fishing mortality proportionally. See [TuneMP()].
#'
#' @inheritParams FitSP
#' @param Model Character. The surplus production model: `'internal'`
#'   (default) or `'spict'`. See [FitSP()].
#' @param IndexSD Character. `'estimate'` (default) estimates the observation
#'   standard deviation of each index; `'data'` calculates it from the index
#'   `CV` in `Data` (with `Model = 'spict'`, the `CV`s scale the estimated
#'   standard deviation; see [SpictControl()]). See [FitSP()].
#' @param MinIndexYears Positive integer. Minimum number of years with index
#'   observations required to fit the model. Default `5`.
#' @param FTarget Positive number. Target fishing mortality as a fraction of
#'   the estimated `FMSY`. Default `1`.
#' @param HCRControlPointsBiomass Numeric vector (length `>= 2`,
#'   non-decreasing). Harvest control rule control points in units of
#'   `B/BMSY`. Default `c(0, 0.5)`: fishing mortality declines linearly from
#'   the target at `0.5 BMSY` to zero at `B = 0`.
#' @param HCRControlPointsRate Numeric vector, the same length as
#'   `HCRControlPointsBiomass`. Multipliers of the target fishing mortality at
#'   each control point. Default `c(0, 1)`.
#' @param RampType Character. `'linear'` (default) or `'smooth'`; see
#'   [HockeyStickHCR()].
#' @param FractileB,FractileF `NULL` (default) uses the point estimates of
#'   `B/BMSY` and `FMSY`. Otherwise a probability in `(0, 1)`; e.g. `0.35`
#'   uses the 35th percentile. See Details, step 3.
#' @param tunepar Positive number. Multiplier of the target fishing mortality
#'   used to tune the MP. Default `1`.
#' @param AdviceYear `NULL` (default) or the calendar year the advice first
#'   applies in. `NULL` uses `Data@@Misc$AdviceYear` (set during a
#'   projection), else the year after the last data year.
#' @param Interval `NULL` (default) or the number of years the advice applies
#'   for. `NULL` uses `Data@@Misc$Interval` (set during a projection), else
#'   `1`.
#' @param GapCatch Character. Catch assumed in the years between the last
#'   data year and `AdviceYear`: `'TAC'` (default; the TAC in force, see
#'   [LastTAC()], converted to `CatchType`) or `'LastCatch'` (the catch in
#'   the last fitted year).
#' @param TACYears Character. `'mean'` (default; mean catch over the
#'   `Interval` years) or `'first'` (catch in the first year).
#' @param AdviceType Character. `'TAC'` (default) or `'Effort'`.
#' @param TACType Character. `'Removals'` (default) or `'Landings'`. See
#'   [Advice()].
#' @param LandedFracYears Positive integer. Number of most recent data years
#'   used to calculate the landed fraction of the removals, used when
#'   `TACType` differs from `CatchType`. Default `3`.
#' @param Responsiveness Positive number. The change in advice is
#'   `exp(log(new / previous) * Responsiveness)`. Default `1`.
#' @param DeltaDown,DeltaUp Numeric vectors, length 2 (`c(min, max)`).
#'   Minimum and maximum fractional decrease (`DeltaDown`) and increase
#'   (`DeltaUp`) in the advice between management cycles. Changes smaller than
#'   the minimum are set to zero; see [ConstrainTAC()].
#' @param TACRange Numeric vector, length 2. Absolute bounds on the TAC.
#'   `NULL` (default) uses `c(0, 100 * max(catch))`. Ignored for effort.
#' @param Allocation `NULL` (default), or a non-negative numeric vector of
#'   length `nFleet` giving the fraction of the TAC allocated to each fleet
#'   (normalised to sum to `1`). See [IndexRate()]. Ignored for effort.
#' @param OnFail Character. `'hold'` (default) or `'trend'`; see Details.
#' @param FallbackYears Positive integer vector, length 2. Recent and
#'   preceding window lengths (years) for `OnFail = 'trend'`. Default
#'   `c(2, 3)`.
#' @param Diagnostics Character. Information stored in
#'   `Advice@@Misc$SurplusProduction`: `'min'` (default; the parameter
#'   estimates and a one-row summary, used by [SPEstimates()]), `'none'` (the
#'   parameter estimates only, used to start the next fit), or `'full'` (also
#'   the `spfit` object, without `Prep` and `Spict`; see [FitSP()]). When the
#'   advice is set by `OnFail`, `'min'` is used.
#'
#' @return An [advice-class] object.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' SurplusProduction(Hist@@Data[[1]][[1]])
#'
#' SP_Fox <- SetMPArgs(SurplusProduction, Shape = 1, FTarget = 0.8)
#' MSE <- Project(Hist, MPs = list(SP = SurplusProduction, SP_Fox = SP_Fox))
#' SPEstimates(MSE)
#' }
#'
#' @seealso [FitSP()], [SPEstimates()], [HockeyStickHCR()], [ConstrainTAC()],
#'   [IndexRate()], [TuneMP()]
#' @export
SurplusProduction <- function(Data,
                              Indices                 = NULL,
                              IndexSource             = 'Survey',
                              IndexFreq               = NULL,
                              IndexWeight             = NULL,
                              IndexSeasons            = NULL,
                              IndexUnits              = c('Biomass', 'Number'),
                              CatchType               = c('Removals', 'Landings'),
                              FitYears                = NULL,
                              MinIndexYears           = 5,
                              Model                  = c('internal', 'spict'),
                              Shape                   = 2,
                              EstShape                = FALSE,
                              ShapeCV                 = 0.5,
                              Depletion               = 1,
                              EstDepletion            = FALSE,
                              IndexSD                 = c('estimate', 'data'),
                              Priors                  = list(),
                              Control                 = NULL,
                              FTarget                 = 1,
                              HCRControlPointsBiomass = c(0, 0.5),
                              HCRControlPointsRate    = c(0, 1),
                              RampType                = c('linear', 'smooth'),
                              FractileB               = NULL,
                              FractileF               = NULL,
                              tunepar                 = 1,
                              AdviceYear              = NULL,
                              Interval                = NULL,
                              GapCatch                = c('TAC', 'LastCatch'),
                              TACYears                = c('mean', 'first'),
                              AdviceType              = c('TAC', 'Effort'),
                              TACType                 = c('Removals', 'Landings'),
                              LandedFracYears         = 3,
                              Responsiveness          = 1,
                              DeltaDown               = c(0.01, 0.5),
                              DeltaUp                 = c(0.01, 0.5),
                              TACRange                = NULL,
                              Allocation              = NULL,
                              OnFail                  = c('hold', 'trend'),
                              FallbackYears           = c(2, 3),
                              Diagnostics             = c('min', 'none', 'full')) {

  .CheckClass(Data, 'data', 'Data')
  CatchType   <- match.arg(CatchType, c('Removals', 'Landings'))
  Model       <- match.arg(Model, c('internal', 'spict'))
  IndexSD     <- match.arg(IndexSD, c('estimate', 'data'))
  RampType    <- match.arg(RampType, c('linear', 'smooth'))
  GapCatch    <- match.arg(GapCatch, c('TAC', 'LastCatch'))
  TACYears    <- match.arg(TACYears, c('mean', 'first'))
  AdviceType  <- match.arg(AdviceType, c('TAC', 'Effort'))
  TACType     <- match.arg(TACType, c('Removals', 'Landings'))
  OnFail      <- match.arg(OnFail, c('hold', 'trend'))
  Diagnostics <- match.arg(Diagnostics, c('min', 'none', 'full'))
  for (nm in c('FractileB', 'FractileF')) {
    x <- get(nm)
    if (!is.null(x) && (length(x) != 1 || !is.finite(x) || x <= 0 || x >= 1))
      cli::cli_abort("{.arg {nm}} must be {.code NULL} or a single probability in (0, 1).")
  }
  .CheckTunePar(tunepar)

  CheckCatch(Data)
  State <- Data@Misc$SurplusProduction
  First <- is.null(State)
  Misc  <- Data@Misc[setdiff(names(Data@Misc), .FrameworkMiscFields)]

  AdviceYear <- AdviceYear %||% Data@Misc$AdviceYear %||% (max(.CalendarYear(Data@Years)) + 1)
  AdviceYear <- .CalendarYear(AdviceYear)
  Interval   <- max(1, round(Interval %||% Data@Misc$Interval %||% 1))
  PrevTAC    <- LastTAC(Data, TACType)
  PrevEffort <- .LastEffortAdvice(Data)

  Prep <- tryCatch(
    .SPPrepData(Data, Indices, IndexSource, IndexFreq, IndexWeight, IndexSeasons,
                IndexUnits, CatchType, FitYears),
    error = function(e) conditionMessage(e)
  )
  if (is.character(Prep))
    return(.SPFallback(Data, NULL, PrevTAC, PrevEffort, OnFail, FallbackYears, Responsiveness,
                       DeltaDown, DeltaUp, TACRange, Allocation, AdviceType, TACType, Misc,
                       State, AdviceYear, paste('Data preparation failed:', Prep)))

  nIndexYears <- sum(rowSums(!is.na(Prep$Index) & Prep$Index > 0) > 0)
  if (nIndexYears < MinIndexYears)
    return(.SPFallback(Data, Prep, PrevTAC, PrevEffort, OnFail, FallbackYears, Responsiveness,
                       DeltaDown, DeltaUp, TACRange, Allocation, AdviceType, TACType, Misc,
                       State, AdviceYear,
                       sprintf('%d year(s) with index observations (MinIndexYears = %d).',
                               nIndexYears, MinIndexYears)))

  WarmStart   <- if (is.null(Control)) TRUE else isTRUE(Control$WarmStart)
  Start       <- if (WarmStart && identical(State$Model, Model)) State$par else NULL
  Uncertainty <- !is.null(FractileB) || !is.null(FractileF) || Diagnostics == 'full'

  Fit <- tryCatch(
    .FitSPPrepped(Prep, Model, Shape, EstShape, ShapeCV, Depletion, EstDepletion, IndexSD,
                  Priors, Control, Start, Uncertainty),
    error = function(e) conditionMessage(e)
  )
  if (is.character(Fit) || !Fit$Converged)
    return(.SPFallback(Data, Prep, PrevTAC, PrevEffort, OnFail, FallbackYears, Responsiveness,
                       DeltaDown, DeltaUp, TACRange, Allocation, AdviceType, TACType, Misc,
                       State, AdviceYear,
                       paste('Model fit failed:', if (is.character(Fit)) Fit else Fit$Message),
                       Fit = if (is.character(Fit)) NULL else Fit))

  LandedFrac <- .SPLandedFraction(Prep, LandedFracYears)
  ToModel    <- .SPTypeConversion(TACType, CatchType, LandedFrac)

  LastYear <- max(Prep$Years)
  nGap     <- max(0, AdviceYear - LastYear - 1)
  GapValue <- if (GapCatch == 'TAC') PrevTAC * ToModel else utils::tail(Prep$Catch, 1)
  Gap      <- .ProjectSP(Fit, rep(GapValue, nGap), rep(0L, nGap))
  BAdvice  <- utils::tail(Gap$B, 1)

  EstB    <- BAdvice / Fit$BMSY
  FMSYHCR <- Fit$FMSY
  if (!is.null(FractileB))
    EstB <- .SPFractile(EstB, Fit$SE[['logB_BMSY']], FractileB)
  if (!is.null(FractileF))
    FMSYHCR <- .SPFractile(FMSYHCR, Fit$SE[['logFMSY']], FractileF)
  if (!is.finite(EstB) || !is.finite(FMSYHCR))
    return(.SPFallback(Data, Prep, PrevTAC, PrevEffort, OnFail, FallbackYears, Responsiveness,
                       DeltaDown, DeltaUp, TACRange, Allocation, AdviceType, TACType, Misc,
                       State, AdviceYear, 'Standard errors for the fractile rule are not available.',
                       Fit = Fit))

  FAdvice <- HockeyStickHCR(tunepar * FTarget * FMSYHCR, Est = EstB, Ref = 1,
                            ControlPointsIndex = HCRControlPointsBiomass,
                            ControlPointsRate  = HCRControlPointsRate,
                            RampType           = RampType)

  Proj     <- .ProjectSP(Fit, rep(FAdvice, Interval), rep(1L, Interval), B0 = BAdvice)
  ModelTAC <- if (TACYears == 'mean') mean(Proj$Catch) else Proj$Catch[1]

  Summary <- data.frame(AdviceYear = AdviceYear, LastDataYear = LastYear,
                        Converged = TRUE, Fallback = FALSE,
                        B_BMSY = Fit$Terminal[['B_BMSY']], F_FMSY = Fit$Terminal[['F_FMSY']],
                        BAdvice_BMSY = BAdvice / Fit$BMSY, MSY = Fit$MSY, FMSY = Fit$FMSY,
                        FAdvice = FAdvice, Advice = NA_real_, Message = 'Converged')

  if (AdviceType == 'Effort') {
    FLH <- .SPFYearLH(Fit, Prep$YearLH)
    if (!(FLH > 0))
      return(.SPFallback(Data, Prep, PrevTAC, PrevEffort, OnFail, FallbackYears, Responsiveness,
                         DeltaDown, DeltaUp, TACRange, Allocation, AdviceType, TACType, Misc,
                         State, AdviceYear,
                         'Estimated fishing mortality in the last historical year is zero.',
                         Fit = Fit))
    Mod            <- exp(log((FAdvice / FLH) / PrevEffort) * Responsiveness)
    Effort         <- ConstrainTAC(PrevEffort, Mod, DeltaDown, DeltaUp, c(0, Inf))
    Summary$Advice <- Effort
    Advice         <- Advice(Effort = Effort, EffType = 'Rel')
  } else {
    TrialTAC <- FilterTAC(ModelTAC / ToModel)
    if (is.null(TACRange))
      TACRange <- c(0, 100 * max(Prep$CatchAll, na.rm = TRUE))
    TAC <- if (PrevTAC > 0 && is.finite(TrialTAC)) {
      ConstrainTAC(PrevTAC, exp(log(TrialTAC / PrevTAC) * Responsiveness), DeltaDown, DeltaUp,
                   TACRange)
    } else {
      min(max(if (is.finite(TrialTAC)) TrialTAC else PrevTAC, TACRange[1]), TACRange[2])
    }
    Summary$Advice <- TAC
    Advice         <- Advice(TAC = .ApplyAllocation(TAC, Allocation, Data), TACType = TACType,
                     TACUnit = .SPTACUnit(Data))
  }

  Advice@Misc <- .SPStoreState(Misc, Model, Fit$par, Summary, Fit, Diagnostics)
  if (First)
    for (msg in Prep$Log)
      Advice <- .CaptureLog(Advice, msg, name = 'SurplusProduction', type = 'assumption')
  Advice
}
class(SurplusProduction) <- 'mp'

.CheckTunePar <- function(tunepar) {
  if (!is.numeric(tunepar) || length(tunepar) != 1 || !is.finite(tunepar) || tunepar <= 0)
    cli::cli_abort("{.arg tunepar} must be a single positive number.")
  invisible(TRUE)
}

.FrameworkMiscFields <- c('MPName', 'StockName', 'DataOM', 'AdviceYear', 'Interval', 'Sim')

.SPFractile <- function(Est, SE, Prob) {
  if (!is.finite(SE)) return(NA_real_)
  exp(log(Est) + stats::qnorm(Prob) * SE)
}

.SPLandedFraction <- function(Prep, LandedFracYears) {
  n    <- length(Prep$Landings)
  Rows <- seq(max(1, n - LandedFracYears + 1), n)
  L    <- Prep$Landings[Rows]
  D    <- Prep$Discards[Rows]
  D[is.na(D)] <- 0
  Frac <- sum(L, na.rm = TRUE) / sum(L + D, na.rm = TRUE)
  if (!is.finite(Frac) || Frac <= 0) 1 else Frac
}

.SPTypeConversion <- function(TACType, CatchType, LandedFrac) {
  if (TACType == CatchType) return(1)
  if (TACType == 'Landings') 1 / LandedFrac else LandedFrac
}

.SPFYearLH <- function(Fit, YearLH) {
  Ind <- match(as.character(YearLH), names(Fit$F))
  if (is.na(Ind)) Ind <- length(Fit$F)
  Fit$F[[Ind]]
}

.SPTACUnit <- function(Data) {
  Units <- Data@Landings@Units
  if (!length(Units) || all(is.na(Units))) 'Biomass' else Units[1]
}

.LastEffortAdvice <- function(Data) {
  Eff <- as.numeric(utils::tail(Data@Advice@Effort, 1))
  Eff <- Eff[is.finite(Eff)]
  if (!length(Eff) || !(mean(Eff) > 0)) 1 else mean(Eff)
}

.SPStoreState <- function(Misc, Model, par, Summary, Fit, Diagnostics) {
  State <- list(Model = Model, par = par)
  if (Diagnostics != 'none')
    State$Summary <- Summary
  if (Diagnostics == 'full' && !is.null(Fit)) {
    Fit$Prep  <- NULL
    Fit$Spict <- NULL
    State$Fit <- Fit
  }
  Misc$SurplusProduction <- State
  Misc
}

.SPFallback <- function(Data, Prep, PrevTAC, PrevEffort, OnFail, FallbackYears, Responsiveness,
                        DeltaDown, DeltaUp, TACRange, Allocation, AdviceType, TACType, Misc,
                        State, AdviceYear, Message, Fit = NULL) {
  Ratio <- 1
  if (OnFail == 'trend' && !is.null(Prep))
    Ratio <- .SPIndexTrend(Prep, FallbackYears)
  Mod <- exp(log(Ratio) * Responsiveness)

  if (AdviceType == 'Effort') {
    Value  <- ConstrainTAC(PrevEffort, Mod, DeltaDown, DeltaUp, c(0, Inf))
    Advice <- Advice(Effort = Value, EffType = 'Rel')
  } else {
    if (is.null(TACRange)) {
      Catch    <- if (is.null(Prep)) PrevTAC else Prep$CatchAll
      TACRange <- c(0, 100 * max(c(Catch, PrevTAC), na.rm = TRUE))
    }
    Value  <- ConstrainTAC(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange)
    Advice <- Advice(TAC = .ApplyAllocation(Value, Allocation, Data), TACType = TACType,
                     TACUnit = .SPTACUnit(Data))
  }

  Summary <- data.frame(AdviceYear = AdviceYear,
                        LastDataYear = if (is.null(Prep)) NA_real_ else max(Prep$Years),
                        Converged = FALSE, Fallback = TRUE,
                        B_BMSY = NA_real_, F_FMSY = NA_real_, BAdvice_BMSY = NA_real_,
                        MSY = NA_real_, FMSY = NA_real_, FAdvice = NA_real_,
                        Advice = Value, Message = Message)
  par         <- if (!is.null(State)) State$par else NULL
  Model      <- if (!is.null(State)) State$Model else NULL
  Advice@Misc <- .SPStoreState(Misc, Model, par, Summary, Fit, 'min')
  Message     <- sub('([^.])$', '\\1.', Message)
  .CaptureLog(Advice, paste0(Message, ' Advice set by OnFail = "', OnFail, '".'),
              name = 'SurplusProduction', type = 'warning')
}

.SPIndexTrend <- function(Prep, FallbackYears) {
  Index  <- Prep$Index
  nYear  <- nrow(Index)
  Recent <- seq(max(1, nYear - FallbackYears[1] + 1), nYear)
  Before <- seq(max(1, min(Recent) - FallbackYears[2]), min(Recent) - 1)
  if (min(Recent) <= 1) return(1)
  Ratio <- colMeans(Index[Recent, , drop = FALSE], na.rm = TRUE) /
    colMeans(Index[Before, , drop = FALSE], na.rm = TRUE)
  ok <- is.finite(Ratio) & Ratio > 0
  if (!any(ok)) return(1)
  exp(stats::weighted.mean(log(Ratio[ok]), Prep$Weight[ok]))
}

#' Extract Surplus Production Model Estimates from an MSE
#'
#' Collects the estimates made by [SurplusProduction()] in every management
#' cycle of a projection, alongside the operating model values for
#' comparison.
#'
#' @param MSE An [mse-class] object.
#' @param MPs Character vector of MP names. `NULL` (default) uses every MP
#'   that stored [SurplusProduction()] estimates.
#'
#' @return A `data.frame` with one row per MP, simulation, stock (complex),
#'   and management cycle: `AdviceYear`, `LastDataYear`, `Converged`,
#'   `Fallback`, the estimated `B_BMSY` (biomass at the start of the year
#'   after the last data year relative to `BMSY`) and `F_FMSY` (in the last
#'   data year), `BAdvice_BMSY`, `MSY`, `FMSY`, `FAdvice`, `Advice` (the TAC
#'   or relative effort before allocation), `Message`, and the operating model
#'   values `OM_B_BMSY` and `OM_F_FMSY` for the same years (`NA` where not
#'   available, e.g. for a complex of several stocks).
#'
#' Estimates are only available for MPs run with `Diagnostics = 'min'` or
#' `'full'`.
#'
#' @seealso [SurplusProduction()], [B_BMSY()], [F_FMSY()]
#' @export
SPEstimates <- function(MSE, MPs = NULL) {
  .CheckClass(MSE, 'mse', 'MSE')
  AdviceByMP <- MSE@Misc$Advice
  if (is.null(MPs)) MPs <- names(AdviceByMP)

  Rows <- list()
  for (mp in MPs) {
    for (yr in names(AdviceByMP[[mp]])) {
      BySim <- AdviceByMP[[mp]][[yr]]
      for (sim in seq_along(BySim)) {
        ByStock <- BySim[[sim]]
        if (!is.list(ByStock)) next
        for (st in names(ByStock)) {
          A <- ByStock[[st]]
          if (!inherits(A, 'advice')) next
          S <- A@Misc$SurplusProduction$Summary
          if (is.null(S)) next
          Rows[[length(Rows) + 1]] <- cbind(data.frame(MP = mp, Sim = sim, Stock = st), S)
        }
      }
    }
  }
  if (!length(Rows))
    cli::cli_abort("No {.fn SurplusProduction} estimates found in {.arg MSE}.")
  Out <- do.call(rbind, Rows)
  Out <- Out[!duplicated(Out[, c('MP', 'Sim', 'Stock', 'AdviceYear')]), ]

  BTrue         <- tryCatch(.SPTrueSeries(B_BMSY(MSE, df = TRUE, Reduce = FALSE)), error = function(e) NULL)
  FTrue         <- tryCatch(.SPTrueSeries(F_FMSY(MSE, df = TRUE, Reduce = FALSE)), error = function(e) NULL)
  Out$OM_B_BMSY <- .SPMatchTrue(Out, BTrue, Out$LastDataYear + 1)
  Out$OM_F_FMSY <- .SPMatchTrue(Out, FTrue, Out$LastDataYear)
  rownames(Out) <- NULL
  Out
}

.SPTrueSeries <- function(df) {
  df         <- as.data.frame(df)
  df$CalYear <- .CalendarYear(df$Year)
  df         <- df[!duplicated(df[, intersect(c('Sim', 'Stock', 'CalYear', 'MP'), names(df))]), ]
  df
}

.SPMatchTrue <- function(Out, True, Year) {
  if (is.null(True)) return(rep(NA_real_, nrow(Out)))
  KeyTrue <- paste(True$Sim, True$Stock, True$CalYear, if ('MP' %in% names(True)) True$MP else '')
  KeyOut  <- paste(Out$Sim, Out$Stock, Year, Out$MP)
  Val     <- True$Value[match(KeyOut, KeyTrue)]
  Miss    <- is.na(Val)
  Val[Miss] <- True$Value[match(paste(Out$Sim, Out$Stock, Year, '')[Miss],
                                paste(True$Sim, True$Stock, True$CalYear, ''))]
  Val
}
