#' Record the Calculations of an MP for ReplayMP()
#'
#' `RecordMP()` stores the values behind the advice of a management procedure
#' so that [ReplayMP()] can show them. Call it inside the MP, before the
#' advice is returned. It does nothing unless the MP is being run by
#' [ReplayMP()], so it has no effect on [Project()].
#'
#' `RecordHCR()`, `RecordIndex()`, and `RecordBiomass()` describe the harvest
#' control rule, the indices, and the estimated biomass used by the MP, for
#' the `HCR`, `Index`, and `Biomass` arguments of `RecordMP()`.
#'
#' @param Values Named list of single values (numbers, logicals, or text),
#'   added as columns of the `Table` returned by [ReplayMP()].
#' @param TrialTAC The TAC calculated by the MP before any constraint on the
#'   change in TAC is applied, shown as a cross in the `Catch` plot.
#' @param HCR The harvest control rule: the value returned by `RecordHCR()`.
#' @param Index The indices used by the MP: the value returned by
#'   `RecordIndex()`.
#' @param Biomass The estimated `B/BMSY` used by the MP: the value returned by
#'   `RecordBiomass()`.
#' @param Unused Logical. `TRUE` when the model of the MP was not used for
#'   the advice (e.g. a model fit failed), shown in red in the `Catch` and
#'   `Status` plots.
#'
#' @details
#' ## Writing an MP for ReplayMP()
#'
#' [ReplayMP()] runs any single-stock MP stored in an [mse-class] object
#' again in each management year, and shows the advice with the operating
#' model stock status (`Status`) and catch (`Catch`). The values recorded by
#' `RecordMP()` add to this:
#' - `Values` and `TrialTAC`: columns of the `Table`;
#' - `TrialTAC` and `Unused`: the trial TAC, and a TAC in red, in the `Catch`
#'   plot;
#' - `HCR`: the `HCR` plot, with the path of the decisions along the
#'   harvest control rule;
#' - `Index`: the `Index` plot, with each index (matched by name to the
#'   indices of the operating model), its status, and its target;
#' - `Biomass`: the estimated `B/BMSY` on the `Status` plot.
#'
#' [ReplayAnimate()] shows these plots side by side as an animation, and
#' [ReplayPlot()] returns the plots of a single management year.
#'
#' Calling `RecordMP()` more than once in a call of the MP adds to the
#' record: `Values` are combined, and the other arguments replace those of
#' earlier calls.
#'
#' A replayed MP gives the advice of the projection when the advice depends
#' only on `Data` (including what the MP passes to its next call in
#' `Advice@@Misc`) and the random numbers set by [Project()]. [ReplayMP()]
#' warns about the years in which the advice differs.
#'
#' [IndexRate()], [IndexTarget()], and [SurplusProduction()] record their
#' calculations with `RecordMP()`.
#'
#' @return `RecordMP()`: `NULL`, invisibly. `RecordHCR()`, `RecordIndex()`,
#'   and `RecordBiomass()`: a list for the matching argument of `RecordMP()`.
#'
#' @examples
#' # an MP that sets the TAC from the recent index relative to a target
#' IndexRatioMP <- function(Data, Target = 1, 
#'                          ControlPointsIndex = c(0.2, 0.8, 1.4),
#'                          ControlPointsRate = c(0, 1, 1.4)) {
#'   Index   <- Data@Survey@Value[, 1]
#'   Years   <- Data@Years
#'   Recent  <- utils::tail(seq_along(Index), 3)
#'   Status  <- mean(Index[Recent], na.rm = TRUE)
#'   Mult    <- HockeyStickHCR(1, 
#'                             Est = Status, 
#'                             Ref = Target, 
#'                             ControlPointsIndex = ControlPointsIndex,
#'                             ControlPointsRate = ControlPointsRate)
#'   Catch   <- rowSums(Data@Landings@Value, na.rm = TRUE)
#'   Trial   <- mean(utils::tail(Catch, 5)) * Mult
#'   PrevTAC <- LastTAC(Data)
#'   TAC     <- ConstrainTAC(PrevTAC, Trial / PrevTAC, 
#'                           DeltaDown = c(0.01, 0.4), 
#'                           DeltaUp = c(0.01, 0.4),
#'                           TACRange = c(0, Inf))
#'   
#'   RecordMP(
#'     Values   = list(Status = Status, Multiplier = Mult),
#'     TrialTAC = Trial,
#'     HCR      = RecordHCR(Est = Status, Ref = Target, Value = Mult,
#'                          ControlPointsIndex = ControlPointsIndex,
#'                          ControlPointsRate = ControlPointsRate, 
#'                          xlab = 'Index / target', ylab = 'TAC multiplier'),
#'     Index    = RecordIndex(Index, Years, Name = Data@Survey@Name[1], Status = Status,
#'                            StatusYears = Years[Recent], IndexTarget = Target,
#'                            ControlPointsIndex = ControlPointsIndex)
#'   )
#'   Advice(TAC = TAC)
#' }
#' class(IndexRatioMP) <- 'mp'
#'
#' \donttest{
#' Hist <- Simulate(SingleStockOM, nSim = 2)
#' MSE  <- Project(Hist, list(IndexRatioMP = SetMPArgs(IndexRatioMP, Target = 0.5)))
#' if (requireNamespace('gganimate', quietly = TRUE)) {
#'   R <- ReplayMP(MSE, 'IndexRatioMP')
#'   R$Table
#'   ReplayPlot(R, R$Table$Year[10])
#'   if (requireNamespace('gifski', quietly = TRUE))
#'     ReplayAnimate(R, fps = 2)
#' }
#' }
#'
#' @seealso [ReplayMP()], [ReplayAnimate()], [ReplayPlot()], [HockeyStickHCR()]
#' @export
RecordMP <- function(Values = NULL, TrialTAC = NULL, HCR = NULL, Index = NULL, Biomass = NULL,
                     Unused = FALSE) {
  if (!.Recording())
    return(invisible(NULL))
  if (!is.null(Values)) {
    Values <- as.list(Values)
    if (is.null(names(Values)) || any(names(Values) == '') ||
        !all(vapply(Values, \(v) is.atomic(v) && length(v) == 1, logical(1))))
      cli::cli_abort("{.arg Values} must be a named list of single values.")
  }
  if (!is.null(TrialTAC) && !(is.numeric(TrialTAC) && length(TrialTAC) == 1))
    cli::cli_abort("{.arg TrialTAC} must be a single number.")
  .CheckRecord(HCR, 'mprecord_hcr', 'HCR', 'RecordHCR')
  .CheckRecord(Index, 'mprecord_index', 'Index', 'RecordIndex')
  .CheckRecord(Biomass, 'mprecord_biomass', 'Biomass', 'RecordBiomass')

  Rec <- .MPReplayState$Record %||% list()
  if (length(Values))
    Rec$Values <- utils::modifyList(Rec$Values %||% list(), Values)
  if (!is.null(TrialTAC)) Rec$TrialTAC <- TrialTAC
  if (!is.null(HCR))      Rec$HCR      <- HCR
  if (!is.null(Index))    Rec$Index    <- Index
  if (!is.null(Biomass))  Rec$Biomass  <- Biomass
  if (isTRUE(Unused))     Rec$Unused   <- TRUE
  .MPReplayState$Record <- Rec
  invisible(NULL)
}

#' @rdname RecordMP
#' @param Est,Ref The status estimate and its reference level, as in
#'   [HockeyStickHCR()]. The decision is plotted at `Est / Ref`.
#' @param Value The output of the harvest control rule at `Est / Ref` (e.g. a
#'   multiplier of the TAC or of a target fishing mortality).
#' @param ControlPointsIndex,ControlPointsRate,RampType The control points
#'   and ramp of the harvest control rule, as in [HockeyStickHCR()], used to
#'   draw the rule. For `RecordIndex()`, `ControlPointsIndex` are drawn as
#'   lines at `ControlPointsIndex * IndexTarget`.
#' @param Scale Multiplier of the harvest control rule drawn from the control
#'   points or `Fun` (e.g. `FTarget * FMSY` when `Value` is an `F`).
#' @param Fun Optional function of `Est / Ref` returning the output of the
#'   harvest control rule, used to draw rules that are not a hockey stick.
#' @param xlab,ylab Axis labels of the `HCR` plot.
#' @export
RecordHCR <- function(Est, Value, Ref = 1, ControlPointsIndex = NULL, ControlPointsRate = NULL,
                      RampType = c('linear', 'smooth'), Scale = 1, Fun = NULL,
                      xlab = 'Est / Ref', ylab = 'HCR output') {
  RampType <- match.arg(RampType)
  if (!.Recording())
    return(invisible(NULL))
  for (nm in c('Est', 'Value', 'Ref', 'Scale'))
    if (!(is.numeric(get(nm)) && length(get(nm)) == 1))
      cli::cli_abort("{.arg {nm}} must be a single number.")
  if (!is.null(ControlPointsIndex) && is.null(ControlPointsRate))
    ControlPointsRate <- seq(0, 1, length.out = length(ControlPointsIndex))
  if (!is.null(Fun) && !is.function(Fun))
    cli::cli_abort("{.arg Fun} must be a function.")
  structure(list(x = Est / Ref, y = Value, ControlPointsIndex = ControlPointsIndex,
                 ControlPointsRate = ControlPointsRate, RampType = RampType, Scale = Scale, Fun = Fun,
                 xlab = xlab, ylab = ylab),
            class = 'mprecord_hcr')
}

#' @rdname RecordMP
#' @param Years Years of the values (`Data@@Years`, or the years of the
#'   values of `Value`).
#' @param Name Names of the indices. Indices are matched by name to the
#'   indices of the operating model. `NULL` uses the column names of `Value`.
#' @param Smoothed,Fitted Optional smoothed or model-fitted index values,
#'   with the dimensions of `Value`.
#' @param Status Optional status of each index used by the MP.
#' @param StatusYears Years over which `Status` is calculated. `NULL` uses
#'   the last of `Years`.
#' @param StatusTrend Optional status of each index projected along a recent
#'   trend, `TrendHorizon` years after the last of `Years`.
#' @param TrendHorizon Years ahead of `StatusTrend`.
#' @param IndexTarget Optional target of each index.
#' @export
RecordIndex <- function(Value, Years, Name = NULL, Smoothed = NULL, Fitted = NULL, Status = NULL,
                        StatusYears = NULL, StatusTrend = NULL, TrendHorizon = 1, IndexTarget = NULL,
                        ControlPointsIndex = NULL) {
  if (!.Recording())
    return(invisible(NULL))
  Value <- as.matrix(Value)
  if (nrow(Value) != length(Years))
    cli::cli_abort("{.arg Value} must have one value (row) for each of {.arg Years}.")
  Name <- Name %||% colnames(Value) %||% if (ncol(Value) == 1) 'Index' else paste('Index', seq_len(ncol(Value)))
  if (length(Name) != ncol(Value))
    cli::cli_abort("{.arg Name} must have one name for each index (column) of {.arg Value}.")
  Same <- function(x, Arg) {
    if (is.null(x)) return(NULL)
    x <- as.matrix(x)
    if (!identical(dim(x), dim(Value)))
      cli::cli_abort("{.arg {Arg}} must have the dimensions of {.arg Value}.")
    x
  }
  PerIndex <- function(x, Arg) {
    if (is.null(x)) return(NULL)
    if (!is.numeric(x) || !length(x) %in% c(1, ncol(Value)))
      cli::cli_abort("{.arg {Arg}} must have one value for each index.")
    rep_len(x, ncol(Value))
  }
  structure(list(Value = Value, Years = as.numeric(Years), Name = as.character(Name),
                 Smoothed = Same(Smoothed, 'Smoothed'), Fitted = Same(Fitted, 'Fitted'),
                 Status = PerIndex(Status, 'Status'),
                 StatusYears = if (is.null(Status)) NULL else StatusYears %||% max(Years),
                 StatusTrend = PerIndex(StatusTrend, 'StatusTrend'), TrendHorizon = TrendHorizon,
                 IndexTarget = PerIndex(IndexTarget, 'IndexTarget'),
                 ControlPointsIndex = ControlPointsIndex),
            class = 'mprecord_index')
}

#' @rdname RecordMP
#' @param EstYear Year of `Est` for `RecordBiomass()`, usually the year the
#'   advice applies to.
#' @param Projected,ProjectedYears Optional `B/BMSY` projected by the MP
#'   (e.g. to and from the advice year), and its years.
#' @details For `RecordBiomass()`, `Value` is the estimated `B/BMSY` in each
#'   of `Years`, and `Est` the `B/BMSY` used by the harvest control rule.
#' @export
RecordBiomass <- function(Value = NULL, Years = NULL, Est = NULL, EstYear = NULL, Projected = NULL,
                          ProjectedYears = NULL) {
  if (!.Recording())
    return(invisible(NULL))
  if (!is.null(Value) && length(Value) != length(Years))
    cli::cli_abort("{.arg Value} must have one value for each of {.arg Years}.")
  if (!is.null(Projected) && length(Projected) != length(ProjectedYears))
    cli::cli_abort("{.arg Projected} must have one value for each of {.arg ProjectedYears}.")
  if (!is.null(Est) && (length(Est) != 1 || length(EstYear) != 1))
    cli::cli_abort("{.arg Est} and {.arg EstYear} must be single values.")
  structure(list(Value = unname(Value), Years = as.numeric(Years), Est = Est, EstYear = EstYear,
                 Projected = unname(Projected), ProjectedYears = as.numeric(ProjectedYears)),
            class = 'mprecord_biomass')
}

.Recording <- function() isTRUE(.MPReplayState$Capture)

.CheckRecord <- function(x, Class, Arg, Fn) {
  if (!is.null(x) && !inherits(x, Class))
    cli::cli_abort("{.arg {Arg}} must be the value returned by {.fn {Fn}}.")
}

# records of the built-in MPs

# MPs stored in an MSE before they called RecordMP() reach .MPPlot() with their values
.RecordFromPlot <- function(E) {
  Calc <- get0('Calc', envir = E)
  if (is.list(Calc) && !is.null(Calc$Est))
    return(.RecordIndexMP(Calc))
  A   <- get0('A', envir = E)
  Env <- get0('Env', envir = E)
  if (inherits(A, 'advice') && is.environment(Env))
    .RecordSP(A, Env)
  invisible(NULL)
}

.RecordIndexMP <- function(Calc) {
  if (!.Recording())
    return(invisible(NULL))
  Years      <- Calc$Years
  StatusCols <- seq(max(1, length(Years) - Calc$RecentYears + 1), length(Years))
  Level      <- Calc$Est / Calc$RefLevel
  RecordMP(
    Values = list(IndexRatio = Level, HCRMultiplier = Calc$HCRMult, PrevTAC = Calc$PrevTAC,
                  TrialTAC = Calc$TrialTAC, TAC = Calc$TAC,
                  Constraint = .TACConstraintNote(Calc$PrevTAC, Calc$TrialTAC, Calc$Mod, Calc$TAC,
                                                  Calc$DeltaDown, Calc$DeltaUp, Calc$TACRange,
                                                  Calc$Responsiveness, Calc$TrialKept)),
    TrialTAC = Calc$TrialTAC,
    HCR = RecordHCR(Est = Calc$Est, Ref = Calc$RefLevel, Value = Calc$HCRMult,
                    ControlPointsIndex = Calc$HCRControlPointsIndex,
                    ControlPointsRate = Calc$HCRControlPointsRate, RampType = Calc$RampType %||% 'linear',
                    xlab = 'Index / IndexTarget',
                    ylab = if (Calc$Type == 'Rate') 'Multiplier of catch-per-index rate' else 'TAC / reference catch'),
    Index = RecordIndex(t(Calc$IndexHist), Years, Name = Calc$Name,
                        Smoothed = if (Calc$Smooth) t(Calc$IndexSmooth),
                        Status = Calc$Status, StatusYears = Years[StatusCols],
                        StatusTrend = if (!is.null(Calc$TrendYears)) Calc$StatusTrend,
                        TrendHorizon = Calc$TrendHorizon, IndexTarget = Calc$Ref,
                        ControlPointsIndex = Calc$HCRControlPointsIndex[Calc$HCRControlPointsIndex > 0])
  )
}

# A: the advice of SurplusProduction(); Env: its evaluation environment
.RecordSP <- function(A, Env) {
  if (!.Recording())
    return(invisible(NULL))
  V <- function(nm) get0(nm, envir = Env, inherits = FALSE)
  Prep     <- V('Prep')
  Fit      <- V('Fit')
  Summary  <- A@Misc$SurplusProduction$Summary
  HasFit   <- is.list(Prep) && inherits(Fit, 'spfit') && isTRUE(Fit$Converged) && isFALSE(Summary$Fallback)
  Fallback <- isTRUE(Summary$Fallback)
  TACAdvice <- identical(V('AdviceType'), 'TAC')
  PrevTAC  <- V('PrevTAC') %||% NA_real_
  Trial    <- if (HasFit && TACAdvice) V('ModelTAC') / (V('ToModel') %||% 1) else NA_real_
  TAC      <- if (TACAdvice) Summary$Advice else NA_real_
  FMSY     <- if (HasFit) Fit$FMSY else NA_real_
  TrialNA  <- HasFit && TACAdvice && !is.finite(Trial)
  Constraint <- if (TACAdvice && HasFit) {
    .TACConstraintNote(PrevTAC, Trial, exp(log(Trial / PrevTAC) * V('Responsiveness')), TAC, V('DeltaDown'),
                       V('DeltaUp'), V('TACRange') %||% c(0, Inf), V('Responsiveness'), TrialKept = TrialNA)
  } else if (Fallback) {
    paste0('OnFail = "', V('OnFail'), '": ', Summary$Message)
  } else NA_character_

  Index <- NULL
  if (is.list(Prep)) {
    Obs <- Prep$Index
    Obs[!(Obs > 0)] <- NA
    Index <- RecordIndex(Obs, Prep$Years, Name = Prep$Name,
                         Fitted = if (HasFit && !is.null(Fit$IndexPred)) Fit$IndexPred)
  }
  HCR <- Biomass <- NULL
  if (HasFit) {
    Gap  <- V('Gap')
    Proj <- V('Proj')
    LastYear   <- max(Prep$Years)
    AdviceYear <- V('AdviceYear')
    HCR <- RecordHCR(Est = V('EstB'), Value = V('FAdvice') / FMSY,
                     ControlPointsIndex = V('HCRControlPointsBiomass'),
                     ControlPointsRate = V('HCRControlPointsRate'), RampType = V('RampType'),
                     Scale = V('tunepar') * V('FTarget') * V('FMSYHCR') / FMSY,
                     xlab = 'B / BMSY', ylab = 'F / FMSY')
    Biomass <- RecordBiomass(Fit$B_BMSY, as.numeric(names(Fit$B)), Est = V('EstB'), EstYear = AdviceYear,
                             Projected = c(Gap$B, Proj$B) / Fit$BMSY,
                             ProjectedYears = c(LastYear + seq_along(Gap$B), AdviceYear + seq_along(Proj$B) - 1))
  } else if (Fallback && inherits(Fit, 'spfit') && length(Fit$B_BMSY)) {
    Last <- utils::tail(which(is.finite(Fit$B_BMSY)), 1)
    if (length(Last))
      Biomass <- RecordBiomass(Est = unname(Fit$B_BMSY[Last]), EstYear = as.numeric(names(Fit$B))[Last])
  }

  RecordMP(
    Values = list(Converged = inherits(Fit, 'spfit') && isTRUE(Fit$Converged), Fallback = Fallback,
                  BBMSY = if (HasFit) V('EstB') else NA_real_,
                  FFMSY = if (HasFit) V('FAdvice') / FMSY else NA_real_,
                  PrevTAC = PrevTAC, TrialTAC = Trial, TAC = TAC, Constraint = Constraint),
    TrialTAC = Trial, HCR = HCR, Index = Index, Biomass = Biomass, Unused = Fallback || TrialNA
  )
}
