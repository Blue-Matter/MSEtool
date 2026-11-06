.MPReplayState <- new.env(parent = emptyenv())

#' Replay the Decisions of an MP in an MSE
#'
#' Runs a management procedure again on the data it received in each
#' management year of a projection, and returns the values behind each
#' decision with animations of how the advice developed over time.
#'
#' @param MSE An [mse-class] object returned by [Project()].
#' @param MP Name or number of the MP in `MSE`.
#' @param Sim Simulation number.
#' @param Stock Name or number of the stock (or stock complex).
#' @param Years Management years to replay. `NULL` (default) replays every
#'   management year of the projection.
#' @param Status Biomass shown in the `Status` animation: `'SB'`
#'   (spawning biomass, default) or `'B'` (total biomass). `NULL` leaves out
#'   the `Status` animation.
#' @param IndexCap For [IndexRate()] and [IndexTarget()], the `Index`
#'   animation starts in the historical year when the observed index last
#'   comes down to `IndexCap` times the largest `IndexTarget` of the index
#'   (the earliest such year over indices), so the target and control points
#'   stay visible. `Inf` shows the full series. Default `1.5`.
#'
#' @details
#' Every management year up to the last of `Years` is run in order, so
#' that information an MP passes to its next call in `Advice@@Misc` (e.g.
#' the starting values of [SurplusProduction()]) is the same as in the
#' projection. For each management year, the data are cut back to the years available
#' to the MP in [Project()] (accounting for `DataLag`), the advice history to
#' the years before the management year, and the MP stored in `MSE` (with any
#' arguments set by [SetMPArgs()] or [TuneMP()]) is run again. The replayed
#' advice is compared with the advice recorded in the projection, and a
#' warning lists the years where they differ, e.g. for MPs that use
#' `Data@@Misc$DataOM`.
#'
#' The animations have one frame per management year. They are `gganim`
#' objects: printing one renders it, [gganimate::animate()] sets the speed
#' and renderer, and [gganimate::anim_save()] saves it.
#' - [IndexRate()] and [IndexTarget()]: `Index` (the indices, status, and
#'   `IndexTarget` at each decision, with the operating model population
#'   each index tracks), `Status`, `HCR` (the path of the decisions along
#'   the harvest control rule), and `Catch`.
#' - [SurplusProduction()]: `Index` (observed and fitted indices, with the
#'   operating model population each index tracks), `Status` (also with the
#'   estimated `B/BMSY` at each decision), `HCR`, and `Catch`.
#' - Other MPs: `Status` and `Catch`.
#'
#' `Index` shows each index in its own panel. The orange bar is the status
#' over the `RecentYears` window, the arrow the projected status when
#' `TrendYears` is set, and the red lines the `IndexTarget` (solid) and the
#' harvest control rule control points (dashed). With `IndexCap`, the panels
#' start in the historical year when the index is closest to `IndexCap`
#' times its `IndexTarget`. The `HCR` point is the weighted mean of
#' `status / IndexTarget` over the indices, as used by the MP.
#'
#' For [SurplusProduction()], the estimated `B/BMSY` at each decision (and
#' its projection to and from the advice year) is drawn on the `Status` plot
#' against the right axis, with the `B/BMSY` used by the harvest control
#' rule as an orange point. In years when the model fit was not used (the
#' fit failed or did not converge, or the advice was otherwise set by
#' `OnFail`), the last `B/BMSY` estimate of a non-converged fit is shown as a
#' red point, and the TAC as a red point in `Catch`; a red TAC also marks a
#' non-finite trial TAC, when the previous TAC is kept. The `Converged`,
#' `Fallback`, and `Constraint` columns of `Table` give the reason, and
#' [SPEstimates()] and `Log(MSE, type = 'warning')` the model diagnostics. Without a `Status` plot (`Status = NULL`), these
#' are shown in a separate `Biomass` plot with the operating model
#' `B/BMSY`. With `Status = 'SB'`, the right axis compares the estimated
#' `B/BMSY` with the operating model `SB/SBMSY`; use `Status = 'B'` to
#' compare like with like.
#'
#' `Status` shows the operating model depletion (`SB/SB0` or `B/B0`, see
#' [SB_SB0()]) up to each management year, with a right axis in MSY units
#' (`SB/SBMSY` or `B/BMSY`) and a dashed line at the MSY level. The right
#' axis is scaled by the median ratio of `SBMSY/SB0` (or `BMSY/B0`) over the
#' years. `Catch` shows the operating model landings, and the dead discards
#' and removals when there are dead discards (see [Landings()]), up to the
#' year before each management year, the TAC recorded in the projection for
#' each year up to the management year (including years between management
#' years), and the trial TAC and TAC of each management year. The operating model
#' population of an index is the index-selected population on the scale of
#' the index (`Efficiency * NomIndex^Beta`, see [PlotIndexFit()]), shown up
#' to the last year of data available to the MP.
#'
#' [ReplayAnimate()] shows the animations side by side, and [ReplayPlot()]
#' returns the plots of a single year.
#'
#' The MP is run with the version of MSEtool currently loaded. MPs without
#' a `plot` argument give the `Catch` animation only. Multi-stock (`mmp`)
#' MPs are not supported.
#'
#' @return An object of class `mpreplay`: a list with `MP`, `Sim`, `Stock`,
#'   `Table` (a `data.frame` with one row per management year: the replayed
#'   `Advice`, the `Recorded` advice, and the values used by the MP), and
#'   `Animations` (a named list of `gganim` objects).
#'
#' @examples
#' \donttest{
#' Hist <- Simulate(SingleStockOM, nSim = 2)
#' MSE <- Project(Hist, 'IndexRate')
#' R <- ReplayMP(MSE, 'IndexRate', Sim = 1)
#' R$Table
#' if (requireNamespace('gganimate', quietly = TRUE))
#'   gganimate::animate(R$Animations$HCR, nframes = nrow(R$Table), fps = 2)
#' }
#'
#' @seealso [ReplayAnimate()], [ReplayPlot()], [IndexRate()],
#'   [SurplusProduction()], [PPD()]
#' @export
ReplayMP <- function(MSE, MP = 1, Sim = 1, Stock = 1, Years = NULL,
                     Status = 'SB', IndexCap = 1.5) {
  .CheckClass(MSE, 'mse', 'MSE')
  if (!is.null(Status) && !(is.character(Status) && length(Status) == 1 && Status %in% c('SB', 'B')))
    cli::cli_abort("{.arg Status} must be {.val SB}, {.val B}, or {.code NULL}.")
  if (!is.numeric(IndexCap) || length(IndexCap) != 1 || is.na(IndexCap) || IndexCap <= 0)
    cli::cli_abort("{.arg IndexCap} must be a single positive number (or {.code Inf}).")
  CheckPackage('gganimate')

  MPName <- .ReplayPick(MP, names(MSE@MPs), 'MP')
  MPfun  <- .ReplayCurrentNamespace(MSE@MPs[[MPName]])
  if (inherits(MPfun, 'mmp'))
    cli::cli_abort("{.fn ReplayMP} does not support multi-stock ({.cls mmp}) MPs.")

  PPDs <- PPD(MSE)[[MPName]]
  if (!is.numeric(Sim) || length(Sim) != 1 || !Sim %in% seq_along(PPDs))
    cli::cli_abort("{.arg Sim} must be a single simulation number from 1 to {length(PPDs)}.")
  StockName <- .ReplayPick(Stock, names(PPDs[[Sim]]), 'Stock')
  D <- PPDs[[Sim]][[StockName]]

  OM        <- MSE@OM
  Seasons   <- OM@Seasons
  YearsProj <- Years(OM, 'Projection')
  Interval  <- .ResolveInterval(OM@Interval, MPName, MPfun, Seasons)
  Interim   <- YearsProj %in% .InterimTimesteps(OM)
  MgmtYears <- .CalcManagementYears(YearsProj[!Interim], Interval, Seasons)
  DataLag   <- attr(MPfun, 'DataLag') %||% OM@DataLag
  if (is.null(Years)) {
    Years <- MgmtYears
  } else if (!all(Years %in% MgmtYears)) {
    cli::cli_abort(c("{.arg Years} must be management years of {.val {MPName}}.",
                     "i" = "Management years: {.val {MgmtYears}}."))
  }

  .MPReplayState$Capture <- TRUE
  on.exit({
    .MPReplayState$Capture <- FALSE
    .MPReplayState$Env <- NULL
  }, add = TRUE)

  # every management year up to the last requested one is run, so Advice@Misc carries forward as in Project()
  RunYears <- MgmtYears[MgmtYears <= max(Years)]
  PB   <- cli::cli_progress_bar(paste("Replaying", MPName), total = length(RunYears), clear = TRUE)
  Misc <- list()
  Runs <- list()
  for (Y in RunYears) {
    cli::cli_progress_update(id = PB)
    DataY <- .ReplayData(D, Y, DataLag, Seasons)
    DataY@Misc <- utils::modifyList(Misc, list(Sim = Sim, MPName = MPName, StockName = StockName,
                                               AdviceYear = Y, Interval = Interval))
    .MPReplayState$Env <- NULL
    Adv <- tryCatch(suppressMessages(.WithMPSeed(MPfun(Data = DataY), OM@Seed, .GlobalSim(OM, Sim),
                                                 Y, StockName)),
                    error = function(e) e)
    if (inherits(Adv, 'advice')) {
      Misc <- Adv@Misc
      Misc$AnnualTAC <- Misc$StatusQuoEffort <- NULL
    }
    if (Y %in% Years)
      Runs[[length(Runs) + 1]] <- list(Year = Y, Data = DataY, Advice = Adv, Env = .MPReplayState$Env,
                                       Recorded = .ReplayRecorded(D@Advice, Y))
  }
  cli::cli_progress_done(id = PB)

  Failed <- vapply(Runs, \(r) !inherits(r$Advice, 'advice'), logical(1))
  if (any(Failed))
    cli::cli_warn(c("!" = "{.val {MPName}} failed when replayed in {.val {Years[Failed]}}.",
                    "i" = "{conditionMessage(Runs[[which(Failed)[1]]]$Advice)}"))
  Runs <- Runs[!Failed]
  if (!length(Runs))
    cli::cli_abort("{.val {MPName}} could not be replayed in any year.")

  Kind <- .ReplayKind(Runs[[1]]$Env)
  if (Kind == 'Other')
    cli::cli_inform(c("i" = "{.val {MPName}} has no {.arg plot} option: only the {.val Catch} animation is available."))
  Rows <- lapply(Runs, \(r) .ReplayFrame(r, Kind))
  rm(Runs)
  Parts <- c('Table', 'Index', 'Status', 'Target', 'Point', 'TACs', 'Biomass', 'BPoint')
  P <- lapply(stats::setNames(Parts, Parts), \(nm) do.call(rbind, lapply(Rows, `[[`, nm)))
  P$Years    <- vapply(Rows, \(r) r$TACs$Year, numeric(1))
  P$Curves   <- Filter(Negate(is.null), lapply(Rows, `[[`, 'Curve'))
  P$Settings <- Rows[[1]]$Settings
  rm(Rows)

  Table <- P$Table
  rownames(Table) <- NULL
  Diff  <- is.finite(Table$Advice) & is.finite(Table$Recorded) &
    abs(Table$Advice - Table$Recorded) > 1e-6 * pmax(1, abs(Table$Recorded))
  if (any(Diff))
    cli::cli_warn(c("!" = "Replayed advice differs from the advice recorded in the projection in {.val {Table$Year[Diff]}}.",
                    "i" = "The MP may use information not stored in the MSE object (e.g. {.code Data@Misc$DataOM})."))

  OMIndex  <- if (Kind == 'Other') NULL else .ReplayOMIndex(MSE, MPName, Sim, StockName)
  P$OMCatch    <- .ReplayOMCatch(MSE, MPName, Sim, StockName)
  P$TACHistory <- .ReplayTACHistory(D@Advice@TAC)
  OMStatus <- if (length(Status)) .ReplayOMStatus(MSE, MPName, Sim, StockName, Status)
  # surplus production estimates go on the Status plot when it has an MSY axis
  EstOnStatus <- Kind == 'SP' && !is.null(P$Biomass) && is.finite(attr(OMStatus, 'K') %||% NA)
  Animations <- switch(Kind,
                       Index = .ReplayAnimIndexMP(P, IndexCap, OMIndex),
                       SP    = .ReplayAnimSP(P, .ReplayOMBBMSY(MSE, MPName, Sim, StockName), OMIndex,
                                             Biomass = !EstOnStatus),
                       list(Catch = .ReplayAnimCatch(P)))
  if (!is.null(OMStatus)) {
    Est <- if (EstOnStatus) list(Biomass = P$Biomass, Point = P$BPoint)
    Animations <- append(Animations, list(Status = .ReplayAnimStatus(OMStatus, P$Years, Est)),
                         after = max(0, match(c('Index', 'Biomass'), names(Animations)), na.rm = TRUE))
  }

  structure(list(MP = MPName, Sim = Sim, Stock = StockName, Table = Table,
                 Animations = Animations),
            class = 'mpreplay')
}

#' @export
print.mpreplay <- function(x, ...) {
  Span <- .MPYears(x$Table$Year)
  cli::cli_text("Replay of {.val {x$MP}}, simulation {x$Sim}, {.val {x$Stock}}: ",
                "{nrow(x$Table)} management year{?s} ({Span}).")
  cli::cli_text("Animations: {.val {names(x$Animations)}}.")
  print(utils::head(x$Table, 10), row.names = FALSE)
  if (nrow(x$Table) > 10) cli::cli_text("... {nrow(x$Table) - 10} more row{?s} in {.code $Table}.")
  invisible(x)
}

# MPs from an earlier load of MSEtool run in the loaded version
.ReplayCurrentNamespace <- function(MPfun) {
  Cur <- asNamespace('MSEtool')
  Fix <- function(E) {
    if (identical(E, Cur) || identical(E, emptyenv()) || identical(E, globalenv()))
      return(E)
    if (isNamespace(E))
      return(if (getNamespaceName(E) == 'MSEtool') Cur else E)
    P <- Fix(parent.env(E))
    if (identical(P, parent.env(E))) E else list2env(as.list(E, all.names = TRUE), parent = P)
  }
  environment(MPfun) <- Fix(environment(MPfun))
  MPfun
}

.ReplayPick <- function(x, Names, Arg) {
  if (is.numeric(x) && length(x) == 1 && x %in% seq_along(Names))
    return(Names[x])
  if (is.character(x) && length(x) == 1 && x %in% Names)
    return(x)
  cli::cli_abort(c("{.arg {Arg}} must be one of {.val {Names}} or a number from 1 to {length(Names)}."))
}

# the data and advice history available to the MP at management year Y
.ReplayData <- function(D, Y, DataLag, Seasons) {
  Avail <- D@Years[D@Years < Y]
  n     <- length(Avail) - DataLag * Seasons
  if (n < 1)
    cli::cli_abort("No data available to the MP in {Y}.")
  Out <- DataTrim(D, Avail[n])
  Out@Advice <- .TrimAdviceHistory(D@Advice, Y)
  Out
}

.TrimAdviceHistory <- function(Advice, Y) {
  for (sl in methods::slotNames(Advice)) {
    x <- methods::slot(Advice, sl)
    if (!is.array(x) || !identical(names(dimnames(x))[1], 'Year'))
      next
    Keep <- as.numeric(dimnames(x)[[1]]) < Y
    x <- if (any(Keep))
      do.call(`[`, c(list(x, Keep), rep(list(TRUE), length(dim(x)) - 1), list(drop = FALSE))) else NULL
    methods::slot(Advice, sl) <- x
  }
  Advice
}

.ReplayRecorded <- function(Advice, Y) {
  for (sl in c('TAC', 'Effort')) {
    x <- methods::slot(Advice, sl)
    if (is.array(x) && identical(names(dimnames(x))[1], 'Year')) {
      Row <- as.numeric(dimnames(x)[[1]]) == Y
      if (any(Row))
        return(list(Type = sl, Value = .ReplayTotal(sl, do.call(`[`, c(list(x, Row), rep(list(TRUE), length(dim(x)) - 1))))))
    }
  }
  list(Type = NA_character_, Value = NA_real_)
}

.ReplayTotal <- function(Type, x) {
  if (is.null(x) || !length(x) || all(is.na(x))) return(NA_real_)
  if (identical(Type, 'TAC')) sum(x, na.rm = TRUE) else mean(x, na.rm = TRUE)
}

.ReplayKind <- function(E) {
  if (is.null(E)) return('Other')
  if (!is.null(get0('Calc', envir = E))) return('Index')
  if (!is.null(get0('A', envir = E)) && is.environment(get0('Env', envir = E))) return('SP')
  'Other'
}

# per-year values and plotting data
.ReplayFrame <- function(r, Kind) {
  Y   <- r$Year
  Adv <- r$Advice
  Type <- r$Recorded$Type
  if (is.na(Type)) Type <- if (!is.null(Adv@TAC)) 'TAC' else 'Effort'
  Base <- data.frame(Year = Y, AdviceType = Type,
                     Advice = .ReplayTotal(Type, methods::slot(Adv, Type)),
                     Recorded = r$Recorded$Value)
  Out <- switch(Kind,
                Index = .ReplayFrameIndexMP(get0('Calc', envir = r$Env), Y),
                SP    = .ReplayFrameSP(get0('A', envir = r$Env), get0('Env', envir = r$Env), Y),
                .ReplayFrameOther(r$Data, Y, Base))
  Out$Table <- if (is.null(Out$Table)) Base else cbind(Base, Out$Table)
  Out
}

# Unused: the MP's model was not used for the TAC (e.g. a failed fit)
.ReplayCatchTACs <- function(Y, Prev, Trial, TAC, Unused = FALSE)
  data.frame(Year = Y, PrevTAC = Prev, TrialTAC = Trial, TAC = TAC, Unused = Unused)

.ReplayFrameOther <- function(Data, Y, Base) {
  list(Table = NULL,
       TACs  = .ReplayCatchTACs(Y, NA_real_, NA_real_, if (Base$AdviceType == 'TAC') Base$Advice else NA_real_))
}

.ReplayFrameIndexMP <- function(Calc, Y) {
  Years  <- Calc$Years
  nIndex <- length(Calc$Name)
  Name   <- factor(Calc$Name, levels = Calc$Name)
  StatusCols <- seq(max(1, length(Years) - Calc$RecentYears + 1), length(Years))
  CP <- Calc$HCRControlPointsIndex[Calc$HCRControlPointsIndex > 0]
  Level <- Calc$Est / Calc$RefLevel
  list(
    Table = data.frame(IndexRatio = Level, HCRMultiplier = Calc$HCRMult, PrevTAC = Calc$PrevTAC,
                       TrialTAC = Calc$TrialTAC, TAC = Calc$TAC,
                       Constraint = .TACConstraintNote(Calc$PrevTAC, Calc$TrialTAC, Calc$Mod, Calc$TAC,
                                                       Calc$DeltaDown, Calc$DeltaUp, Calc$TACRange,
                                                       Calc$Responsiveness, Calc$TrialKept)),
    Index = data.frame(Frame = Y, Name = rep(Name, each = length(Years)), Year = rep(Years, nIndex),
                       Observed = as.vector(t(Calc$IndexHist)),
                       Smoothed = if (Calc$Smooth) as.vector(t(Calc$IndexSmooth)) else NA_real_),
    Status = data.frame(Frame = Y, Name = Name, From = Years[min(StatusCols)] - 0.5,
                        To = Years[max(StatusCols)] + 0.5, Year = max(Years),
                        Value = Calc$Status, Trend = Calc$StatusTrend,
                        TrendYear = max(Years) + if (is.null(Calc$TrendYears)) 0 else Calc$TrendHorizon),
    Target = rbind(data.frame(Frame = Y, Name = Name, Value = Calc$Ref, Series = 'IndexTarget'),
                   data.frame(Frame = Y, Name = rep(Name, each = length(CP)),
                              Value = as.vector(outer(CP, Calc$Ref)), Series = 'HCR control points')),
    Point = data.frame(Frame = Y, x = Level, y = Calc$HCRMult),
    Curve = NULL,
    TACs  = .ReplayCatchTACs(Y, Calc$PrevTAC, Calc$TrialTAC, Calc$TAC),
    Settings = Calc[c('Type', 'Years', 'YearLH', 'CalibRows', 'TrendYears', 'HCRControlPointsIndex',
                      'HCRControlPointsRate', 'RampType')]
  )
}

.ReplayFrameSP <- function(A, Env, Y) {
  V <- function(nm) get0(nm, envir = Env, inherits = FALSE)
  Prep    <- V('Prep')
  Fit     <- V('Fit')
  Summary <- A@Misc$SurplusProduction$Summary
  HasFit  <- is.list(Prep) && inherits(Fit, 'spfit') && isTRUE(Fit$Converged) &&
    isFALSE(Summary$Fallback)
  AdviceType <- V('AdviceType')
  PrevTAC <- V('PrevTAC') %||% NA_real_
  Trial   <- if (HasFit && AdviceType == 'TAC') V('ModelTAC') / (V('ToModel') %||% 1) else NA_real_
  TAC     <- if (AdviceType == 'TAC') Summary$Advice else NA_real_
  FMSY    <- if (HasFit) Fit$FMSY else NA_real_
  Fallback <- isTRUE(Summary$Fallback)
  TrialNA  <- HasFit && AdviceType == 'TAC' && !is.finite(Trial)

  Out <- list(
    Table = data.frame(Converged = inherits(Fit, 'spfit') && isTRUE(Fit$Converged),
                       Fallback = isTRUE(Summary$Fallback),
                       BBMSY = if (HasFit) V('EstB') else NA_real_,
                       FFMSY = if (HasFit) V('FAdvice') / FMSY else NA_real_,
                       PrevTAC = PrevTAC, TrialTAC = Trial, TAC = TAC,
                       Constraint = if (AdviceType == 'TAC' && HasFit)
                         .TACConstraintNote(PrevTAC, Trial, exp(log(Trial / PrevTAC) * V('Responsiveness')),
                                            TAC, V('DeltaDown'), V('DeltaUp'), V('TACRange') %||% c(0, Inf),
                                            V('Responsiveness'), TrialKept = TrialNA)
                       else if (Fallback) paste0('OnFail = "', V('OnFail'), '": ', Summary$Message)
                       else NA_character_),
    TACs  = .ReplayCatchTACs(Y, PrevTAC, Trial, TAC, Unused = Fallback || TrialNA)
  )
  # B/BMSY of a fit that was not used: its last estimate
  if (Fallback && inherits(Fit, 'spfit') && length(Fit$B_BMSY)) {
    Last <- utils::tail(which(is.finite(Fit$B_BMSY)), 1)
    if (length(Last))
      Out$BPoint <- data.frame(Frame = Y, Year = as.numeric(names(Fit$B))[Last], Value = unname(Fit$B_BMSY[Last]),
                               Unused = TRUE)
  }
  if (!is.list(Prep))
    return(Out)

  nIndex <- ncol(Prep$Index)
  Name   <- factor(Prep$Name, levels = Prep$Name)
  Out$Index <- data.frame(Frame = Y, Name = rep(Name, each = nrow(Prep$Index)),
                          Year = rep(Prep$Years, nIndex), Observed = as.vector(Prep$Index),
                          Fitted = if (HasFit && !is.null(Fit$IndexPred)) as.vector(Fit$IndexPred) else NA_real_)
  Out$Index$Observed[!(Out$Index$Observed > 0)] <- NA
  if (HasFit) {
    Gap  <- V('Gap')
    Proj <- V('Proj')
    BMSY <- Fit$BMSY
    LastYear <- max(Prep$Years)
    AdviceYear <- V('AdviceYear')
    Out$Biomass <- rbind(
      data.frame(Frame = Y, Year = as.numeric(names(Fit$B)), Value = unname(Fit$B_BMSY), Series = 'Estimated'),
      data.frame(Frame = Y, Year = c(LastYear + seq_along(Gap$B), AdviceYear + seq_along(Proj$B) - 1),
                 Value = c(Gap$B, Proj$B) / BMSY, Series = 'Projected'))
    Out$BPoint <- data.frame(Frame = Y, Year = AdviceYear, Value = V('EstB'), Unused = FALSE)
    Out$Point  <- data.frame(Frame = Y, x = V('EstB'), y = V('FAdvice') / FMSY)
    Out$Curve  <- list(Frame = Y, CP = V('HCRControlPointsBiomass'), Rates = V('HCRControlPointsRate'),
                       RampType = V('RampType'), Scale = V('tunepar') * V('FTarget') * V('FMSYHCR') / FMSY)
  }
  Out
}


# ---- animations ----

.ReplayStatusColour <- '#1b6ca8'
.ReplayUnusedColour <- '#d62728'

# B/BMSY used by the HCR (orange), or of a fit that was not used (red)
.ReplayBPointLayers <- function(BPoint, Years, Scale = 1) {
  BPoint$Value <- BPoint$Value * Scale
  BPoint$What  <- factor(ifelse(BPoint$Unused, 'B/BMSY (fit not used)', 'B/BMSY used by HCR'),
                         levels = c('B/BMSY used by HCR', 'B/BMSY (fit not used)'))
  list(ggplot2::geom_point(data = .ReplayFrames(BPoint, Years),
                           ggplot2::aes(x = .data$Year, y = .data$Value, fill = .data$What),
                           shape = 21, colour = 'black', size = 3.5, inherit.aes = FALSE),
       ggplot2::scale_fill_manual(values = c('B/BMSY used by HCR' = .MPColours[['Status']],
                                             'B/BMSY (fit not used)' = .ReplayUnusedColour)))
}
.ReplayCatchColours <- c(Landings = '#264653', 'Dead discards' = '#9c6644', Removals = '#2a9d8f',
                         'Past TACs' = '#e67e22')

.ReplayFrames <- function(x, Years) {
  x$Frame <- factor(x$Frame, levels = Years)
  x
}

.ReplayTitle <- function(Title) paste0(Title, ': advice for {current_frame}')

# rows up to and including each frame (Before = TRUE: years before each frame)
.ReplayTrail <- function(Points, Years, Before = FALSE) {
  do.call(rbind, lapply(Years, \(f) {
    P <- Points[if (Before) Points$Year < f else Points$Year <= f, , drop = FALSE]
    if (nrow(P)) P$Frame <- f
    P
  }))
}

# year the index plot starts: per index, the year nearest the last historical drop to the cap
.ReplayIndexStart <- function(Index, Cap, YearLH) {
  Starts <- vapply(names(Cap), \(nm) {
    D <- Index[as.character(Index$Name) == nm & Index$Year <= YearLH & is.finite(Index$Observed), ]
    D <- D[order(D$Year), ]
    Above <- which(D$Observed > Cap[[nm]])
    if (!length(Above)) return(min(Index$Year))
    i <- max(Above)
    if (i < nrow(D) && abs(D$Observed[i + 1] - Cap[[nm]]) < abs(D$Observed[i] - Cap[[nm]])) i <- i + 1
    D$Year[i]
  }, numeric(1))
  min(Starts)
}

# frames with at least two points, for lines
.ReplayLines <- function(x) x[x$Frame %in% names(which(table(x$Frame) > 1)), , drop = FALSE]

# operating model population of each index, on the index scale
.ReplayOMIndex <- function(MSE, MPName, Sim, StockName) {
  X <- do.call(rbind, lapply(c('CPUE', 'Survey'), \(ty)
    tryCatch(.BuildIndexFitDF(MSE, ty), error = function(e) NULL)))
  if (is.null(X)) return(NULL)
  X <- X[X$Series == 'True' & X$Sim == Sim & as.character(X$Stock) == StockName &
           X$MP %in% c('Historical', MPName) & is.finite(X$Value), ]
  if (!nrow(X)) return(NULL)
  X <- stats::aggregate(Value ~ Fleet + Year, data = data.frame(Fleet = X$Fleet, Year = floor(X$Year),
                                                                Value = X$Value), FUN = mean)
  data.frame(Name = X$Fleet, Year = X$Year, Value = X$Value)
}

.ReplayOMRelative <- function(MSE, MPName, Sim, StockName, Var) {
  X <- tryCatch(as.data.frame(switch(Var, SB_SBMSY = SB_SBMSY(MSE), SB_SB0 = SB_SB0(MSE),
                                     B_BMSY = B_BMSY(MSE), B_B0 = B_B0(MSE))),
                error = function(e) NULL)
  if (is.null(X)) return(NULL)
  X <- X[X$Sim == Sim & as.character(X$Stock) == StockName & X$MP %in% c('Historical', MPName) &
           is.finite(X$Value), ]
  if (!nrow(X)) return(NULL)
  stats::aggregate(Value ~ Year, data = data.frame(Year = floor(X$Year), Value = X$Value), FUN = mean)
}

# depletion of SB or B, with the MSY level on the depletion scale (e.g. SBMSY/SB0) in attr K
.ReplayOMStatus <- function(MSE, MPName, Sim, StockName, Type = 'SB') {
  Dep <- .ReplayOMRelative(MSE, MPName, Sim, StockName, paste0(Type, '_', Type, '0'))
  if (is.null(Dep)) return(NULL)
  MSY <- .ReplayOMRelative(MSE, MPName, Sim, StockName, paste0(Type, '_', Type, 'MSY'))
  K <- NA_real_
  if (!is.null(MSY)) {
    M <- merge(Dep, MSY, by = 'Year')
    K <- stats::median(M$Value.x / M$Value.y, na.rm = TRUE)
  }
  structure(Dep, Type = Type, K = K)
}

# TAC recorded in the projection for every year, summed over fleets
.ReplayTACHistory <- function(TAC) {
  if (!is.array(TAC) || !identical(names(dimnames(TAC))[1], 'Year')) return(NULL)
  Out <- data.frame(Year = floor(as.numeric(dimnames(TAC)[[1]])), Value = apply(TAC, 1, sum, na.rm = TRUE))
  stats::aggregate(Value ~ Year, data = Out[is.finite(Out$Value), ], FUN = sum)
}

# operating model landings, dead discards, and removals, summed over fleets
.ReplayOMCatch <- function(MSE, MPName, Sim, StockName) {
  Get <- function(Fn, Series) {
    X <- tryCatch(as.data.frame(Fn(MSE)), error = function(e) NULL)
    if (is.null(X)) return(NULL)
    X <- X[X$Sim == Sim & as.character(X$Stock) == StockName & X$MP %in% c('Historical', MPName) &
             is.finite(X$Value), ]
    if (!nrow(X)) return(NULL)
    Out <- stats::aggregate(Value ~ Year, data = data.frame(Year = floor(X$Year), Value = X$Value), FUN = sum)
    Out$Series <- Series
    attr(Out, 'Units') <- unique(X$Units)[1]
    Out
  }
  L <- Get(Landings, 'Landings')
  Out <- rbind(L, Get(Discards, 'Dead discards'), Get(Removals, 'Removals'))
  if (!is.null(Out)) attr(Out, 'Units') <- attr(L, 'Units')
  Out
}

.ReplayOMBBMSY <- function(MSE, MPName, Sim, StockName)
  .ReplayOMRelative(MSE, MPName, Sim, StockName, 'B_BMSY')

.ReplayAnimHCR <- function(Points, Curve, Years, xlab, ylab) {
  Points$Year <- as.numeric(as.character(Points$Frame))
  Trail <- .ReplayTrail(Points[, c('Year', 'x', 'y')], Years)
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(data = .ReplayFrames(Curve, Years), linewidth = 1.2) +
    ggplot2::geom_path(data = .ReplayFrames(.ReplayLines(Trail), Years), colour = 'grey60', linewidth = 0.7) +
    ggplot2::geom_point(data = .ReplayFrames(Trail, Years), colour = 'grey50', size = 1.8) +
    ggplot2::geom_point(data = .ReplayFrames(Points, Years), colour = .MPColours[['Status']], size = 4) +
    ggplot2::expand_limits(x = 0, y = 0) +
    .MPXScale0() + .MPYScale0() +
    ggplot2::labs(x = xlab, y = ylab, title = .ReplayTitle('Harvest control rule')) +
    .MPTheme() +
    gganimate::transition_manual(Frame)
}

.ReplayAnimCatch <- function(P) {
  Years <- P$TACs$Year
  OM    <- if (is.null(P$OMCatch)) NULL else .ReplayTrail(P$OMCatch, Years, Before = TRUE)
  Catch <- OM[is.finite(OM$Value), c('Frame', 'Year', 'Value', 'Series')]
  if (!any(Catch$Series == 'Dead discards' & Catch$Value > 0))
    Catch <- Catch[!Catch$Series %in% c('Dead discards', 'Removals'), ]
  TACs  <- P$TACs
  Trail <- .ReplayTrail(P$TACHistory %||% data.frame(Year = TACs$Year, Value = TACs$TAC), Years)
  Trail <- Trail[is.finite(Trail$Value), ]
  if (nrow(Trail)) Trail$Series <- 'Past TACs'
  Lines <- rbind(Catch[, c('Frame', 'Year', 'Value', 'Series')], Trail[, c('Frame', 'Year', 'Value', 'Series')])
  Lines <- Lines[stats::ave(Lines$Value, Lines$Frame, Lines$Series, FUN = length) > 1, ]
  Series <- intersect(names(.ReplayCatchColours), unique(Lines$Series))
  Observed <- c('Landings', 'Dead discards', 'Removals')
  Lines$Series <- factor(Lines$Series, levels = c(setdiff(Series, Observed), intersect(Series, Observed)))

  Now <- rbind(data.frame(Frame = TACs$Year, Year = TACs$Year, Value = TACs$TrialTAC, What = 'Trial TAC'),
               data.frame(Frame = TACs$Year, Year = TACs$Year, Value = TACs$TAC,
                          What = ifelse(TACs$Unused, 'TAC (fit not used)', 'TAC')))
  Now <- Now[is.finite(Now$Value), ]
  Now$What <- factor(Now$What, levels = c('Trial TAC', 'TAC', 'TAC (fit not used)'))

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year, y = .data$Value)) +
    ggplot2::geom_line(data = .ReplayFrames(Lines, Years), ggplot2::aes(colour = .data$Series), linewidth = 1.1)
  if (nrow(Trail))
    p <- p + ggplot2::geom_point(data = .ReplayFrames(Trail, Years), colour = .ReplayCatchColours[['Past TACs']],
                                 size = 1.6, show.legend = FALSE)
  if (nrow(Now))
    p <- p +
      ggplot2::geom_point(data = .ReplayFrames(Now[order(Now$What == 'Trial TAC'), ], Years),
                          ggplot2::aes(shape = .data$What, fill = .data$What), size = 3.5, stroke = 1.1) +
      ggplot2::scale_shape_manual(values = c('Trial TAC' = 4, 'TAC' = 21, 'TAC (fit not used)' = 21)) +
      ggplot2::scale_fill_manual(values = c('Trial TAC' = 'black', 'TAC' = .MPColours[['Status']],
                                            'TAC (fit not used)' = .ReplayUnusedColour))
  p +
    ggplot2::scale_colour_manual(values = .ReplayCatchColours[Series], breaks = Series) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                    shape = ggplot2::guide_legend(order = 2), fill = ggplot2::guide_legend(order = 2)) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale() + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = .CatchLabel('Catch', attr(P$OMCatch, 'Units')),
                  title = .ReplayTitle('Catch and TAC')) +
    .MPTheme() +
    gganimate::transition_manual(Frame)
}

# depletion on the left axis, the same line in MSY units on the right axis;
# Est: surplus production B/BMSY estimates, drawn against the right axis
.ReplayAnimStatus <- function(S, Years, Est = NULL) {
  Type <- attr(S, 'Type')
  K    <- attr(S, 'K')
  HasK <- is.finite(K) && K > 0
  OMLabel <- paste0('Operating model ', Type, '/', Type, '0')
  Shown <- .ReplayTrail(data.frame(Year = S$Year, Value = S$Value, Series = OMLabel), Years)
  Now   <- Shown[Shown$Year == Shown$Frame, ]
  Cols  <- stats::setNames('grey10', OMLabel)
  Lty   <- stats::setNames('solid', OMLabel)
  if (HasK && !is.null(Est)) {
    B <- Est$Biomass
    B$Series <- ifelse(B$Series == 'Projected', 'Projected B/BMSY', 'Estimated B/BMSY')
    B$Value  <- B$Value * K
    Shown <- rbind(Shown, B[, names(Shown)])
    Cols  <- c(Cols, 'Estimated B/BMSY' = .MPColours[['Fitted']], 'Projected B/BMSY' = .MPColours[['Projected']])
    Lty   <- c(Lty, 'Estimated B/BMSY' = 'solid', 'Projected B/BMSY' = 'dashed')
  }
  Shown$Series <- factor(Shown$Series, levels = names(Cols))
  YScale <- ggplot2::scale_y_continuous(
    labels = .MPNumberLabels, expand = ggplot2::expansion(mult = c(0, 0.05)),
    sec.axis = if (HasK) ggplot2::sec_axis(~ . / K, name = paste0(Type, '/', Type, 'MSY'),
                                           labels = .MPNumberLabels) else ggplot2::waiver())
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year, y = .data$Value))
  if (HasK)
    p <- p + ggplot2::geom_hline(yintercept = K, colour = .MPColours[['Target']], linetype = 'dashed',
                                 linewidth = 0.8)
  p <- p +
    ggplot2::geom_line(data = .ReplayFrames(Shown, Years),
                       ggplot2::aes(colour = .data$Series, linetype = .data$Series), linewidth = 1.2) +
    ggplot2::geom_point(data = .ReplayFrames(Now, Years), colour = 'grey10', size = 3)
  if (HasK && !is.null(Est$Point))
    p <- p + .ReplayBPointLayers(Est$Point, Years, K)
  p +
    ggplot2::scale_colour_manual(values = Cols) +
    ggplot2::scale_linetype_manual(values = Lty) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1), linetype = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::expand_limits(x = range(S$Year), y = 0) +
    .MPYearScale() + YScale +
    ggplot2::labs(x = 'Year', y = paste0(Type, '/', Type, '0'), title = .ReplayTitle('Stock status')) +
    .MPTheme() +
    gganimate::transition_manual(Frame)
}

# operating model index population, revealed up to the last data year of each frame
.ReplayOMIndexLayer <- function(OMIndex, Index, Start, Years) {
  if (is.null(OMIndex)) return(NULL)
  X <- OMIndex[OMIndex$Name %in% levels(Index$Name) & OMIndex$Year >= Start, ]
  if (!nrow(X)) return(NULL)
  X$Name <- factor(X$Name, levels = levels(Index$Name))
  X <- .ReplayTrail(X, Years, Before = TRUE)
  ggplot2::geom_line(data = .ReplayFrames(X, Years), ggplot2::aes(y = .data$Value, colour = 'Operating model'),
                     linetype = 'dashed', linewidth = 0.9)
}

.ReplayAnimIndexMP <- function(P, IndexCap = Inf, OMIndex = NULL) {
  Years <- P$Years
  Calc  <- P$Settings

  Index  <- P$Index
  Cap    <- IndexCap * tapply(P$Target$Value[P$Target$Series == 'IndexTarget'],
                              P$Target$Name[P$Target$Series == 'IndexTarget'], max, na.rm = TRUE)
  Start  <- .ReplayIndexStart(Index[Index$Frame == max(Index$Frame), ], Cap, Calc$YearLH)
  Index  <- Index[Index$Year >= Start, ]
  Status <- P$Status
  Target <- P$Target
  nIndex <- length(unique(Index$Name))

  pI <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year)) +
    ggplot2::geom_hline(data = .ReplayFrames(Target, Years),
                        ggplot2::aes(yintercept = .data$Value, linetype = .data$Series),
                        colour = .MPColours[['Target']], linewidth = 0.8) +
    .ReplayOMIndexLayer(OMIndex, Index, Start, Years) +
    ggplot2::geom_point(data = .ReplayFrames(Index[is.finite(Index$Observed), ], Years),
                        ggplot2::aes(y = .data$Observed, colour = 'Observed'), size = 1.8)
  if (any(is.finite(Index$Smoothed)))
    pI <- pI + ggplot2::geom_line(data = .ReplayFrames(Index[is.finite(Index$Smoothed), ], Years),
                                  ggplot2::aes(y = .data$Smoothed, colour = 'Smoothed'), linewidth = 1.2)
  pI <- pI +
    ggplot2::geom_segment(data = .ReplayFrames(Status, Years),
                          ggplot2::aes(x = .data$From, xend = .data$To, y = .data$Value, yend = .data$Value),
                          colour = .MPColours[['Status']], linewidth = 1.6) +
    ggplot2::geom_point(data = .ReplayFrames(Status, Years), ggplot2::aes(y = .data$Value),
                        colour = .MPColours[['Status']], size = 3.5)
  if (!is.null(Calc$TrendYears))
    pI <- pI + ggplot2::geom_segment(data = .ReplayFrames(Status, Years),
                                     ggplot2::aes(x = .data$Year, xend = .data$TrendYear, y = .data$Value,
                                                  yend = .data$Trend),
                                     colour = .MPColours[['Status']], linewidth = 0.9,
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.2, 'cm')))
  pI <- pI +
    ggplot2::scale_colour_manual(values = c(.MPColours[c('Observed', 'Smoothed')], 'Operating model' = 'grey10'),
                                 breaks = c('Observed', 'Smoothed', 'Operating model')) +
    ggplot2::scale_linetype_manual(values = c('IndexTarget' = 'solid', 'HCR control points' = 'dashed')) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 2)) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale(n = if (nIndex > 1) 3 else 6) + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = 'Index', title = .ReplayTitle('Index status')) +
    .MPTheme() +
    gganimate::transition_manual(Frame)
  if (nIndex > 1)
    pI <- pI + ggplot2::facet_wrap(~Name, scales = 'free_y', ncol = min(nIndex, 2))

  Points <- P$Point
  xMax  <- max(c(1.5, 1.25 * Calc$HCRControlPointsIndex, 1.15 * Points$x), na.rm = TRUE)
  Curve <- .HCRCurve(Calc$HCRControlPointsIndex, Calc$HCRControlPointsRate, Calc$RampType, xMax)
  Curve <- do.call(rbind, lapply(Years, \(f) cbind(Curve, Frame = f)))
  pH <- .ReplayAnimHCR(Points, Curve, Years, 'Index / IndexTarget',
                       if (Calc$Type == 'Rate') 'Multiplier of catch-per-index rate' else 'TAC / reference catch')

  list(Index = pI, HCR = pH, Catch = .ReplayAnimCatch(P))
}

.ReplayAnimSP <- function(P, OMB, OMIndex = NULL, Biomass = TRUE) {
  Years <- P$Years
  Out <- list()

  Index <- P$Index
  if (!is.null(Index)) {
    nIndex <- length(unique(Index$Name))
    p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year)) +
      .ReplayOMIndexLayer(OMIndex, Index, min(Index$Year), Years) +
      ggplot2::geom_point(data = .ReplayFrames(Index[is.finite(Index$Observed), ], Years),
                          ggplot2::aes(y = .data$Observed, colour = 'Observed'), size = 1.8)
    if (any(is.finite(Index$Fitted)))
      p <- p + ggplot2::geom_line(data = .ReplayFrames(Index[is.finite(Index$Fitted), ], Years),
                                  ggplot2::aes(y = .data$Fitted, colour = 'Fitted'), linewidth = 1.2)
    Out$Index <- p +
      ggplot2::scale_colour_manual(values = c(.MPColours[c('Observed', 'Fitted')], 'Operating model' = 'grey10'),
                                   breaks = c('Observed', 'Fitted', 'Operating model')) +
      ggplot2::expand_limits(y = 0) +
      .MPYearScale(n = if (nIndex > 1) 3 else 6) + .MPYScale0() +
      ggplot2::labs(x = 'Year', y = 'Index', title = .ReplayTitle('Index fit')) +
      .MPTheme() +
      gganimate::transition_manual(Frame)
    if (nIndex > 1)
      Out$Index <- Out$Index + ggplot2::facet_wrap(~Name, scales = 'free_y', ncol = min(nIndex, 2))
  }

  B <- P$Biomass
  if (Biomass && !is.null(B)) {
    B$Series <- factor(B$Series, levels = c('Estimated', 'Projected'))
    p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year, y = .data$Value)) +
      ggplot2::geom_hline(yintercept = 1, colour = 'grey50')
    if (!is.null(OMB))
      p <- p + ggplot2::geom_line(data = .ReplayFrames(.ReplayTrail(OMB, Years), Years),
                                  ggplot2::aes(linetype = 'Operating model'), colour = 'grey10', linewidth = 0.9)
    Out$Biomass <- p +
      ggplot2::geom_line(data = .ReplayFrames(B, Years),
                         ggplot2::aes(colour = .data$Series, group = .data$Series), linewidth = 1.2) +
      .ReplayBPointLayers(P$BPoint, Years) +
      ggplot2::scale_colour_manual(values = c(Estimated = .MPColours[['Fitted']],
                                              Projected = .MPColours[['Projected']])) +
      ggplot2::scale_linetype_manual(values = c('Operating model' = 'dashed')) +
      ggplot2::expand_limits(y = 0) +
      .MPYearScale() + .MPYScale0() +
      ggplot2::labs(x = 'Year', y = 'B / BMSY', title = .ReplayTitle('Estimated biomass')) +
      .MPTheme() +
      gganimate::transition_manual(Frame)
  }

  Points <- P$Point
  if (!is.null(Points)) {
    Curves <- P$Curves
    xMax <- max(c(1.5, 1.25 * Curves[[1]]$CP, 1.15 * Points$x), na.rm = TRUE)
    Curve <- do.call(rbind, lapply(Curves, \(C)
      cbind(.HCRCurve(C$CP, C$Rates, C$RampType, xMax, C$Scale), Frame = C$Frame)))
    Out$HCR <- .ReplayAnimHCR(Points, Curve, Years, 'B / BMSY', 'F / FMSY')
  }

  Out$Catch <- .ReplayAnimCatch(P)
  Out
}


#' Plot One Year of an MP Replay
#'
#' Returns the frames of [ReplayMP()] animations for one management year as
#' ordinary `ggplot` objects, with the same axes as the animations.
#'
#' @param Replay An object returned by [ReplayMP()].
#' @param Year A management year in `Replay$Table$Year`.
#' @param Plot Names of the animations to return. `NULL` (default) returns
#'   all of them.
#'
#' @return A `ggplot` object when `Plot` names one animation, otherwise a
#'   named list of `ggplot` objects.
#'
#' @examples
#' \donttest{
#' Hist <- Simulate(SingleStockOM, nSim = 2)
#' MSE <- Project(Hist, 'IndexRate')
#' if (requireNamespace('gganimate', quietly = TRUE)) {
#'   R <- ReplayMP(MSE, 'IndexRate', Sim = 1)
#'   ReplayPlot(R, R$Table$Year[5], 'HCR')
#' }
#' }
#'
#' @seealso [ReplayMP()]
#' @export
ReplayPlot <- function(Replay, Year, Plot = NULL) {
  if (!inherits(Replay, 'mpreplay'))
    cli::cli_abort("{.arg Replay} must be an object returned by {.fn ReplayMP}.")
  if (length(Year) != 1 || !Year %in% Replay$Table$Year)
    cli::cli_abort(c("{.arg Year} must be one of the replayed management years.",
                     "i" = "Years: {.val {Replay$Table$Year}}."))
  Plot <- Plot %||% names(Replay$Animations)
  Unknown <- setdiff(Plot, names(Replay$Animations))
  if (length(Unknown))
    cli::cli_abort(c("No animation{?s} named {.val {Unknown}}.",
                     "i" = "Available: {.val {names(Replay$Animations)}}."))
  Out <- lapply(Replay$Animations[Plot], \(a) .ReplayStatic(a, Year))
  if (length(Out) == 1) Out[[1]] else Out
}

# layers are environments, so filtering one in place would change the animation
.ReplayCloneLayer <- function(L) {
  New <- list2env(as.list(L, all.names = TRUE), parent = parent.env(L))
  attributes(New) <- attributes(L)
  New
}

.ReplayPositionAes <- c('x', 'y', 'xend', 'yend', 'xmin', 'xmax', 'ymin', 'ymax', 'xintercept', 'yintercept')

# one frame of an animation, with blank layers holding every frame so the axes match
.ReplayStatic <- function(Anim, Year) {
  p <- Anim
  Layers <- list()
  Blanks <- list()
  for (L in Anim$layers) {
    D <- L$data
    if (is.data.frame(D) && 'Frame' %in% names(D)) {
      Map <- if (isTRUE(L$inherit.aes)) as.list(Anim$mapping) else list()
      Map[names(L$mapping)] <- as.list(L$mapping)
      Pos <- do.call(ggplot2::aes, Map[intersect(names(Map), .ReplayPositionAes)])
      Blanks <- c(Blanks, list(ggplot2::geom_blank(data = D, mapping = Pos, inherit.aes = FALSE)))
      Keep <- as.character(D$Frame) == as.character(Year)
      if (!any(Keep)) next
      L <- .ReplayCloneLayer(L)
      L$data <- D[Keep, , drop = FALSE]
    }
    Layers <- c(Layers, list(L))
  }
  p$layers <- c(Blanks, Layers)
  # drop non-position scales that no remaining layer maps (e.g. a point type absent this year)
  Mapped <- unique(c(names(Anim$mapping), unlist(lapply(Layers, \(L) names(L$mapping)))))
  Scales <- p$scales$clone()
  Scales$scales <- Filter(\(sc) any(sc$aesthetics %in% c(Mapped, .ReplayPositionAes)), Scales$scales)
  p$scales <- Scales
  p$transition <- NULL
  class(p) <- setdiff(class(p), 'gganim')
  p + ggplot2::labs(title = gsub('{current_frame}', Year, Anim$labels$title, fixed = TRUE))
}


#' Animate the Plots of an MP Replay Side by Side
#'
#' Draws the [ReplayMP()] plots for each management year next to each other
#' and writes them to a GIF, so the plots stay in step.
#'
#' @param Replay An object returned by [ReplayMP()].
#' @param Plot Names of the animations to include, in order. `NULL`
#'   (default) includes all of them.
#' @param ncol Number of columns. `NULL` (default) puts up to three plots in
#'   one row, and more in two rows.
#' @param fps Frames (management years) per second.
#' @param width,height Size of each frame in pixels. `NULL` (default) gives
#'   450 x 420 pixels per plot.
#' @param res Resolution in pixels per inch.
#' @param file Path of the GIF. `NULL` (default) writes a temporary file.
#'
#' @return A `gif_image` object (see [gganimate::gif_file()]): printing it
#'   shows the GIF in the viewer, and [gganimate::anim_save()] saves it.
#'
#' @examples
#' \donttest{
#' Hist <- Simulate(SingleStockOM, nSim = 2)
#' MSE <- Project(Hist, 'IndexRate')
#' if (requireNamespace('gganimate', quietly = TRUE) && requireNamespace('gifski', quietly = TRUE)) {
#'   R <- ReplayMP(MSE, 'IndexRate', Sim = 1)
#'   ReplayAnimate(R, fps = 1)
#' }
#' }
#'
#' @seealso [ReplayMP()], [ReplayPlot()]
#' @export
ReplayAnimate <- function(Replay, Plot = NULL, ncol = NULL, fps = 2, width = NULL, height = NULL,
                          res = 96, file = NULL) {
  if (!inherits(Replay, 'mpreplay'))
    cli::cli_abort("{.arg Replay} must be an object returned by {.fn ReplayMP}.")
  CheckPackage('gifski')
  Plot <- Plot %||% names(Replay$Animations)
  Unknown <- setdiff(Plot, names(Replay$Animations))
  if (length(Unknown))
    cli::cli_abort(c("No animation{?s} named {.val {Unknown}}.",
                     "i" = "Available: {.val {names(Replay$Animations)}}."))
  if (!is.numeric(fps) || length(fps) != 1 || !is.finite(fps) || fps <= 0)
    cli::cli_abort("{.arg fps} must be a single positive number.")
  ncol   <- ncol %||% (if (length(Plot) <= 3) length(Plot) else ceiling(length(Plot) / 2))
  nrow   <- ceiling(length(Plot) / ncol)
  width  <- width %||% (450 * ncol)
  height <- height %||% (420 * nrow)
  file   <- file %||% tempfile(fileext = '.gif')

  Subtitle <- sprintf('Simulation %s, %s', Replay$Sim, Replay$Stock)
  Advice   <- stats::setNames(Replay$Table$Advice, Replay$Table$Year)
  Type     <- stats::setNames(ifelse(Replay$Table$AdviceType == 'TAC', 'TAC', 'relative effort'),
                              Replay$Table$Year)
  TitleTheme <- ggplot2::theme(plot.title = ggplot2::element_text(face = 'bold', size = 20),
                               plot.subtitle = ggplot2::element_text(size = 14, colour = 'grey30'))
  PB <- cli::cli_progress_bar("Drawing frames", total = nrow(Replay$Table), clear = TRUE)
  on.exit(cli::cli_progress_done(id = PB), add = TRUE)
  gifski::save_gif({
    for (Y in Replay$Table$Year) {
      cli::cli_progress_update(id = PB)
      Figs <- ReplayPlot(Replay, Y, Plot) |> .ReplayAsList(Plot)
      Figs <- lapply(Figs, \(p) p + ggplot2::labs(title = sub(': advice for .*$', '', p$labels$title)))
      Key  <- as.character(Y)
      print(patchwork::wrap_plots(Figs, ncol = ncol) +
              patchwork::plot_annotation(title = sprintf('%s: %s %s = %s', Replay$MP, Key, Type[[Key]],
                                                         .MPFormat(Advice[[Key]])),
                                         subtitle = Subtitle, theme = TitleTheme))
    }
  }, gif_file = file, width = width, height = height, delay = 1 / fps, progress = FALSE, res = res)
  gganimate::gif_file(file)
}

.ReplayAsList <- function(x, Plot) {
  if (inherits(x, 'ggplot')) stats::setNames(list(x), Plot) else x
}
