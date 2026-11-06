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
#' @param IndexCap For MPs that record an `IndexTarget` (e.g. [IndexRate()]
#'   and [IndexTarget()]), the `Index` animation starts in the historical
#'   year when the observed index last comes down to `IndexCap` times the
#'   largest `IndexTarget` of the index (the earliest such year over
#'   indices), so the target and control points stay visible. `Inf` shows
#'   the full series. Default `1.5`.
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
#' - `Status` and `Catch`, for every MP.
#' - `Index`, `HCR`, and the estimated `B/BMSY` on the `Status` plot, from
#'   the values the MP records with [RecordMP()]. [IndexRate()],
#'   [IndexTarget()], and [SurplusProduction()] record the indices (with
#'   their status and `IndexTarget`, or their fitted values), the harvest
#'   control rule, and the estimated `B/BMSY` ([SurplusProduction()]). See
#'   [RecordMP()] to add these to other MPs.
#'
#' The columns of `Table` after `Recorded` are the `Values` and `TrialTAC`
#' recorded by the MP.
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
#' In the `Index`, `Biomass`, `Status`, and `Catch` plots, a light solid
#' line marks the end of the historical period and, when the OM has an
#' `MPStartYear` after the first projection year (with interim advice
#' before it), a light dashed line the year MPs are first applied.
#'
#' [ReplayAnimate()] shows the animations side by side, and [ReplayPlot()]
#' returns the plots of a single year.
#'
#' The MP is run with the version of MSEtool currently loaded. Multi-stock
#' (`mmp`) MPs are not supported.
#'
#' @return An object of class `mpreplay`: a list with `MP`, `Sim`, `Stock`,
#'   `Table` (a `data.frame` with one row per management year: the replayed
#'   `Advice`, the `Recorded` advice, and the values recorded by the MP), and
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
#' @seealso [RecordMP()], [ReplayAnimate()], [ReplayPlot()], [IndexRate()],
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
    .MPReplayState$Record <- NULL
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
    .MPReplayState$Record <- NULL
    Adv <- tryCatch(suppressMessages(.WithMPSeed(MPfun(Data = DataY), OM@Seed, .GlobalSim(OM, Sim),
                                                 Y, StockName)),
                    error = function(e) e)
    if (inherits(Adv, 'advice')) {
      Misc <- Adv@Misc
      Misc$AnnualTAC <- Misc$StatusQuoEffort <- NULL
    }
    if (Y %in% Years)
      Runs[[length(Runs) + 1]] <- list(Year = Y, YearLH = DataY@YearLH, Advice = Adv,
                                       Record = .MPReplayState$Record,
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

  if (all(vapply(Runs, \(r) is.null(r$Record), logical(1))))
    cli::cli_inform(c("i" = "{.val {MPName}} does not record its calculations with {.fn RecordMP}: only the {.val Status} and {.val Catch} animations are available."))
  Rows <- lapply(Runs, .ReplayFrame)
  P <- list(YearLH = Runs[[1]]$YearLH)
  rm(Runs)
  Parts <- c('Table', 'Index', 'IndexStatus', 'Target', 'Point', 'TACs', 'Biomass', 'BPoint')
  P[Parts] <- lapply(Parts, \(nm) do.call(rbind, lapply(Rows, `[[`, nm)))
  P$Years  <- vapply(Rows, \(r) r$TACs$Year, numeric(1))
  P$Curves <- Filter(Negate(is.null), lapply(Rows, `[[`, 'Curve'))
  rm(Rows)

  Table <- P$Table
  rownames(Table) <- NULL
  Diff  <- is.finite(Table$Advice) & is.finite(Table$Recorded) &
    abs(Table$Advice - Table$Recorded) > 1e-6 * pmax(1, abs(Table$Recorded))
  if (any(Diff))
    cli::cli_warn(c("!" = "Replayed advice differs from the advice recorded in the projection in {.val {Table$Year[Diff]}}.",
                    "i" = "The MP may use information not stored in the MSE object (e.g. {.code Data@Misc$DataOM})."))

  P$OMCatch    <- .ReplayOMCatch(MSE, MPName, Sim, StockName)
  P$TACHistory <- .ReplayTACHistory(D@Advice@TAC)
  P$Boundaries  <- .ReplayBoundaries(P$YearLH, OM@MPStartYear, YearsProj)
  OMStatus <- if (length(Status)) .ReplayOMStatus(MSE, MPName, Sim, StockName, Status)
  # estimated B/BMSY goes on the Status plot when it has an MSY axis
  HasBiomass  <- !is.null(P$Biomass) || !is.null(P$BPoint)
  EstOnStatus <- HasBiomass && is.finite(attr(OMStatus, 'K') %||% NA)

  Animations <- list()
  if (!is.null(P$Index))
    Animations$Index <- .ReplayAnimIndex(P, IndexCap, .ReplayOMIndex(MSE, MPName, Sim, StockName))
  if (HasBiomass && !EstOnStatus)
    Animations$Biomass <- .ReplayAnimBiomass(P, .ReplayOMBBMSY(MSE, MPName, Sim, StockName))
  if (!is.null(OMStatus))
    Animations$Status <- .ReplayAnimStatus(OMStatus, P$Years,
                                           if (EstOnStatus) list(Biomass = P$Biomass, Point = P$BPoint),
                                           P$Boundaries)
  if (!is.null(P$Point))
    Animations$HCR <- .ReplayAnimHCR(P)
  Animations$Catch <- .ReplayAnimCatch(P)

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

# per-year values and plotting data, from the advice and the record of RecordMP()
.ReplayFrame <- function(r) {
  Y   <- r$Year
  Adv <- r$Advice
  Rec <- r$Record %||% list()
  Type <- r$Recorded$Type
  if (is.na(Type)) Type <- if (!is.null(Adv@TAC)) 'TAC' else 'Effort'
  Base <- data.frame(Year = Y, AdviceType = Type,
                     Advice = .ReplayTotal(Type, methods::slot(Adv, Type)),
                     Recorded = r$Recorded$Value)
  Tab <- if (length(Rec$Values)) as.data.frame(Rec$Values, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(Rec$TrialTAC) && !'TrialTAC' %in% names(Tab))
    Tab <- if (is.null(Tab)) data.frame(TrialTAC = Rec$TrialTAC) else cbind(Tab, TrialTAC = Rec$TrialTAC)
  Unused <- isTRUE(Rec$Unused)
  Out <- list(Table = if (is.null(Tab)) Base else cbind(Base, Tab),
              TACs  = data.frame(Year = Y, TrialTAC = Rec$TrialTAC %||% NA_real_,
                                 TAC = if (Type == 'TAC') Base$Advice else NA_real_, Unused = Unused))
  if (!is.null(Rec$Index))
    Out <- c(Out, .ReplayFrameIndex(Rec$Index, Y))
  if (!is.null(Rec$HCR)) {
    Out$Point <- data.frame(Frame = Y, x = Rec$HCR$x, y = Rec$HCR$y)
    Out$Curve <- c(list(Frame = Y), unclass(Rec$HCR))
  }
  B <- Rec$Biomass
  if (!is.null(B)) {
    Out$Biomass <- rbind(
      if (length(B$Value)) data.frame(Frame = Y, Year = B$Years, Value = B$Value, Series = 'Estimated'),
      if (length(B$Projected)) data.frame(Frame = Y, Year = B$ProjectedYears, Value = B$Projected,
                                          Series = 'Projected'))
    if (length(B$Est) && is.finite(B$Est))
      Out$BPoint <- data.frame(Frame = Y, Year = B$EstYear, Value = B$Est, Unused = Unused)
  }
  Out
}

.ReplayFrameIndex <- function(I, Y) {
  nYear  <- length(I$Years)
  nIndex <- length(I$Name)
  Name   <- factor(I$Name, levels = I$Name)
  Col    <- function(x) if (is.null(x)) NA_real_ else as.vector(x)
  Out <- list(Index = data.frame(Frame = Y, Name = rep(Name, each = nYear), Year = rep(I$Years, nIndex),
                                 Observed = as.vector(I$Value), Smoothed = Col(I$Smoothed),
                                 Fitted = Col(I$Fitted)))
  if (!is.null(I$Status))
    Out$IndexStatus <- data.frame(Frame = Y, Name = Name, From = min(I$StatusYears) - 0.5,
                                  To = max(I$StatusYears) + 0.5, Year = max(I$Years), Value = I$Status,
                                  Trend = I$StatusTrend %||% NA_real_,
                                  TrendYear = max(I$Years) + I$TrendHorizon)
  if (!is.null(I$IndexTarget)) {
    CP <- I$ControlPointsIndex[I$ControlPointsIndex > 0]
    Out$Target <- rbind(data.frame(Frame = Y, Name = Name, Value = I$IndexTarget, Series = 'IndexTarget'),
                        if (length(CP)) data.frame(Frame = Y, Name = rep(Name, each = length(CP)),
                                                   Value = as.vector(outer(CP, I$IndexTarget)),
                                                   Series = 'HCR control points'))
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

# end of the historical period and, when later, the first year MPs are applied
.ReplayBoundaries <- function(YearLH, MPStartYear, YearsProj) {
  Out <- c('End of historical period' = floor(YearLH) + 0.5)
  if (length(MPStartYear) && is.finite(MPStartYear) && MPStartYear > min(floor(YearsProj)))
    Out <- c(Out, 'MP start year' = MPStartYear - 0.5)
  Out
}

.ReplayBoundaryLayer <- function(Boundaries) {
  if (!length(Boundaries)) return(NULL)
  ggplot2::geom_vline(xintercept = unname(Boundaries), colour = 'grey70', linewidth = 0.5,
                      linetype = ifelse(names(Boundaries) == 'MP start year', 'dashed', 'solid'))
}

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

.ReplayAnimHCR <- function(P) {
  Years  <- P$Years
  Points <- P$Point
  Rules  <- P$Curves
  xMax   <- max(c(1.5, 1.25 * Rules[[1]]$ControlPointsIndex, 1.15 * Points$x), na.rm = TRUE)
  Curve  <- do.call(rbind, lapply(Rules, \(H) {
    C <- if (!is.null(H$Fun)) {
      x <- seq(0, xMax, length.out = 400)
      data.frame(x = x, y = H$Scale * vapply(x, H$Fun, numeric(1)))
    } else if (!is.null(H$ControlPointsIndex)) {
      .HCRCurve(H$ControlPointsIndex, H$ControlPointsRate, H$RampType, xMax, H$Scale)
    }
    if (!is.null(C)) cbind(C, Frame = H$Frame)
  }))
  Points$Year <- as.numeric(as.character(Points$Frame))
  Trail <- .ReplayTrail(Points[, c('Year', 'x', 'y')], Years)
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$x, y = .data$y))
  if (!is.null(Curve))
    p <- p + ggplot2::geom_line(data = .ReplayFrames(Curve, Years), linewidth = 1.2)
  p +
    ggplot2::geom_path(data = .ReplayFrames(.ReplayLines(Trail), Years), colour = 'grey60', linewidth = 0.7) +
    ggplot2::geom_point(data = .ReplayFrames(Trail, Years), colour = 'grey50', size = 1.8) +
    ggplot2::geom_point(data = .ReplayFrames(Points, Years), colour = .MPColours[['Status']], size = 4) +
    ggplot2::expand_limits(x = 0, y = 0) +
    .MPXScale0() + .MPYScale0() +
    ggplot2::labs(x = Rules[[1]]$xlab, y = Rules[[1]]$ylab, title = .ReplayTitle('Harvest control rule')) +
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
    .ReplayBoundaryLayer(P$Boundaries) +
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
.ReplayAnimStatus <- function(S, Years, Est = NULL, Boundaries = NULL) {
  Type <- attr(S, 'Type')
  K    <- attr(S, 'K')
  HasK <- is.finite(K) && K > 0
  OMLabel <- paste0('Operating model ', Type, '/', Type, '0')
  Shown <- .ReplayTrail(data.frame(Year = S$Year, Value = S$Value, Series = OMLabel), Years)
  Now   <- Shown[Shown$Year == Shown$Frame, ]
  Cols  <- stats::setNames('grey10', OMLabel)
  Lty   <- stats::setNames('solid', OMLabel)
  if (HasK && !is.null(Est$Biomass)) {
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
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year, y = .data$Value)) +
    .ReplayBoundaryLayer(Boundaries)
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

# indices with, where recorded, the smoothed and fitted values, status, trend, target, and control points
.ReplayAnimIndex <- function(P, IndexCap = Inf, OMIndex = NULL) {
  Years  <- P$Years
  Index  <- P$Index
  Target <- P$Target
  Status <- P$IndexStatus
  Start  <- min(Index$Year)
  if (!is.null(Target) && is.finite(IndexCap)) {
    Ref <- Target[Target$Series == 'IndexTarget', ]
    Cap <- IndexCap * tapply(Ref$Value, as.character(Ref$Name), max, na.rm = TRUE)
    Start <- .ReplayIndexStart(Index[Index$Frame == max(Index$Frame), ], Cap[is.finite(Cap)], P$YearLH)
  }
  Index  <- Index[Index$Year >= Start, ]
  nIndex <- length(unique(Index$Name))

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year)) +
    .ReplayBoundaryLayer(P$Boundaries[P$Boundaries > Start])
  if (!is.null(Target))
    p <- p + ggplot2::geom_hline(data = .ReplayFrames(Target, Years),
                                 ggplot2::aes(yintercept = .data$Value, linetype = .data$Series),
                                 colour = .MPColours[['Target']], linewidth = 0.8)
  p <- p +
    .ReplayOMIndexLayer(OMIndex, Index, Start, Years) +
    ggplot2::geom_point(data = .ReplayFrames(Index[is.finite(Index$Observed), ], Years),
                        ggplot2::aes(y = .data$Observed, colour = 'Observed'), size = 1.8)
  for (S in c('Smoothed', 'Fitted'))
    if (any(is.finite(Index[[S]])))
      p <- p + ggplot2::geom_line(data = .ReplayFrames(Index[is.finite(Index[[S]]), ], Years),
                                  ggplot2::aes(y = .data[[!!S]], colour = !!S), linewidth = 1.2)
  if (!is.null(Status)) {
    p <- p +
      ggplot2::geom_segment(data = .ReplayFrames(Status, Years),
                            ggplot2::aes(x = .data$From, xend = .data$To, y = .data$Value, yend = .data$Value),
                            colour = .MPColours[['Status']], linewidth = 1.6) +
      ggplot2::geom_point(data = .ReplayFrames(Status, Years), ggplot2::aes(y = .data$Value),
                          colour = .MPColours[['Status']], size = 3.5)
    Trend <- Status[is.finite(Status$Trend), ]
    if (nrow(Trend))
      p <- p + ggplot2::geom_segment(data = .ReplayFrames(Trend, Years),
                                     ggplot2::aes(x = .data$Year, xend = .data$TrendYear, y = .data$Value,
                                                  yend = .data$Trend),
                                     colour = .MPColours[['Status']], linewidth = 0.9,
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.2, 'cm')))
  }
  Series <- c('Observed', if (any(is.finite(Index$Smoothed))) 'Smoothed', if (any(is.finite(Index$Fitted))) 'Fitted',
              if (!is.null(OMIndex)) 'Operating model')
  Title <- if (!is.null(Target) || !is.null(Status)) 'Index status' else if ('Fitted' %in% Series) 'Index fit' else 'Indices'
  p <- p +
    ggplot2::scale_colour_manual(values = c(.MPColours[c('Observed', 'Smoothed', 'Fitted')], 'Operating model' = 'grey10'),
                                 breaks = Series) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 2)) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale(n = if (nIndex > 1) 3 else 6) + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = 'Index', title = .ReplayTitle(Title)) +
    .MPTheme() +
    gganimate::transition_manual(Frame)
  if (!is.null(Target))
    p <- p + ggplot2::scale_linetype_manual(values = c('IndexTarget' = 'solid', 'HCR control points' = 'dashed'))
  if (nIndex > 1)
    p <- p + ggplot2::facet_wrap(~Name, scales = 'free_y', ncol = min(nIndex, 2))
  p
}

# estimated B/BMSY with the operating model B/BMSY, when there is no Status plot to draw it on
.ReplayAnimBiomass <- function(P, OMB) {
  Years <- P$Years
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year, y = .data$Value)) +
    .ReplayBoundaryLayer(P$Boundaries) +
    ggplot2::geom_hline(yintercept = 1, colour = 'grey50')
  if (!is.null(OMB))
    p <- p + ggplot2::geom_line(data = .ReplayFrames(.ReplayTrail(OMB, Years), Years),
                                ggplot2::aes(linetype = 'Operating model'), colour = 'grey10', linewidth = 0.9)
  B <- P$Biomass
  if (!is.null(B)) {
    B$Series <- factor(B$Series, levels = c('Estimated', 'Projected'))
    p <- p + ggplot2::geom_line(data = .ReplayFrames(B, Years),
                                ggplot2::aes(colour = .data$Series, group = .data$Series), linewidth = 1.2)
  }
  if (!is.null(P$BPoint))
    p <- p + .ReplayBPointLayers(P$BPoint, Years)
  p +
    ggplot2::scale_colour_manual(values = c(Estimated = .MPColours[['Fitted']],
                                            Projected = .MPColours[['Projected']])) +
    ggplot2::scale_linetype_manual(values = c('Operating model' = 'dashed')) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale() + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = 'B / BMSY', title = .ReplayTitle('Estimated biomass')) +
    .MPTheme() +
    gganimate::transition_manual(Frame)
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
