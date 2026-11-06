#' Draw the Figures of an MP Called with `plot`
#'
#' @param plot The MP's `plot` argument: `TRUE`, `FALSE`, or the names of the
#'   figures to draw.
#' @param Build A function with no arguments returning a named list of
#'   `ggplot` objects, with the calculation steps (character vector) in its
#'   `Steps` attribute and a one-line summary in its `Heading` attribute.
#' @return The named list of `ggplot` objects (all figures, including those
#'   not drawn), or `NULL` when no figures were made. The steps are printed
#'   to the console and the selected figures are drawn.
#' @keywords internal
.MPPlot <- function(plot, Build) {
  if (isTRUE(.MPReplayState$Capture)) {
    .MPReplayState$Env <- environment(Build)
    return(NULL)
  }
  if (is.null(plot) || isFALSE(plot) || !length(plot))
    return(NULL)
  if (!isTRUE(plot) && !is.character(plot))
    cli::cli_abort("{.arg plot} must be {.code TRUE}, {.code FALSE}, or the names of figures to draw.")
  if (.InProjection()) {
    cli::cli_inform(c("i" = "{.arg plot} is ignored when an MP is run by {.fn Project}."),
                    .frequency = 'once', .frequency_id = 'MPPlotProject')
    return(NULL)
  }
  Plots <- tryCatch(Build(), error = function(e) {
    cli::cli_warn(c("!" = "The MP figures could not be drawn: {conditionMessage(e)}"))
    NULL
  })
  if (is.null(Plots))
    return(NULL)
  Steps   <- attr(Plots, 'Steps')
  Heading <- attr(Plots, 'Heading')
  Plots   <- Plots[names(Plots)]
  if (length(Steps)) {
    Esc <- function(x) gsub('}', '}}', gsub('{', '{{', x, fixed = TRUE), fixed = TRUE)
    if (length(Heading)) cli::cli_h3(Esc(Heading))
    cli::cli_bullets(stats::setNames(Esc(Steps), rep('*', length(Steps))))
  }
  Show <- if (isTRUE(plot)) names(Plots) else plot
  Unknown <- setdiff(Show, names(Plots))
  if (length(Unknown))
    cli::cli_warn(c("!" = "No figure{?s} named {.val {Unknown}}.",
                    "i" = "Available: {.val {names(Plots)}}."))
  for (nm in intersect(Show, names(Plots))) print(Plots[[nm]])
  Plots
}

# store the figures from .MPPlot() in the advice object
.AddPlots <- function(Advice, Plots) {
  if (!is.null(Plots)) Advice@Misc$Plots <- Plots
  Advice
}

# number of recent years shown in the index status and catch figures
.MPPlotYears <- 15

# values, labelled by index name when there is more than one
.MPByIndex <- function(x, Names) {
  Out <- .MPFormat(x)
  if (length(Names) > 1) Out <- paste0(Out, ' (', Names, ')')
  paste(Out, collapse = ', ')
}

.MPYears <- function(Years) {
  Years <- range(Years)
  if (Years[1] == Years[2]) as.character(Years[1]) else paste0(Years[1], '-', Years[2])
}

.InProjection <- function() {
  Fns <- c('.CalcAdviceSimMP', '.CalcAdviceSimMMP')
  any(vapply(sys.calls(), \(cl) is.name(cl[[1]]) && as.character(cl[[1]]) %in% Fns, logical(1)))
}

.MPFormat <- function(x) {
  if (!length(x)) return('NA')
  vapply(x, \(v) if (is.finite(v)) format(signif(v, 4), big.mark = ',', scientific = FALSE, trim = TRUE)
         else 'NA', character(1))
}

.MPColours <- c(Observed = 'grey35', Smoothed = '#1b6ca8', Fitted = '#1b6ca8',
                Target = '#c0392b', Status = '#e67e22', Projected = '#e67e22',
                Gap = 'grey50')

.MPTheme <- function(base_size = 13) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position = 'bottom', legend.box = 'vertical',
                   legend.title = ggplot2::element_blank(), legend.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.key.width = ggplot2::unit(1.8, 'lines'),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = 'bold'),
                   axis.text = ggplot2::element_text(colour = 'grey15'),
                   plot.caption = ggplot2::element_text(size = ggplot2::rel(0.75), colour = 'grey35'),
                   strip.background = ggplot2::element_rect(fill = 'grey95'),
                   strip.text = ggplot2::element_text(face = 'bold'))
}

.MPNumberLabels <- function(x) format(x, big.mark = ',', scientific = FALSE, trim = TRUE)

# whole-year breaks, with Extra (e.g. the advice year) added and nearby breaks dropped
.MPYearScale <- function(Extra = NULL, n = 6) {
  Breaks <- function(Lim) {
    b <- pretty(Lim, n = n)
    b <- b[b == round(b) & b >= Lim[1] & b <= Lim[2]]
    if (!is.null(Extra)) b <- sort(c(b[abs(b - Extra) >= 3], Extra))
    b
  }
  ggplot2::scale_x_continuous(breaks = Breaks, labels = \(x) format(x, scientific = FALSE, trim = TRUE),
                              expand = ggplot2::expansion(mult = c(0.01, 0.03)))
}

# axes that start at zero
.MPYScale0 <- function()
  ggplot2::scale_y_continuous(labels = .MPNumberLabels, expand = ggplot2::expansion(mult = c(0, 0.05)))

.MPXScale0 <- function()
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.03)))

# one-line description of how ConstrainTAC() changed the trial TAC
.TACConstraintNote <- function(PrevTAC, TrialTAC, Mod, TAC, DeltaDown, DeltaUp, TACRange,
                               Responsiveness, TrialKept = FALSE) {
  if (TrialKept)
    return('Trial TAC not finite: previous TAC kept.')
  Notes <- character()
  if (Responsiveness != 1)
    Notes <- c(Notes, sprintf('change scaled by Responsiveness = %s', Responsiveness))
  if (!is.finite(Mod)) return(paste(c(Notes, 'TAC set by TACRange.'), collapse = '; '))
  if (Mod > 1 + DeltaUp[2]) {
    Notes <- c(Notes, sprintf('increase capped at DeltaUp = %s%%', 100 * DeltaUp[2]))
  } else if (Mod < 1 - DeltaDown[2]) {
    Notes <- c(Notes, sprintf('decrease capped at DeltaDown = %s%%', 100 * DeltaDown[2]))
  } else if (Mod != 1 && Mod < 1 + DeltaUp[1] && Mod > 1 - DeltaDown[1]) {
    Notes <- c(Notes, 'change smaller than the minimum change: TAC unchanged')
  }
  if (isTRUE(TAC == TACRange[1]) || isTRUE(TAC == TACRange[2]))
    Notes <- c(Notes, 'TAC at a TACRange bound')
  if (!length(Notes)) return('Trial TAC applied without constraint.')
  paste0(toupper(substring(Notes[1], 1, 1)), substring(paste(Notes, collapse = '; '), 2), '.')
}

# HCR multiplier over a grid of the status level
.HCRCurve <- function(ControlPoints, Rates, RampType, xMax, Scale = 1) {
  x <- seq(0, xMax, length.out = 400)
  y <- Scale * vapply(x, \(v) HockeyStickHCR(1, Est = v, Ref = 1, ControlPointsIndex = ControlPoints,
                                             ControlPointsRate = Rates, RampType = RampType),
                      numeric(1))
  data.frame(x = x, y = y)
}

# HCR curve over a grid of the status level, with the current point
.PlotHCRPanel <- function(ControlPoints, Rates, RampType, Level, Value, Scale = 1,
                          xlab, ylab, Path = NULL) {
  xMax  <- max(c(1.5, 1.25 * ControlPoints, 1.15 * Level, Path$x), na.rm = TRUE)
  Curve <- .HCRCurve(ControlPoints, Rates, RampType, xMax, Scale)
  Pts   <- data.frame(x = ControlPoints, y = Scale * Rates)
  Now   <- data.frame(x = Level, y = Value,
                      Label = sprintf('(%s, %s)', .MPFormat(Level), .MPFormat(Value)))

  p <- ggplot2::ggplot(Curve, ggplot2::aes(x = .data$x, y = .data$y))
  if (!is.null(Path)) {
    p <- p +
      ggplot2::geom_path(data = Path, colour = 'grey70') +
      ggplot2::geom_point(data = Path, colour = 'grey60', size = 1.3) +
      ggplot2::geom_text(data = Path[c(1, nrow(Path)), ], ggplot2::aes(label = .data$Year),
                         colour = 'grey45', size = 2.8, vjust = -0.7)
  }
  p +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(data = Pts, shape = 21, fill = 'white', size = 2.4) +
    ggplot2::geom_vline(xintercept = Level, colour = .MPColours[['Status']], linetype = 'dashed') +
    ggplot2::geom_point(data = Now, colour = .MPColours[['Status']], size = 3.2) +
    ggplot2::geom_text(data = Now, ggplot2::aes(label = .data$Label), hjust = -0.1, vjust = 1.4,
                       size = 3.2, colour = .MPColours[['Status']]) +
    ggplot2::expand_limits(x = 0, y = 0) +
    .MPXScale0() + .MPYScale0() +
    ggplot2::labs(x = xlab, y = ylab, title = 'Harvest control rule') +
    .MPTheme()
}

# catch history and the TAC calculation at the advice year
.PlotTACPanel <- function(Catch, AdviceYear, PrevTAC, TrialTAC, TAC,
                          ylab, Lines = NULL, Shade = NULL, Ref = NULL) {
  Advice <- data.frame(Year = AdviceYear - c(1, 0, 0), Value = c(PrevTAC, TrialTAC, TAC),
                       What = factor(c('Previous TAC', 'Trial TAC', 'TAC'),
                                     levels = c('Previous TAC', 'Trial TAC', 'TAC')))
  Advice <- Advice[is.finite(Advice$Value), ]

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year))
  if (!is.null(Shade))
    p <- p + ggplot2::annotate('rect', xmin = Shade[1] - 0.5, xmax = Shade[2] + 0.5,
                               ymin = -Inf, ymax = Inf, fill = 'grey85', alpha = 0.6)
  p <- p +
    ggplot2::geom_line(data = Catch, ggplot2::aes(y = .data$Value, colour = .data$Series,
                                                   linetype = .data$Series), linewidth = 1) +
    ggplot2::geom_point(data = Catch[Catch$Series == Catch$Series[1], ],
                        ggplot2::aes(y = .data$Value, colour = .data$Series), size = 1)
  if (!is.null(Lines))
    p <- p + ggplot2::geom_line(data = Lines, ggplot2::aes(y = .data$Value, colour = .data$Series,
                                                           linetype = .data$Series),
                                linewidth = 1)
  if (!is.null(Ref))
    p <- p +
      ggplot2::geom_segment(data = Ref, ggplot2::aes(x = .data$From - 0.5, xend = .data$To + 0.5,
                                                     y = .data$Value, yend = .data$Value),
                            colour = .MPColours[['Target']], linewidth = 0.9) +
      ggplot2::geom_text(data = Ref, ggplot2::aes(x = .data$From - 0.5, y = .data$Value,
                                                  label = .data$Label),
                         colour = .MPColours[['Target']], hjust = 1.05, size = 3)
  if (nrow(Advice))
    p <- p +
      ggplot2::geom_point(data = Advice[order(Advice$What == 'Trial TAC'), ],
                          ggplot2::aes(y = .data$Value, shape = .data$What,
                                                      fill = .data$What), size = 3) +
      ggplot2::scale_shape_manual(values = c('Previous TAC' = 21, 'Trial TAC' = 4, 'TAC' = 21)) +
      ggplot2::scale_fill_manual(values = c('Previous TAC' = 'white', 'Trial TAC' = 'black',
                                            'TAC' = .MPColours[['Status']]))
  p +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale(Extra = AdviceYear) + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = ylab, title = 'Catch and TAC') +
    .MPTheme() +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 1),
                    shape = ggplot2::guide_legend(order = 2), fill = ggplot2::guide_legend(order = 2))
}

.CatchLabel <- function(Type, Units) {
  Units <- Units[!is.na(Units)]
  if (length(Units)) paste0(Type, ' (', Units[1], ')') else Type
}

.HistBoundary <- function(p, Years, YearLH) {
  if (max(Years) > YearLH)
    p <- p + ggplot2::geom_vline(xintercept = YearLH + 0.5, linetype = 'dotted', colour = 'grey40')
  p
}


#' Figures of an `IndexRate()`/`IndexTarget()` Calculation
#'
#' @param Calc List returned by `.IndexMPCalc()`.
#' @param MPName Name used in the console heading.
#' @param Data The [data-class] object passed to the MP.
#' @return A named list of `ggplot` objects (`Index`, `HCR`, `Catch`), with
#'   the calculation steps (character vector) in its `Steps` attribute and a
#'   one-line summary in its `Heading` attribute.
#' @keywords internal
.PlotIndexMP <- function(Calc, MPName, Data) {
  Years      <- Calc$Years
  AdviceYear <- max(Years) + 1
  nIndex     <- length(Calc$Name)
  Rate       <- Calc$Type == 'Rate'
  CalibYrs   <- range(Years[Calc$CalibRows])
  StatusCols <- seq(max(1, length(Years) - Calc$RecentYears + 1), length(Years))
  TrendCols  <- if (is.null(Calc$TrendYears)) NULL else
    seq(max(1, length(Years) - Calc$TrendYears + 1), length(Years))
  FirstYear  <- min(max(Years) - .MPPlotYears + 1, Years[c(Calc$CalibRows, StatusCols, TrendCols)])
  Shown      <- Years >= FirstYear

  Name <- factor(Calc$Name, levels = Calc$Name)
  Long <- function(M, Series)
    data.frame(Name = rep(Name, each = length(Years)), Year = rep(Years, nIndex),
               Value = as.vector(t(M)), Series = Series)
  Obs <- Long(Calc$IndexHist, 'Observed')
  Obs <- Obs[is.finite(Obs$Value) & Obs$Year >= FirstYear, ]
  Smo <- if (Calc$Smooth) Long(Calc$IndexSmooth, 'Smoothed') else NULL
  Smo <- Smo[is.finite(Smo$Value) & Smo$Year >= FirstYear, ]

  Target <- data.frame(Name = Name, Value = Calc$Ref, Series = 'IndexTarget')
  CP     <- Calc$HCRControlPointsIndex[Calc$HCRControlPointsIndex > 0]
  CPLine <- data.frame(Name = rep(Name, each = length(CP)),
                       Value = as.vector(outer(CP, Calc$Ref)),
                       Series = 'HCR control points')
  Status <- data.frame(Name = Name, From = Years[min(StatusCols)] - 0.5,
                       To = Years[max(StatusCols)] + 0.5, Year = max(Years),
                       Value = Calc$Status, Trend = Calc$StatusTrend)

  p1 <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year))
  if (Rate)
    p1 <- p1 + ggplot2::annotate('rect', xmin = CalibYrs[1] - 0.5, xmax = CalibYrs[2] + 0.5,
                                 ymin = -Inf, ymax = Inf, fill = 'grey85', alpha = 0.6)
  p1 <- p1 +
    ggplot2::geom_hline(data = Target, ggplot2::aes(yintercept = .data$Value, linetype = .data$Series),
                        colour = .MPColours[['Target']]) +
    ggplot2::geom_hline(data = CPLine, ggplot2::aes(yintercept = .data$Value, linetype = .data$Series),
                        colour = .MPColours[['Target']], alpha = 0.7) +
    ggplot2::geom_line(data = Obs, ggplot2::aes(y = .data$Value, colour = .data$Series), alpha = 0.5) +
    ggplot2::geom_point(data = Obs, ggplot2::aes(y = .data$Value, colour = .data$Series), size = 1.6)
  if (!is.null(Smo))
    p1 <- p1 + ggplot2::geom_line(data = Smo, ggplot2::aes(y = .data$Value, colour = .data$Series),
                                  linewidth = 1.2)
  p1 <- p1 +
    ggplot2::geom_segment(data = Status, ggplot2::aes(x = .data$From, xend = .data$To,
                                                      y = .data$Value, yend = .data$Value),
                          colour = .MPColours[['Status']], linewidth = 1.4) +
    ggplot2::geom_point(data = Status, ggplot2::aes(y = .data$Value), colour = .MPColours[['Status']],
                        size = 3)
  if (!is.null(Calc$TrendYears))
    p1 <- p1 + ggplot2::geom_segment(data = Status,
                                     ggplot2::aes(x = .data$Year, xend = .data$Year + Calc$TrendHorizon,
                                                  y = .data$Value, yend = .data$Trend),
                                     colour = .MPColours[['Status']], linewidth = 0.8,
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.18, 'cm')))
  Units <- unique(Calc$Units[!is.na(Calc$Units)])
  Caption <- paste0(if (any(!Shown)) sprintf('Last %d years shown.\n', sum(Shown)),
                    if (Rate) 'Grey shading: CalibYears. ', 'Orange bar: RecentYears status window.')
  p1 <- .HistBoundary(p1, Years, Calc$YearLH) +
    ggplot2::scale_colour_manual(values = .MPColours[c('Observed', 'Smoothed')]) +
    ggplot2::scale_linetype_manual(values = c('IndexTarget' = 'solid', 'HCR control points' = 'dashed')) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale(n = if (nIndex > 1) 3 else 6) + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = if (length(Units) == 1) paste0('Index (', Units, ')') else 'Index',
                  title = 'Index status', caption = Caption) +
    .MPTheme()
  if (nIndex > 1)
    p1 <- p1 + ggplot2::facet_wrap(~Name, scales = 'free_y', ncol = min(nIndex, 2))

  Level <- Calc$Est / Calc$RefLevel
  if (Rate) {
    p2 <- .PlotHCRPanel(Calc$HCRControlPointsIndex, Calc$HCRControlPointsRate, Calc$RampType,
                        Level, Calc$HCRMult, xlab = 'Index / IndexTarget',
                        ylab = 'Multiplier of catch-per-index rate')
  } else {
    p2 <- .PlotHCRPanel(Calc$HCRControlPointsIndex, Calc$HCRControlPointsRate, Calc$RampType,
                        Level, Calc$HCRMult, xlab = 'Index / IndexTarget',
                        ylab = 'TAC / reference catch')
  }

  CatchDF <- data.frame(Year = Years, Value = Calc$Catch, Series = paste('Catch:', Calc$TACType))
  CatchDF <- CatchDF[Shown, ]
  CatchDF$Series <- factor(CatchDF$Series)
  RefDF <- if (Rate) {
    data.frame(From = CalibYrs[1], To = CalibYrs[2], Value = Calc$CalibCatch, Label = 'Mean')
  } else {
    data.frame(From = CalibYrs[1], To = CalibYrs[2], Value = Calc$RefCatch, Label = 'Reference')
  }
  p3 <- .PlotTACPanel(CatchDF, AdviceYear, Calc$PrevTAC, Calc$TrialTAC, Calc$TAC,
                      ylab = .CatchLabel(Calc$TACType, Calc$Data@Landings@Units),
                      Shade = CalibYrs, Ref = RefDF) +
    ggplot2::scale_colour_manual(values = 'grey20') +
    ggplot2::scale_linetype_manual(values = 'solid') +
    ggplot2::labs(caption = 'Grey shading: CalibYears.')
  p3 <- .HistBoundary(p3, Years, Calc$YearLH)

  Fmt      <- .MPFormat
  Catch    <- tolower(Calc$TACType)
  Weighted <- if (nIndex > 1) ' (weighted mean over indices)' else ''
  Steps <- character()
  if (Rate) {
    Mult <- c(if (Calc$IndexFactor != 1) sprintf(' x IndexFactor %s', Calc$IndexFactor),
              if (Calc$tunepar != 1) sprintf(' x tunepar %s', Fmt(Calc$tunepar)))
    Steps <- c(Steps, sprintf('Catch-per-index rate = mean %s %s (%s) / mean index (%s)%s = %s.',
                              Catch, .MPYears(CalibYrs), Fmt(Calc$CalibCatch),
                              .MPByIndex(Calc$CalibIndex, Calc$Name), paste(Mult, collapse = ''),
                              .MPByIndex(Calc$CatchPerIndex * Calc$IndexFactor * Calc$tunepar, Calc$Name)))
  } else {
    Steps <- c(Steps, sprintf('Reference catch = mean %s %s = %s.', Catch, .MPYears(CalibYrs),
                              if (Calc$tunepar != 1) sprintf('%s x tunepar %s = %s', Fmt(Calc$CalibCatch),
                                                             Fmt(Calc$tunepar), Fmt(Calc$RefCatch))
                              else Fmt(Calc$RefCatch)))
  }
  Steps <- c(Steps, sprintf('Status = %s index averaged over %s = %s.',
                            if (Calc$Smooth) 'smoothed' else 'observed',
                            .MPYears(Years[StatusCols]), .MPByIndex(Calc$Status, Calc$Name)))
  if (!is.null(Calc$TrendYears))
    Steps <- c(Steps, sprintf('Status projected %s year(s) along the log-linear trend of the last %d years = %s.',
                              Calc$TrendHorizon, Calc$TrendYears, .MPByIndex(Calc$StatusTrend, Calc$Name)))
  Steps <- c(Steps,
             sprintf('IndexTarget = %s.', .MPByIndex(Calc$Ref, Calc$Name)),
             sprintf('Status / IndexTarget%s = %s: HCR multiplier = %s.', Weighted, Fmt(Level),
                     Fmt(Calc$HCRMult)),
             if (Rate) {
               sprintf('Trial TAC = status x rate x multiplier%s = %s.', Weighted, Fmt(Calc$TrialTAC))
             } else {
               sprintf('Trial TAC = reference catch x multiplier = %s.', Fmt(Calc$TrialTAC))
             },
             sprintf('Previous TAC = %s. %s TAC = %s.', Fmt(Calc$PrevTAC),
                     .TACConstraintNote(Calc$PrevTAC, Calc$TrialTAC, Calc$Mod, Calc$TAC, Calc$DeltaDown,
                                        Calc$DeltaUp, Calc$TACRange, Calc$Responsiveness,
                                        Calc$TrialKept),
                     Fmt(Calc$TAC)))

  if (max(1L, as.integer(Data@Seasons %||NA% 1)) > 1)
    Steps <- c('Seasonal data aggregated to calendar years with AnnualData().', Steps)

  structure(list(Index = p1, HCR = p2, Catch = p3), Steps = Steps,
            Heading = sprintf('%s: TAC for %s = %s', MPName, AdviceYear, .MPFormat(Calc$TAC)))
}


#' Figures of a `SurplusProduction()` Calculation
#'
#' @param Advice The [advice-class] object returned by the MP.
#' @param Env The evaluation environment of the MP call.
#' @param Data The [data-class] object passed to the MP.
#' @return A named list of `ggplot` objects (`Index`, `Biomass`, `HCR`,
#'   `Catch`; `Index` and `Catch` when the advice is set by `OnFail`), with
#'   the calculation steps (character vector) in its `Steps` attribute and a
#'   one-line summary in its `Heading` attribute.
#' @keywords internal
.PlotSPMP <- function(Advice, Env, Data) {
  V <- function(nm) get0(nm, envir = Env, inherits = FALSE)
  Prep    <- V('Prep')
  Fit     <- V('Fit')
  Summary <- Advice@Misc$SurplusProduction$Summary
  if (!is.list(Prep))
    cli::cli_abort("The data could not be prepared: {Summary$Message}")
  HasFit <- inherits(Fit, 'spfit') && isTRUE(Fit$Converged) && isFALSE(Summary$Fallback)

  AdviceYear <- V('AdviceYear')
  Interval   <- V('Interval')
  CatchType  <- V('CatchType')
  TACType    <- V('TACType')
  AdviceType <- V('AdviceType')
  Units      <- Prep$CatchUnits

  # index panel
  nIndex <- ncol(Prep$Index)
  Name   <- factor(Prep$Name, levels = Prep$Name)
  Obs <- data.frame(Name = rep(Name, each = nrow(Prep$Index)), Year = rep(Prep$Years, nIndex),
                    Value = as.vector(Prep$Index), Series = 'Observed')
  Obs <- Obs[is.finite(Obs$Value) & Obs$Value > 0, ]
  p1 <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$Year)) +
    ggplot2::geom_point(data = Obs, ggplot2::aes(y = .data$Value, colour = .data$Series), size = 1.6)
  if (HasFit && !is.null(Fit$IndexPred)) {
    Pred <- Fit$IndexPred
    PredDF <- data.frame(Name = rep(Name, each = nrow(Pred)), Year = rep(Prep$Years, nIndex),
                         Value = as.vector(Pred), Series = 'Fitted')
    p1 <- p1 + ggplot2::geom_line(data = PredDF, ggplot2::aes(y = .data$Value, colour = .data$Series),
                                  linewidth = 1.2)
  } else if (identical(V('OnFail'), 'trend')) {
    FB     <- V('FallbackYears')
    nYear  <- length(Prep$Years)
    Recent <- seq(max(1, nYear - FB[1] + 1), nYear)
    Before <- seq(max(1, min(Recent) - FB[2]), max(1, min(Recent) - 1))
    p1 <- p1 +
      ggplot2::annotate('rect', xmin = min(Prep$Years[Before]) - 0.5, xmax = max(Prep$Years[Before]) + 0.5,
                        ymin = -Inf, ymax = Inf, fill = 'grey85', alpha = 0.6) +
      ggplot2::annotate('rect', xmin = min(Prep$Years[Recent]) - 0.5, xmax = max(Prep$Years[Recent]) + 0.5,
                        ymin = -Inf, ymax = Inf, fill = .MPColours[['Status']], alpha = 0.25)
  }
  if (!HasFit && identical(V('OnFail'), 'trend'))
    p1 <- p1 + ggplot2::labs(caption = 'Orange shading: recent years; grey shading: preceding years (FallbackYears).')
  p1 <- p1 +
    ggplot2::scale_colour_manual(values = .MPColours[c('Observed', 'Fitted')]) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale(n = if (nIndex > 1) 3 else 6) + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = 'Index', title = if (HasFit) 'Index fit' else 'Indices') +
    .MPTheme()
  if (nIndex > 1)
    p1 <- p1 + ggplot2::facet_wrap(~Name, scales = 'free_y', ncol = min(nIndex, 2))

  # catch panel
  LastYear <- max(Prep$Years)
  CatchDF <- data.frame(Year = Prep$AllYears, Value = Prep$CatchAll,
                        Series = paste('Observed', tolower(CatchType)))
  CatchDF <- CatchDF[is.finite(CatchDF$Value), ]
  Lines <- NULL
  if (HasFit) {
    Gap  <- V('Gap')
    Proj <- V('Proj')
    Lines <- rbind(
      if (all(is.finite(Fit$CatchPred)))
        data.frame(Year = Fit$Years, Value = unname(Fit$CatchPred), Series = 'Fitted'),
      if (length(Gap$Catch))
        data.frame(Year = LastYear + seq_along(Gap$Catch), Value = Gap$Catch,
                   Series = 'Gap years'),
      data.frame(Year = AdviceYear + seq_along(Proj$Catch) - 1, Value = Proj$Catch,
                 Series = 'Projected at advice F')
    )
    Lines <- Lines[is.finite(Lines$Value), ]
    Lines <- Lines[Lines$Series %in% names(which(table(Lines$Series) > 1)), ]
    if (!nrow(Lines)) Lines <- NULL
  }
  Series <- unique(c(CatchDF$Series, Lines$Series))
  ColMap <- stats::setNames(c('grey20', .MPColours[['Fitted']], .MPColours[['Gap']],
                              .MPColours[['Projected']])[seq_along(Series)], Series)
  ColMap[Series == 'Fitted'] <- .MPColours[['Fitted']]
  ColMap[Series == 'Gap years'] <- .MPColours[['Gap']]
  ColMap[Series == 'Projected at advice F'] <- .MPColours[['Projected']]
  LtyMap <- stats::setNames(ifelse(Series %in% c('Gap years', 'Projected at advice F'), 'dashed', 'solid'),
                            Series)
  CatchDF$Series <- factor(CatchDF$Series, levels = Series)
  if (!is.null(Lines)) Lines$Series <- factor(Lines$Series, levels = Series)

  PrevTAC <- V('PrevTAC')
  if (AdviceType == 'TAC') {
    ToModel  <- V('ToModel') %||% 1
    TrialTAC <- if (HasFit) V('ModelTAC') / ToModel else NA_real_
    TAC      <- Summary$Advice
    p4 <- .PlotTACPanel(CatchDF, AdviceYear, PrevTAC, TrialTAC, TAC,
                        ylab = .CatchLabel(if (TACType == CatchType) CatchType else
                          paste0(CatchType, ' (TAC: ', TACType, ')'), Units),
                        Lines = Lines)
  } else {
    p4 <- .PlotTACPanel(CatchDF, AdviceYear, NA, NA, NA,
                        ylab = .CatchLabel(CatchType, Units), Lines = Lines)
  }
  p4 <- p4 + ggplot2::scale_colour_manual(values = ColMap) +
    ggplot2::scale_linetype_manual(values = LtyMap)

  Heading <- if (AdviceType == 'TAC') {
    sprintf('SurplusProduction: TAC for %s = %s', AdviceYear, .MPFormat(Summary$Advice))
  } else {
    sprintf('SurplusProduction: relative effort for %s = %s', AdviceYear, .MPFormat(Summary$Advice))
  }

  Seasonal <- if (max(1L, as.integer(Data@Seasons %||NA% 1)) > 1)
    'Seasonal data aggregated to calendar years with AnnualData().'
  Fmt <- .MPFormat
  Prev <- if (AdviceType == 'TAC') sprintf('Previous TAC = %s.', Fmt(PrevTAC)) else
    sprintf('Previous relative effort = %s.', Fmt(V('PrevEffort')))
  Final <- if (AdviceType == 'TAC') sprintf('TAC = %s.', Fmt(Summary$Advice)) else
    sprintf('Relative effort = %s.', Fmt(Summary$Advice))

  if (!HasFit) {
    Rule <- if (identical(V('OnFail'), 'trend')) {
      FB <- V('FallbackYears')
      sprintf(paste0('Advice set by OnFail = "trend": previous advice x (mean index of the last %d ',
                     'years / mean of the %d years before)^Responsiveness = x %s.'),
              FB[1], FB[2], Fmt(exp(log(.SPIndexTrend(Prep, FB)) * V('Responsiveness'))))
    } else {
      'Advice set by OnFail = "hold": previous advice kept.'
    }
    Steps <- c(Seasonal, sprintf('Model fit not used: %s', sub('([^.])$', '\\1.', Summary$Message)),
               Rule, paste(Prev, Final))
    return(structure(list(Index = p1, Catch = p4), Steps = Steps, Heading = Heading))
  }

  # biomass panel
  Gap     <- V('Gap')
  Proj    <- V('Proj')
  BMSY    <- Fit$BMSY
  EstB    <- V('EstB')
  BAdvice <- V('BAdvice')
  CPB     <- V('HCRControlPointsBiomass')
  BDF <- rbind(
    data.frame(Year = as.numeric(names(Fit$B)), Value = unname(Fit$B_BMSY), Series = 'Fitted'),
    if (length(Gap$B) > 1)
      data.frame(Year = LastYear + seq_along(Gap$B), Value = Gap$B / BMSY, Series = 'Gap years'),
    data.frame(Year = AdviceYear + seq_along(Proj$B) - 1, Value = Proj$B / BMSY,
               Series = 'Projected at advice F')
  )
  BDF$Series <- factor(BDF$Series, levels = c('Fitted', 'Gap years', 'Projected at advice F'))
  Point <- data.frame(Year = AdviceYear, Value = c(BAdvice / BMSY, EstB),
                      What = c('B/BMSY at advice year', 'B/BMSY used by HCR'))
  Point <- Point[!duplicated(round(Point$Value, 10)), ]
  p2 <- ggplot2::ggplot(BDF, ggplot2::aes(x = .data$Year, y = .data$Value)) +
    ggplot2::geom_hline(yintercept = 1, colour = 'grey50') +
    ggplot2::geom_hline(yintercept = CPB[CPB > 0], colour = .MPColours[['Target']], linetype = 'dashed',
                        alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(colour = .data$Series, linetype = .data$Series), linewidth = 1.2) +
    ggplot2::geom_point(data = Point, ggplot2::aes(shape = .data$What), colour = .MPColours[['Status']],
                        size = 3) +
    ggplot2::scale_colour_manual(values = c('Fitted' = .MPColours[['Fitted']], 'Gap years' = .MPColours[['Gap']],
                                            'Projected at advice F' = .MPColours[['Projected']])) +
    ggplot2::scale_linetype_manual(values = c('Fitted' = 'solid', 'Gap years' = 'dashed',
                                              'Projected at advice F' = 'dashed')) +
    ggplot2::scale_shape_manual(values = c('B/BMSY at advice year' = 16, 'B/BMSY used by HCR' = 1)) +
    ggplot2::expand_limits(y = 0) +
    .MPYearScale() + .MPYScale0() +
    ggplot2::labs(x = 'Year', y = 'B / BMSY', title = 'Estimated biomass',
                  caption = 'Dashed red lines: HCR control points.') +
    .MPTheme()

  # HCR panel, F/FMSY against B/BMSY
  FTargetMult <- V('tunepar') * V('FTarget') * V('FMSYHCR') / Fit$FMSY
  nFit <- length(Fit$Years)
  Path <- data.frame(x = unname(Fit$B_BMSY[seq_len(nFit)]), y = unname(Fit$F_FMSY), Year = Fit$Years)
  p3 <- .PlotHCRPanel(CPB, V('HCRControlPointsRate'), V('RampType'), EstB,
                      V('FAdvice') / Fit$FMSY, Scale = FTargetMult, xlab = 'B / BMSY',
                      ylab = 'F / FMSY', Path = Path)

  nGap    <- V('nGap') %||% 0
  FAdvice <- V('FAdvice')
  HCRMult <- FAdvice / (V('tunepar') * V('FTarget') * V('FMSYHCR'))
  Steps <- c(
    Seasonal,
    sprintf('%s surplus production model fitted to %s: MSY = %s, FMSY = %s, BMSY = %s, K = %s.',
            if (Fit$Model == 'spict') 'spict' else 'Internal', .MPYears(Fit$Years), Fmt(Fit$MSY),
            Fmt(Fit$FMSY), Fmt(BMSY), Fmt(Fit$K)),
    sprintf('B/BMSY at the start of %s = %s%s.', AdviceYear, Fmt(BAdvice / BMSY),
            if (nGap > 0) sprintf(', after projecting %d year(s) at catch %s (GapCatch = "%s")',
                                  nGap, Fmt(Gap$Catch[1]), V('GapCatch')) else ''),
    if (!is.null(V('FractileB')))
      sprintf('B/BMSY used by the HCR = %s quantile (FractileB) = %s.', V('FractileB'), Fmt(EstB)),
    if (!is.null(V('FractileF')))
      sprintf('FMSY used by the HCR = %s quantile (FractileF) = %s.', V('FractileF'), Fmt(V('FMSYHCR'))),
    sprintf('F = HCR multiplier %s x tunepar %s x FTarget %s x FMSY %s = %s (%s x estimated FMSY).',
            Fmt(HCRMult), Fmt(V('tunepar')), Fmt(V('FTarget')), Fmt(V('FMSYHCR')), Fmt(FAdvice),
            Fmt(FAdvice / Fit$FMSY)),
    sprintf('Projected %d year(s) at F from %s: %s catch = %s.', Interval, AdviceYear,
            if (V('TACYears') == 'mean' && Interval > 1) 'mean' else 'first-year',
            Fmt(V('ModelTAC')))
  )
  if (AdviceType == 'TAC') {
    ToModel <- V('ToModel') %||% 1
    Trial   <- V('ModelTAC') / ToModel
    Mod     <- exp(log(Trial / PrevTAC) * V('Responsiveness'))
    Steps <- c(Steps,
               if (ToModel != 1) sprintf('Trial TAC (%s) = %s catch / %s = %s.', tolower(TACType),
                                         tolower(CatchType), Fmt(ToModel), Fmt(Trial)) else
                 sprintf('Trial TAC = %s.', Fmt(Trial)),
               paste(Prev, .TACConstraintNote(PrevTAC, Trial, Mod, Summary$Advice, V('DeltaDown'),
                                              V('DeltaUp'), V('TACRange') %||% c(0, Inf),
                                              V('Responsiveness')), Final))
  } else {
    Steps <- c(Steps, sprintf('Trial relative effort = F / estimated F in %s (%s) = %s.', Prep$YearLH,
                              Fmt(V('FLH')), Fmt(FAdvice / V('FLH'))),
               paste(Prev, 'Change constrained by Responsiveness and DeltaDown/DeltaUp.', Final))
  }
  structure(list(Index = p1, Biomass = p2, HCR = p3, Catch = p4), Steps = Steps, Heading = Heading)
}
