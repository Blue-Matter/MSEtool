#' Plot Initial/Final Depletion
#'
#' Histogram of each stock's sampled depletion values (relative to
#' `Depletion@Reference`, e.g. `"B0"` or `"SB0"`) across simulation
#' replicates; the `Initial`/`Final` slots of `[Depletion()]`. Both are
#' shown, side by side, when a stock has both populated.
#'
#' For a [hist-class] or [mse-class] `object`, a second panel is added
#' below: the actual historical relative-biomass trajectory (`[B_B0()]` or
#' `[SB_SB0()]`, matching `Depletion@Reference`) for every simulation, so
#' the sampled targets above can be checked against what was actually
#' achieved. Where `Depletion@Initial`/`Final` are specified, the target
#' value for each simulation is overlaid as a point on its own trajectory
#' at the first/last historical year; a simulation whose achieved value
#' misses its target by more than `tol` (relative) is colored red rather
#' than grey, and listed in a warning (via [cli::cli_warn()]). This can
#' legitimately happen -- e.g. a low target depletion combined with
#' selectivity maturing well before maturity -- and [Simulate()] performs
#' the same check itself, recording any misses to `Hist@Log` (see [Log()])
#' rather than printing them.
#'
#' @param object   [stock-class], [om-class], [hist-class], or [mse-class] object.
#' @param byStock Logical. Facet by stock? Default `NULL` facets
#'   automatically when `object` has more than one stock.
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param bins Integer. Number of histogram bins. Default `15`.
#' @param tol Numeric. Relative tolerance for flagging a simulation as
#'   having missed its `Depletion@Initial`/`Final` target -- a simulation is
#'   flagged when `abs(log(achieved / target)) > log(1 + tol)`. Default
#'   `0.1` (10%). Ignored for [stock-class]/[om-class] `object` (nothing yet
#'   simulated to check against).
#'
#' @return A `ggplot` object, or (for [hist-class]/[mse-class] `object`) a
#'   `patchwork` object combining the histogram and trajectory panels.
#'
#' @seealso [Depletion()], [B_B0()], [SB_SB0()], [PlotSRR()], [Stock()]
#' @export
PlotDepletion <- function(object, byStock = NULL, Stocks = NULL, bins = 15, tol = 0.1) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  allStocks  <- StockNames(OM)

  df <- purrr::map(seq_along(allStocks), \(st) {
    if (!allStocks[st] %in% stockNames) return(NULL)
    Dep <- OM@Stock[[st]]@Depletion
    purrr::map(c('Initial', 'Final'), \(what) {
      val <- slot(Dep, what)
      if (!length(val)) return(NULL)
      data.frame(Stock = allStocks[st], Type = what, Value = as.numeric(val))
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  if (!nrow(df))
    cli::cli_abort("No populated {.field Initial}/{.field Final} depletion values found.")

  refs <- purrr::map_chr(seq_along(allStocks), \(st) OM@Stock[[st]]@Depletion@Reference)
  names(refs) <- allStocks
  xlab <- if (length(unique(refs[stockNames])) == 1)
    paste0('Depletion (', unique(refs[stockNames]), ')') else 'Depletion'

  facetVars <- character(0)
  if (!isFALSE(byStock) && length(unique(df$Stock)) > 1)
    facetVars <- c(facetVars, 'Stock')

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Value, fill = .data$Type)) +
    ggplot2::geom_histogram(bins = bins, position = 'identity', alpha = 0.5, color = NA) +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(x = c(0,1)) +
    ggplot2::labs(x = xlab, y = 'Count', fill = NULL)

  if (length(unique(df$Type)) == 1)
    p <- p + ggplot2::guides(fill = 'none')

  if (length(facetVars))
    p <- p + ggplot2::facet_wrap(facetVars, scales = 'free')

  if (!inherits(object, c('hist', 'mse'))) return(p)

  trendP <- .PlotDepletionTrend(object, stockNames, refs, byStock, tol)
  if (is.null(trendP)) return(p)

  patchwork::wrap_plots(list(Target = p, Trend = trendP), ncol = 1)
}

.PlotDepletionTrend <- function(object, stockNames, refs, byStock, tol) {
  haveB0  <- !is.null(object@Unfished@Equilibrium@Biomass)
  haveSB0 <- !is.null(object@Unfished@Equilibrium@SBiomass)
  if (!haveB0 && !haveSB0) return(NULL)

  b0df  <- if (haveB0)  B_B0(object,  type = 'Equilibrium', silent = TRUE) else NULL
  sb0df <- if (haveSB0) SB_SB0(object, type = 'Equilibrium', silent = TRUE) else NULL

  trendDF <- purrr::map(stockNames, \(nm) {
    useB0 <- identical(refs[[nm]], 'B0')
    src <- if (useB0) b0df else sb0df
    if (is.null(src)) return(NULL)
    dplyr::filter(src, .data$Stock == nm, .data$Period == 'Historical') |>
      dplyr::transmute(Sim = .data$Sim, Stock = as.character(.data$Stock),
                       Year = .data$Year, Value = .data$Value)
  }) |> dplyr::bind_rows()

  if (!nrow(trendDF)) return(NULL)

  targetDF <- .ComputeDepletionAchievement(object, stockNames, refs, tol, trendDF)

  if (nrow(targetDF)) {
    failed <- dplyr::filter(targetDF, !.data$Pass)
    if (nrow(failed)) {
      msg <- failed |>
        dplyr::group_by(.data$Stock, .data$Type) |>
        dplyr::summarise(Sims = paste(sort(.data$Sim), collapse = ', '), .groups = 'drop') |>
        dplyr::mutate(line = paste0(.data$Stock, ' ', .data$Type, ': Sim ', .data$Sims)) |>
        dplyr::pull('line')
      cli::cli_warn(c(
        "!" = "{length(unique(failed$Sim))} simulation{?s} missed {?its/their} target depletion by more than {tol*100}%:",
        stats::setNames(msg, rep('*', length(msg)))
      ))
    }
  }

  ylab <- if (length(unique(refs[stockNames])) == 1)
    paste0('Depletion (', unique(refs[stockNames]), ')') else 'Depletion'

  p <- ggplot2::ggplot(trendDF, ggplot2::aes(x = .data$Year, y = .data$Value, group = .data$Sim)) +
    ggplot2::geom_line(alpha = 0.3, color = 'grey30') +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab)

  if (nrow(targetDF))
    p <- p + ggplot2::geom_point(
      data = targetDF,
      ggplot2::aes(x = .data$Year, y = .data$Target, color = .data$Pass, shape = .data$Type),
      inherit.aes = FALSE, size = 1.8
    ) +
    ggplot2::scale_color_manual(values = c('TRUE' = 'grey20', 'FALSE' = 'red'), guide = 'none') +
    ggplot2::labs(shape = NULL)

  if (!isFALSE(byStock) && length(unique(trendDF$Stock)) > 1)
    p <- p + ggplot2::facet_wrap('Stock', scales = 'free')

  p
}


.ComputeDepletionAchievement <- function(object, stockNames, refs, tol, trendDF = NULL) {
  if (is.null(trendDF)) {
    haveB0  <- !is.null(object@Unfished@Equilibrium@Biomass)
    haveSB0 <- !is.null(object@Unfished@Equilibrium@SBiomass)
    if (!haveB0 && !haveSB0) return(data.frame())

    b0df  <- if (haveB0)  B_B0(object,  type = 'Equilibrium', silent = TRUE) else NULL
    sb0df <- if (haveSB0) SB_SB0(object, type = 'Equilibrium', silent = TRUE) else NULL

    trendDF <- purrr::map(stockNames, \(nm) {
      useB0 <- identical(refs[[nm]], 'B0')
      src <- if (useB0) b0df else sb0df
      if (is.null(src)) return(NULL)
      dplyr::filter(src, .data$Stock == nm, .data$Period == 'Historical') |>
        dplyr::transmute(Sim = .data$Sim, Stock = as.character(.data$Stock),
                         Year = .data$Year, Value = .data$Value)
    }) |> dplyr::bind_rows()
    if (!nrow(trendDF)) return(data.frame())
  }

  OM <- object@OM
  purrr::map(stockNames, \(nm) {
    Dep <- OM@Stock[[nm]]@Depletion
    yrs <- sort(unique(trendDF$Year[trendDF$Stock == nm]))
    purrr::map(c('Initial', 'Final'), \(what) {
      target <- slot(Dep, what)
      if (!length(target)) return(NULL)
      yr    <- if (what == 'Initial') min(yrs) else max(yrs)
      achieved <- trendDF |>
        dplyr::filter(.data$Stock == nm, .data$Year == yr) |>
        dplyr::arrange(.data$Sim)
      target <- rep(target, length.out = nrow(achieved))
      data.frame(
        Stock    = nm, Type = what, Sim = achieved$Sim, Year = yr,
        Target   = target, Achieved = achieved$Value,
        Pass     = abs(log(achieved$Value / target)) <= log(1 + tol)
      )
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()
}

.LogDepletionAchievement <- function(Hist, tol = 0.1) {
  stockNames <- StockNames(Hist@OM)
  refs <- purrr::map_chr(stockNames, \(nm) Hist@OM@Stock[[nm]]@Depletion@Reference)
  names(refs) <- stockNames

  achieved <- .ComputeDepletionAchievement(Hist, stockNames, refs, tol)
  failed   <- dplyr::filter(achieved, !.data$Pass)
  if (!nrow(failed)) return(Hist)

  for (i in seq_len(nrow(failed))) {
    row <- failed[i, ]
    msg <- cli::format_inline(
      "Stock {.val {row$Stock}} did not reach its target {row$Type} depletion: achieved {.val {round(row$Achieved, 3)}}, target {.val {round(row$Target, 3)}}."
    )
    Hist <- .CaptureLog(Hist, msg, name = 'Simulate', type = 'warning', sim = row$Sim)
  }
  Hist
}
