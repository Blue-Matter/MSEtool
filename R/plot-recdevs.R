#' Plot Recruitment Deviations
#'
#' Plots a stock's recruitment deviations
#' (`RecDevInit`/`RecDevHist`/`RecDevProj`; see [srr-class])
#'
#' `PlotRecDevs()` plots the full log-recruitment-deviation timeseries as one
#' continuous series per stock. When `object` has more than one simulation, the
#' median is drawn with a `probs` quantile ribbon. A dashed vertical line marks
#' the historical/projection boundary (the first projection year); a dotted
#' vertical line marks the initial-age-structure/historical boundary, when
#' initial-age deviations are present.
#'
#' @param object A [stock-class]/[om-class]/[hist-class]/[mse-class] object.
#' @param Sim Integer or `NULL` (default). Which simulation replicate to
#'   plot. `NULL` takes the median across all simulations and adds a `probs`
#'   quantile ribbon (unless `nSim == 1`).
#' @param probs Numeric vector of length 2. Lower and upper quantiles of the
#'   across-simulation ribbon drawn when `Sim = NULL`. Default `c(0.05, 0.95)`.
#' @param byStock One of `TRUE`, `FALSE`, or `NULL` (default, facets
#'   automatically when `object` has more than one stock).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#'
#' @return A `ggplot` object.
#' @export
#'
#' @seealso [PlotSRR()], [PlotSRRCurve()], [GenRecDevs()], [srr-class]
PlotRecDevs <- function(object, Sim = NULL, byStock = NULL, Stocks = NULL, probs = c(0.05, 0.95)) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  allStocks  <- StockNames(OM)

  df <- purrr::map(seq_along(allStocks), \(st) {
    if (!allStocks[st] %in% stockNames) return(NULL)
    .RecDevSeriesDF(OM@Stock[[st]]) |> dplyr::mutate(Stock = allStocks[st])
  }) |> dplyr::bind_rows()

  if (!nrow(df)) {
    cli::cli_alert_info("No populated recruitment deviations found.")
    return(invisible(NULL))
  }

  hasSim <- 'Sim' %in% colnames(df)
  if (hasSim && !is.null(Sim)) {
    if (!Sim %in% unique(df$Sim))
      cli::cli_abort("`Sim = {Sim}` not found; {length(unique(df$Sim))} simulation{?s} available.")
    df     <- dplyr::filter(df, .data$Sim == Sim)
    hasSim <- FALSE
  }

  nSim <- if (hasSim) length(unique(df$Sim)) else 1L
  summ <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c('Stock', 'Period', 'Year')))) |>
    dplyr::summarise(
      Lower = stats::quantile(.data$Value, min(probs), na.rm = TRUE),
      Upper = stats::quantile(.data$Value, max(probs), na.rm = TRUE),
      Value = stats::median(.data$Value, na.rm = TRUE),
      .groups = 'drop'
    )
  showRibbon <- hasSim && nSim > 1 && any(round(summ$Upper - summ$Lower, 4) > 0)

  summ$Period <- factor(summ$Period, levels = c('Init', 'Hist', 'Proj'))

  boundaries <- df |>
    dplyr::group_by(.data$Stock) |>
    dplyr::summarise(
      HistProj = suppressWarnings(min(.data$Year[.data$Period == 'Proj'], na.rm = TRUE)),
      InitHist = suppressWarnings(min(.data$Year[.data$Period == 'Hist'], na.rm = TRUE)),
      .groups = 'drop'
    )

  p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$Year, y = .data$Value, color = .data$Period, fill = .data$Period))

  if (showRibbon)
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper), alpha = 0.2, color = NA)

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey50') +
    ggplot2::geom_vline(data = boundaries, ggplot2::aes(xintercept = .data$HistProj), linetype = 'dashed', color = 'grey30', inherit.aes = FALSE) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = 'log Recruitment Deviation', color = NULL, fill = NULL)

  if (any(is.finite(boundaries$InitHist)))
    p <- p + ggplot2::geom_vline(data = boundaries, ggplot2::aes(xintercept = .data$InitHist), linetype = 'dotted', color = 'grey30', inherit.aes = FALSE)

  if (!isFALSE(byStock) && length(unique(summ$Stock)) > 1)
    p <- p + ggplot2::facet_wrap('Stock', scales = 'free_x')

  p
}

# Long-format Year x Sim data frame of log-recruitment-deviations, chained
# across Init/Hist/Proj into one chronological series. Init columns are
# ascending age (nearest-to-Hist first); their Year is back-calculated as
# `HistYears[1] - Age`, matching the chronology documented in GenRecDevs().
.RecDevSeriesDF <- function(Stock) {
  SRR        <- Stock@SRR
  histYears  <- as.numeric(dimnames(SRR@RecDevHist)[[2]])
  projYears  <- as.numeric(dimnames(SRR@RecDevProj)[[2]])

  parts <- list()
  if (!is.null(SRR@RecDevInit) && length(dim(SRR@RecDevInit)) == 2 && ncol(SRR@RecDevInit) > 0) {
    ages <- as.numeric(dimnames(SRR@RecDevInit)[[2]])
    parts$Init <- .RecDevArrToLongDF(SRR@RecDevInit, histYears[1] - ages, 'Init')
  }
  if (length(histYears)) parts$Hist <- .RecDevArrToLongDF(SRR@RecDevHist, histYears, 'Hist')
  if (length(projYears)) parts$Proj <- .RecDevArrToLongDF(SRR@RecDevProj, projYears, 'Proj')

  dplyr::bind_rows(parts)
}

.RecDevArrToLongDF <- function(arr, years, period) {
  if (!length(years)) return(NULL)
  nSim <- nrow(arr)
  data.frame(
    Sim    = rep(seq_len(nSim), times = length(years)),
    Year   = rep(years, each = nSim),
    Value  = as.numeric(log(arr)),
    Period = period
  )
}
