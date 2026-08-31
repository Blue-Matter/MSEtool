#' Plot the Age-Weight Key
#'
#' Heatmap of a stock's age-weight key (`Weight@AWK`; see [AWK()]); the
#' probability of belonging to each weight class given age.
#'
#' Only one calendar year is shown by default (the last available); pass
#' `Years` for more. When `object` has more than one simulation, the
#' probability shown at each age/weight cell is the mean across simulations
#' before that age's probabilities are rescaled (see below).
#'
#' Color is rescaled within each age class (its own max -> darkest color,
#' `0` -> white); no color legend is shown.
#'
#' When `object` has more than one stock, each gets its own panel
#' (`patchwork`-combined).
#'
#' Returns `NULL` invisibly when a stock's `Weight@CVatAge` isn't set --
#' `AWK` is only populated when it is (see [Weight()]).
#'
#' @param object A [stock-class]/[om-class]/[hist-class]/[mse-class] object.
#' @param Sim Integer or `NULL` (default). Which simulation replicate to
#'   plot. `NULL` averages across all simulations.
#' @param byStock Unused (present for signature consistency with the other
#'   `Plot*()` functions).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param Years Optional numeric vector, or `"all"`. Default `NULL`: the
#'   last available calendar year only. `"all"` facets by every available
#'   year.
#' @param units Logical. `TRUE` (default) labels the age/weight axes with
#'   the stock's `Ages@Units`/`Weight@Units` when set and agrees across
#'   every plotted stock. `FALSE` suppresses unit labeling.
#'
#' @return A `ggplot` object, or `NULL` invisibly if no stock has a
#'   populated `AWK`.
#'
#' @seealso [PlotWeight()], [AWK()], [CalcAgeSizeKey()], [Weight()]
#' @export
PlotAWK <- function(object, Sim = NULL, byStock = NULL, Stocks = NULL, Years = NULL, units = TRUE) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  allStocks  <- StockNames(OM)

  dfs <- purrr::map(seq_along(allStocks), \(st) {
    if (!allStocks[st] %in% stockNames) return(NULL)
    df <- .AWKDF(OM@Stock[[st]], Sim, Years)
    if (is.null(df) || !nrow(df)) return(NULL)
    df$Stock <- allStocks[st]
    df
  }) |> purrr::compact()

  if (!length(dfs)) return(invisible(NULL))

  ylab <- if (isFALSE(units)) 'Weight' else .AppendUnits('Weight', .GetStockUnits(OM, 'Weight', stockNames))
  xlab <- .AgeAxisLabel(OM, stockNames, units)

  plots <- purrr::map(dfs, \(df) {
    p <- ggplot2::ggplot(df, ggplot2::aes(xmin = .data$Age, xmax = .data$Age + .data$AgeWidth,
                                          ymin = .data$Weight, ymax = .data$Weight + .data$WeightWidth,
                                          fill = .data$RelProbability)) +
      ggplot2::geom_rect() +
      ggplot2::scale_fill_gradient(low = 'white', high = '#0d366b', limits = c(0, 1), guide = 'none') +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = xlab, y = ylab, title = if (length(dfs) > 1) unique(df$Stock) else NULL)

    if (length(unique(df$Year)) > 1)
      p <- p + ggplot2::facet_wrap('Year', scales = 'free')
    p
  })

  if (length(plots) == 1) return(plots[[1]])

  patchwork::wrap_plots(plots, ncol = min(2, length(plots)))
}

.AWKDF <- function(Stock, Sim, Years) {
  AWKArr <- Stock@Weight@AWK
  if (is.null(AWKArr)) return(NULL)

  df <- Array2DF(AWKArr) |> dplyr::rename(Weight = 'Class', Probability = 'Value')

  .binWidths <- function(x) {
    breaks <- sort(unique(x))
    widths <- diff(breaks)
    widths <- c(widths, utils::tail(widths, 1))
    stats::setNames(widths, as.character(breaks))
  }
  df$WeightWidth <- .binWidths(df$Weight)[as.character(df$Weight)]
  df$AgeWidth    <- .binWidths(df$Age)[as.character(df$Age)]

  yrs <- sort(unique(df$Year))
  if (!is.null(Years) && !identical(Years, 'all')) {
    keep <- yrs[yrs %in% Years]
    if (!length(keep))
      cli::cli_abort("None of {.arg Years} found in this stock's `AWK`; available: {.val {yrs}}.")
    df <- dplyr::filter(df, .data$Year %in% keep)
  } else if (is.null(Years)) {
    df <- dplyr::filter(df, .data$Year == max(yrs))
  }

  if (!is.null(Sim)) {
    if (!Sim %in% unique(df$Sim))
      cli::cli_abort("`Sim = {Sim}` not found; {length(unique(df$Sim))} simulation{?s} available.")
    df <- dplyr::filter(df, .data$Sim == Sim)
  }

  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c('Age', 'Weight', 'Year', 'WeightWidth', 'AgeWidth')))) |>
    dplyr::summarise(Probability = mean(.data$Probability, na.rm = TRUE), .groups = 'drop')

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c('Age', 'Year')))) |>
    dplyr::mutate(RelProbability = if (max(.data$Probability) > 0)
      .data$Probability / max(.data$Probability) else 0) |>
    dplyr::ungroup()
}
