#' Plot Spatial Closures
#'
#' Heatmap of each fleet's spatio-temporal closure schedule
#' (`Fleet@Closure`);the proportion of simulations in which an area is
#' closed to fishing, by year. Areas that are open in every year for every
#' simulation contribute nothing to look at, so a stock/fleet with no
#' closures specified is silently omitted; if none has any, an
#' informational message is printed and `NULL` is returned invisibly.
#'
#' @param object A [fleet-class] object, or an [om-class], [hist-class], or
#'   [mse-class] object.
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `object`. Ignored otherwise. Default
#'   `NULL` uses an example stock, with a message noting this.
#' @param byStock Logical. Facet by stock? Default `NULL` facets
#'   automatically when `object` has more than one stock.
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param Years Optional numeric vector, or `"all"`. Default `NULL` plots
#'   every available year.
#'
#' @return A `ggplot` object, or `NULL` invisibly if no selected stock/fleet
#'   has any closures specified.
#'
#' @seealso [Fleet()]
#' @export
PlotClosure <- function(object, Stock = NULL, byStock = NULL, Stocks = NULL, Years = NULL) {
  .CheckClass(object, c('fleet', 'om', 'hist', 'mse'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)

  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  fleetNames <- FleetNames(OM)

  df <- purrr::map(stockNames, \(nm) {
    purrr::map(fleetNames, \(fl) {
      arr <- OM@Fleet[[nm]][[fl]]@Closure
      if (is.null(arr) || all(arr == 1)) return(NULL)
      Array2DF(arr) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c('Year', 'Area')))) |>
        dplyr::summarise(PropOpen = mean(.data$Value), .groups = 'drop') |>
        dplyr::mutate(Stock = nm, Fleet = fl, PropClosed = 1 - .data$PropOpen)
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows() |> .FilterYears(Years)

  if (!nrow(df)) {
    cli::cli_alert_info("No closures specified (every area is open in every year); nothing to plot.")
    return(invisible(NULL))
  }

  df$Area <- factor(df$Area)

  facetVars <- character(0)
  if (!isFALSE(byStock) && length(unique(df$Stock)) > 1) facetVars <- c(facetVars, 'Stock')
  if (length(unique(df$Fleet)) > 1) facetVars <- c(facetVars, 'Fleet')

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Year, y = .data$Area, fill = .data$PropClosed)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = 'white', high = '#b3182b', limits = c(0, 1), name = 'Prop.\nClosed') +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = 'Area')

  if (length(facetVars) == 2) {
    p <- p + ggplot2::facet_grid(stats::reformulate(facetVars[2], facetVars[1]), scales = 'free')
  } else if (length(facetVars) == 1) {
    p <- p + ggplot2::facet_wrap(facetVars, scales = 'free')
  }

  p
}
