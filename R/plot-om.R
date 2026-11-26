#' Plot an Operating Model Summary
#'
#' `PlotOM()` arranges a summary of the main contents of an operating model
#' into one figure: length-at-age, weight-at-age, natural mortality, fishery
#' selectivity with maturity (and retention, when any fleet retains less than
#' all of its catch), historical fishing effort, and the stock-recruit
#' relationship. Panels for stock transitions ([PlotHerm()]) and spatial
#' structure ([PlotSpatial()]) are added when the operating model has them.
#' `plot()` on an [om-class] object calls `PlotOM()`.
#'
#' The [om-class] object is populated with [PopulateOM()] if it is not
#' already. See [PlotStock()], [PlotFleet()], and [PlotData()] for more
#' detail on the stocks, fleets, and data.
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], or [mse-class] object.
#' @param byStock,byFleet See [plot_schedule]. `byStock = FALSE` (default)
#'   draws each stock as its own line on the same panel.
#' @param x An [om-class] object.
#' @param y Unused.
#' @param ... Additional arguments passed to `PlotOM()`.
#'
#' @return A `patchwork` object.
#' @examples
#' PlotOM(SingleStockOM)
#' plot(MultiStockOM)
#' plot(HermOM)
#' @seealso [PlotStock()], [PlotFleet()], [PlotData()], [plot_schedule]
#' @export
PlotOM <- function(object, Sim = NULL, byStock = FALSE, byFleet = NULL, Stocks = NULL,
                   Years = NULL, probs = c(0.05, 0.95)) {
  .CheckClass(object, c('om', 'hist', 'mse'), 'object')
  if (inherits(object, 'om'))
    object <- PopulateOM(object, silent = TRUE)
  OM <- .ResolveOM(object)

  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  nAreas     <- purrr::map_int(match(stockNames, StockNames(OM)), \(st) nArea(OM, st))
  hasHerm    <- length(OM@Herm) > 0
  hasRet     <- any(purrr::map_lgl(unlist(OM@Fleet), \(fl) any(fl@Retention@MeanAtAge < 1, na.rm = TRUE)))

  sched <- list(object = object, Sim = Sim, byStock = byStock, Years = Years, Stocks = Stocks,
                probs = probs)
  panels <- suppressMessages(list(
    Length           = do.call(PlotLengthCurve, sched),
    Weight           = do.call(PlotWeightCurve, sched),
    NaturalMortality = do.call(PlotNaturalMortalityCurve, sched),
    Selectivity      = do.call(PlotSelectivityCurve, c(sched, list(byFleet = byFleet, IncMaturity = TRUE,
                                                                 IncRetention = hasRet))),
    Effort           = PlotEffort(object, byStock = byStock, byFleet = FALSE, Stocks = Stocks,
                                  Years = Years, probs = probs),
    SRR              = PlotSRRCurve(object, Sim = Sim, byStock = byStock, Stocks = Stocks),
    Herm             = if (hasHerm) PlotHerm(object, Sim = Sim, Years = Years, probs = probs),
    Spatial          = if (any(nAreas > 1)) PlotSpatial(object, Stocks = stockNames[nAreas > 1])
  )) |> purrr::compact()
  lines <- setdiff(names(panels), 'Spatial')
  panels[lines] <- purrr::map(panels[lines], \(p) p + ggplot2::guides(fill = 'none'))
  stockLegend <- lines[purrr::map_lgl(panels[lines], \(p) identical(ggplot2::get_labs(p)$colour, 'Stock'))]
  panels[stockLegend[-1]] <- purrr::map(panels[stockLegend[-1]], \(p) p + ggplot2::guides(colour = 'none'))

  patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_layout(guides = 'collect')
}

#' @rdname PlotOM
#' @aliases plot,om,ANY-method
#' @export
setMethod('plot', 'om', function(x, y, ...) {
  PlotOM(x, ...)
})
