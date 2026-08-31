#' Plot Catchability
#'
#' Plots each fleet's catchability trend (`Fleet@Catchability@Efficiency`)
#' over time. For a [hist-class]/[mse-class] `object` this already reflects
#' any depletion-driven optimization applied to the historical period (see
#' [Simulate()]) -- there's no separate specified-vs-realized distinction
#' the way there is for [Effort()].
#'
#' @param object A [fleet-class] object, or an [om-class], [hist-class], or
#'   [mse-class] object.
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `object`. Ignored otherwise. Default
#'   `NULL` uses an example stock, with a message noting this.
#' @param byStock,byFleet One of `TRUE`, `FALSE`, or `NULL` (default,
#'   facets automatically when `object` has more than one stock/fleet).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param Years Optional numeric vector, or `"all"`. Default `NULL` plots
#'   every available year.
#' @param probs Numeric vector of length 2. Lower and upper quantiles of
#'   the across-simulation ribbon. Default `c(0.05, 0.95)`.
#' @param nsim Integer. Number of individual simulation trajectories to
#'   overlay as thin lines, in addition to the median/ribbon. Default `0`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [Catchability()], [PlotEffort()], [Fleet()]
#' @export
PlotCatchability <- function(object, Stock = NULL, byStock = NULL, byFleet = NULL, Stocks = NULL,
                             Years = NULL, probs = c(0.05, 0.95), nsim = 0) {
  .CheckClass(object, c('fleet', 'om', 'hist', 'mse'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)

  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  fleetNames <- FleetNames(OM)
  if (is.null(byFleet)) byFleet <- length(fleetNames) > 1

  df <- purrr::map(stockNames, \(nm) {
    purrr::map(fleetNames, \(fl) {
      Array2DF(OM@Fleet[[nm]][[fl]]@Catchability@Efficiency) |>
        dplyr::mutate(Stock = nm, Fleet = fl)
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows() |> .FilterYears(Years)

  colorVar <- if (!isTRUE(byFleet) && length(fleetNames) > 1) 'Fleet' else NULL

  .BuildTsPlot(df, byStock = byStock, byFleet = byFleet, ylab = 'Catchability',
               probs = probs, nsim = nsim, free_y = TRUE, colorVar = colorVar)
}
