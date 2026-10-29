#' Plot All Fishery Schedules for a Fleet
#'
#' Runs every fleet-related `Plot*()` function against a [fleet-class]
#' object: [PlotSelectivity()], [PlotRetention()], [PlotDiscardMortality()],
#' [PlotCatchability()], and [PlotClosure()] (omitted when no
#' closure is specified).
#'
#' @param Fleet A [fleet-class] object, or an [om-class], [hist-class], or
#'   [mse-class] object.
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `Fleet`. Ignored otherwise. Default
#'   `NULL` uses an example stock, with a message noting this.
#' @param byStock Logical. Facet by stock? Passed to each `Plot*()`
#'   function. Default `NULL` facets automatically when `Fleet` has more
#'   than one stock.
#' @param byFleet Logical. Facet by fleet? Passed to each `Plot*()`
#'   function. Default `NULL` facets automatically when `Fleet` has more
#'   than one fleet.
#' @param Stocks Character or numeric vector. Restrict the plots to
#'   specific stocks, either by name (matching [StockNames()]) or by
#'   index. Default `NULL` (all stocks).
#' @param Years Optional numeric vector, or `"all"`. Default `NULL` plots
#'   every available year.
#' @param probs Numeric vector of length 2. Lower/upper quantiles for
#'   simulation envelopes. Default `c(0.05, 0.95)`.
#' @param silent Logical. If `TRUE`, the returned list doesn't announce what
#'   it contains when printed at the console (see [print.PlotFleetList()]).
#'   Default `FALSE`.
#'
#' @return A named list of `ggplot`/`patchwork` objects: `Selectivity`,
#'   `Retention`, `DiscardMortality`, `Effort`, `Catchability`, `Closure`
#'   (omitted when no closure is specified).
#'
#' @seealso [Fleet()], [PlotSelectivity()], [PlotRetention()],
#'   [PlotDiscardMortality()], [PlotEffort()], [PlotCatchability()],
#'   [PlotClosure()]
#' @export
PlotFleet <- function(Fleet, Stock = NULL, byStock = NULL, byFleet = NULL, Stocks = NULL,
                      Years = NULL, probs = c(0.05, 0.95), silent = FALSE) {
  .CheckClass(Fleet, c('fleet', 'om', 'hist', 'mse'), 'Fleet')

  if (inherits(Fleet, 'fleet') && is.null(Stock)) {
    cli::cli_alert_info(
      "No {.arg Stock} provided; using example stock {.val {AlbacoreExStock@Name}} to populate this fleet."
    )
    Stock <- AlbacoreExStock
  }

  result <- suppressMessages(list(
    Selectivity      = PlotSelectivity(Fleet, Stock = Stock, byStock = byStock, byFleet = byFleet,
                                       Years = Years, Stocks = Stocks, probs = probs),
    Retention        = PlotRetention(Fleet, Stock = Stock, byStock = byStock, byFleet = byFleet,
                                     Years = Years, Stocks = Stocks, probs = probs),
    DiscardMortality = PlotDiscardMortality(Fleet, Stock = Stock, byStock = byStock, byFleet = byFleet,
                                            Years = Years, Stocks = Stocks, probs = probs),
    Effort           = PlotEffort(Fleet, Stock = Stock, byStock = byStock, byFleet = byFleet,
                                  Stocks = Stocks, Years = Years, probs = probs),
    Catchability     = PlotCatchability(Fleet, Stock = Stock, byStock = byStock, byFleet = byFleet,
                                        Stocks = Stocks, Years = Years, probs = probs),
    Closure          = PlotClosure(Fleet, Stock = Stock, byStock = byStock, Stocks = Stocks, Years = Years)
  )) |> purrr::compact()

  class(result) <- c('PlotFleetList', class(result))
  attr(result, 'silent') <- silent
  result
}
