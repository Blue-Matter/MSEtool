#' Plot All Biological Schedules for a Stock
#'
#' Runs every stock-related `Plot*()` function against a bare [stock-class]
#' object: [PlotLength()], [PlotWeight()], [PlotMaturity()],
#' [PlotNaturalMortality()], [PlotFecundity()] (each combining the
#' age-based curve with any additional plot available for that schedule --
#' a length-based curve, an age-size key -- see their own docs),
#' [PlotSpatial()] (when the stock has more than one area),
#' [PlotDepletion()], and [PlotSRR()].
#'
#' @param Stock A [Stock()] object (populated or not), or an [om-class],
#'   [hist-class], or [mse-class] object -- in which case the plots are
#'   built from the single stock it resolves to (see `Stocks`), already
#'   populated as part of that object.
#' @param Stocks Character or numeric vector. Selects which stock to plot
#'   when `Stock` is an [om-class]/[hist-class]/[mse-class] object with more
#'   than one, either by name (matching [StockNames()]) or by index; must
#'   resolve to exactly one stock. Ignored for a bare [Stock()] object.
#' @param nYear,pYear,CurrentYear,nSim,Seasons,ALK,AWK,seed,force,CalcAtLength
#'   Passed to [PopulateStock()]; same arguments, same defaults, except
#'   `nYear`/`pYear` also accept `NULL` in which case defaults
#'   `nYear = 20` and `pYear = 0` are used. An already-populated bare
#'   `Stock` is plotted as-is (with whatever `nYear`/`pYear`/`nSim` it
#'   already has) unless `nYear`, `pYear`, `CurrentYear`, or `force` is
#'   explicitly set, in which case it's re-populated with these values.
#'
#' @return A named list of `ggplot`/`patchwork` objects: `Length`, `Weight`,
#'   `Maturity`, `NaturalMortality`, `Fecundity` (each a `patchwork` when
#'   more than one plot is available for that schedule, otherwise a plain
#'   `ggplot`), `Spatial` (omitted for a single-area stock), `Depletion`,
#'   `SRR`.
#'
#' @param silent Logical. If `TRUE`, the returned list doesn't announce what
#'   it contains when printed at the console (see [print.PlotStockList()]).
#'   Default `FALSE`.
#'
#' @seealso [Stock()], [PopulateStock()], [PlotSRR()], [PlotALK()], [PlotAWK()], [PlotDepletion()]
#' @export
PlotStock <- function(Stock,
                      Stocks       = NULL,
                      nYear        = NULL,
                      pYear        = NULL,
                      CurrentYear  = NULL,
                      nSim         = 5,
                      Seasons      = 1,
                      ALK          = TRUE,
                      AWK          = TRUE,
                      seed         = 102,
                      force        = FALSE,
                      CalcAtLength = FALSE,
                      silent       = FALSE) {

  .CheckClass(Stock, c('stock', 'om', 'hist', 'mse'), 'Stock')

  if (inherits(Stock, 'stock')) {
    alreadyPopulated <- !is.null(Stock@Length@MeanAtAge)
    wantsRepopulate  <- !is.null(nYear) || !is.null(pYear) || !is.null(CurrentYear) || force

    if (!alreadyPopulated || wantsRepopulate) {
      nYear       <- nYear %||% 20
      pYear       <- pYear %||% 0
      CurrentYear <- CurrentYear %||% as.numeric(format(Sys.Date(), '%Y'))

      Stock <- PopulateStock(Stock, nYear = nYear, pYear = pYear,
                             CurrentYear = CurrentYear, nSim = nSim, Seasons = Seasons,
                             ALK = ALK, AWK = AWK, seed = seed, silent = TRUE,
                             force = force, CalcAtLength = CalcAtLength)
    }
  } else {
    OM         <- .ResolveOM(Stock)
    stockNames <- .ResolveStocks(Stock, Stocks)
    stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
    if (length(stockNames) != 1)
      cli::cli_abort(c(
        "x" = "{.arg Stock} resolves to {length(stockNames)} stocks.",
        "i" = "Use {.arg Stocks} to select exactly one: {.val {StockNames(OM)}}."
      ))
    Stock <- OM@Stock[[stockNames]]
  }

  result <- suppressMessages(list(
    Length           = PlotLength(Stock),
    Weight           = PlotWeight(Stock),
    Maturity         = PlotMaturity(Stock),
    NaturalMortality = PlotNaturalMortality(Stock),
    Fecundity        = PlotFecundity(Stock),
    Spatial          = if (nArea(Stock) > 1) PlotSpatial(Stock) else NULL,
    Depletion        = PlotDepletion(Stock),
    SRR              = PlotSRR(Stock)
  )) |> purrr::compact()

  class(result) <- c('PlotStockList', class(result))
  attr(result, 'silent') <- silent
  result
}
