#' Plot All Biological Schedules for a Stock
#'
#' Runs every stock-related `Plot*()` function against a bare [stock-class]
#' object: [PlotLength()], [PlotWeight()], [PlotMaturity()],
#' [PlotNaturalMortality()], [PlotFecundity()], [PlotSpatial()] (when the
#' stock has more than one area), [PlotDepletion()], and [PlotSRR()]. 
#'
#' @param Stock A [Stock()] object, populated or not.
#' @param nYear,pYear,CurrentYear,nSim,Seasons,ALK,AWK,seed,force,CalcAtLength
#'   Passed to [PopulateStock()]; same arguments, same defaults, except
#'   `nYear`/`pYear` also accept `NULL` (the default here) -- unlike
#'   [PopulateStock()], which requires them -- since their exact value
#'   doesn't affect any of these schedules; `NULL` resolves to `nYear = 20`,
#'   `pYear = 0`.
#'
#' @return A named list of `ggplot` objects: `Length`, `Weight`, `Maturity`,
#'   `NaturalMortality`, `Fecundity`, `Spatial` (omitted for a single-area
#'   stock), `Depletion`, `SRR`.
#'
#' @seealso [Stock()], [PopulateStock()], [PlotSRR()], [PlotDepletion()]
#' @export
PlotStock <- function(Stock,
                      nYear        = NULL,
                      pYear        = NULL,
                      CurrentYear  = NULL,
                      nSim         = 5,
                      Seasons      = 1,
                      ALK          = TRUE,
                      AWK          = TRUE,
                      seed         = 102,
                      force        = FALSE,
                      CalcAtLength = FALSE) {

  .CheckClass(Stock, 'stock', 'Stock')

  nYear       <- nYear %||% 20
  pYear       <- pYear %||% 0
  CurrentYear <- CurrentYear %||% as.numeric(format(Sys.Date(), '%Y'))

  Stock <- PopulateStock(Stock, nYear = nYear, pYear = pYear,
                         CurrentYear = CurrentYear, nSim = nSim, Seasons = Seasons,
                         ALK = ALK, AWK = AWK, seed = seed, silent = TRUE,
                         force = force, CalcAtLength = CalcAtLength)

  suppressMessages(list(
    Length           = PlotLength(Stock),
    Weight           = PlotWeight(Stock),
    Maturity         = PlotMaturity(Stock),
    NaturalMortality = PlotNaturalMortality(Stock),
    Fecundity        = PlotFecundity(Stock),
    Spatial          = if (nArea(Stock) > 1) PlotSpatial(Stock) else NULL,
    Depletion        = PlotDepletion(Stock),
    SRR              = PlotSRR(Stock)
  )) |> purrr::compact()
}
