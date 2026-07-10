#' Simulate Operating Model Dynamics
#'
#' Simulate historical fishery dynamics and calculate reference points for an
#' [om-class] or [OM-legacy-class] object. Returns a populated [hist-class]
#' or [Hist-class] object containing historical population dynamics, fishery
#' data, and reference points.
#'
#' @param OM An [om-class] or [OM-legacy-class] object. If `NULL` (default),
#'   `MSEtool::SingleStockOM` is used.
#' @param parallel Logical. Use parallel processing for MSY reference point
#'   calculation (see [CalcMSY()])? Requires a `future` plan established by
#'   [SetupParallel()]. Only used for [om-class] objects. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param nSim Integer. Number of simulation replicates. If `NULL` (default),
#'   the value in `OM` is used. Only used for [om-class] objects; use `nsim`
#'   for [OM-legacy-class] objects.
#' @param nsim Integer. Synonym for `nSim` for [OM-legacy-class] objects. If
#'   `NULL` (default), `nSim` is used if provided.
#' @param control A [SimControl()] object controlling which optional
#'   calculations are performed. Only used for [om-class] objects. Default
#'   `SimControl()`.
#' @param Reduce Logical. Reduce object size after simulation for memory
#'   efficiency? Default `TRUE`. Only used for [om-class] objects.
#' @param ... Additional arguments passed to sub-functions. Not currently
#'   used
#'
#' @details
#' `Simulate()` is a dispatcher that calls internal functions `Simulate_om()` (for
#' [om-class] objects) or `SimulateOM()` (for legacy [OM-legacy-class]
#' objects) depending on the class of `OM`.
#'
#' @return
#' - If `OM` is an [om-class] object: a [hist-class] object.
#' - If `OM` is an [OM-legacy-class] object: a [Hist-legacy-class] object.
#'
#' @seealso [OM()], [runMSE()]
#'
#' @examples
#' \dontrun{
#' # Simulate using the default example OM
#' hist <- Simulate()
#'
#' # Simulate with specific options
#' hist <- Simulate(
#'   OM      = MyOM,
#'   control = SimControl(RefLandings = TRUE, GenerateData = FALSE),
#'   silent  = TRUE
#' )
#' }
#'
#' @export
Simulate <- function(OM       = NULL,
                     parallel = FALSE,
                     silent   = FALSE,
                     nSim     = NULL,
                     nsim     = NULL,
                     control  = SimControl(),
                     Reduce   = TRUE,
                     ...) {

  if (is.null(OM))
    OM <- MSEtool::SingleStockOM

  if (inherits(OM, 'om'))
    return(
      Simulate_om(OM       = OM,
                  parallel = parallel,
                  silent   = silent,
                  nSim     = nSim,
                  control  = control,
                  Reduce   = Reduce,
                  ...)
    )

  if (!is.null(nSim) && is.null(nsim))
    nsim <- nSim

  SimulateOM(OM       = OM,
             parallel = parallel,
             silent   = silent,
             nsim     = nsim)
}