#' Simulation Control Options
#'
#' Constructs a named list of optional-calculation switches for [Simulate()].
#' Pass the result to the `control` argument of [Simulate()] to toggle which
#' computations are performed.
#'
#' @param DynamicUnfished Logical. Calculate the dynamic unfished population
#'   dynamics? Default `TRUE`.
#' @param RefLandings Logical. Calculate reference yield based on landings?
#'   Computationally expensive. Default `FALSE`.
#' @param RefRemovals Logical. Calculate reference yield based on total
#'   removals (landings + discards)? Default `FALSE`.
#' @param ConditionObs Logical. Condition the observation model on historical
#'   fishery data? Default `TRUE`.
#' @param EstimateBeta Logical. Estimate the index hyperstability/
#'   hyperdepletion parameter `Beta` by regression when conditioning on real
#'   data (see [IndicesObs()])? Default `FALSE`. If `FALSE`, `Beta` is fixed at `1`
#'   unless the user already supplied a value for that index. If `TRUE`,
#'   `Beta` is estimated per simulation; see [EstimateBeta()] and
#'   `Index_Obs@Misc$BetaFit` for diagnostics.
#' @param GenerateData Logical. Generate historical fishery data from the
#'   observation model? Default `TRUE`.
#' @param MSYRefs Logical. Calculate MSY-based reference points (see
#'   [CalcMSY()])? Numerically optimizes apical F per simulation, so cost
#'   scales with `nSim`; use `parallel = TRUE` in [Simulate()] for large
#'   `nSim`. Default `TRUE`.
#' @param MSYRefsCpp Logical. Use the C++
#'   implementation of the MSY reference point calculation instead of the R
#'   implementation ([CalcMSY()])? Default `TRUE`.
#' @param RefPoints Logical. Calculate F0.1, Fmax, Fx%SPR, Fmed, Fcrash and
#'   SPRcrash reference points (see [CalcRefPoints()])? Default `TRUE`.
#' @param MGT Logical. Calculate mean generation time (see [CalcMGT()])?
#'  Default  `TRUE`.
#' @param BLow Logical. Calculate the `BLow` rebuilding reference point (see
#'   [CalcBLow()])? Substantially more expensive than the other reference
#'   points. Default `FALSE`.
#' @param ... Additional named arguments. Any unrecognised names trigger a
#'   warning.
#'
#' @return A named list of simulation control settings.
#'
#' @examples
#' # Default settings
#' SimControl()
#'
#' # Skip MSY reference points and reference yield calculations
#' SimControl(MSYRefs = FALSE)
#' 
#' # Calculate reference yield based on landings
#' SimControl(RefLandings = TRUE)
#'
#' # Also calculate the (expensive) BLow rebuilding reference point
#' SimControl(BLow = TRUE)
#'
#' @seealso [Simulate()], [CalcRefPoints()], [CalcMSY()], [CalcMGT()],
#'   [CalcBLow()]
#' @export
SimControl <- function(DynamicUnfished = TRUE,
                       RefLandings     = FALSE,
                       RefRemovals     = FALSE,
                       ConditionObs    = TRUE,
                       EstimateBeta    = FALSE,
                       GenerateData    = TRUE,
                       MSYRefs         = TRUE,
                       MSYRefsCpp      = TRUE,
                       RefPoints       = TRUE,
                       MGT             = TRUE,
                       BLow            = FALSE,
                       ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    cli::cli_alert_warning(
      'Unknown {.fn SimControl} argument{?s}: {.val {names(dots)}}. Ignored.'
    )
  }

  list(
    DynamicUnfished = DynamicUnfished,
    RefLandings     = RefLandings,
    RefRemovals     = RefRemovals,
    ConditionObs    = ConditionObs,
    EstimateBeta    = EstimateBeta,
    GenerateData    = GenerateData,
    MSYRefs         = MSYRefs,
    MSYRefsCpp      = MSYRefsCpp,
    RefPoints       = RefPoints,
    MGT             = MGT,
    BLow            = BLow
  )
}
