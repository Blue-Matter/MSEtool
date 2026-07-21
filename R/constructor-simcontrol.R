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
#' @param .ConditionObs Logical. Condition the observation model on historical
#'   fishery data? Default `TRUE`.
#' @param GenerateData Logical. Generate historical fishery data from the
#'   observation model? Default `TRUE`.
#' @param MSYRefs Logical. Calculate MSY-based reference points (see
#'   [CalcMSY()])? Numerically optimizes apical F per simulation, so cost
#'   scales with `nSim`; use `parallel = TRUE` in [Simulate()] for large
#'   `nSim`. Default `TRUE`.
#' @param RefPoints Logical. Calculate F0.1, Fmax, Fx%SPR, Fmed, Fcrash and
#'   SPRcrash reference points (see [CalcRefPoints()])? Cheap relative to
#'   `MSYRefs` -- evaluated once on a shared per-recruit grid, fully
#'   vectorized across simulations, with no per-simulation numerical search.
#'   Default `TRUE`.
#' @param MGT Logical. Calculate mean generation time (see [CalcMGT()])?
#'   A closed-form, fully vectorized calculation -- negligible cost. Default
#'   `TRUE`.
#' @param BLow Logical. Calculate the `BLow` rebuilding reference point (see
#'   [CalcBLow()])? Substantially more expensive than the other reference
#'   points -- a full population projection plus a numerical search, repeated
#'   per simulation and stock. Default `FALSE`.
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
                       .ConditionObs    = TRUE,
                       GenerateData    = TRUE,
                       MSYRefs         = TRUE,
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
    .ConditionObs    = .ConditionObs,
    GenerateData    = GenerateData,
    MSYRefs         = MSYRefs,
    RefPoints       = RefPoints,
    MGT             = MGT,
    BLow            = BLow
  )
}
