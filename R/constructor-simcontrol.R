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
#' @param GenerateData Logical. Generate historical fishery data from the
#'   observation model? Default `TRUE`.
#' @param MSYRefs Logical. Calculate MSY-based reference points? Default `TRUE`.
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
#' @seealso [Simulate()]
#' @export
SimControl <- function(DynamicUnfished = TRUE,
                       RefLandings     = FALSE,
                       RefRemovals     = FALSE,
                       ConditionObs    = TRUE,
                       GenerateData    = TRUE,
                       MSYRefs         = TRUE,
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
    GenerateData    = GenerateData,
    MSYRefs         = MSYRefs
  )
}
