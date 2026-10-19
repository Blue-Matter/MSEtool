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
#' @param CalcCatchAtSize Logical, or `NA`. Calculate historical and
#'   projected landings/discards-at-size?
#'   Computationally expensive for models with many fleets and/or fine
#'   seasonal time steps. `NA` (default) auto-detects, per stock: skipped
#'   unless either a fleet's size-composition observation model is
#'   configured (relevant when `GenerateData = TRUE`) or real
#'   size-composition data is supplied for conditioning (relevant when
#'   `ConditionObs = TRUE`). Set `TRUE` to always calculate it regardless or `FALSE` to always skip it.
#' @param CalcCatchAtSizeCpp Logical. Use the C++ implementation of the
#'   landings/discards-at-size projection
#'   instead of its pure-R equivalent? Default
#'   `TRUE`. Set `FALSE` as a fallback if the C++ version has problems
#' @param DataCV Default coefficient of variation reported in the `CV` slot
#'   of generated effort, catch, and index data, used where neither real data
#'   nor the series' `DataCV` (see [IndicesObs()], [CatchObs()], [EffortObs()])
#'   give one. It sets the reported CV only; observation error is set by the
#'   observation model. Either a non-negative number for all data types, or a
#'   list named by data type (`"Effort"`, `"Landings"`, `"Discards"`, `"CPUE"`,
#'   `"Survey"`), with `0.2` for types not named. Default `0.2`.
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
#' # Reported CV of generated data by data type
#' SimControl(DataCV = list(Landings = 0.05, Survey = 0.3))
#'
#'
#'
#' @seealso [Simulate()], [CalcRefPoints()], [CalcMSY()], [CalcMGT()],
#'   [CalcBLow()]
#' @export
SimControl <- function(DynamicUnfished    = TRUE,
                       RefLandings        = FALSE,
                       RefRemovals        = FALSE,
                       ConditionObs       = TRUE,
                       EstimateBeta       = FALSE,
                       GenerateData       = TRUE,
                       MSYRefs            = TRUE,
                       MSYRefsCpp         = TRUE,
                       RefPoints          = TRUE,
                       MGT                = TRUE,
                       BLow               = FALSE,
                       CalcCatchAtSize    = NA,
                       CalcCatchAtSizeCpp = TRUE,
                       DataCV             = 0.2,
                       ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    cli::cli_alert_warning(
      'Unknown {.fn SimControl} argument{?s}: {.val {names(dots)}}. Ignored.'
    )
  }

  DataCV <- .ExpandDataCV(DataCV)

  list(
    DynamicUnfished    = DynamicUnfished,
    RefLandings        = RefLandings,
    RefRemovals        = RefRemovals,
    ConditionObs       = ConditionObs,
    EstimateBeta       = EstimateBeta,
    GenerateData       = GenerateData,
    MSYRefs            = MSYRefs,
    MSYRefsCpp         = MSYRefsCpp,
    RefPoints          = RefPoints,
    MGT                = MGT,
    BLow               = BLow,
    CalcCatchAtSize    = CalcCatchAtSize,
    CalcCatchAtSizeCpp = CalcCatchAtSizeCpp,
    DataCV             = DataCV
  )
}

.DataCVTypes <- c("Effort", "Landings", "Discards", "CPUE", "Survey")

# default reported CV for each data type, from a scalar or a list/vector named by type
.ExpandDataCV <- function(DataCV, Default = 0.2) {
  if (is.list(DataCV)) DataCV <- unlist(DataCV)
  Named <- !is.null(names(DataCV))
  if (!is.numeric(DataCV) || !length(DataCV) || anyNA(DataCV) || any(DataCV < 0) ||
      (!Named && length(DataCV) != 1))
    cli::cli_abort("{.arg DataCV} must be a non-negative number, or a list of numbers named by data type.")
  if (!Named) return(stats::setNames(rep(DataCV, length(.DataCVTypes)), .DataCVTypes))
  BadNames <- setdiff(names(DataCV), .DataCVTypes)
  if (length(BadNames))
    cli::cli_abort("{.arg DataCV} names must be in {.val {(.DataCVTypes)}}, not {.val {BadNames}}.")
  out <- stats::setNames(rep(Default, length(.DataCVTypes)), .DataCVTypes)
  out[names(DataCV)] <- DataCV
  out
}
