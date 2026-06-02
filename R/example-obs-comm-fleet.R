#' Commercial-Fleet Observation Model
#'
#' An [obs-class] object representing a fleet-monitored data scenario in which
#' landings, discards, effort, and a commercial CPUE index are all observed,
#' but no fishery-independent survey or composition data are available.
#'
#' @format An [obs-class] object. Populated slots:
#'
#' - **`Landings`** ([catchobs-class]): reported landings with CV in
#'   `[0.05, 0.15]` and a positive bias in `[1.00, 1.15]`, reflecting some misreporting.
#' - **`Discards`** ([catchobs-class]): estimated discards with CV in
#'   `[0.20, 0.40]` and bias in `[0.80, 1.20]`, capturing the greater 
#'    uncertainty in at-sea discard estimation.
#' - **`Effort`** ([effortobs-class]): vessel-days from logbooks with CV in
#'   `[0.02, 0.08]` and near-unbiased reporting `[0.95, 1.05]`.
#' - **`CPUE`** ([indicesobs-class]): standardised commercial CPUE with CV in
#'   `[0.15, 0.25]` and autocorrelation in `[0.0, 0.3]`.
#'
#' Empty slots (no data simulated): `Survey`, `LandingsAtAge`,
#' `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize`.
#'
#'
#' @seealso
#' [obs-class], [Obs()], [CatchObs()], [EffortObs()], [IndicesObs()],
#' [CatchAndSurveyObs], [AgeStructuredObs], [LengthStructuredObs],
#' [DataRichObs]
#'
#' @family obs-examples
#'
#' @examples
#' CommercialFleetObs
#' Effort(CommercialFleetObs)
#' CPUE(CommercialFleetObs)
#'
"CommercialFleetObs"

