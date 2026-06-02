#' Data-Rich Observation Model
#'
#' An [obs-class] object representing a fully observed scenario in which all
#' data types are available: landings, discards, fishing effort, a commercial
#' CPUE index, a fishery-independent survey, and both age- and
#' length-composition samples from landed and discarded fish.
#'
#' @format An [obs-class] object with all slots populated:
#'
#' - **`Landings`** ([catchobs-class]): precisely reported landings with CV in
#'   `[0.03, 0.08]` and near-unbiased reporting `[0.98, 1.02]`, reflecting
#'   well-monitored logbook compliance.
#' - **`Discards`** ([catchobs-class]): observer-estimated discards with CV in
#'   `[0.10, 0.20]` and bias in `[0.90, 1.10]`.
#' - **`Effort`** ([effortobs-class]): electronically monitored vessel-days
#'   with CV in `[0.02, 0.05]` and negligible bias `[0.98, 1.02]`.
#' - **`CPUE`** ([indicesobs-class]): logbook-derived standardised CPUE with
#'   CV in `[0.10, 0.20]` and mild autocorrelation `[0.0, 0.2]`.
#' - **`Survey`** ([indicesobs-class]): annual spawning-biomass survey with CV
#'   in `[0.08, 0.15]` and negligible autocorrelation `[0.0, 0.1]`.
#' - **`LandingsAtAge`** ([compobs-class]): stratified port-sampling age
#'   compositions of landed fish. Nominal sample size `[300, 500]`;
#'   ESS `[80, 150]`; `Theta` in `[0.6, 1.0]`.
#' - **`DiscardsAtAge`** ([compobs-class]): observer age samples from discarded
#'   fish. Nominal sample size `[150, 250]`; ESS `[50, 100]`;
#'   `Theta` in `[0.5, 0.9]`.
#' - **`LandingsAtSize`** ([compobs-class]): dockside length-composition
#'   samples from landed fish. Nominal sample size `[400, 700]`;
#'   ESS `[100, 180]`; `Theta` in `[0.6, 1.0]`.
#' - **`DiscardsAtSize`** ([compobs-class]): observer-measured length
#'   compositions from discarded fish. Nominal sample size `[200, 350]`;
#'   ESS `[60, 120]`; `Theta` in `[0.5, 0.9]`.
#'
#'
#' @seealso
#' [obs-class], [Obs()], [CatchObs()], [EffortObs()], [IndicesObs()],
#' [CompObs()], [CatchAndSurveyObs], [CommercialFleetObs],
#' [AgeStructuredObs], [LengthStructuredObs]
#'
#' @family obs-examples
#'
#' @examples
#' DataRichObs
#' Survey(DataRichObs)
#' LandingsAtAge(DataRichObs)
#' DiscardsAtSize(DataRichObs)
#'
"DataRichObs"

