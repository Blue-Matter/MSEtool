#' Age-Structured Observation Model
#'
#' An [obs-class] object representing a data-moderate scenario suitable for
#' age-structured stock assessment. Landings, dicards, a fishery-independent survey,
#' and annual age-composition samples from both landed and discarded fish are
#' simulated.
#'
#' @format An [obs-class] object. Populated slots:
#'
#' - **`Landings`** ([catchobs-class]): precisely reported landings with CV in
#'   `[0.05, 0.10]` and near-unbiased reporting `[0.95, 1.05]`.
#' - **`Discards`** ([catchobs-class]): less precisely reported discards with CV in
#'   `[0.15, 0.25]` and near-unbiased reporting `[0.95, 1.05]`.
#' - **`Survey`** ([indicesobs-class]): annual survey targeting spawning
#'   biomass with CV in `[0.10, 0.20]` and negligible autocorrelation
#'   `[0.0, 0.1]`.
#' - **`LandingsAtAge`** ([compobs-class]): age-composition samples of landed
#'   fish from a port-sampling programme. Nominal sample size `[200, 400]`
#'   fish per year; ESS `[50, 100]` reflecting within-trip clustering;
#'   Dirichlet-Multinomial dispersion `Theta` in `[0.5, 1.0]`.
#' - **`DiscardsAtAge`** ([compobs-class]): age-composition samples of
#'   discarded fish from at-sea observers. Nominal sample size `[100, 200]`;
#'   ESS `[30, 80]`; `Theta` in `[0.4, 0.8]`, reflecting higher uncertainty
#'   relative to port sampling.
#'
#' Empty slots (no data simulated): `Effort`, `CPUE`,
#' `LandingsAtSize`, `DiscardsAtSize`.
#'
#'
#' @seealso
#' [obs-class], [Obs()], [CatchObs()], [IndicesObs()], [CompObs()],
#' [CatchAndSurveyObs], [CommercialFleetObs], [LengthStructuredObs],
#' [DataRichObs]
#'
#' @family obs-examples
#'
#' @examples
#' AgeStructuredObs
#' LandingsAtAge(AgeStructuredObs)
#' DiscardsAtAge(AgeStructuredObs)
#'
"AgeStructuredObs"

