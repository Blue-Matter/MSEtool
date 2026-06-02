#' Length-Structured Observation Model
#'
#' An [obs-class] object representing a data-moderate scenario suitable for
#' length-based stock assessment. Landings, discards, a fishery-independent survey, and
#' annual length-composition samples from both landed and discarded fish are
#' simulated.
#'
#' @format An [obs-class] object. Populated slots:
#'
#' - **`Landings`** ([catchobs-class]): reported landings with CV in
#'   `[0.10, 0.20]` and slight negative-to-neutral bias `[0.90, 1.05]`.
#' - **`Discards`** ([catchobs-class]): less precisely reported discards with CV in
#'   `[0.15, 0.25]` and near-unbiased reporting `[0.95, 1.05]`.
#' - **`Survey`** ([indicesobs-class]): survey index assuming
#'   biomass-proportional selectivity, with CV in `[0.15, 0.30]` and
#'   autocorrelation in `[0.0, 0.2]`.
#' - **`LandingsAtSize`** ([compobs-class]): length-composition samples of
#'   landed fish from dockside measurements. Nominal sample size
#'   `[300, 600]` fish per year; ESS `[60, 120]` reflecting within-haul
#'   length clustering; `Theta` in `[0.5, 1.0]`.
#' - **`DiscardsAtSize`** ([compobs-class]): length-composition samples of
#'   discarded fish from at-sea observers. Nominal sample size `[150, 300]`;
#'   ESS `[40, 80]`; `Theta` in `[0.4, 0.8]`.
#'
#' Empty slots (no data simulated): `Effort`, `Discards`, `CPUE`,
#' `LandingsAtAge`, `DiscardsAtAge`.
#'
#'
#' @seealso
#' [obs-class], [Obs()], [CatchObs()], [IndicesObs()], [CompObs()],
#' [CatchAndSurveyObs], [CommercialFleetObs], [AgeStructuredObs],
#' [DataRichObs]
#'
#' @family obs-examples
#'
#' @examples
#' LengthStructuredObs
#' LandingsAtSize(LengthStructuredObs)
#' DiscardsAtSize(LengthStructuredObs)
#'
"LengthStructuredObs"