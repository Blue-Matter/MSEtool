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



#' Catch-and-Survey Observation Model
#'
#' An [obs-class] object representing a moderate data scenario combining
#' reported landings with a fishery-independent survey index. No effort,
#' CPUE, or composition data are simulated.
#'
#' @format An [obs-class] object. Populated slots:
#'
#' - **`Landings`** ([catchobs-class]): reported landings with CV in
#'   `[0.10, 0.25]` and multiplicative bias in `[0.80, 1.00]`, capturing
#'   moderate reporting uncertainty with a tendency toward under-reporting.
#' - **`Survey`** ([indicesobs-class]): fishery-independent index assuming
#'   biomass-proportional selectivity, with CV in `[0.15, 0.30]` and
#'   autocorrelation in `[0.0, 0.2]`.
#'
#' Empty slots (no data simulated): `Effort`, `Discards`, `CPUE`,
#' `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize`.
#'
#' `CatchAndSurveyObs` represents a stock for which total catch is reported
#' and a survey provides a relative abundance index, but finer-scale data
#' (effort, CPUE, age or length compositions) are unavailable. 
#'
#' @seealso
#' [obs-class], [Obs()], [CatchObs()], [IndicesObs()],
#' [CommercialFleetObs], [AgeStructuredObs], [LengthStructuredObs],
#' [DataRichObs]
#'
#' @family obs-examples
#'
#' @examples
#' CatchAndSurveyObs
#' Landings(CatchAndSurveyObs)
#' Survey(CatchAndSurveyObs)
#'
"CatchAndSurveyObs"

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
#' Empty slots (no data simulated): `Effort`, `CPUE`,
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