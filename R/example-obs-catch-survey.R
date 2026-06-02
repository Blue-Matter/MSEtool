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