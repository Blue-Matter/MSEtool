#' Multi-Stock Multi-Fleet Example Operating Model
#'
#' An example [om-class] object representing the most complex structural
#' case: two biologically contrasting stocks each exploited by two fleets,
#' with observation models varying by both stock and fleet. Intended to
#' illustrate multi-stock, multi-fleet OM construction in `MSEtool`. Pairings
#' of stocks, fleets, and observation models are for illustration only and
#' should not be taken as ecologically meaningful. See also [SingleStockOM],
#' [TwoFleetOM], and [ComplexOM] for simpler structural examples.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Multi Stock - Multi Fleet"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `30`. Number of projection years.
#' - **`Stock`**: a length-2 list of `[[AlbacoreExStock, ButterfishExStock]]`.
#' - **`Fleet`**: a length-2 list, each containing a length-2 list of
#'   `[[AsympExFleet, DomeExFleet]]`. Structure is `[[stock]][[fleet]]`.
#'   Both stocks are fished by the same two fleet types.
#' - **`Obs`**: a length-2 list with structure `[[stock]][[fleet]]`:
#'   - [AlbacoreExStock]: `[[AgeStructuredObs, CommercialFleetObs]]`
#'   - [ButterfishExStock]: `[[CatchAndSurveyObs, LengthStructuredObs]]`
#' - **`Complexes`**: `NULL`. Stocks are managed independently.
#' - **`Imp`**: `NULL`. Implementation error is not specified; the `Imp`
#'   slot retains its class default. See [Imp()] for details.
#'
#' @details
#' ## Stocks
#' Two biologically contrasting stocks are included:
#'
#' - [AlbacoreExStock]: long-lived, slow-growing, low natural mortality.
#' - [ButterfishExStock]: short-lived, fast-growing, high natural mortality.
#'
#' See [AlbacoreExStock] and [ButterfishExStock] for full parameter details.
#'
#' ## Fleets
#' Both stocks are fished by the same two fleet types:
#'
#' - [AsympExFleet]: asymptotic length-based selectivity; effort stabilises
#'   early in the historical period.
#' - [DomeExFleet]: dome-shaped selectivity; effort increases gradually
#'   across the historical period.
#'
#' See [AsympExFleet] and [DomeExFleet] for full parameter details.
#'
#' ## Observation Models
#' Observation models vary by stock and fleet, illustrating a range of
#' monitoring intensities across the two stocks:
#'
#' - [AlbacoreExStock], Fleet 1 ([AsympExFleet]): [AgeStructuredObs] —
#'   landings, a spawning-biomass survey, and age-composition samples from
#'   landed and discarded fish.
#' - [AlbacoreExStock], Fleet 2 ([DomeExFleet]): [CommercialFleetObs] —
#'   landings, discards, effort, and a commercial CPUE index; no survey or
#'   composition data.
#' - [ButterfishExStock], Fleet 1 ([AsympExFleet]): [CatchAndSurveyObs] —
#'   reported landings and a biomass-proportional survey index only.
#' - [ButterfishExStock], Fleet 2 ([DomeExFleet]): [LengthStructuredObs] —
#'   landings, a survey, and annual length-composition samples from landed
#'   and discarded fish.
#'
#' ## Implementation Error
#' The `Imp` slot is `NULL`. The [imp-class] is currently a placeholder and
#' implementation error is not applied during simulation. See [Imp()] for
#' details.
#'
#' @seealso
#' [SingleStockOM], [TwoFleetOM], [ComplexOM] for simpler structural variants.
#' [AlbacoreExStock], [ButterfishExStock], [AsympExFleet], [DomeExFleet],
#' [AgeStructuredObs], [CommercialFleetObs], [CatchAndSurveyObs],
#' [LengthStructuredObs] for the component objects.
#' [OM()], [om-class], [runMSE()], [PopulateOM()], [ExampleMPs()]
#'
#' @family om
#'
#' @examples
#' MultiStockOM
#' nStock(MultiStockOM)
#' nFleet(MultiStockOM)
#'
#' \dontrun{
#' Hist <- runMSE(MultiStockOM, MPs = ExampleMPs())
#' }
#'
"MultiStockOM"