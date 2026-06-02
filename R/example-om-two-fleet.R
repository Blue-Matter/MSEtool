#' Two-Fleet Example Operating Model
#'
#' An example [om-class] object representing a single stock exploited by two
#' fleets with contrasting selectivity patterns and monitoring intensities.
#' Intended to illustrate multi-fleet OM construction in `MSEtool`. Pairings
#' of stocks, fleets, and observation models are for illustration only and
#' should not be taken as ecologically meaningful. See also [SingleStockOM],
#' [ComplexOM], and [MultiStockOM] for structural variants.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Single Stock - Two Fleets"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `30`. Number of projection years.
#' - **`Stock`**: [AlbacoreExStock]. A single [stock-class] object.
#' - **`Fleet`**: a length-1 list containing a length-2 list of
#'   `[[AsympExFleet, DomeExFleet]]`. Structure is `[[stock]][[fleet]]`.
#' - **`Obs`**: a length-1 list containing a length-2 list of
#'   `[[AgeStructuredObs, CatchAndSurveyObs]]`. Structure is
#'   `[[stock]][[fleet]]`. Fleet 1 has a data-moderate age-structured
#'   monitoring programme; Fleet 2 has catch and a survey only.
#' - **`Imp`**: `NULL`. Implementation error is not specified; the `Imp`
#'   slot retains its class default. See [Imp()] for details.
#'
#' @details
#' ## Stock
#' [AlbacoreExStock] represents a long-lived, slow-growing large pelagic
#' species with low natural mortality. See [AlbacoreExStock] for full
#' parameter details.
#'
#' ## Fleets
#' Two contrasting fleets are included:
#'
#' - [AsympExFleet]: asymptotic length-based selectivity; effort stabilises
#'   early in the historical period.
#' - [DomeExFleet]: dome-shaped selectivity; effort increases gradually
#'   across the historical period.
#'
#' See [AsympExFleet] and [DomeExFleet] for full parameter details.
#'
#' ## Observation Models
#' Each fleet has its own observation model, illustrating a contrast between
#' a well-monitored commercial fleet and a less-monitored one:
#'
#' - Fleet 1 ([AsympExFleet]) uses [AgeStructuredObs]: landings, a
#'   spawning-biomass survey, and annual age-composition samples from landed
#'   and discarded fish.
#' - Fleet 2 ([DomeExFleet]) uses [CatchAndSurveyObs]: reported landings
#'   and a biomass-proportional survey index only, with no composition data.
#'
#' ## Implementation Error
#' The `Imp` slot is `NULL`. The [imp-class] is currently a placeholder and
#' implementation error is not applied during simulation. See [Imp()] for
#' details.
#'
#' @seealso
#' [SingleStockOM], [ComplexOM], [MultiStockOM] for structural variants.
#' [AlbacoreExStock], [AsympExFleet], [DomeExFleet], [AgeStructuredObs],
#' [CatchAndSurveyObs] for the component objects.
#' [OM()], [om-class], [runMSE()], [PopulateOM()], [ExampleMPs()]
#'
#' @family om
#'
#' @examples
#' TwoFleetOM
#' nFleet(TwoFleetOM)
#'
#' \dontrun{
#' Hist <- runMSE(TwoFleetOM, MPs = ExampleMPs())
#' }
#'
"TwoFleetOM"