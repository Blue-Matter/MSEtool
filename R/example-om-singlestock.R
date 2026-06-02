#' Single Stock Example Operating Model
#'
#' An example [om-class] object representing the simplest operating model
#' structure: one stock and one fleet. Intended to illustrate basic OM
#' construction and use in `MSEtool`. Pairings of stocks, fleets, and
#' observation models are for illustration only and should not be taken as
#' ecologically meaningful. See also [TwoFleetOM], [ComplexOM], and
#' [MultiStockOM] for more complex structural examples.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Single Stock - Single Fleet"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `30`. Number of projection years.
#' - **`Stock`**: [AlbacoreExStock]. A single [stock-class] object.
#' - **`Fleet`**: a length-1 list containing a length-1 list of
#'   [AsympExFleet]. Structure is `[[stock]][[fleet]]`.
#' - **`Obs`**: a length-1 list containing a length-1 list of
#'   [AgeStructuredObs]. Structure is `[[stock]][[fleet]]`.
#' - **`Imp`**: `NULL`. Implementation error is not specified; the `Imp`
#'   slot retains its class default. See [Imp()] for details.
#'
#' @details
#' ## Stock
#' [AlbacoreExStock] represents a long-lived, slow-growing pelagic species
#' with low natural mortality. See [AlbacoreExStock] for full parameter
#' details.
#'
#' ## Fleet
#' [AsympExFleet] represents a fleet with asymptotic length-based selectivity
#' and effort that stabilises early in the historical period. See
#' [AsympExFleet] for full parameter details.
#'
#' ## Observation Model
#' [AgeStructuredObs] provides landings, a spawning-biomass survey, and
#' annual age-composition samples from both landed and discarded fish — a
#' data-moderate scenario typical of stocks assessed with a statistical
#' catch-at-age model. See [AgeStructuredObs] for full parameter details.
#'
#' ## Implementation Error
#' The `Imp` slot is `NULL`. The [imp-class] is currently a placeholder and
#' implementation error is not applied during simulation. See [Imp()] for
#' details.
#'
#' @seealso
#' [TwoFleetOM], [ComplexOM], [MultiStockOM] for structural variants.
#' [AlbacoreExStock], [AsympExFleet], [AgeStructuredObs] for the component
#' objects. [OM()], [om-class], [runMSE()], [PopulateOM()], [ExampleMPs()]
#'
#' @family om
#'
#' @examples
#' SingleStockOM
#' Stock(SingleStockOM)
#' Fleet(SingleStockOM)
#'
#' \dontrun{
#' Hist <- runMSE(SingleStockOM, MPs = ExampleMPs())
#' }
#'
"SingleStockOM"

