#' Stock Complex Example Operating Model
#'
#' An example [om-class] object representing two stocks managed jointly as a
#' single complex, each exploited by one fleet. The `Complexes` slot groups
#' both stocks so that data and management advice are aggregated across the
#' complex. Intended to illustrate stock complex construction.
#' Pairings of stocks, fleets, and observation models are for illustration
#' only and should not be taken as ecologically meaningful. See also
#' [SingleStockOM], [TwoFleetOM], and [MultiStockOM] for structural variants.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Stock Complex - Single Fleet"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `30`. Number of projection years.
#' - **`Stock`**: a length-2 list of `[[AlbacoreExStock, ButterfishExStock]]`.
#' - **`Fleet`**: a length-2 list, each containing a length-1 list of
#'   [AsympExFleet]. Structure is `[[stock]][[fleet]]`. Both stocks are
#'   fished by the same fleet type.
#' - **`Obs`**: a length-1 list containing a length-1 list of
#'   [AgeStructuredObs]. Structure is `[[complex]][[fleet]]`. A single
#'   observation model applies to the whole complex, since data are
#'   aggregated across both stocks before being observed.
#' - **`Complexes`**: `list(Stock_Complex = 1:2)`. Both stocks are assigned
#'   to a single complex named `"Stock_Complex"`, so data and management
#'   advice are aggregated across stocks 1 and 2.
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
#' ## Stock Complex
#' Setting `Complexes = list(Stock_Complex = 1:2)` instructs the operating
#' model to treat both stocks as a single management unit. Catch and index
#' data are aggregated across the complex when generating management advice,
#' while population dynamics are simulated independently for each stock.
#'
#' ## Fleet
#' [AsympExFleet] is applied to both stocks, representing a single fleet type
#' with asymptotic length-based selectivity. See [AsympExFleet] for full
#' parameter details.
#'
#' ## Observation Model
#' Because both stocks are aggregated into a single complex, the `Obs` slot
#' is indexed by complex and fleet rather than by individual stock. A single
#' [AgeStructuredObs] object applies to the complex as a whole: landings, a
#' spawning-biomass survey, and annual age-composition samples from landed
#' and discarded fish. See [AgeStructuredObs] for full parameter details.
#'
#' ## Implementation Error
#' The `Imp` slot is `NULL`. The [imp-class] is currently a placeholder and
#' implementation error is not applied during simulation. See [Imp()] for
#' details.
#'
#' @seealso
#' [SingleStockOM], [TwoFleetOM], [MultiStockOM] for structural variants.
#' [AlbacoreExStock], [ButterfishExStock], [AsympExFleet], [AgeStructuredObs]
#' for the component objects.
#' [OM()], [om-class], [runMSE()], [PopulateOM()], [ExampleMPs()]
#'
#' @family om
#'
#' @examples
#' ComplexOM
#' ComplexOM@Complexes
#' nStock(ComplexOM)
#'
#' \dontrun{
#' Hist <- runMSE(ComplexOM, MPs = ExampleMPs())
#' }
#'
"ComplexOM"