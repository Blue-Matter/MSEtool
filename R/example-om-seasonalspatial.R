#' Seasonal Spatial Example Operating Model
#'
#' An example [om-class] object pairing [SeasonalSpatialExStock] with
#' [DomeExFleet] and [CommercialFleetObs]. Intended to demonstrate
#' operating models with monthly time steps, seasonal recruitment, and
#' age-based movement across three spatial areas. Parameter values are
#' illustrative only and do not represent a real stock or fishery. See
#' also [SingleStockOM], [TwoFleetOM], [MultiStockOM], and [ComplexOM]
#' for other structural configurations.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Seasonal Spatial - Single Fleet"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `10`. Number of projection years.
#' - **`Seasons`**: `12`. Monthly time steps within each calendar year.
#'   Must match `SeasonalSpatialExStock@Ages@Units = "month"`.
#' - **`Stock`**: [SeasonalSpatialExStock]. A single [stock-class] object.
#' - **`Fleet`**: [DomeExFleet]. A single [fleet-class] object.
#' - **`Obs`**: [CommercialFleetObs]. A single [obs-class] object.
#' - **`Imp`**: `NULL`. Implementation error is not specified.
#'
#' @details
#'
#' ## Stock
#' [SeasonalSpatialExStock] represents a short-lived reef fish (maximum age
#' 36 months) with fixed biological parameters, a seasonal recruitment pulse
#' peaking in month 6, and an ontogenetic habitat shift across three spatial
#' areas (inshore lagoon → mid-shelf reef → outer slope). See
#' [SeasonalSpatialExStock] for full parameter details.
#'
#' ## Seasons
#' `Seasons = 12` sets the OM to monthly time steps, matching
#' `Ages@Units = "month"` in [SeasonalSpatialExStock]. All within-year
#' dynamics — recruitment pulses, seasonal movement, and seasonal mortality
#' — operate at the monthly scale during simulation.
#'
#' ## Fleet
#' [DomeExFleet] uses dome-shaped length-based selectivity, with reduced
#' retention of the largest fish. This reflects a gear type (e.g. traps or
#' gillnets) that is size-selective rather than retaining everything above
#' a knife-edge length. The effort trajectory rises gradually across the
#' historical period to full effort at the terminal year.
#'
#' See [DomeExFleet] for full parameter details.
#'
#' ## Observation Model
#' [CommercialFleetObs] provides landings, discards, fishing effort, and a
#' commercial CPUE index. There are no fishery-independent survey or
#' composition data, representing a data-limited monitoring scenario typical
#' of small-scale reef fisheries.
#'
#' See [CommercialFleetObs] for full parameter details.
#'
#' ## Implementation Error
#' The `Imp` slot is `NULL`. The [imp-class] is currently a placeholder and
#' implementation error is not applied during simulation. See [Imp()] for
#' details.
#'
#' @seealso
#' [SingleStockOM], [TwoFleetOM], [MultiStockOM], [ComplexOM] for
#' structural variants.
#' [SeasonalSpatialExStock], [DomeExFleet], [CommercialFleetObs] for the
#' component objects.
#' [OM()], [om-class], [runMSE()], [PopulateOM()], [ExampleMPs()]
#'
#' @family om
#'
#' @examples
#' SeasonalSpatialOM
#'
#' \dontrun{
#' Hist <- runMSE(SeasonalSpatialOM, MPs = ExampleMPs())
#' }
#'
"SeasonalSpatialOM"
