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
#'   [AsympExFleet]. .Structure is `[[stock]][[fleet]]`. Both stocks are
#'   fished by the same fleet type.
#' - **`Obs`**: a length-1 list containing a length-1 list of
#'   [DataRichObs]. .Structure is `[[complex]][[fleet]]`. A single
#'   observation model applies to the whole complex, since data are
#'   aggregated across both stocks before being observed.
#' - **`Complexes`**: `list(StockComplex = 1:2)`. Both stocks are assigned
#'   to a single complex named `"StockComplex"`, so data and management
#'   advice are aggregated across stocks 1 and 2.
#' - **`Imp`**: a length-1 list containing a length-1 list of
#'   [FullComplianceImp]. .Structure is `[[complex]][[fleet]]`. Perfect TAC/
#'   effort implementation and full immediate size-regulation compliance.
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
#' Setting `Complexes = list(StockComplex = 1:2)` instructs the operating
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
#' [DataRichObs] object applies to the complex as a whole: landings, discards,
#' effort, a commercial CPUE index, a spawning-biomass survey, and annual
#' age- and length-composition samples from landed and discarded fish. A
#' fully-monitored scenario is appropriate here since joint management of a
#' stock complex relies on comprehensive, aggregated data across its member
#' stocks. See [DataRichObs] for full parameter details.
#'
#' ## Implementation Error
#' [FullComplianceImp] models perfect implementation: `TAC@Mean = 1`/
#' `SD = 0` and `Effort@Mean = 1`/`SD = 0` mean realised TAC/effort always
#' equal advice exactly, and
#' `Size@Compliance = 1` means any size-regulation advice (a change to
#' `Advice@Retention`/`Advice@Selectivity`) is adopted immediately by the
#' whole fleet (see `.UpdateSelectivitySim()`). See [Imp()] for details,
#' and [OverageImp], [UnderageImp], [PartialSizeComplianceImp] for
#' contrasting examples.
#'
#' @seealso
#' [SingleStockOM], [TwoFleetOM], [MultiStockOM] for structural variants.
#' [AlbacoreExStock], [ButterfishExStock], [AsympExFleet], [DataRichObs],
#' [FullComplianceImp] for the component objects.
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
#'   `[[AsympExFleet, DomeExFleet]]`. .Structure is `[[stock]][[fleet]]`.
#'   Both stocks are fished by the same two fleet types.
#' - **`Obs`**: a length-2 list with structure `[[stock]][[fleet]]`:
#'   - [AlbacoreExStock]: `[[AgeStructuredObs, CommercialFleetObs]]`
#'   - [ButterfishExStock]: `[[CatchAndSurveyObs, LengthStructuredObs]]`
#' - **`Complexes`**: `NULL`. Stocks are managed independently.
#' - **`Imp`**: a length-2 list with structure `[[stock]][[fleet]]`, each
#'   entry [FullComplianceImp]. Perfect TAC/effort implementation and full
#'   immediate size-regulation compliance, for both stocks and fleets.
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
#' [FullComplianceImp] models perfect implementation: `TAC@Mean = 1`/
#' `SD = 0` and `Effort@Mean = 1`/`SD = 0` mean realised TAC/effort always
#' equal advice exactly, and
#' `Size@Compliance = 1` means any size-regulation advice (a change to
#' `Advice@Retention`/`Advice@Selectivity`) is adopted immediately by the
#' whole fleet (see `.UpdateSelectivitySim()`). See [Imp()] for details,
#' and [OverageImp], [UnderageImp], [PartialSizeComplianceImp] for
#' contrasting examples.
#'
#' @seealso
#' [SingleStockOM], [TwoFleetOM], [ComplexOM] for simpler structural variants.
#' [AlbacoreExStock], [ButterfishExStock], [AsympExFleet], [DomeExFleet],
#' [AgeStructuredObs], [CommercialFleetObs], [CatchAndSurveyObs],
#' [LengthStructuredObs], [FullComplianceImp] for the component objects.
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
#' - **`Imp`**: [FullComplianceImp]. Perfect TAC/effort implementation and
#'   full immediate size-regulation compliance.
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
#' [FullComplianceImp] models perfect implementation: `TAC@Mean = 1`/
#' `SD = 0` and `Effort@Mean = 1`/`SD = 0` mean realised TAC/effort always
#' equal advice exactly, and
#' `Size@Compliance = 1` means any size-regulation advice (a change to
#' `Advice@Retention`/`Advice@Selectivity`) is adopted immediately by the
#' whole fleet (see `.UpdateSelectivitySim()`). See [Imp()] for details,
#' and [OverageImp], [UnderageImp], [PartialSizeComplianceImp] for
#' contrasting examples.
#'
#' @seealso
#' [SingleStockOM], [TwoFleetOM], [MultiStockOM], [ComplexOM] for
#' structural variants.
#' [SeasonalSpatialExStock], [DomeExFleet], [CommercialFleetObs],
#' [FullComplianceImp] for the component objects.
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
#'   [AsympExFleet]. .Structure is `[[stock]][[fleet]]`.
#' - **`Obs`**: a length-1 list containing a length-1 list of
#'   [AgeStructuredObs]. .Structure is `[[stock]][[fleet]]`.
#' - **`Imp`**: a length-1 list containing a length-1 list of
#'   [FullComplianceImp]. .Structure is `[[stock]][[fleet]]`. Perfect TAC/
#'   effort implementation and full immediate size-regulation compliance.
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
#' [FullComplianceImp] models perfect implementation: `TAC@Mean = 1`/
#' `SD = 0` and `Effort@Mean = 1`/`SD = 0` mean realised TAC/effort always
#' equal advice exactly, and
#' `Size@Compliance = 1` means any size-regulation advice (a change to
#' `Advice@Retention`/`Advice@Selectivity`) is adopted immediately by the
#' whole fleet (see `.UpdateSelectivitySim()`). See [Imp()] for details,
#' and [OverageImp], [UnderageImp], [PartialSizeComplianceImp] for
#' contrasting examples.
#'
#' @seealso
#' [TwoFleetOM], [ComplexOM], [MultiStockOM] for structural variants.
#' [AlbacoreExStock], [AsympExFleet], [AgeStructuredObs], [FullComplianceImp]
#' for the component objects. [OM()], [om-class], [runMSE()], [PopulateOM()],
#' [ExampleMPs()]
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
#'   `[[AsympExFleet, DomeExFleet]]`. .Structure is `[[stock]][[fleet]]`.
#' - **`Obs`**: a length-1 list containing a length-2 list of
#'   `[[AgeStructuredObs, CatchAndSurveyObs]]`. .Structure is
#'   `[[stock]][[fleet]]`. Fleet 1 has a data-moderate age-structured
#'   monitoring programme; Fleet 2 has catch and a survey only.
#' - **`Imp`**: a length-1 list containing a length-2 list of
#'   [FullComplianceImp]. .Structure is `[[stock]][[fleet]]`. Perfect TAC/
#'   effort implementation and full immediate size-regulation compliance.
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
#' [FullComplianceImp] models perfect implementation: `TAC@Mean = 1`/
#' `SD = 0` and `Effort@Mean = 1`/`SD = 0` mean realised TAC/effort always
#' equal advice exactly, and
#' `Size@Compliance = 1` means any size-regulation advice (a change to
#' `Advice@Retention`/`Advice@Selectivity`) is adopted immediately by the
#' whole fleet (see `.UpdateSelectivitySim()`). See [Imp()] for details,
#' and [OverageImp], [UnderageImp], [PartialSizeComplianceImp] for
#' contrasting examples.
#'
#' @seealso
#' [SingleStockOM], [ComplexOM], [MultiStockOM] for structural variants.
#' [AlbacoreExStock], [AsympExFleet], [DomeExFleet], [AgeStructuredObs],
#' [CatchAndSurveyObs], [FullComplianceImp] for the component objects.
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
