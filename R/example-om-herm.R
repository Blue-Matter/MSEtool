#' Hermaphroditic Wrasse Example Operating Model
#'
#' An example [om-class] object representing a protogynous hermaphrodite
#' wrasse (*Thalassoma* spp.): a female stock and a male stock, each
#' exploited by two fleets. 
#' 
#' Linked by two mechanisms that operate together
#' but are otherwise independent:  `SRR@SPFrom` (the male sources its
#' spawning production from the female) and a [Herm()] transition (a
#' fraction of females change sex into males as they age). 
#' 
#' Intended to illustrate both mechanisms together on a realistic case study. Pairings
#' of stocks, fleets, and observation models are for illustration only and
#' should not be taken as ecologically meaningful. See also [MultiStockOM]
#' for a non-hermaphroditic two-stock example.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Hermaphroditic Wrasse"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `30`. Number of projection years.
#' - **`Stock`**: a length-2 list of
#'   `[[WrasseFemaleExStock, WrasseMaleExStock]]`.
#' - **`Fleet`**: a length-2 list, each containing a length-2 list of
#'   `[[AsympExFleet, DomeExFleet]]`. Structure is `[[stock]][[fleet]]`.
#'   Both stocks are fished by the same two fleet types.
#' - **`Obs`**: a length-2 list, each containing a length-2 list of
#'   `[[AgeStructuredObs, CommercialFleetObs]]`. Structure is
#'   `[[stock]][[fleet]]`.
#' - **`Imp`**: a length-1 [FullComplianceImp], broadcast to both stocks and
#'   fleets. Perfect TAC/effort implementation and full immediate
#'   size-regulation compliance.
#' - **`Herm`**: a length-1 list containing one [stocktransition-class]
#'   object (built via [Herm()]) transitioning individuals from the female
#'   stock into the male stock as they age. See `## Herm Transition` below.
#'
#' @details
#' ## Stocks
#' [WrasseFemaleExStock] and [WrasseMaleExStock] share an identical age
#' structure (`Ages@Classes = 0:15`) - required for the [Herm()] transition,
#' which reclassifies individuals age-for-age between the two stocks - but
#' differ in growth, maturity, natural mortality, and `SRR`. See
#' [WrasseFemaleExStock]/[WrasseMaleExStock] for full parameter details.
#'
#' ## Fleets
#' Both stocks are fished by the same two fleet types, as in [MultiStockOM]:
#'
#' - [AsympExFleet]: asymptotic length-based selectivity; effort stabilises
#'   early in the historical period.
#' - [DomeExFleet]: dome-shaped selectivity; effort increases gradually
#'   across the historical period.
#'
#' ## Observation Model
#' Both stocks use the same pair of observation models by fleet:
#' [AgeStructuredObs] (landings, a spawning-biomass survey, and
#' age-composition samples) on the [AsympExFleet] fleet, and
#' [CommercialFleetObs] (landings, discards, effort, and a commercial CPUE
#' index) on the [DomeExFleet] fleet.
#'
#' ## Implementation Error
#' [FullComplianceImp] models perfect implementation - realised TAC/effort
#' always equal advice exactly, and size-regulation advice is adopted
#' immediately. See [Imp()] for details.
#'
#' ## SPFrom
#' `SRR@SPFrom` lets one stock's recruitment be driven by another stock's
#' spawning production, rather than its own. 
#' 
#' In this example only the
#' female stock spawns: [WrasseMaleExStock]'s `SRR` sets
#' `SPFrom = "Example Wrasse Female Stock"`, so the male stock's spawning
#' biomass/production (`SP`/`SP0`) is read from the female stock. 
#'
#' ## Herm Transition
#' [Herm()] physically moves individuals from one stock's numbers-at-age
#' into another's as they age. `HermOM@Herm` is a [stocktransition-class] object,
#' `Herm(From = "Example Wrasse Female Stock", To = "Example Wrasse Male
#' Stock", Frac = ...)`, where `Frac` is the cumulative fraction of
#' individuals remaining female by age: close to 1 at young ages,
#' decreasing smoothly (a logistic curve centred on age 8 here) toward 0 by
#' the oldest ages. Once transitioned, an individual is an
#' age-matched male from that point on: weight, maturity, natural
#' mortality, and selectivity are all determined by the male stock's own
#' biology. 
#'
#' @seealso
#' [MultiStockOM] for a non-hermaphroditic two-stock structural variant.
#' [WrasseFemaleExStock], [WrasseMaleExStock], [AsympExFleet], [DomeExFleet],
#' [AgeStructuredObs], [CommercialFleetObs], [FullComplianceImp] for the
#' component objects.
#' [SRR()] for `SPFrom`, [Herm()] and [stocktransition-class] for the
#' transition mechanism.
#' [OM()], [om-class], [runMSE()], [PopulateOM()], [ExampleMPs()]
#'
#' @family om
#'
#' @examples
#' HermOM
#' nStock(HermOM)
#' HermOM@Herm
#'
#' \dontrun{
#' Hist <- runMSE(HermOM, MPs = ExampleMPs())
#' }
#'
"HermOM"
