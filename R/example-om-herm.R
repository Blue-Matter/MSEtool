#' Hermaphroditic Grouper Example Operating Model
#'
#' An example [om-class] object representing a monandric protogynous grouper
#' (*Epinephelus* spp.): a female stock and a male stock, each exploited by
#' two fleets. Every fish is born female; males arise only when females
#' change sex.
#'
#' The two stocks are linked by two mechanisms: [Herm()] moves a fraction of
#' females into the male stock as they age, and `SRR@SPFrom` makes the
#' female stock's spawning production the source of all recruitment.
#'
#' Intended to illustrate both mechanisms together on a realistic case study. Pairings
#' of stocks, fleets, and observation models are for illustration only and
#' should not be taken as ecologically meaningful. See also [MultiStockOM]
#' for a non-hermaphroditic two-stock example.
#'
#' @format An [om-class] object with the following slots populated:
#'
#' - **`Name`**: `"Hermaphroditic Grouper"`.
#' - **`nSim`**: `8`. Number of stochastic simulations.
#' - **`nYear`**: `20`. Number of historical years.
#' - **`pYear`**: `30`. Number of projection years.
#' - **`Stock`**: a length-2 list of
#'   `[[GrouperFemaleExStock, GrouperMaleExStock]]`.
#' - **`Fleet`**: a length-2 list, each containing a length-2 list of
#'   `[[AsympExFleet, DomeExFleet]]`. Structure is `[[stock]][[fleet]]`.
#'   Both stocks are fished by the same two fleet types.
#' - **`Obs`**: a length-1 list (one complex) containing a length-2 list of
#'   `[[AgeStructuredObs, CommercialFleetObs]]`.
#' - **`Imp`**: a length-1 [FullComplianceImp], broadcast to both stocks and
#'   fleets. Perfect TAC/effort implementation and full immediate
#'   size-regulation compliance.
#' - **`Herm`**: a length-1 list containing one [stocktransition-class]
#'   object (built via [Herm()]) transitioning individuals from the female
#'   stock into the male stock as they age. See `## Herm Transition` below.
#' - **`Complexes`**: `list(Grouper = 1:2)`. Both stocks form one complex,
#'   so data are aggregated and reference points are calculated across both
#'   sexes.
#'
#' @details
#' ## Stocks
#' [GrouperFemaleExStock] and [GrouperMaleExStock] share an identical age
#' structure (`Ages@Classes = 0:30`) - required for the [Herm()] transition,
#' which reclassifies individuals age-for-age between the two stocks. Males
#' grow to a larger size than females. See
#' [GrouperFemaleExStock]/[GrouperMaleExStock] for full parameter details.
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
#' The complex uses the same pair of observation models by fleet:
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
#' ## Recruitment and Spawning
#' Only the female stock spawns and receives recruits. The male stock has
#' `R0 = 0` and `SPFrom = "Example Grouper Female Stock"`: it has no
#' recruitment of its own, and its catchability in the historical
#' depletion fit follows the female stock's. Spawning production is that of
#' the female stock only, so recruitment does not depend on the number of
#' males.
#'
#' ## Herm Transition
#' [Herm()] physically moves individuals from one stock's numbers-at-age
#' into another's as they age. `HermOM@Herm` is a [stocktransition-class] object,
#' `Herm(From = "Example Grouper Female Stock", To = "Example Grouper Male
#' Stock", Frac = ...)`, where `Frac` is the cumulative fraction of
#' individuals remaining female by age: close to 1 at young ages,
#' decreasing smoothly (a logistic curve centred on age 10) toward 0 by
#' the oldest ages. Sex change depends on age only. Once transitioned, an
#' individual is an age-matched male from that point on: weight, maturity,
#' natural mortality, and selectivity are all determined by the male stock's
#' own biology.
#'
#' Equilibrium and reference point calculations follow each cohort of female
#' recruits through sex change, so male equilibrium numbers and male catch
#' are included in MSY and yield-per-recruit reference points.
#'
#' @seealso
#' [MultiStockOM] for a non-hermaphroditic two-stock structural variant.
#' [GrouperFemaleExStock], [GrouperMaleExStock], [AsympExFleet], [DomeExFleet],
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
#' MSE <- runMSE(HermOM, MPs = ExampleMPs())
#' }
#'
"HermOM"
