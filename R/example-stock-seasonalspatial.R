#' Seasonal Spatial Example Stock
#'
#' An example [stock-class] object representing a short-lived tropical reef fish 
#' with monthly time steps, seasonal recruitment, and an ontogenetic
#' habitat shift across three spatial areas. All biological parameters are
#' fixed (no stochastic bounds). This object is intended to demonstrate
#' seasonal and spatial model features rather than reflect a real species.
#'
#' @format A [stock-class] object with the following slots populated:
#'
#' - `Name`: `"Example Seasonal Spatial Stock"`.
#' - `CommonName`: `"Reef Fish"`.
#' - `Species`: `"Example species"`.
#' - `Ages`: An [Ages()] object with `MaxAge = 36` months (`Units = "month"`),
#'   giving 37 monthly age classes expressed in years (0, 1/12, 2/12, …, 3).
#' - `Length`: A [Length()] object with fixed von Bertalanffy parameters —
#'   `Linf = 30` cm, `K = 0.8` yr^-1^, `t0 = -0.1` yr — and
#'   `CVatAge = 0.1`.
#' - `Weight`: A [Weight()] object with allometric parameters
#'   `alpha = 1e-05` and `beta = 3`.
#' - `NaturalMortality`: A [NaturalMortality()] object with fixed
#'   `M = 0.09` per month (~1.1 yr^-1^). This is a per-time-step rate and must
#'    match `Ages@Units = "month"` (see Details).
#' - `Maturity`: A [Maturity()] object with fixed logistic maturity-at-length
#'   parameters `L50 = 15` cm and `L50_95 = 3` cm.
#' - `Fecundity`: An empty [Fecundity()] object; fecundity-at-age is
#'   computed internally during population (see Details).
#' - `SRR`: A [SRR()] object with Beverton-Holt steepness `h = 0.7`,
#'   seasonal unfished recruitment from [SetSeasonalR0()] with an annual
#'   pulse peaking in month 6 (`Sigma = 1.5` months), recruitment
#'   variability `SD = 0.4`, and autocorrelation `AC = 0.3`.
#' - `Spatial`: A [Spatial()] object specifying a three-area ontogenetic
#'   habitat shift (see Details).
#' - `Depletion`: A [Depletion()] object with terminal depletion of
#'   35% of `B0`.
#' - `nYear`, `pYear`, `nSim`, `CurrentYear`, `Years`, `Seasons`: Set by
#'   [PopulateStock()].
#' - `Misc`: Empty list for user-defined information.
#' - `Log`: Internal list for tracking object state.
#'
#' @details
#'
#' ## Monthly Time Steps
#'
#' `Ages@Units = "month"` sets the within-year resolution to monthly time
#' steps. This stock should be paired with an [OM()] with `Seasons = 12`.
#' [CalcAgeClasses()] produces 37 age classes from 0 to 3 years in steps of
#' 1/12 year. 
#'
#' ## Seasonal Recruitment
#'
#' [SetSeasonalR0()] distributes annual unfished recruitment (`AnnualR0 =
#' 1000`) across months using a Gaussian curve centred on month 6 with
#' standard deviation 1.5 months, creating a sharply seasonal pulse. 
#'
#' ## Spatial Structure
#'
#' The three areas represent a linear inshore-to-offshore gradient:
#'
#' | Area | Habitat | Relative size |
#' |:----:|---------|:-------------:|
#' | 1 | Inshore lagoon / seagrass | 15% |
#' | 2 | Mid-shelf reef flat | 35% |
#' | 3 | Outer reef slope | 50% |
#'
#' The unfished biomass distribution shifts progressively from Area 1
#' (juveniles) to Area 3 (adults) with age, specified via a `Sim × Area ×
#' Age` array of Gaussian curves centred at months 1, 18, and 36
#' respectively. Movement is constrained by adjacency: `FracOther`
#' connects Areas 1↔2 and 2↔3 freely, while Areas 1↔3 are weakly
#' connected (`FracOther = 0.02`). [PopulateSpatial()] fits the movement
#' matrix to reproduce the target distribution at each age.
#'
#' ## Length
#' [PopulateLength()] computes mean length-at-age from the fixed von
#' Bertalanffy parameters using the growth equation, then applies log-normal
#' variation scaled by `CVatAge = 0.1` to construct an age-length key.
#'
#' See also [LengthModels()].
#'
#' ## Weight
#' [PopulateWeight()] applies the allometric relationship
#' *W* = `alpha` × *L*^`beta`^ to mean length-at-age to produce
#' mean weight-at-age.
#'
#' See also [WeightModels()].
#'
#' ## Natural Mortality
#' [PopulateNaturalMortality()] expands the fixed `M` into a constant
#' natural mortality-at-age array. Stochastic annual deviates are added
#' via `PopulateRandom()` using `SD` and `AC` from the `SRR` slot.
#'
#' See also [NaturalMortalityModels()].
#'
#' ## Maturity
#' [PopulateMaturity()] applies the logistic maturity-at-length curve,
#' parameterised by `L50` and `L50_95`, to the populated length-at-age
#' array to produce a maturity-at-age array.
#'
#' See also [MaturityModels()].
#'
#' ## Fecundity
#' Because the `Fecundity` slot is empty, [PopulateFecundity()] computes
#' fecundity-at-age as the element-wise product of weight-at-age and
#' maturity-at-age.
#'
#' See also [FecundityModels()].
#'
#' ## Stock-Recruitment
#' [PopulateSRR()] uses the fixed steepness `h = 0.7` and generates a
#' time series of log-normal recruitment deviates with `SD = 0.4` and
#' autocorrelation `AC = 0.3`.
#'
#' See also [SRRModels()].
#'
#' ## Depletion
#' [PopulateDepletion()] sets the terminal historical biomass to a fixed
#' 35% of `B0` across all simulations.
#'
#' @seealso [AlbacoreExStock], [ButterfishExStock], [Stock()],
#'   [PopulateStock()], [Populate()], [stock-class], [SetSeasonalR0()],
#'   [Spatial()]
#'
#' @examples
#' SeasonalSpatialExStock
#'
#' \dontrun{
#' PopulatedStock <- PopulateStock(
#'   SeasonalSpatialExStock,
#'   nYear = 20,
#'   pYear = 10,
#'   nSim  = 48,
#'   seed  = 42
#' )
#' }
#'
"SeasonalSpatialExStock"
