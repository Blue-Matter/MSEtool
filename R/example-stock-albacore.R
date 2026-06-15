#' Albacore Example Stock
#'
#' An example [stock-class] object representing an Albacore tuna
#' (*Thunnus alalunga*) stock. This is a long-lived, slow-growing, large
#' pelagic species with low natural mortality. See also [ButterfishExStock]
#' for a contrasting short-lived, fast-growing example.
#'
#' @format A [stock-class] object with the following slots populated:
#'
#' - `Name`: `"AlbacoreExStock"`. 
#' - `CommonName`: `"Albacore"`.
#' - `Species`: `"Thunnus alalunga"`.
#' - `Ages`: An [Ages()] object with `MaxAge = 20` years.
#' - `Length`: A [Length()] object with von Bertalanffy parameters —
#'   `Linf` (121–135 cm), `K` (0.16–0.22 yr^-1^), `t0` (-1.86 to -1.41 yr)
#'   — and `CVatAge` (0.1–0.15).
#' - `Weight`: A [Weight()] object with allometric parameters
#'   `alpha = 1.34e-05` and `beta = 3.106`.
#' - `NaturalMortality`: A [NaturalMortality()] object with
#'   `M` = 0.35–0.45 yr^-1^.
#' - `Maturity`: A [Maturity()] object with logistic maturity-at-length
#'   parameters `L50` (81–91 cm) and `L50_95` (10–12 cm).
#' - `Fecundity`: An empty [Fecundity()] object; fecundity-at-age is
#'   computed internally during population (see Details).
#' - `SRR`: A [SRR()] object with Beverton-Holt steepness `h` (0.65–0.85),
#'   unfished recruitment `R0 = 1000`, recruitment variability `SD` (0.15–0.3),
#'   and autocorrelation `AC` (0.1–0.9).
#' - `Spatial`: A [Spatial()] object with `UnfishedDist` (0.095–0.105),
#'   `ProbStaying` (0.8–0.9), and `RelativeSize` (0.095–0.105).
#' - `Depletion`: A [Depletion()] object with current biomass sampled from
#'   5–60% of `B0`.
#' - `nYear`, `pYear`, `nSim`, `CurrentYear`, `Years`, `Seasons`: Set by
#'   [PopulateStock()].
#' - `Misc`: Empty list for user-defined information.
#' - `Log`: Internal list for tracking object state.
#'
#' Two-element numeric vectors in the slot descriptions above are uniform 
#' bounds. [PopulateStock()] samples from these bounds across simulations and
#' expands the results into `[nSim x nAge x nYear]` arrays. The sections below
#' describe what each component-level `Populate*()` function produces.
#'
#' ## Ages
#' [CalcAgeClasses()] generates the integer age vector `Ages@Classes`
#' (`0:20`), which is used as the age dimension in all subsequent arrays.
#'
#' ## Length
#' [PopulateLength()] samples `Linf`, `K`, and `t0` independently for each
#' simulation from uniform distributions defined by the bounds, then
#' computes mean length-at-age using the von Bertalanffy growth equation.
#' `CVatAge` is expanded across simulations and, when `ALK = TRUE`, an
#' age–length key is constructed.
#' 
#' See also [LengthModels()].
#'
#' ## Weight
#' [PopulateWeight()] applies the allometric relationship
#' *W* = `alpha` × *L*^`beta`^ to the mean length-at-age array to produce
#' mean weight-at-age. An age–weight key is optionally constructed when
#' `AWK = TRUE`.
#' 
#' See also [WeightModels()].
#'
#' ## Natural Mortality
#' [PopulateNaturalMortality()] samples `M` for each simulation from the
#' uniform bounds and expands the result into a natural mortality-at-age array.
#' Stochastic annual deviates are added via `PopulateRandom()`.
#'
#' See also [NaturalMortalityModels()].
#' 
#' ## Maturity
#' [PopulateMaturity()] applies the logistic maturity-at-length curve,
#' parameterised by `L50` and `L50_95`, to the populated length-at-age array
#' to produce a maturity-at-age array. 
#' 
#' See also [MaturityModels()].
#' 
#' ## Fecundity
#' Because the `Fecundity` slot is empty, [PopulateFecundity()] computes
#' fecundity-at-age as the element-wise product of weight-at-age and
#' maturity-at-age, representing spawning output per individual.
#' 
#' See also [FecundityModels()].
#'
#' ## Stock–Recruitment
#' [PopulateSRR()] samples `h` for each simulation from the uniform prior and
#' generates a time series of log-normal recruitment deviates with standard
#' deviation `SD` and autocorrelation `AC`.
#'
#' ## Spatial
#' [PopulateSpatial()] samples `UnfishedDist`, `ProbStaying`, and
#' `RelativeSize` for each simulation and uses them to construct a full
#' inter-area movement matrix, then expands all spatial arrays across
#' simulations and years. The relatively high `ProbStaying` (0.8–0.9) reflects
#' low connectivity between the two areas.
#' 
#' See also [SRRModels()].
#'
#' ## Depletion
#' [PopulateDepletion()] samples current biomass depletion relative to `B0`
#' for each simulation from the uniform prior, setting the initial conditions
#' of the operating model.
#'
#' @seealso [ButterfishExStock], [SeasonalSpatialExStock()], [Stock()], 
#' [PopulateStock()], [Populate()], [stock-class]
#'
#' @examples
#' AlbacoreExStock
#'
#' \dontrun{
#' PopulatedStock <- PopulateStock(
#'   AlbacoreExStock,
#'   nYear = 50,
#'   pYear = 30,
#'   nSim = 48,
#'   seed = 42
#' )
#' }
#'
"AlbacoreExStock"