#' Butterfish Example Stock
#'
#' An example [stock-class] object representing a Butterfish
#' (*Peprilus triacanthus*) stock. This is a short-lived, fast-growing
#' forage species with high natural mortality and wide uncertainty on
#' recruitment and maturity. See also [AlbacoreExStock] for a contrasting
#' long-lived, slow-growing example.
#'
#' @format A [stock-class] object with the following slots populated:
#'
#' - `Name`: `"ButterfishExStock"`. 
#' - `CommonName`: `"Butterfish"`.
#' - `Species`: `"Peprilus triacanthus"`.
#' - `Ages`: An [Ages()] object with `MaxAge = 8`.
#' - `Length`: A [Length()] object with von Bertalanffy parameters —
#'   `Linf` (38–42 cm), `K` (0.16–0.24 yr^-1^), `t0` (-0.032 to -0.028 yr)
#'   — and `CVatAge` (0.1–0.15).
#' - `Weight`: A [Weight()] object with allometric parameters
#'   `alpha = 1.59e-05` and `beta = 3.1`.
#' - `NaturalMortality`: A [NaturalMortality()] object with
#'   `M` = 0.7–0.9 yr^-1^.
#' - `Maturity`: A [Maturity()] object with logistic maturity-at-length
#'   parameters `L50` (4.5–10.2 cm) and `L50_95` (1–8 cm). The wide
#'   ranges reflect substantial uncertainty in maturity schedule.
#' - `Fecundity`: An empty [Fecundity()] object; fecundity-at-age is
#'   computed internally during population (see Details).
#' - `SRR`: A [SRR()] object with Beverton-Holt steepness `h` (0.4–0.8),
#'   unfished recruitment `R0 = 1000`, recruitment variability `SD` (0.7–1.1),
#'   and autocorrelation `AC` (0.1–0.9). The high `SD` reflects the
#'   characteristically variable recruitment of forage species.
#' - `Spatial`: A [Spatial()] object with `UnfishedDist` (0.095–0.105),
#'   `ProbStaying` (0.4–0.6), and `RelativeSize` (0.095–0.105).
#' - `Depletion`: A [Depletion()] object with current biomass sampled from
#'   5–60% of `B0`.
#' - `nYear`, `pYear`, `nSim`, `CurrentYear`, `Years`, `Seasons`: Set by
#'   [PopulateStock()].
#' - `Misc`: Empty list for user-defined information.
#' - `Log`: Internal list for tracking object state.
#'
#' @details
#' Two-element numeric vectors in the slot descriptions above are uniform
#' bounds. [PopulateStock()] samples from these bounds across simulations and
#' expands the results into `[nSim x nAge x nYear]` arrays. The sections below
#' describe what each component-level `Populate*()` function produces.
#'
#' ## Ages
#' [CalcAgeClasses()] generates the age vector `Ages@Classes`
#' (`0:8`), which is used as the age dimension in all subsequent arrays.
#'
#' ## Length
#' [PopulateLength()] samples `Linf`, `K`, and `t0` independently for each
#' simulation from uniform distributions defined by the bounds, then
#' computes mean length-at-age using the von Bertalanffy growth equation.
#' `CVatAge` is expanded across simulations and, when `ALK = TRUE`, an
#' age–length key is constructed.
#'
#' ## Weight
#' [PopulateWeight()] applies the allometric relationship
#' *W* = `alpha` × *L*^`beta`^ to the mean length-at-age array to produce
#' mean weight-at-age. An age–weight key is optionally constructed when
#' `AWK = TRUE`.
#'
#' ## Natural Mortality
#' [PopulateNaturalMortality()] samples `M` for each simulation from the
#' uniform prior and expands the result into a natural mortality-at-age array.
#' Stochastic annual deviates are added via `PopulateRandom()`. The high `M`
#' range (0.7–0.9 yr^-1^) is consistent with the short lifespan of this species.
#'
#' ## Maturity
#' [PopulateMaturity()] applies the logistic maturity-at-length curve,
#' parameterised by `L50` and `L50_95`, to the populated length-at-age array
#' to produce a maturity-at-age array. The wide priors on both parameters
#' result in high inter-simulation variability in the maturity schedule.
#'
#' ## Fecundity
#' Because the `Fecundity` slot is empty, [PopulateFecundity()] computes
#' fecundity-at-age as the element-wise product of weight-at-age and
#' maturity-at-age, representing spawning output per individual.
#'
#' ## Stock–Recruitment
#' [PopulateSRR()] samples `h` for each simulation from the uniform prior and
#' generates a time series of log-normal recruitment deviates with standard
#' deviation `SD` and autocorrelation `AC`. The high `SD` (0.7–1.1) produces
#' strongly variable recruitment, typical of forage fish.
#'
#' ## Spatial
#' [PopulateSpatial()] samples `UnfishedDist`, `ProbStaying`, and
#' `RelativeSize` for each simulation and uses them to construct a full
#' inter-area movement matrix, then expands all spatial arrays across
#' simulations and years. The moderate `ProbStaying` (0.4–0.6) allows
#' substantial mixing between the two areas.
#'
#' ## Depletion
#' [PopulateDepletion()] samples current biomass depletion relative to `B0`
#' for each simulation from the uniform prior, setting the initial conditions
#' of the operating model.
#'
#' @seealso [AlbacoreExStock], [SeasonalSpatialExStock()], [Stock()], 
#' [PopulateStock()], [Populate()], [stock-class]
#'
#' @examples
#' ButterfishExStock
#'
#' \dontrun{
#' PopulatedStock <- PopulateStock(
#'   ButterfishExStock,
#'   nYear = 50,
#'   pYear = 30,
#'   nSim = 48,
#'   seed = 42
#' )
#' }
#'
"ButterfishExStock"