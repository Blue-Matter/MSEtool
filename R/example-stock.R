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
#' Stochastic annual deviates are added via `.PopulateRandom()`.
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
#' Stochastic annual deviates are added via `.PopulateRandom()`. The high `M`
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
#' ## Spatial .Structure
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
#' via `.PopulateRandom()` using `SD` and `AC` from the `SRR` slot.
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


#' Wrasse Example Stocks (Female / Male)
#'
#' A pair of example [stock-class] objects representing the female and
#' (terminal) male phase of a protogynous hermaphrodite wrasse (*Thalassoma*
#' spp.), paired via [HermOM]'s [Herm()] transition and the male's
#' `SRR@SPFrom`. Not intended to represent any specific stock; illustrates
#' [SRR()]'s `SPFrom` cross-stock spawning-production sourcing and
#' [Herm()]'s age-dependent stock transition together. See [HermOM] for the
#' full worked example and how the two stocks and the transition are
#' combined.
#'
#' @format Each is a [stock-class] object with the following slots
#'   populated:
#'
#' - `Name`: `"Example Wrasse Female Stock"` / `"Example Wrasse Male Stock"`.
#' - `CommonName`: `"Wrasse"`.
#' - `Species`: `"Thalassoma spp."`.
#' - `Ages`: An [Ages()] object with `MaxAge = 15`, identical for both
#'   stocks - required for the [Herm()] transition, which reclassifies
#'   individuals age-for-age between the two stocks.
#' - `Length`: A [Length()] object with von Bertalanffy parameters. Terminal
#'   males grow larger than females (`Linf` 30-35 cm vs 23-27 cm), typical
#'   of this style of protogynous species; `K`/`t0` otherwise similar.
#' - `Weight`, `Maturity`: [Weight()]/[Maturity()] objects; males mature
#'   at a larger size than females, consistent with their larger size-at-age.
#' - `NaturalMortality`: A [NaturalMortality()] object; males have somewhat
#'   lower `M` (0.2-0.3 yr^-1^) than females (0.3-0.4 yr^-1^).
#' - `SRR`: A [SRR()] object. The female is self-recruiting (`SPFrom`
#'   unset) with `R0 = 5000`. The male's `SPFrom = "Example Wrasse Female
#'   Stock"` sources its spawning production (`SP`/`SP0`) from the female stock. #'   
#' - `Spatial`, `Depletion`: matching [Spatial()]/[Depletion()] objects for
#'   both stocks.
#'
#' @seealso [HermOM] for the full worked example (including the [Herm()]
#'   transition curve linking these two stocks), [SRR()] for `SPFrom`,
#'   [stocktransition-class] for the transition mechanism, [AlbacoreExStock]
#'   for a single-stock example with the full per-slot `Populate*()` walk-through.
#'
#' @examples
#' WrasseFemaleExStock
#' WrasseMaleExStock
#'
#' @name WrasseExStock
#' @aliases WrasseFemaleExStock WrasseMaleExStock
"WrasseFemaleExStock"

#' @rdname WrasseExStock
"WrasseMaleExStock"
