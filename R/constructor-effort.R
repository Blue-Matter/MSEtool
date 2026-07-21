#' Effort Constructor and Accessors
#'
#' Construct and manipulate an [effort-class] object defining historical
#' fishing effort and spatial structure for a [Fleet()] object.
#'
#' @param Effort Numeric array, data frame, or `NULL`. Historical fishing
#'   effort. Accepted forms:
#'   - `NULL` (default): creates an empty [effort-class] object; no effort is
#'     populated by [PopulateEffort()].
#'   - Numeric array with dimensions `Sim x Year` and named dimnames. The
#'     `Year` dimension must match the historical years of the OM.
#'   - Data frame in the format required by [GenHistEffort()]; a stochastic
#'     `Sim x Year` array is generated automatically during population.
#'
#'   If `Effort` is a [fleet-class], [effort-class], [hist-class],
#'   [obs-class], or [mse-class] object, the `Effort` slot of that object is
#'   extracted and returned (see Details).
#'
#' @param Units Character or `NULL`. Units of fishing effort (e.g.,
#'   `"hours"`, `"trips"`). Default `NULL`. Note that effort is converted to
#'   fishing mortality via gear efficiency (`q`) defined in the associated
#'   [catchability-class] object. When `Units = "trips"` the effort array is
#'   already on the absolute trips scale and `TripsScalar` is ignored by
#'   bag-limit management procedures.
#'   
#' @param Distribution Numeric array or `NULL`. Fraction of total effort
#'   allocated to each spatial area. Used only for spatial models
#'   (`nArea > 1`). Must have dimensions `Sim x Year x Area` with named
#'   dimnames, and values must sum to 1 over the `Area` dimension within each
#'   simulation and year. The `Sim` and `Year` dimensions may each be length 1
#'   (replicated internally) or match `nSim` and the historical years
#'   respectively. Default `NULL`; if not specified, [PopulateEffort()]
#'   initialises all cells to `NA` and the internal spatial allocation
#'   algorithm fills them (see Details). Any cells set to a non-`NA` numeric
#'   value are treated as fixed overrides and are not modified by the
#'   algorithm.
#'   
#' @param Targeting Numeric array or `NULL`. Spatial targeting concentration
#'   parameter (`lambda >= 0`) with dimensions `Sim x Year`. The `Sim`
#'   dimension may be length 1 or match `nSim`. The `Year` dimension must be
#'   length 1, match the historical years, or have named `Year` dimnames.
#'   Default `NULL`; a value of `0.8` is applied to all simulations and years
#'   during [PopulateEffort()]. See Details for how this parameter controls
#'   the concentration of effort across areas.
#'   
#' @param Maximum Numeric array or `NULL`. Maximum allowable effort. Default
#'   `NULL`. Not currently used.
#'   
#' @param Mode Character or `NULL`. Controls whether spatial utility is
#'   calculated per unit area or as raw biomass. `"Density"` (default) divides
#'   exploitable biomass by the relative area size (`RelSize`) before computing
#'   utility, so fleets are attracted to areas with high biomass concentration
#'   regardless of area size. `"Biomass"` uses raw exploitable biomass, so
#'   larger areas are intrinsically more attractive. See the Technical Manual
#'   for equations.
#'   
#' @param TripsScalar Numeric array or `NULL`. Time-varying scalar
#'   \eqn{\alpha_f(t)} converting the effort index to absolute angler trip
#'   counts via \eqn{T_f(t) = \alpha_f(t) \cdot E_f(t)}. Must have dimensions
#'   `Sim x Year` with named dimnames. The `Sim` dimension may be length 1
#'   (replicated internally). The `Year` dimension may be length 1 (replicated
#'   internally) or span the full model time series; values for projection
#'   years beyond the historical period are typically held constant at the last
#'   historical value or extended by the analyst prior to use. Default `NULL`.
#'   Required by bag-limit management procedures when `Units != "trips"`.
#'   Ignored when `Units = "trips"`. See Details.
#'   
#' @param AnglerPerTrip Numeric array or `NULL`. Mean number of anglers per
#'   trip. Must have dimensions `Sim x Year` with named dimnames. The `Sim`
#'   dimension may be length 1 (replicated internally). The `Year` dimension
#'   may be length 1 (replicated internally) if the value does not vary over
#'   time, or span the full model time series if it does. Used by bag-limit
#'   management procedures to scale a per-angler bag limit to a fleet-level
#'   retention cap. Default `NULL`. See Details.
#'   
#' @param Theta Numeric, numeric array, or `NULL`. Overdispersion parameter
#'   \eqn{\theta} of the negative binomial within-trip catch distribution.
#'   Accepted forms:
#'   - `NULL` (default): `Theta` is not populated; bag-limit management
#'     procedures will error if `Theta` is required and not supplied.
#'   - Scalar numeric (e.g. `1.2`): constant overdispersion applied across
#'     all simulations and years.
#'   - Numeric vector length 2. Treated as lower and upper bounds of a
#'     uniform distribution, sampled once per simulation and held constant
#'     across years.
#'   - Numeric vector length `nSim`. One value per simulation, held
#'     constant across years.
#'   - Numeric array with dimensions `Sim x Year` and named dimnames, for
#'     `Theta` that varies over time. The `Sim` dimension may be length 1
#'     (replicated internally). The `Year` dimension may be length 1
#'     (replicated internally) or span the full model time series.
#'
#'   In every form other than a full `Sim x Year` array, the resolved value
#'   is stored with a `Year` dimension of length 1, named with the first
#'   historical year, and is replicated across all historical and
#'   projection years during [Simulate()].
#'
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#'
#' @param df Logical. Only used when `Effort` is a [hist-class] or
#'   [mse-class] object. If `TRUE` (default), a tidy `data.frame` is
#'   returned. If `FALSE`, the raw `Effort` array is returned.
#'   
#' @param x An [effort-class] object for accessor and replacement functions.
#' @param value The replacement value for the corresponding slot.
#'
#' @details
#' An [effort-class] object defines total fleet fishing activity prior to
#' spatial allocation. Effort is converted to fishing mortality via gear
#' efficiency (`q`) in the associated [catchability-class] object.
#'
#' ## Effort Array Format
#'
#' When supplied as a numeric array, `Effort` must have dimensions `Sim x Year` 
#' with named dimnames. The `Sim` dimension may be length 1 (replicated
#' internally by [PopulateEffort()]). The `Year` dimension must match the
#' historical years of the OM exactly.
#'
#' When supplied as a data frame, the format must meet the requirements of
#' [GenHistEffort()], which generates a stochastic `Sim x Year` array during
#' population. The `Units` slot is set to `"unitless"` automatically in this
#' case.
#'
#' ## Spatial Distribution
#'
#' `Distribution` is only used for spatial models (`nArea > 1`). It must sum
#' to 1 over the `Area` dimension within each simulation and year. The `Sim`
#' and `Year` dimensions may be length 1 (replicated internally). When not
#' specified (default), [PopulateEffort()] initialises all cells to `NA`.
#'
#' In [Simulate()] and [Project()], the spatial allocation algorithm only 
#' fills cells that remain  `NA`; any non-`NA` values supplied in `Distribution`
#' are left unchanged and act as fixed overrides for those 
#' simulation-year-area combinations.
#'
#' ## Spatial Utility and Effort Allocation
#'
#' When `Distribution` cells are `NA`, effort allocation across areas is
#' determined internally at each timestep by a two-stage algorithm:
#'
#' **Stage 1 — Exploitable biomass.** For each fleet and area, raw exploitable
#' biomass is computed as the sum over ages of abundance × weight ×
#' selectivity × retention, scaled by catchability (`q`). When
#' `Mode = "Density"` (default), this biomass is divided by the relative area
#' size (`RelSize`) to give biomass density; when `Mode = "Biomass"`, raw
#' biomass is used. 
#'
#' **Stage 2 — Depletion-adjusted utility.** A depletion discount
#' `h(phi) = (1 - exp(-phi)) / phi` is applied, where `phi` is the local
#' fishing pressure (own-fleet effort × `q` × `D0` in the area, plus a
#' one-timestep-lagged contribution from competing fleets). This discounts
#' utility in heavily fished areas, approximating an ideal free distribution
#' under depletion.
#'
#' **Stage 3 — Softmax targeting.** The depletion-adjusted utilities are
#' normalised and passed through a softmax function with concentration
#' parameter `lambda` (`Targeting`). The resulting probabilities are written
#' to `Distribution`.
#'
#' ## Targeting
#'
#' `Targeting` (lambda) is a non-negative concentration parameter controlling
#' how strongly effort is concentrated in high-utility areas. At `lambda = 0`,
#' effort is distributed uniformly across all accessible areas regardless of
#' their relative utility. As `lambda` increases, effort becomes progressively
#' more concentrated in the highest-utility area. The default value of `0.8`
#' produces moderate concentration, broadly consistent with opportunistic
#' targeting behaviour where fleets favour productive areas but do not
#' exclusively fish the single best one. 
#' 
#' Values around `0.5` approach near-uniform allocation; values above `2`–`3` 
#' produce strongly directed behaviour where most effort concentrates in the 
#' top one or two areas. The parameter is open-ended with no fixed upper bound,
#' but very large values (e.g. `> 5`) effectively collapse all effort to the 
#' single highest-utility area in most configurations.
#'
#' ## Effort-to-Trips Conversion and Angler Scaling
#'
#' Bag-limit management procedures require the effort index to be expressed as
#' absolute angler trip counts. When `Units = "trips"` the effort array is
#' already on the trips scale and no conversion is needed. 
#'
#' When the bag limit is defined per angler (the default), `AnglerPerTrip`
#' provides the mean number of anglers per trip \eqn{A_f(t)}, such that the
#' fleet-level retention cap is:
#'
#' \deqn{C^{\text{bag}}_f(t) = B_f \cdot A_f(t) \cdot T_f(t)}
#'
#' where \eqn{B_f} is the bag limit in fish per angler per trip. The `Year`
#' dimension of `AnglerPerTrip` may be length 1 if the value is assumed
#' constant over time; it is replicated internally to match the full model
#' time series. 
#' 
#' When the bag limit is defined per vessel (boat limit), `AnglerPerTrip` is
#' not used.
#'
#' ## Within-Trip Overdispersion: `Theta`
#'
#' `Theta` (\eqn{\theta}) parameterises the negative binomial distribution
#' used by bag-limit management procedures to model within-trip catch counts:
#'
#' \deqn{n_{f}(t) \sim \text{NegBin}(\mu_{f}(t),\; \theta_{f}(t))}
#'
#' where \eqn{\mu_{f}(t)} is the mean per-trip catch at time \eqn{t} that
#' clears all other retention rules (size limits, discard mortality, etc.)
#' - i.e. what would be kept absent any bag limit. The bag limit itself
#' then truncates \eqn{n_{f}(t)} at the cap; \eqn{\mu_{f}(t)} is the
#' input to that truncation, not its output. The variance of within-trip
#' catch (before truncation) is:
#'
#' \deqn{\text{Var}(n_{f}) = \mu_{f} + \frac{\mu_{f}^2}{\theta_{f}(t)}}
#'
#' It determines the fraction of trips that catch at or above the bag
#' limit at any given mean catch rate, and therefore how strongly the
#' regulation constrains total retention as stock abundance changes.
#'
#' Smaller values (e.g. 0.5-2) produce high trip-to-trip variability with
#' many zero-catch trips and occasional large catches, typical of
#' recreational marine fisheries. Larger values approach the Poisson
#' distribution. Only required when a bag-limit management procedure is
#' active for this fleet.
#'
#' The `Year` dimension of `Theta` may be length 1 if the value is assumed
#' constant over time (the typical case, absent creel data suggesting
#' otherwise); it is replicated internally to match the full model time
#' series. A time-varying `Theta` can be supplied directly as a `Sim x Year`
#' array, e.g. to represent a hypothesis that within-trip catch variability
#' changes with stock depletion.
#'
#' When trip-level creel data are available, `Theta` is estimated by
#' maximum likelihood fitting of the negative binomial to observed
#' per-trip counts. In the absence of creel data, `Theta` must be assumed
#' - e.g. a single plausible value, or a range sampled across simulations
#' to test management performance against this uncertainty.
#'
#' ## Pass-Through Extraction
#'
#' When `Effort` is a [fleet-class], [effort-class], [hist-class],
#' [obs-class], or [mse-class] object, `Effort()` returns the `Effort` slot
#' of that object rather than constructing a new one. For [hist-class] and
#' [mse-class] objects, the `df` argument controls whether a tidy
#' `data.frame` (`TRUE`) or the raw array (`FALSE`) is returned.
#'
#' ## Attaching to a Fleet
#'
#' An [effort-class] object can be attached to a [Fleet()] with
#' `Effort(Fleet) <- MyEffort` and retrieved with `Effort(Fleet)`.
#'
#' @return
#' - `Effort()` returns an [effort-class] object. If `Effort` is a
#'   [fleet-class], [effort-class], [hist-class], [obs-class], or [mse-class]
#'   object, the `Effort` slot of that object is returned (as a `data.frame`
#'   or array for [hist-class] and [mse-class] depending on `df`).
#' - `Effort<-` returns `x` with the `Effort` slot replaced by `value`.
#' - `Distribution()`, `Targeting()`, `Maximum()`, `Mode()`,
#'   `TripsScalar()`, `AnglerPerTrip()`, `Theta()` return the corresponding
#'   slot from the [effort-class] object `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso
#' - [effort-class] for the class definition and slot-level documentation.
#' - [Fleet()] for the enclosing fleet constructor.
#' - [GenHistEffort()] for the data frame format used to generate stochastic
#'   historical effort.
#' - [Catchability()] for the gear efficiency that converts effort to fishing
#'   mortality.
#' - [PopulateEffort()] for how effort, distribution, and targeting arrays are
#'   expanded across simulations and years.
#'
#' @family fleet
#'
#' @example man-examples/class-Effort.R
#'
#' @include class-unions.R
#' @name Effort
#' @export
Effort <- function(Effort         = NULL,
                   Units          = NULL,
                   Distribution   = NULL,
                   Targeting      = NULL,
                   Maximum        = NULL,
                   Mode           = NULL,
                   TripsScalar    = NULL,
                   AnglerPerTrip  = NULL,
                   Theta          = NULL,
                   Misc           = list(),
                   df             = TRUE) {
  
  if (.IsFleetOrList(Effort))
    return(.ExtractFleetSlot(Effort, 'Effort'))

  if (inherits(Effort, 'advice'))
    return(.AccessSlot(Effort, 'Effort'))

  if (inherits(Effort, c('fleet', 'effort', 'hist', 'obs', 'imp', 'mse')))
    return(.ExtractEffort(Effort, df))
  
  if (is.null(Mode)) {
    Mode <- 'Density'
  }
  
  if (!Mode %in% c('Biomass', 'Density'))
    cli::cli_abort("`Mode` must be either `Density` or `Biomass`")
  
  methods::new(
    "effort",
    Effort        = Effort,
    Units         = Units,
    Distribution  = Distribution,
    Targeting     = Targeting,
    Maximum       = Maximum,
    Mode          = Mode,
    TripsScalar   = TripsScalar,
    AnglerPerTrip = AnglerPerTrip,
    Theta         = Theta,
    Misc          = Misc
  )
}
#' @rdname Effort
#' @export
`Effort<-` <- function(x, value) {
  .AssignSlot(x, value, 'Effort')
}

#' @rdname Effort
#' @export
Distribution <- function(x) {
  .CheckClass(x, "effort", "x")
  x@Distribution
}

#' @rdname Effort
#' @export
`Distribution<-` <- function(x, value) {
  .CheckClass(x, "effort", "x")
  x@Distribution <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Targeting <- function(x) {
  .CheckClass(x, "effort", "x")
  x@Targeting
}

#' @rdname Effort
#' @export
`Targeting<-` <- function(x, value) {
  .CheckClass(x, "effort", "x")
  x@Targeting <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Maximum <- function(x) {
  .CheckClass(x, "effort", "x")
  x@Maximum
}

#' @rdname Effort
#' @export
`Maximum<-` <- function(x, value) {
  .CheckClass(x, "effort", "x")
  x@Maximum <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Mode <- function(x) {
  .CheckClass(x, "effort", "x")
  x@Mode
}

#' @rdname Effort
#' @export
`Mode<-` <- function(x, value) {
  .CheckClass(x, "effort", "x")
  x@Mode <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
TripsScalar <- function(x) {
  .CheckClass(x, "effort", "x")
  x@TripsScalar
}

#' @rdname Effort
#' @export
`TripsScalar<-` <- function(x, value) {
  .CheckClass(x, "effort", "x")
  x@TripsScalar <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
AnglerPerTrip <- function(x) {
  .CheckClass(x, "effort", "x")
  x@AnglerPerTrip
}

#' @rdname Effort
#' @export
`AnglerPerTrip<-` <- function(x, value) {
  .CheckClass(x, "effort", "x")
  x@AnglerPerTrip <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Theta <- function(x) {
  .AccessSlot(x, 'Theta')
}

#' @rdname Effort
#' @export
`Theta<-` <- function(x, value) {
  .AssignSlot(x, value, 'Theta')
}
