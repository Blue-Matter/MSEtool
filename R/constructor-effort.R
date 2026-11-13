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
#' @param Targeting Numeric, numeric array, or `NULL`. Spatial targeting
#'   exponent (`lambda >= 0`). A scalar, a length-2 vector of uniform bounds
#'   sampled per simulation (one value per simulation when `nSim = 2`), a length-`nSim` vector, or an array with
#'   dimensions `Sim x Year`. The `Sim` dimension may be length 1 or match
#'   `nSim`. The `Year` dimension must be length 1, match the historical
#'   years, or have named `Year` dimnames. Default `NULL`; a value of `1`
#'   (effort follows exploitable biomass) is applied during
#'   [PopulateEffort()]. See the Spatial Effort Allocation section.
#'
#' @param ReleaseValue Numeric, numeric array, or `NULL`. Value of a unit of
#'   released catch relative to a unit of retained catch when the fleet chooses
#'   where to fish. Accepts the same forms as `Targeting`. `0` (the default
#'   when `NULL`) means only retained catch attracts effort, as for a
#'   commercial fleet; `1` means released and retained catch attract effort
#'   equally, as for a catch-and-release recreational fleet. A fleet that
#'   retains nothing in any area is always attracted by the biomass it selects.
#'
#' @param Maximum Numeric, numeric array, or `NULL`. Maximum allowable effort
#'   in projections, in the units of `Effort`. A scalar, or an array with
#'   dimensions `Sim x Year` and named dimnames; the `Sim` dimension may be
#'   length 1 or `nSim`, and the `Year` dimension length 1 or the full model
#'   time series. `NA` imposes no maximum. Effort set from effort advice or
#'   solved to catch a TAC never exceeds it; a fleet that cannot catch its TAC
#'   fishes at `Maximum`, or, where `Maximum` is `NULL` or `NA`, at the effort
#'   where the fishing mortality cap (`OM@maxF`) is reached. A fleet that
#'   fishes several stocks must have the same `Maximum` for each. Default
#'   `NULL`.
#'   
#' @param Mode Character or `NULL`. How effort in an area becomes fishing
#'   mortality. `"Density"` (default): fishing mortality in an area is
#'   proportional to effort per unit area (effort share divided by
#'   `Spatial@RelativeSize`), so effort spread evenly over space gives the same
#'   fishing mortality in every area. `"Biomass"`: each area is a discrete
#'   fishing ground and fishing mortality is proportional to the effort share
#'   in that area, regardless of its size. See the Spatial Effort Allocation
#'   section.
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
#'     across years. When `nSim = 2`, it is one value per simulation instead.
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
#' @param StockTargetingLambda Numeric, numeric array, or `NULL`. Multiplier on
#'   this fleet's resistance to changing its stock-targeting mix when solving
#'   for effort against multiple stock complexes. Accepts the same forms as
#'   `Theta` (scalar, `Sim x 1`, or `Sim x Year`).
#'
#'   `NULL` (default) is equivalent to `1` and leaves the internally derived
#'   resistance unchanged. `0` removes the resistance entirely, letting the
#'   fleet reallocate its targeting freely. Values above `1` make the fleet
#'   hold its existing mix more strongly, approaching a fixed mix by around
#'   `100`. Only consulted for multi-complex operating models; ignored
#'   otherwise. See the Stock Targeting Resistance section below.
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
#' ## Spatial Effort Allocation
#'
#' When `Distribution` cells are `NA`, effort is allocated across the open
#' areas at each time step as
#'
#' \deqn{D_{f,k} \propto w_k \, u_{f,k}^{\lambda_f}}
#'
#' where \eqn{\lambda_f} is `Targeting`, \eqn{w_k} is the relative area size
#' (`Spatial@RelativeSize`) for `Mode = "Density"` and 1 for
#' `Mode = "Biomass"`, and \eqn{u_{f,k}} is the expected catch per unit of
#' effort intensity in area \eqn{k}:
#'
#' \deqn{u_{f,k} = \frac{1}{w_k} \sum_{s} q_{s,f} \sum_a N_{a,k} v_{a,f,k}
#'   \left[ r_{a,f,k} W^{\mathrm{ret}}_{a,f} + \omega_f (1 - r_{a,f,k})
#'   W^{\mathrm{sel}}_{a,f} \right] h(F_{s,k})}
#'
#' with selectivity \eqn{v}, retention \eqn{r}, `ReleaseValue`
#' \eqn{\omega_f}, and depletion discount \eqn{h(F) = (1 - e^{-F}) / F}, the
#' mean fraction of the biomass available over the time step. \eqn{F_{s,k}} is
#' the total apical fishing mortality on stock \eqn{s} in area \eqn{k} from all
#' fleets in the same time step, capped at `OM@maxF`, so the allocations of all
#' fleets are solved jointly.
#'
#' `Targeting` is the elasticity of effort intensity with respect to catch
#' rate:
#'
#' - `0`: effort is spread evenly over space (in proportion to \eqn{w_k}),
#'   regardless of where the fish are.
#' - `1` (default): each area's share of effort is proportional to its
#'   exploitable biomass, discounted for the depletion caused by fishing within
#'   the time step; with light fishing this is its share of exploitable
#'   biomass. For `Mode = "Density"`, fishing mortality is then proportional to
#'   exploitable biomass density.
#' - Larger values concentrate effort in the areas with the highest catch
#'   rates; as `Targeting` increases the allocation approaches the ideal free
#'   distribution, where catch rates are equal across all fished areas.
#'
#' The allocation does not depend on the number of areas: splitting an area
#' into identical parts leaves the effort and fishing mortality in it
#' unchanged. Areas with no exploitable biomass receive no effort when
#' `Targeting > 0`. Areas closed to the fleet for every stock (`Closure = 0`)
#' receive no effort.
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
#' [obs-class], [data-class], [advice-class], [advicedata-class],
#' [imp-class], or [mse-class] object (or a list of [data-class] objects),
#' `Effort()` returns the `Effort` slot of that object rather than
#' constructing a new one. For an [om-class] object, or a list of
#' [fleet-class] objects or a list of them by stock, it returns a list with
#' the `Effort` slot of each fleet, with the same structure as `OM@Fleet`.
#'
#' `Effort(x) <- value` replaces the `Effort` slot of any of these objects
#' except [hist-class] and [mse-class] objects. For an [om-class] object,
#' `value` is a single [effort-class] object when the OM has one stock and
#' one fleet, a list named by fleet when it has one stock and several
#' fleets, and a list named by stock of lists named by fleet when it has
#' several stocks. For [hist-class] and
#' [mse-class] objects, the `df` argument controls whether a tidy
#' `data.frame` (`TRUE`) or the raw array (`FALSE`) is returned.
#'
#' ## Stock Targeting Resistance: `StockTargetingLambda`
#'
#' When a fleet operates on several stock complexes, a single effort level
#' usually cannot meet every complex's TAC at once. Effort solving therefore
#' also adjusts each fleet's stock-targeting weights, subject to a penalty on
#' how far the mix moves from the previous year. The weight on that penalty is
#' derived per fleet from the historical variability of its own targeting: a
#' fleet whose mix has shifted a lot in the past is allowed to shift it again
#' cheaply, while a fleet with a stable record resists.
#'
#' The derived weight is normalised so that, at a multiplier of `1`, shifting
#' the targeting mix by a given proportion costs about as much as missing the
#' TAC by the same proportion. 
#' 
#' `StockTargetingLambda` scales that derived weight, and is specified per
#' stock and fleet since it lives on the fleet object:
#'
#' - `1` (or `NULL`) keeps the derived value: targeting shifts and TAC misses
#'   are weighted comparably.
#' - `0` removes the penalty, so the fleet reallocates targeting freely to hit
#'   its TACs.
#' - Values below `1` (say `0.1`) let targeting move more readily.
#' - Values above `1` hold the existing mix more strongly; by around `100` the
#'   mix is effectively fixed at the previous year's.
#'
#' It has no effect on single-complex operating models, where there is no
#' competing TAC to reconcile against.
#'
#' ## Attaching to a Fleet
#'
#' An [effort-class] object can be attached to a [Fleet()] with
#' `Effort(Fleet) <- MyEffort` and retrieved with `Effort(Fleet)`.
#'
#' @return
#' - `Effort()` returns an [effort-class] object. If `Effort` is a
#'   [fleet-class], [effort-class], [hist-class], [obs-class], [data-class],
#'   [advicedata-class], or [mse-class] object, the `Effort` slot of that object is returned (as a `data.frame`
#'   or array for [hist-class] and [mse-class] depending on `df`).
#' - `Effort<-` returns `x` with the `Effort` slot replaced by `value`.
#' - `Distribution()`, `Targeting()`, `ReleaseValue()`, `Maximum()`, `Mode()`,
#'   `TripsScalar()`, `AnglerPerTrip()`, `Theta()`,
#'   `StockTargetingLambda()` return the corresponding slot from the
#'   [effort-class] object `x`.
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
                   ReleaseValue   = NULL,
                   Maximum        = NULL,
                   Mode           = NULL,
                   TripsScalar    = NULL,
                   AnglerPerTrip  = NULL,
                   Theta          = NULL,
                   StockTargetingLambda = NULL,
                   Misc           = list(),
                   df             = TRUE) {
  
  if (inherits(Effort, c('fleet', 'om')) || .IsFleetList(Effort) || .IsStockFleetList(Effort))
    return(.GetFleetSlot(Effort, 'Effort'))

  if (inherits(Effort, c('advice', 'data', 'advicedata')) || .IsDataList(Effort))
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
    ReleaseValue  = ReleaseValue,
    Maximum       = Maximum,
    Mode          = Mode,
    TripsScalar   = TripsScalar,
    AnglerPerTrip = AnglerPerTrip,
    Theta         = Theta,
    StockTargetingLambda = StockTargetingLambda,
    Misc          = Misc
  )
}
#' @rdname Effort
#' @export
`Effort<-` <- function(x, value) {
  if (inherits(x, 'om') || .IsFleetList(x) || .IsStockFleetList(x))
    return(.AssignFleetSlot(x, value, 'Effort'))
  .AssignSlot(x, value, 'Effort')
}

#' @rdname Effort
#' @export
Distribution <- function(x) {
  .CheckClass(x, c("effort", "hist", "mse"), "x")
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
  .CheckClass(x, c("effort", "stocktargeting"), "x")
  x@Targeting
}

#' @rdname Effort
#' @export
`Targeting<-` <- function(x, value) {
  .CheckClass(x, c("effort", "stocktargeting"), "x")
  x@Targeting <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
ReleaseValue <- function(x) {
  .AccessSlot(x, 'ReleaseValue')
}

#' @rdname Effort
#' @export
`ReleaseValue<-` <- function(x, value) {
  .AssignSlot(x, value, 'ReleaseValue')
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

#' @rdname Effort
#' @export
StockTargetingLambda <- function(x) {
  .AccessSlot(x, 'StockTargetingLambda')
}

#' @rdname Effort
#' @export
`StockTargetingLambda<-` <- function(x, value) {
  .AssignSlot(x, value, 'StockTargetingLambda')
}
