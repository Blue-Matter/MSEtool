#' CatchObs Constructor and Accessors
#'
#' Construct a [catchobs-class] object defining observation error for landed or
#' discarded catch, or access and replace individual slots.
#'
#' @param CV `numeric` or `NULL`. Coefficient of variation of the lognormal
#'   catch observation error. Accepted forms:
#'   - `NULL` (default): no observation error is generated unless `Error` is
#'     supplied directly.
#'   - Length-1 scalar: a single CV applied uniformly across all simulations
#'     and years when generating the `Error` array internally.
#'   - Length-2 vector `c(mean, sd)`: `CV` is itself drawn stochastically —
#'     a unique CV per simulation is generated before
#'     being used to generate the `Error` array.
#'   - Length-`nSim` named array: used directly, one CV per simulation.
#'
#'   When conditioning on real data, a `CV` supplied here sets the observation
#'   error for projection years; otherwise `CV` is estimated as the
#'   simulation-specific standard deviation of historical log-residuals.
#' @param Bias `numeric` or `NULL`. Multiplicative observation bias. Accepted
#'   forms:
#'   - `NULL` (default): bias defaults to `1` (unbiased) internally.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique bias is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Bias), sdconv(1, Bias))`.
#'   - Length-2 vector `c(lower, upper)` (when `nSim != 2`): treated as
#'     uniform bounds; a unique bias per simulation is drawn via
#'     `runif(nSim, lower, upper)`.
#'   - Length-`nSim` vector or named array: used directly, one bias per
#'     simulation.
#'
#'   When conditioning on real data, a `Bias` supplied here is used;
#'   otherwise `Bias` is estimated as the mean ratio of observed to simulated
#'   catch across the conditioning years (`Years`), per simulation.
#' @param Error `numeric` array or `NULL`. Pre-specified lognormal observation
#'   error multipliers with dimensions `nSim x nYear` and named dimnames
#'   (`Sim`, `Year`). When supplied, `Error` is used directly and `CV` is
#'   ignored for error generation. Values should be positive multipliers
#'   centred near 1. Default `NULL`, in which case `Error` is generated from
#'   `CV` internally. When conditioning on real data, the projection years of
#'   a supplied `Error` are used and the historical years are replaced by the
#'   conditioned residuals.
#' @param Years `numeric` vector or `NULL`. Calendar years over which to
#'   condition the observation error when real catch data are provided. When
#'   `NULL` (default), all historical years with positive observed catch are
#'   used; otherwise the supplied years that have positive observed catch.
#'   Has no effect when no real data are supplied.
#' @param Units `character(1)` or `NULL`. Units of catch measurement:
#'   `"Biomass"`, `"Number"`, or a mass or count unit (e.g. `"t"`, `"kg"`,
#'   `"n"`). Determines whether simulated catch is
#'   aggregated as biomass or numbers when generating data.
#'   Values are not rescaled, so catch in biomass is in the OM's `Weight`
#'   units. Default `NULL` (treated as `"Biomass"`).
#' @param Ref `numeric` or `NULL`. Reference catch value. Accepted forms:
#'   - `NULL` (default): no reference value.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique reference value is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Ref), sdconv(1, Ref))` internally.
#'   - Length-`nSim` vector or named array: used directly.
#' @param DataCV `numeric` or `NULL`. Coefficient of variation reported in
#'   the `CV` slot of the generated catch data, which MPs can use. A scalar,
#'   or values named by year that apply from that year onwards (e.g.
#'   `c('1990' = 0.4, '2010' = 0.2)`). Real data CVs take precedence in
#'   historical years, and CVs supplied for projection years in `OM@Data`
#'   take precedence in projection years. `NULL` (default) uses the real data
#'   CV of the last historical year where there is one, otherwise
#'   `SimControl(DataCV)`. Also get or set with [DataCV()] and `DataCV<-`.
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Observation Error Generation
#'
#' The error applied to simulated catch is `Bias[sim] * Error[sim, year]`. The
#' `Error` array (`nSim x nYear`) is obtained in one of three ways, in order
#' of precedence:
#'
#' 1. **User-supplied `Error`**: stored directly; `CV` is not used.
#' 2. **Stochastic from `CV`**: if `Error` is `NULL` and `CV` is specified,
#'    Internally the model generates a lognormal array where each cell is
#'    `rlnorm(1, mconv(1, CV[sim]), sdconv(1, CV[sim]))`.
#' 3. **Conditioned from real data**: [Simulate()] sets the historical years
#'    of `Error` from the residuals between simulated and observed catch, and
#'    generates projection years from `CV` (see below).
#'
#' `Bias` is expanded internally: a scalar input is treated as
#' the CV of a lognormal from which one bias per simulation is drawn; a
#' length-`nSim` input is used directly.
#'
#' `Ref` is expanded internally using the same convention as
#' `Bias`.
#'
#' ## Conditioning on Real Data
#'
#' When real catch data are provided (via `OM@Data`), [Simulate()]
#' conditions the observation model. The historical years of `Error` are the
#' residuals between simulated and observed catch, so historical data match
#' the observed catch. Values supplied by the user before conditioning are
#' used for projection years:
#'
#' - `CV`: sets the lognormal error of projection years; otherwise `CV` is
#'   estimated from the historical residuals.
#' - `Bias`: used for all years; otherwise estimated as the mean ratio of
#'   observed to simulated catch.
#' - `Error`: its projection years are used as-is.
#'
#' Values are recorded as user-supplied in `Misc$UserSupplied` when the
#' object is populated.
#'
#' ## Attaching to an Obs Object
#'
#' ```r
#' Landings(obs) <- CatchObs(CV = 0.2)
#' Discards(obs) <- CatchObs(CV = 0.3, Bias = 1.1)
#' ```
#'
#' @return
#' - `CatchObs()` returns a [catchobs-class] object.
#' - `Landings()` and `Discards()`, when called on an [obs-class] or
#'   [data-class] object, return the corresponding [catchobs-class] or
#'   [catchdata-class] slot directly (no data frame conversion).
#' - `Landings<-` and `Discards<-` return `x` with the slot replaced.
#'
#' @seealso
#' - [catchobs-class] for the class definition.
#' - [Obs()] for the enclosing observation model constructor.
#' - [Simulate()] for conditioning the observation model on real data.
#'
#' @family obs
#'
#' @example man-examples/CatchObs.R
#'
#' @include class-unions.R
#' @name CatchObs
#' @export
CatchObs <- function(CV    = NULL,
                     Bias  = NULL,
                     Error = NULL,
                     Years = NULL,
                     Units = NULL,
                     Ref   = NULL,
                     DataCV = NULL,
                     Misc  = list()) {
  .Object <- methods::new("catchobs")
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Bias))  .Object@Bias  <- Bias
  if (!is.null(Error)) .Object@Error <- Error
  if (!is.null(Years)) .Object@Years <- Years
  if (!is.null(Units)) .Object@Units <- Units
  if (!is.null(Ref))   .Object@Ref   <- Ref
  if (!is.null(DataCV)) .Object@DataCV <- DataCV
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}


#' CompObs Constructor and Accessors
#'
#' Construct a [compobs-class] object defining the observation error structure
#' for age or length composition data, or access and replace composition
#' observation slots of an [obs-class] object.
#'
#' @param SampleSize `numeric` or `NULL`. Nominal sample size (number of fish
#'   aged or measured per year), used as the count argument of the final
#'   multinomial draw. `NULL` (default) uses `ESS` as the sample size; if both
#'   are `NULL`, no composition data are generated for this data type.
#'   Accepted input forms:
#'   - Scalar: constant across all simulations and years.
#'   - Length-2 vector `c(lower, upper)`: bounds of a Uniform distribution
#'     from which one value per simulation is drawn, constant across years.
#'   - Named matrix with dimensions `Sim` and/or `Year`: specifying values at
#'     change-point years, expanded to `[nSim x nYear]` by
#'     [PopulateCompObs()]. See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html)
#'     for the full matrix input convention.
#'
#' @param ESS `numeric` or `NULL`. Effective sample size, which scales the
#'   Dirichlet-Multinomial concentration vector (see Details). Controls
#'   stochastic variability in the composition draw independently of
#'   `SampleSize`. Accepted input forms are identical to those for `SampleSize`
#'   above. `NULL` (default) uses `SampleSize` as the effective sample size.
#'   Typically `ESS <= SampleSize`; values of `ESS < SampleSize` produce
#'   overdispersion relative to a pure multinomial with `SampleSize` draws.
#'
#' @param Theta `numeric` or `NULL`. Dirichlet-Multinomial dispersion
#'   parameter in `(0, 1]`. `Theta = 1` (default) recovers the standard
#'   multinomial. Values less than 1 produce overdispersed compositions, with
#'   `Theta -> 0` giving maximum overdispersion. Accepted input forms are
#'   identical to those for `SampleSize` above.
#'
#' @param Years `numeric` vector or `NULL`. Calendar years of observed
#'   composition data to use during conditioning. Only
#'   relevant when real composition data are supplied in `OM@Data`; has no
#'   effect during simulation-only runs. `NULL` (default) uses all available
#'   historical years during conditioning.
#'
#' @param Shift `numeric` or `NULL`. Systematic per-bin offset on the
#'   log-concentration scale applied to the Dirichlet concentration vector
#'   before drawing, capturing directional bias between observed and
#'   OM-predicted compositions. The concentration for bin \eqn{b} is scaled as
#'   \eqn{\alpha_b' = \mathrm{ESS} \times q_b \times \exp(\mathrm{Shift}_b)},
#'   so `Shift = 0` leaves a bin unchanged, positive values inflate its
#'   concentration, and negative values deflate it. Because the transformation
#'   is log-multiplicative, `Shift` cannot produce invalid (negative)
#'   concentrations regardless of magnitude. `NULL` (default) applies no
#'   shift. Accepted input forms:
#'   - Scalar: constant offset across all simulations, years, and bins.
#'   - Vector of length `nBin`: bin-specific offset, constant across
#'     simulations and years.
#'   - Named matrix with any subset of `Sim`, `Year`, and `Bin` dimensions,
#'     with change-point years: expanded to `[nSim x nYear x nBin]` by
#'     [PopulateCompObs()].
#'   - Full `[nSim x nYear x nBin]` array.
#'   When conditioning on real data, `Shift` is set from the per-bin
#'   log-concentration residuals of the historical years; a `Shift` supplied
#'   here is used for projection years.
#'
#' @param MaxGap `numeric(1)` or `NULL`. Largest number of calendar years
#'   between the fleet's last historical composition observation and the last
#'   historical year for which compositions are generated in projection years.
#'   `NULL` (default) is `5`; `0` generates compositions only when observed in
#'   the last historical year; `Inf` or `NA` always generates them. Fleets with
#'   no historical observations are generated whenever `SampleSize` is set. Also get
#'   or set with [MaxGap()] and `MaxGap<-`.
#'
#' @param Misc `list`. Reserved for internal use. Default `list()`.
#'
#' @details
#' ## Composition generation model
#'
#' Let **q** be the OM-predicted composition vector (length `nBin`, sums to 1)
#' for a given simulation, year, and fleet. The observation error model
#' proceeds as follows:
#'
#' **Step 1 — Base concentration vector**
#'
#' \deqn{\alpha = \mathrm{ESS} \times \mathbf{q}}
#'
#' **Step 2 — Apply log-concentration offset (if `Shift` non-`NULL`)**
#'
#' \deqn{\alpha_b' = \alpha_b \times \exp(\mathrm{Shift}[\mathrm{sim}, \mathrm{year}, b])}
#'
#' When `Shift` is `NULL`, \eqn{\alpha' = \alpha}.
#'
#' **Step 3 — Dirichlet draw**
#'
#' \deqn{\mathbf{p}^* \sim \mathrm{Dirichlet}(\alpha' / \mathrm{Theta})}
#'
#' where `Theta` \eqn{\in (0, 1]} controls overdispersion. `Theta = 1`
#' recovers the standard Dirichlet with concentration \eqn{\alpha'}.
#' Smaller values of `Theta` shrink the total concentration and increase
#' variance.
#'
#' **Step 4 — Multinomial draw**
#'
#' \deqn{\mathrm{obs} \sim \mathrm{Multinomial}(\mathrm{SampleSize},\, \mathbf{p}^*)}
#'
#' ## Conditioning mode
#'
#' When real composition data are supplied via `OM@Data`, [Simulate()]
#' compares observed compositions with OM-predicted compositions across the
#' historical years specified in `Years`, and sets:
#'
#' - `SampleSize`: the observed sample size in historical years. Projection
#'   years use a `SampleSize` supplied by the user before conditioning, or
#'   else the sample size of the last observed year.
#' - `ESS`: estimated in historical years and capped at the observed sample
#'   size. Projection years use an `ESS` supplied by the user, or else the
#'   last estimate, capped at a user-supplied `SampleSize`.
#' - `Shift`: the per-bin log-concentration residual in historical years.
#'   Projection years use a `Shift` supplied by the user, or else the
#'   residual of the last observed year.
#'
#' `Theta` is not estimated. Values are recorded as user-supplied in
#' `Misc$UserSupplied` when the object is populated.
#'
#' Note: when observed compositions differ substantially from OM-predicted
#' compositions (e.g. a length distribution shifted far left or right, or
#' with markedly different spread), the resulting `Shift` values will be
#' large. This is treated as observation error rather than model
#' mis-specification — an assumption that should be evaluated carefully. See
#' the technical manual for a full discussion.
#'
#' ## Attaching to an Obs object
#'
#' ```r
#' obs <- Obs()
#' LandingsAtAge(obs)  <- CompObs(SampleSize = 200, ESS = 50)
#' LandingsAtSize(obs) <- CompObs(SampleSize = 150, ESS = 40, Theta = 0.5)
#' ```
#'
#' @return
#' - `CompObs()` returns a [compobs-class] object.
#' - `LandingsAtAge()`, `DiscardsAtAge()`, `LandingsAtSize()`,
#'   `DiscardsAtSize()` return the corresponding [compobs-class] slot from
#'   an [obs-class] object `x`.
#' - Their replacement forms return `x` with the corresponding slot updated
#'   and the object revalidated.
#'
#' @seealso
#' - [compobs-class] for the class definition and slot descriptions.
#' - [PopulateCompObs()] for the population function.
#' - [Simulate()] for conditioning the observation model on real data.
#' - [Obs()] for the enclosing observation model constructor.
#'
#' @family obs
#'
#' @example man-examples/CompObs.R
#'
#' @include class-unions.R
#' @name CompObs
#' @export
CompObs <- function(SampleSize = NULL,
                    ESS        = NULL,
                    Theta      = NULL,
                    Years      = NULL,
                    Shift      = NULL,
                    MaxGap     = NULL,
                    Misc       = list()) {
  .Object <- methods::new("compobs")
  if (!is.null(SampleSize)) .Object@SampleSize <- SampleSize
  if (!is.null(ESS))        .Object@ESS        <- ESS
  if (!is.null(Theta))      .Object@Theta      <- Theta
  if (!is.null(Years))      .Object@Years      <- Years
  if (!is.null(Shift))      .Object@Shift      <- Shift
  if (!is.null(MaxGap))     .Object@MaxGap     <- as.numeric(MaxGap)
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}


#' EffortObs Constructor and Accessors
#'
#' Construct an [effortobs-class] object defining observation error for fishing
#' effort data, or access and replace individual slots.
#'
#' @param CV `numeric` or `NULL`. Coefficient of variation of the lognormal
#'   effort observation error. Accepted forms:
#'   - `NULL` (default): no observation error is generated unless `Error` is
#'     supplied directly.
#'   - Length-1 scalar: a single CV applied uniformly across all simulations
#'     and years when generating the `Error` array internally.
#'   - Length-2 vector `c(mean, sd)`: `CV` is itself drawn stochastically —
#'     a unique CV per simulation is generated before
#'     being used to generate the `Error` array.
#'   - Length-`nSim` named array: used directly, one CV per simulation.
#' @param Bias `numeric` or `NULL`. Multiplicative observation bias. Accepted
#'   forms:
#'   - `NULL` (default): bias defaults to `1` (unbiased) internally.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique bias is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Bias), sdconv(1, Bias))`.
#'   - Length-2 vector `c(lower, upper)` (when `nSim != 2`): treated as
#'     uniform bounds; a unique bias per simulation is drawn via
#'     `runif(nSim, lower, upper)`.
#'   - Length-`nSim` vector or named array: used directly, one bias per
#'     simulation.
#' @param Error `numeric` array or `NULL`. Pre-specified lognormal observation
#'   error multipliers with dimensions `nSim x nYear` and named dimnames
#'   (`Sim`, `Year`). When supplied, `Error` is used directly and `CV` is
#'   ignored for error generation. Default `NULL`, in which case `Error` is
#'   generated from `CV` internally.
#' @param Years `numeric` vector or `NULL`. Calendar years over which to
#'   condition the observation error when real effort data are provided. When
#'   `NULL` (default), all historical years with positive observed effort are
#'   used; otherwise the supplied years that have positive observed effort.
#' @param Units `character(1)` or `NULL`. Units of effort
#'   (e.g., `"hours"`, `"trips"`, `"unitless"`). Default `NULL`.
#' @param Ref `numeric` or `NULL`. Reference effort value. Accepted forms:
#'   - `NULL` (default): no reference value.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique reference value is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Ref), sdconv(1, Ref))` internally.
#'   - Length-`nSim` vector or named array: used directly.
#' @param DataCV `numeric` or `NULL`. Coefficient of variation reported in
#'   the `CV` slot of the generated effort data, which MPs can use. A scalar,
#'   or values named by year that apply from that year onwards (e.g.
#'   `c('1990' = 0.4, '2010' = 0.2)`). Real data CVs take precedence in
#'   historical years, and CVs supplied for projection years in `OM@Data`
#'   take precedence in projection years. `NULL` (default) uses the real data
#'   CV of the last historical year where there is one, otherwise
#'   `SimControl(DataCV)`. Also get or set with [DataCV()] and `DataCV<-`.
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Observation Error Generation
#'
#' The error applied to simulated effort is `Bias[sim] * Error[sim, year]`. The
#' `Error` array (`nSim x nYear`) is obtained in one of two ways, in order of
#' precedence:
#'
#' 1. **User-supplied `Error`**: stored directly; `CV` is not used.
#' 2. **Stochastic from `CV`**: if `Error` is `NULL` and `CV` is specified,
#'    Internally the model generates a lognormal array where each cell is
#'    `rlnorm(1, mconv(1, CV[sim]), sdconv(1, CV[sim]))`. `CV` is first
#'    expanded to length `nSim` internally — a length-2 input
#'    triggers a stochastic draw of a unique CV per simulation.
#'
#' `Bias` is expanded internally: a scalar input is treated as
#' the CV of a lognormal from which one bias per simulation is drawn; a
#' length-`nSim` input is used directly.
#'
#' `Ref` is expanded internally using the same convention as
#' `Bias`.
#'
#' ## Conditioning on Real Data
#'
#' When real effort data are provided (via `OM@Data`), [Simulate()]
#' conditions the observation model. The historical years of `Error` are the
#' residuals between simulated and observed effort, so historical data match
#' the observed effort. Values supplied by the user before conditioning are
#' used for projection years:
#'
#' - `CV`: sets the lognormal error of projection years; otherwise `CV` is
#'   estimated from the historical residuals.
#' - `Bias`: used for all years; otherwise estimated as the mean ratio of
#'   observed to simulated effort.
#' - `Error`: its projection years are used as-is.
#'
#' Values are recorded as user-supplied in `Misc$UserSupplied` when the
#' object is populated.
#'
#' ## Attaching to an Obs Object
#'
#' ```
#' Effort(obs) <- EffortObs(CV = 0.15)
#' ```
#' @return
#' - `EffortObs()` returns an [effortobs-class] object.
#' - `Effort()` (accessor) returns the `Effort` slot from object `x`.
#' - `Effort<-` returns `x` with the `Effort` slot replaced.
#' - `CV()`, `Bias()`, `Units()`, `Years()`, `Ref()` return the corresponding
#'   slot from an [effortobs-class] object `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso
#' - [effortobs-class] for the class definition.
#' - [Obs()] for the enclosing observation model constructor.
#'
#' @family obs
#'
#' @examples
#' # Uniform 15% CV
#' eo <- EffortObs(CV = 0.15)
#'
#' # Stochastic CV — unique CV per simulation drawn from lognormal(mean=0.15, sd=0.03)
#' eo <- EffortObs(CV = c(0.15, 0.03))
#'
#' # Bias as a CV — unique bias per simulation
#' eo <- EffortObs(CV = 0.15, Bias = 0.05)
#'
#' # Attach to obs
#' obs <- Obs(Effort = EffortObs(CV = 0.15))
#'
#' @include class-unions.R
#' @name EffortObs
#' @export
EffortObs <- function(CV    = NULL,
                      Bias  = NULL,
                      Error = NULL,
                      Years = NULL,
                      Units = NULL,
                      Ref   = NULL,
                      DataCV = NULL,
                      Misc  = list()) {
  .Object <- methods::new("effortobs")
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Bias))  .Object@Bias  <- Bias
  if (!is.null(Error)) .Object@Error <- Error
  if (!is.null(Years)) .Object@Years <- Years
  if (!is.null(Units)) .Object@Units <- Units
  if (!is.null(Ref))   .Object@Ref   <- Ref
  if (!is.null(DataCV)) .Object@DataCV <- DataCV
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}


#' ExploitationObs Constructor
#'
#' Construct an [exploitationobs-class] object defining the observation error
#' of the exploitation pattern (selectivity, retention and discard mortality) of
#' a fleet. The observed exploitation is stored in `Data@Exploitation`.
#'
#' @param Selectivity `list`. Observation error of selectivity, as
#'   `list(Bias = , CV = )`. Default `list()`: selectivity is not observed.
#' @param Retention `list`. Observation error of retention. Default `list()`.
#' @param DiscardMortality `list`. Observation error of discard mortality.
#'   Default `list()`.
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Specifying Observation Error
#'
#' Each component is a list with the elements `Bias` and `CV`, or `list()` when
#' the component is not observed. A component of the fleet that has no
#' observation error specified is not stored in `Data@Exploitation`, unless it
#' is supplied with the real data in `OM@Data`.
#'
#' `Bias` and `CV` are either one specification used for all parameters of the
#' component, or a list (or named vector) with one specification per
#' parameter, named by the names in the `Pars` of the fleet's component, e.g.
#' `L5`, `LFS` and `Vmaxlen` for `DoubleNormal` selectivity. A parameter that is
#' not named is observed without error.
#'
#' - `Bias`: multiplicative bias. A single number is the CV of a lognormal
#'   from which one bias per simulation is drawn; a vector `c(lower, upper)`
#'   draws one bias per simulation from a uniform distribution (use
#'   `c(1.2, 1.2)` for a bias of 1.2 in every simulation); a vector of length
#'   `nSim` gives the bias of each simulation.
#' - `CV`: coefficient of variation of the lognormal observation error, with
#'   one draw per simulation and parameter. A vector `c(mean, sd)` draws the
#'   CV of each simulation from a lognormal. The CV is stored with the
#'   observed parameter in `Pars`, as `<parameter>_CV`.
#'
#' The observed value of a parameter is its value in the last historical time
#' step multiplied by the bias and the error. The observed selectivity and
#' retention are regenerated from the observed parameters, using the observed
#' life history when it is available ([LifeHistoryObs()]).
#'
#' A component that the fleet defines by `MeanAtAge` or `MeanAtLength` without
#' `Pars`, such as `DiscardMortality`, takes an unnamed `Bias` or `CV`, which
#' scales the whole schedule by a single factor per simulation, bounded to
#' the range 0 to 1.
#'
#' The observed exploitation is fixed at the last historical year and is the
#' same in projection years. In an OM with several stocks, the specification of
#' a fleet applies to every stock, with independent draws.
#'
#' ## Conditioning on Real Data
#'
#' Selectivity, retention and discard mortality supplied for a stock and fleet
#' in `OM@Data` (see [Data()]) are used as supplied. When the real data and the
#' OM have the same parameter, [Simulate()] sets the bias of each simulation to
#' the ratio of the real value to the simulated value, and the CV to the
#' reported `<parameter>_CV`. The values are stored by stock in the `Stock`
#' element of the component. Values supplied by the user in `Bias` and `CV` are
#' kept.
#'
#' @return An [exploitationobs-class] object.
#'
#' @seealso
#' - [exploitationobs-class] for the class definition.
#' - [Obs()] for the enclosing observation model constructor.
#' - [exploitationdata-class] for the observed values.
#' - [LifeHistoryObs()] for the observation of life history.
#'
#' @family obs
#'
#' @examples
#' # Selectivity parameters observed with error; discard mortality known
#' eo <- ExploitationObs(
#'   Selectivity = list(CV = list(L5 = 0.1, LFS = 0.1))
#' )
#'
#' # A bias of +20% in the length at full selection in every simulation
#' eo <- ExploitationObs(
#'   Selectivity = list(Bias = list(LFS = c(1.2, 1.2)))
#' )
#'
#' @include class-unions.R
#' @name ExploitationObs
#' @export
ExploitationObs <- function(Selectivity      = list(),
                            Retention        = list(),
                            DiscardMortality = list(),
                            Misc             = list()) {
  Object <- methods::new(
    "exploitationobs",
    Selectivity      = Selectivity,
    Retention        = Retention,
    DiscardMortality = DiscardMortality,
    Misc             = Misc
  )
  .PopulateObsSpecs(Object, .ExComponents, "ExploitationObs")
}


#' IndicesObs Constructor and Accessors
#'
#' Construct an [indicesobs-class] object defining observation error for a
#' CPUE or survey abundance index, or access and replace individual slots.
#'
#' @param CV `numeric` or `NULL`. Coefficient of variation of the lognormal
#'   index observation error. Accepted forms:
#'   - `NULL` (default): no observation error is generated unless `Error` is
#'     supplied directly.
#'   - Length-1 scalar: a single CV applied uniformly across all simulations
#'     and years when generating the `Error` array internally.
#'   - Length-2 vector `c(mean, sd)`: `CV` is itself drawn stochastically —
#'     a unique CV per simulation is generated before
#'     being used to generate the `Error` array.
#'   - Length-`nSim` named array: used directly, one CV per simulation.
#'
#'   When conditioning on real data, a `CV` supplied here sets the observation
#'   error for projection years; otherwise `CV` is set from the
#'   simulation-specific standard deviation of historical log-residuals
#'   estimated by [CalcResidualStats()]. Ignored if `Error` is supplied
#'   directly.
#' @param Error `numeric` array or `NULL`. Pre-specified lognormal observation
#'   error multipliers with dimensions `nSim x nYear` and named dimnames
#'   (`Sim`, `Year`). When supplied, `Error` is used directly and `CV` is
#'   ignored for error generation. When real index data are provided in
#'   `OM@Data`, the projection years of a supplied `Error` are used and the
#'   historical years are replaced by the conditioned residuals. Default
#'   `NULL`, in which case `Error` is generated from `CV` internally.
#' @param Areas `integer` vector or `NULL`. Indices of the spatial areas that
#'   contribute to this index (e.g., `c(1, 2)` for a two-area model where the
#'   index covers only the first two areas). Default `NULL`, in which case all
#'   areas are used.
#' @param Units `character(1)` or `NULL`. Units determining how the simulated
#'   population is converted to an index value. Must be one of:
#'   - `"Biomass"` (default): sum of numbers-at-age multiplied by
#'     weight-at-age over the selected ages and areas.
#'   - `"Number"`: sum of numbers-at-age over the selected ages and areas.
#'   - `"Recruitment"`: abundance of the youngest age class, summed over
#'     areas. For seasonal models, the first non-zero age class in each
#'     simulation and year is used.
#'   Default `NULL` (treated as `"Biomass"`).
#' @param Selectivity Selectivity-at-age of this index, multiplied by
#'   numbers-at-age before they are summed into the index. Accepted forms:
#'   - `NULL` (default): selectivity of the fleet in `OM@Fleet` with the same
#'     name as the index. Indices that are not fleets must set `Selectivity`.
#'   - `"Biomass"`: flat selectivity of 1 for all age classes.
#'   - `"SBiomass"`: maturity-at-age.
#'   - Numeric vector named by age, e.g. `c('3' = 1, '4' = 1, '5' = 1)`.
#'     Ages not named have selectivity 0. An unnamed vector must have one
#'     value per age class.
#'   - Array with named dimnames including `Age` and optionally `Sim`,
#'     `Year`, and `Area`, for selectivity that varies by simulation, year,
#'     or area. `Age` must cover the stock's age classes.
#'   - List of any of the numeric forms, named by stock (or in stock order),
#'     for indices of multi-stock complexes.
#'
#'   [AddIndex()] adds an index and its selectivity to an operating model in
#'   one step; [AddAgeIndices()] adds an age-disaggregated index as one index
#'   per age class.
#' @param TruncSD `numeric(1)`. Number of standard deviations at which to
#'   truncate the lognormal residual distribution when generating observation
#'   errors for projection years via [GenResiduals()]. Default `2`.
#' @param Ref `numeric` or `NULL`. Reference index value, reported in the
#'   `Ref` slot of simulated index data. The reference value is the index
#'   value expected when the total biomass of the stocks in the complex is at
#'   `BMSY` (or, when `BMSY` is not available, in the historical year closest
#'   to half of unfished biomass), on the scale of the simulated index
#'   (`Efficiency * NomIndex^Beta`), multiplied by a per-simulation value.
#'   Accepted forms:
#'   - `NULL` (default): no reference value.
#'   - Non-negative scalar: interpreted as the CV of a lognormal distribution;
#'     a unique multiplier is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Ref), sdconv(1, Ref))` internally. `0` gives a
#'     multiplier of 1.
#'   - Length-`nSim` vector or named array: multipliers used directly.
#' @param DataCV `numeric` or `NULL`. Coefficient of variation reported in
#'   the `CV` slot of the generated index data, which MPs can use. A scalar,
#'   or values named by year that apply from that year onwards (e.g.
#'   `c('1990' = 0.4, '2010' = 0.2)`). Real data CVs take precedence in
#'   historical years, and CVs supplied for projection years in `OM@Data`
#'   take precedence in projection years. `NULL` (default) uses the real data
#'   CV of the last historical year where there is one, otherwise
#'   `SimControl(DataCV)`. Also get or set with [DataCV()] and `DataCV<-`.
#' @param Beta `numeric` or `NULL`. Hyperstability/hyperdepletion parameter:
#'   `Observed_t = Efficiency * NomIndex_t^Beta * Error_t` (Harley et al.
#'   2001). `Beta < 1` is hyperstable, `Beta > 1` hyperdeplete. Must be
#'   positive. Default `NULL` (`Beta = 1`, proportional). A supplied value is
#'   used as-is for historical/projected data generation; only `Efficiency`
#'   is then fit when conditioning on real data. If `NULL` and
#'   `SimControl(EstimateBeta = TRUE)`, `Beta` is instead estimated per
#'   simulation by log-linear regression when conditioned -- otherwise it
#'   stays at `1`.
#' @param AC `numeric`, `array`, or `NULL`. Lag-1 autocorrelation of index
#'   residuals, one value per simulation. Values must be within (-1, 1),
#'   exclusive. A scalar applies to all simulations, a length-2 vector
#'   `c(lower, upper)` gives one value per simulation drawn from a uniform
#'   distribution, and a length-`nSim` vector is used directly. Default `NULL`
#'   (`AC = 0`, independent errors). When `Error` is generated from `CV`, the
#'   log errors follow a stationary AR(1) process with this autocorrelation,
#'   with mean error 1 and log-scale standard deviation `sdconv(1, CV)`. When
#'   conditioning on real data, an `AC` supplied here is used for projection
#'   years; otherwise `AC` is estimated from the historical residuals.
#' @param qInc `numeric` or `NULL`. Annual percentage change in index
#'   catchability during projection years (e.g. `2` = 2% per year; negative
#'   values indicate declining catchability). Default `NULL` (no change).
#'   Accepted forms:
#'   - Scalar: applied to all simulations.
#'   - Length-2 vector `c(lower, upper)`: one value per simulation drawn from
#'     a uniform distribution.
#'   - Length-`nSim` vector: used directly.
#'
#'   See "Projected Change in Catchability" below. Also get or set with
#'   [qInc()] and `qInc<-`.
#' @param MaxGap `numeric(1)` or `NULL`. Largest number of calendar years
#'   between the last historical observation of the index and the last
#'   historical year for which the index is generated in projection years.
#'   `NULL` (default) is `5`; `0` generates the index only when it is observed
#'   in the last historical year; `Inf` or `NA` always generates it. An index
#'   with no historical observations is not generated. Index values supplied
#'   for projection years in `OM@Data` are always used. Also get
#'   or set with [MaxGap()] and `MaxGap<-`.
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Projected Change in Catchability
#'
#' In projection years, simulated index values are
#' `Efficiency * NomIndex_t^Beta * Error_t * (1 + qInc / 100)^(t - t_H)`,
#' where `t - t_H` is the time in years since the last historical time step
#' (a fraction of a year for each season of a seasonal model). The multiplier
#' is 1 in the historical period, so `qInc` has no effect on historical data,
#' conditioning on real data, or `Ref`. Index values supplied for projection
#' years in `OM@Data` are used as-is.
#'
#' ## Observation Error Generation
#'
#' The `Error` array (`nSim x nYear`) is obtained in one of three ways, in
#' order of precedence:
#'
#' 1. **User-supplied `Error`**: stored directly; `CV` is not used.
#' 2. **Stochastic from `CV`**: if `Error` is `NULL` and `CV` is specified,
#'    Internally the model generates a lognormal array where each cell is
#'    `rlnorm(1, mconv(1, CV[sim]), sdconv(1, CV[sim]))`. `CV` is first
#'    expanded to length `nSim` internally — a length-2 input
#'    triggers a stochastic draw of a unique CV per simulation.
#' 3. **Conditioned from real data**: [Simulate()] sets the historical years
#'    of `Error` from the residuals between the observed and simulated index,
#'    and generates projection years from `CV` and `AC` (see "Conditioning on
#'    Real Data").
#'
#' `Ref` is expanded internally: a scalar is treated as the CV of
#' a lognormal from which one reference value multiplier per simulation is
#' drawn; a length-`nSim` input is used directly.
#'
#' ## Conditioning on Real Data
#'
#' When real index data are provided (via `OM@Data`), [Simulate()]
#' conditions the observation model and the following slots are estimated:
#'
#' - `Efficiency` (`q`) is always fit per simulation by log-linear regression
#'   of `log(Observed)` on `log(NomIndex)`, under
#'   `Observed_t = Efficiency * NomIndex_t^Beta`. `Beta` is jointly estimated
#'   the same way, but only when `SimControl(EstimateBeta = TRUE)` and the
#'   user has not already supplied a value for that index; otherwise `Beta`
#'   is held fixed (at the user's value, or `1`). When estimated, each
#'   simulation gets an explicit fit `Status` (`"estimated"`,
#'   `"fixed_low_variance"`, `"fixed_not_significant"`,
#'   `"fixed_bounds"`, ...) plus `SE_Beta`, a confidence interval, `R2`, and
#'   `PValue`, stored in `Misc$BetaFit` (see [EstimateBeta()]) and
#'   summarized by [IndexFitTable()].
#' - `Stats`: a data frame computed by [CalcResidualStats()] with columns
#'   `Sim`, `AC` (weighted lag-1 autocorrelation of log-residuals), `SD`
#'   (standard deviation of log-residuals), and `NA_Season` (a list-column
#'   indicating which seasons, if any, have no observations in any year).
#' - `AC` and `CV`: values supplied by the user before conditioning are kept
#'   and used for projection years. Otherwise they are set from `Stats$AC`
#'   and `Stats$SD` (the latter converted from log-scale SD to CV via the
#'   inverse of [sdconv()]).
#' - `Error`: the historical years are the residuals, so historical data
#'   match the observed index. The projection years are taken from a
#'   user-supplied `Error`, or else generated by [GenResiduals()] from `CV`
#'   and `AC`, starting from the last historical residual.
#'
#' `Stats` always holds the estimated residual statistics. Values are
#' recorded as user-supplied in `Misc$UserSupplied` when the object is
#' populated.
#'
#' ## Selectivity Notes
#'
#' Index selectivity is set only on the [indicesobs-class] object, and is used
#' both when conditioning on real index data and when generating simulated
#' index data. CPUE indices named after a fleet use that fleet's selectivity
#' when `Selectivity` is `NULL`. Survey indices must set `Selectivity` with one
#' of the forms described under the `Selectivity` parameter.
#'
#' ## Attaching to an Obs Object
#'
#' ```
#' CPUE(obs)   <- IndicesObs(CV = 0.3)
#' Survey(obs) <- IndicesObs(CV = 0.2, Selectivity = "SBiomass")
#'
#' # index of ages 3-5
#' Survey(obs) <- IndicesObs(Selectivity = c('3' = 1, '4' = 1, '5' = 1))
#' ```
#'
#' @return
#' - `IndicesObs()` returns an [indicesobs-class] object.
#' - `CPUE()`, `Survey()` return the corresponding [indicesobs-class] slot
#'   from an [obs-class] object `x`.
#' - `CPUE<-`, `Survey<-` return `x` with the slot replaced.
#'
#' @seealso
#' - [indicesobs-class] for the class definition and slot-level documentation.
#' - [Obs()] for the enclosing observation model constructor.
#' - [CalcResidualStats()] for residual statistic computation.
#' - [GenResiduals()], [ApplyAC()] for projection error generation.
#' - [Simulate()] for conditioning the observation model on real data.
#'
#' @family obs
#'
#' @example man-examples/IndicesObs.R
#'
#' @include class-unions.R
#' @name IndicesObs
#' @export
IndicesObs <- function(CV          = NULL,
                       Error       = NULL,
                       Areas       = NULL,
                       Units       = NULL,
                       Selectivity = NULL,
                       TruncSD     = 2,
                       Ref         = NULL,
                       Beta        = NULL,
                       AC          = NULL,
                       qInc        = NULL,
                       MaxGap      = NULL,
                       DataCV      = NULL,
                       Misc        = list()) {
  .Object <- methods::new("indicesobs")
  if (!is.null(CV))          .Object@CV          <- CV
  if (!is.null(Error))       .Object@Error       <- Error
  if (!is.null(Areas))       .Object@Areas       <- Areas
  if (!is.null(Units))       .Object@Units       <- Units
  if (!is.null(Selectivity)) .Object@Selectivity <- Selectivity
  if (!is.null(TruncSD))     .Object@TruncSD     <- TruncSD
  if (!is.null(Ref))         .Object@Ref         <- Ref
  if (!is.null(Beta))        .Object@Beta        <- Beta
  if (!is.null(AC))          .Object@AC          <- AC
  if (!is.null(qInc))        .Object@qInc        <- qInc
  if (!is.null(MaxGap))      .Object@MaxGap      <- as.numeric(MaxGap)
  if (!is.null(DataCV))      .Object@DataCV      <- DataCV

  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}


#' LifeHistoryObs Constructor
#'
#' Construct a [lifehistoryobs-class] object defining the observation error of
#' the life history of a stock (growth, weight, natural mortality, maturity,
#' fecundity, stock-recruit relationship and depletion). The observed life
#' history is stored in `Data@LifeHistory`.
#'
#' @param Ages `list`. Not used: the ages are copied to the data without error
#'   when any component is observed.
#' @param Length `list`. Observation error of growth, as
#'   `list(Bias = , CV = )`. Default `list()`: growth is not observed.
#' @param Weight `list`. Observation error of the length-weight relationship or
#'   weight-at-age. Default `list()`.
#' @param NaturalMortality `list`. Observation error of natural mortality.
#'   Default `list()`.
#' @param Maturity `list`. Observation error of maturity. Default `list()`.
#' @param Fecundity `list`. Observation error of fecundity. Default `list()`.
#' @param SRR `list`. Observation error of the stock-recruit relationship.
#'   Default `list()`.
#' @param Spatial `list`. Not used: spatial structure is not observed.
#' @param Depletion `list`. Observation error of the final depletion. Default
#'   `list()`.
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Specifying Observation Error
#'
#' Each component is a list with the elements `Bias` and `CV`, or `list()` when
#' the component is not observed. A component of the stock that has no
#' observation error specified is not stored in `Data@LifeHistory`, unless it
#' is supplied with the real data in `OM@Data`.
#'
#' `Bias` and `CV` are either one specification used for all parameters of the
#' component, or a list (or named vector) with one specification per
#' parameter, named by the names in the `Pars` of the stock's component, e.g.
#' `Linf`, `K` and `t0` for `vonBert` growth. A parameter that is not named is
#' observed without error. Besides `Pars`, these can be named:
#' `CVatAge` for `Length`; `R0`, `SD` and `AC` for `SRR`; `Final` for
#' `Depletion`. They are only observed when named.
#'
#' - `Bias`: multiplicative bias. A single number is the CV of a lognormal
#'   from which one bias per simulation is drawn; a vector `c(lower, upper)`
#'   draws one bias per simulation from a uniform distribution (use
#'   `c(1.2, 1.2)` for a bias of 1.2 in every simulation); a vector of length
#'   `nSim` gives the bias of each simulation.
#' - `CV`: coefficient of variation of the lognormal observation error, with
#'   one draw per simulation and parameter. A vector `c(mean, sd)` draws the
#'   CV of each simulation from a lognormal. The CV is stored with the
#'   observed parameter in `Pars`, as `<parameter>_CV`.
#'
#' The observed value of a parameter is its value in the last historical time
#' step multiplied by the bias and the error. The observed schedules
#' (`MeanAtAge`, `MeanAtLength`) are regenerated from the observed parameters
#' in the order growth, weight, natural mortality, maturity and fecundity, so a
#' component that depends on another, such as weight at age from length at age,
#' follows the observed values. The steepness of a Beverton-Holt
#' relationship is bounded to 0.2 to 0.999, and the autocorrelation to -0.99
#' to 0.99. A simulation whose observed schedule is not valid is redrawn.
#'
#' A component that the stock defines by `MeanAtAge` or `MeanAtLength` without
#' `Pars` takes an unnamed `Bias` or `CV`, which scales the whole schedule by a
#' single factor per simulation, bounded to the range 0 to 1 for maturity.
#' Without an observation error specification, a stock's schedule is not
#' observed.
#'
#' The observed life history is fixed at the last historical year and is the
#' same in projection years. The specification is read from the `Obs` of the
#' first fleet of the stock complex that has one, and applies to every stock in
#' the complex, with independent draws.
#'
#' ## Conditioning on Real Data
#'
#' Components supplied for a stock in `OM@Data` (see [Data()]) are used as
#' supplied. When the real data and the OM have the same parameter,
#' [Simulate()] sets the bias of each simulation to the ratio of the real value
#' to the simulated value, and the CV to the reported `<parameter>_CV`. The
#' values are stored by stock in the `Stock` element of the component. Values
#' supplied by the user in `Bias` and `CV` are kept. Components defined by
#' schedules in both are compared through the sum of the final-year schedule.
#'
#' @return A [lifehistoryobs-class] object.
#'
#' @seealso
#' - [lifehistoryobs-class] for the class definition.
#' - [Obs()] for the enclosing observation model constructor.
#' - [lifehistorydata-class] for the observed values.
#' - [ExploitationObs()] for the observation of selectivity and retention.
#'
#' @family obs
#'
#' @examples
#' # Growth and natural mortality observed with error
#' lho <- LifeHistoryObs(
#'   Length           = list(CV = list(Linf = 0.05, K = 0.15)),
#'   NaturalMortality = list(CV = 0.2)
#' )
#'
#' # Natural mortality overestimated by 20% in every simulation
#' lho <- LifeHistoryObs(NaturalMortality = list(Bias = c(1.2, 1.2)))
#'
#' # Perfect knowledge of the maturity ogive
#' lho <- LifeHistoryObs(Maturity = list(CV = 0))
#'
#' @include class-unions.R
#' @name LifeHistoryObs
#' @export
LifeHistoryObs <- function(Ages             = list(),
                           Length           = list(),
                           Weight           = list(),
                           NaturalMortality = list(),
                           Maturity         = list(),
                           Fecundity        = list(),
                           SRR              = list(),
                           Spatial          = list(),
                           Depletion        = list(),
                           Misc             = list()) {
  Object <- methods::new(
    "lifehistoryobs",
    Ages             = Ages,
    Length           = Length,
    Weight           = Weight,
    NaturalMortality = NaturalMortality,
    Maturity         = Maturity,
    Fecundity        = Fecundity,
    SRR              = SRR,
    Spatial          = Spatial,
    Depletion        = Depletion,
    Misc             = Misc
  )
  .PopulateObsSpecs(Object, .LHComponents, "LifeHistoryObs")
}


#' Obs Constructor and Accessor
#'
#' Construct an [obs-class] object defining the observation error structure for
#' each data type in the operating model, or extract the `Obs` slot from an
#' enclosing object.
#'
#' @param Name `character(1)` or an S4 object. Unique identifier for this
#'   observation model. Default `NULL`.
#'
#'   If `Name` is an [om-class] object, `Obs()` returns `Name@Obs` (the
#'   top-level observation model list) rather than constructing a new object.
#'   If `Name` is a [hist-class] or [mse-class] object, `Obs()` returns
#'   the `Obs` slot of the embedded OM (`Name@OM@Obs`). See *Pass-Through
#'   Access* in Details.
#'
#' @param LifeHistory A [lifehistoryobs-class] object, or `NULL` (default).
#'   When `NULL`, an empty [lifehistoryobs-class] is created via
#'   [LifeHistoryObs()], which defines the observation error of the life history
#'   stored in `Data@LifeHistory`.
#'
#' @param Exploitation An [exploitationobs-class] object, or `NULL` (default).
#'   When `NULL`, an empty [exploitationobs-class] is created via
#'   [ExploitationObs()], which defines the observation error of the exploitation
#'   pattern stored in `Data@Exploitation`.
#'
#' @param Effort An [effortobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [effortobs-class] is created via [EffortObs()].
#'
#' @param Landings A [catchobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [catchobs-class] is created via [CatchObs()]. Defines
#'   observation error for landed catch.
#'
#' @param Discards A [catchobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [catchobs-class] is created via [CatchObs()]. Defines
#'   observation error for discarded catch.
#'
#' @param CPUE An [indicesobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [indicesobs-class] is created via [IndicesObs()].
#'   Defines observation error for CPUE indices.
#'
#' @param Survey An [indicesobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [indicesobs-class] is created via [IndicesObs()].
#'   Defines observation error for fishery-independent survey indices.
#'
#' @param LandingsAtAge A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for landed catch-at-age composition.
#'
#' @param DiscardsAtAge A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for discarded catch-at-age composition.
#'
#' @param LandingsAtSize A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for landed catch-at-length composition.
#'
#' @param DiscardsAtSize A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for discarded catch-at-length composition.
#'
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Sub-Object Initialisation
#'
#' All sub-object slots are initialised to empty objects of the appropriate
#' class when not supplied. Empty sub-objects cause the model to skip
#' observation error for that data type (e.g., an empty `Landings` slot means
#' catch is reported without observation error). To activate observation error
#' for a data type, supply a sub-object with at least `CV` specified.
#'
#' ## Pass-Through Access
#'
#' When `Name` is an [om-class], [hist-class], or [mse-class] object, `Obs()`
#' extracts the `Obs` slot of the embedded OM rather than constructing a new
#' object:
#'
#' - `Obs(om)` → `om@Obs`
#' - `Obs(hist)` → `hist@OM@Obs`
#' - `Obs(mse)` → `mse@OM@Obs`
#'
#' The result in all cases is the two-level named list
#' `[[stock_complex]][[fleet_name]]` of [obs-class] objects stored in the OM.
#'
#' ## Attaching to an OM
#'
#' A single [obs-class] object can be assigned to a specific stock complex and
#' fleet slot with:
#' ```r
#' Obs(om) <- MyObs           # replaces the full Obs list
#' ```
#' Individual sub-objects are more commonly set directly on the `obs` object
#' before it is inserted into the OM.
#'
#' @return
#' - `Obs()` returns an [obs-class] object. When `Name` is an [om-class],
#'   [hist-class], or [mse-class] object, returns the `Obs` slot of the
#'   embedded OM (a two-level named list of [obs-class] objects).
#' - `Obs<-` returns `x` with the `Obs` slot replaced by `value`.
#'
#' @seealso
#' - [obs-class] for the class definition and slot-level documentation.
#' - [CatchObs()], [EffortObs()], [IndicesObs()], [CompObs()] for
#'   sub-object constructors.
#' - [LifeHistoryObs()], [ExploitationObs()] for the life history and exploitation sub-objects.
#' - [data-class] and [Data()] for the complementary observed-values object.
#' - [OM()] for the operating model constructor.
#' - [ConvertObs()] for converting legacy [Obs-legacy-class] objects.
#'
#' @family obs
#'
#' @example man-examples/Obs.R
#'
#' @name Obs
#' @rdname Obs
#'
#' @export
Obs <- function(Name           = NULL,
                LifeHistory    = NULL,
                Exploitation   = NULL,
                Effort         = NULL,
                Landings       = NULL,
                Discards       = NULL,
                CPUE           = NULL,
                Survey         = NULL,
                LandingsAtAge  = NULL,
                DiscardsAtAge  = NULL,
                LandingsAtSize = NULL,
                DiscardsAtSize = NULL,
                Misc           = list()) {

  if (inherits(Name, "om"))
    return(Name@Obs)

  if (inherits(Name, c("hist", "mse")))
    return(Name@OM@Obs)

  if (is.null(LifeHistory))    LifeHistory    <- new("lifehistoryobs")
  if (is.null(Exploitation))   Exploitation   <- new("exploitationobs")
  if (is.null(Effort))         Effort         <- new("effortobs")
  if (is.null(Landings))       Landings       <- new("catchobs")
  if (is.null(Discards))       Discards       <- new("catchobs")
  if (is.null(CPUE))           CPUE           <- new("indicesobs")
  if (is.null(Survey))         Survey         <- new("indicesobs")
  if (is.null(LandingsAtAge))  LandingsAtAge  <- new("compobs")
  if (is.null(DiscardsAtAge))  DiscardsAtAge  <- new("compobs")
  if (is.null(LandingsAtSize)) LandingsAtSize <- new("compobs")
  if (is.null(DiscardsAtSize)) DiscardsAtSize <- new("compobs")

  .Object <- methods::new("obs")

  if (!is.null(Name))
    .Object@Name <- Name

  .Object@LifeHistory    <- LifeHistory
  .Object@Exploitation   <- Exploitation
  .Object@Effort         <- Effort
  .Object@Landings       <- Landings
  .Object@Discards       <- Discards
  .Object@CPUE           <- CPUE
  .Object@Survey         <- Survey
  .Object@LandingsAtAge  <- LandingsAtAge
  .Object@DiscardsAtAge  <- DiscardsAtAge
  .Object@LandingsAtSize <- LandingsAtSize
  .Object@DiscardsAtSize <- DiscardsAtSize
  .Object@Misc           <- Misc

  methods::validObject(.Object)
  .Object
}

#' @rdname Obs
#' @param x An [om-class] object.
#' @param value A two-level named list of [obs-class] objects, or a single
#'   [obs-class] object, to assign to the `Obs` slot.
#' @export
`Obs<-` <- function(x, value) {
  .CheckClass(x, "om", "x")

  OM <- x
  Obs <- value

  Complexes    <- Complexes(OM)
  nComplex     <- length(Complexes)
  ComplexNames <- names(Complexes)

  if (is.null(ComplexNames) || nComplex < 1) {
    Complexes    <- MakeNamedList(StockNames(OM))
    for (i in seq_along(Complexes))
      Complexes[[i]] <- i
    ComplexNames <- StockNames(OM)
    nComplex     <- length(Complexes)
  }

  if (is.null(ComplexNames) || nComplex < 1)
    cli::cli_abort("Add `Stock` object(s) to `OM` first")

  FleetNames <- FleetNames(OM)
  nFleet     <- length(FleetNames)

  # validate and name a flat list of obs objects.
  check_and_name_obs <- function(obs_list) {
    cls <- purrr::map_chr(obs_list, class)
    if (any(cls != "obs"))
      cli::cli_abort(c(
        'x' = 'All elements of `value` must be a {.help MSEtool::Obs} object',
        'i' = 'Current classes of `value` are: {.val {cls}}'
      ))

    if (length(obs_list) < nFleet)
      cli::cli_abort(c(
        'x' = 'Each complex must have at least one `Obs` object per fleet',
        'i' = 'Expected {.val {nFleet}} fleet{?s}, got {.val {length(obs_list)}}'
      ))

    # Name the fleet elements positionally
    names(obs_list)[seq_len(nFleet)] <- FleetNames

    # Validate survey elements (beyond nFleet)
    nSurvey <- length(obs_list) - nFleet
    if (nSurvey > 0) {
      survey_names <- names(obs_list)[seq(nFleet + 1, length(obs_list))]

      if (any(is.null(survey_names)) || any(nchar(survey_names) == 0))
        cli::cli_abort(c(
          'x' = 'Survey `Obs` objects (beyond fleet elements) must be explicitly named',
          'i' = 'Provide unique names for elements {nFleet + 1} to {length(obs_list)}'
        ))

      all_names <- names(obs_list)
      if (anyDuplicated(all_names))
        cli::cli_abort(c(
          'x' = 'All names in `value` must be unique across fleets and surveys',
          'i' = 'Duplicated name{?s}: {.val {all_names[duplicated(all_names)]}}'
        ))
    }

    obs_list
  }

  # Case 1: single obs object — replicate across all complexes and fleets
  if (inherits(Obs, "obs")) {
    OM@Obs <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, Obs))
    return(OM)
  }

  if (inherits(Obs, "list")) {
    is_nested <- purrr::every(Obs, is.list)

    # Case 2: nested list [complex][fleet + surveys]
    if (is_nested) {
      if (length(Obs) != nComplex)
        cli::cli_abort(c(
          'x' = 'Nested `value` must have one element per complex',
          'i' = 'Expected {.val {nComplex}} complex{?es}, got {.val {length(Obs)}}'
        ))

      Obs <- purrr::map(Obs, check_and_name_obs)
      names(Obs) <- ComplexNames
      OM@Obs <- Obs
      return(OM)
    }

    # Case 3: flat list of obs objects — replicate across all complexes
    named_obs <- check_and_name_obs(Obs)
    OM@Obs <- MakeNamedList(ComplexNames, named_obs)
    return(OM)
  }

  .AssignSlot(OM, Obs, 'Obs')
}
