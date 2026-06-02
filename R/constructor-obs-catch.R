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
#'     a unique CV per simulation is generated via [StructurePars()] before
#'     being used to generate the `Error` array.
#'   - Length-`nSim` named array: used directly, one CV per simulation.
#'
#'   When conditioning on real data via [ConditionObs()], `CV` is replaced by
#'   the simulation-specific standard deviation of historical log-residuals.
#' @param Bias `numeric` or `NULL`. Multiplicative observation bias. Accepted
#'   forms:
#'   - `NULL` (default): bias defaults to `1` (unbiased) internally.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique bias is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Bias), sdconv(1, Bias))`.
#'   - Length-`nSim` vector or named array: used directly, one bias per
#'     simulation.
#'
#'   When conditioning on real data via [ConditionObs()], `Bias` is replaced
#'   by the mean ratio of simulated to observed catch across historical years,
#'   per simulation.
#' @param Error `numeric` array or `NULL`. Pre-specified lognormal observation
#'   error multipliers with dimensions `nSim x nYear` and named dimnames
#'   (`Sim`, `Year`). When supplied, `Error` is used directly and `CV` is
#'   ignored for error generation. Values should be positive multipliers
#'   centred near 1. Default `NULL`, in which case `Error` is generated from
#'   `CV` internally. When conditioning on real data via
#'   [ConditionObs()], any user-supplied `Error` is replaced.
#' @param Years `numeric` vector or `NULL`. Calendar years over which to
#'   condition the observation error when real catch data are provided. When
#'   `NULL` (default), all historical years in the `Data` object are used.
#'   Has no effect when no real data are supplied.
#' @param Units `character(1)` or `NULL`. Units of catch measurement. Must be
#'   `"Biomass"` or `"Number"`. Determines how simulated catch is aggregated
#'   when generating pseudo-observed data. Default `NULL` (treated as
#'   `"Biomass"`).
#' @param Ref `numeric` or `NULL`. Reference catch value. Accepted forms:
#'   - `NULL` (default): no reference value.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique reference value is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Ref), sdconv(1, Ref))` internally.
#'   - Length-`nSim` vector or named array: used directly.
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
#' 3. **Conditioned from real data**: [ConditionObs()] estimates `CV`, `Bias`,
#'    and `Error` from historical residuals; any user-supplied values are
#'    replaced.
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
#' When real catch data are provided (via `OM@Data`) and [ConditionObs()] is
#' run, `CV` and `Bias` are estimated from the historical residuals between
#' simulated and observed catch, and `Error` is populated to cover both
#' historical and projection years. Any user-supplied `CV`, `Bias`, and `Error`
#' values are replaced.
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
#' - [ConditionObs()] for how these slots are estimated from real data.
#'
#' @family obs
#'
#' @examples
#' # Uniform 20% CV — same CV used for all simulations
#' co <- CatchObs(CV = 0.2)
#'
#' # Stochastic CV — unique CV per simulation drawn from lognormal(mean=0.2, sd=0.05)
#' co <- CatchObs(CV = c(0.2, 0.05))
#'
#' # Bias as a CV — unique bias per simulation drawn from lognormal(mean=1, sd=0.1)
#' co <- CatchObs(CV = 0.2, Bias = 0.1)
#'
#' # Pre-specified error array — bypasses CV entirely
#' err <- array(rlnorm(48 * 20), dim = c(48, 20),
#'              dimnames = list(Sim = 1:48, Year = 2001:2020))
#' co <- CatchObs(Error = err)
#'
#' # Attach to an obs object
#' obs <- Obs(Landings = CatchObs(CV = 0.2),
#'            Discards = CatchObs(CV = 0.3, Bias = 0.05))
#'
#' # Access slots via the generic Landings() / Discards() functions
#' Landings(obs)
#' Discards(obs)
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
                     Misc  = list()) {
  .Object <- methods::new("catchobs")
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Bias))  .Object@Bias  <- Bias
  if (!is.null(Error)) .Object@Error <- Error
  if (!is.null(Years)) .Object@Years <- Years
  if (!is.null(Units)) .Object@Units <- Units
  if (!is.null(Ref))   .Object@Ref   <- Ref
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}