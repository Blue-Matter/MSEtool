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
#'     a unique CV per simulation is generated via [StructurePars()] before
#'     being used to generate the `Error` array.
#'   - Length-`nSim` named array: used directly, one CV per simulation.
#' @param Bias `numeric` or `NULL`. Multiplicative observation bias. Accepted
#'   forms:
#'   - `NULL` (default): bias defaults to `1` (unbiased) internally.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique bias is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Bias), sdconv(1, Bias))`.
#'   - Length-`nSim` vector or named array: used directly, one bias per
#'     simulation.
#' @param Error `numeric` array or `NULL`. Pre-specified lognormal observation
#'   error multipliers with dimensions `nSim x nYear` and named dimnames
#'   (`Sim`, `Year`). When supplied, `Error` is used directly and `CV` is
#'   ignored for error generation. Default `NULL`, in which case `Error` is
#'   generated from `CV` internally.
#' @param Years `numeric` vector or `NULL`. Calendar years over which the
#'   observation error applies. Default `NULL` (all historical years).
#' @param Units `character(1)` or `NULL`. Units of effort
#'   (e.g., `"hours"`, `"trips"`, `"unitless"`). Default `NULL`.
#' @param Ref `numeric` or `NULL`. Reference effort value. Accepted forms:
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
#' **Note**: Effort observation error is not currently conditioned from observed
#' data.
#'
#' `Bias` is expanded internally: a scalar input is treated as
#' the CV of a lognormal from which one bias per simulation is drawn; a
#' length-`nSim` input is used directly.
#'
#' `Ref` is expanded internally using the same convention as
#' `Bias`.
#'
#' Unlike catch and index observation error, effort observation error is not
#' conditioned on real data; the user-supplied `CV`, `Bias`, and `Error` are
#' always used as specified.
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
                      Misc  = list()) {
  .Object <- methods::new("effortobs")
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

