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
#'     a unique CV per simulation is generated via [StructurePars()] before
#'     being used to generate the `Error` array.
#'   - Length-`nSim` named array: used directly, one CV per simulation.
#'
#'   When conditioning on real data via [ConditionObs()], `CV` is replaced by
#'   the simulation-specific standard deviation of historical log-residuals
#'   estimated by [CalcResidualStats()]. Ignored if `Error` is supplied
#'   directly.
#' @param Error `numeric` array or `NULL`. Pre-specified lognormal observation
#'   error multipliers with dimensions `nSim x nYear` and named dimnames
#'   (`Sim`, `Year`). When supplied, `Error` is used directly and `CV` is
#'   ignored for error generation. When real index data are provided and
#'   [ConditionObs()] is run, `Error` is replaced by conditioned values
#'   regardless of whether it was user-supplied. Default `NULL`, in which case
#'   `Error` is generated from `CV` internally.
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
#' @param Selectivity Selectivity specification for this index. Accepted forms:
#'   - `NULL` (default): for CPUE indices, the corresponding fleet selectivity
#'     from `OM@Fleet` is used automatically. For survey indices with no
#'     selectivity specified, a warning is issued and flat selectivity
#'     (all ages equally selected) is assumed.
#'   - `"Biomass"`: flat selectivity of 1 for all age classes (equivalent to
#'     a biomass-weighted survey).
#'   - `"SBiomass"`: maturity-at-age is used as selectivity (equivalent to
#'     a spawning biomass index).
#'   - `"Obs"`: the selectivity array stored in this slot is used directly.
#'     In this case `Selectivity` must also contain the array (or list of
#'     arrays by stock) as set by the user.
#' @param TruncSD `numeric(1)`. Number of standard deviations at which to
#'   truncate the lognormal residual distribution when generating observation
#'   errors for projection years via [GenResiduals()]. Default `2`.
#' @param Ref `numeric` or `NULL`. Reference index value. Accepted forms:
#'   - `NULL` (default): no reference value.
#'   - Positive scalar: interpreted as the CV of a lognormal distribution; a
#'     unique reference value is drawn per simulation as
#'     `rlnorm(nSim, mconv(1, Ref), sdconv(1, Ref))` internally.
#'   - Length-`nSim` vector or named array: used directly.
#' @param Beta `numeric` or `NULL`. Hyperstability/hyperdepletion parameter
#'   relating true population size to the observed index. **Reserved for
#'   future use; currently has no effect.** Default `NULL`.
#' @param AC `numeric`, `array`, or `NULL`. Lag-1 autocorrelation of index
#'   residuals, one value per simulation
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
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
#' 3. **Conditioned from real data**: [ConditionObs()] estimates `CV`,
#'    `Efficiency`, `Stats`, and `Error` from historical residuals; any
#'    user-supplied values are replaced.
#'
#' `Ref` is expanded internally: a scalar is treated as the CV of
#' a lognormal from which one reference value per simulation is drawn; a
#' length-`nSim` input is used directly.
#'
#' ## Conditioning on Real Data
#'
#' When real index data are provided (via `OM@Data`) and [ConditionObs()] is
#' run, the following slots are estimated and populated internally:
#'
#' - `Efficiency` (`q`): estimated as the ratio of the mean observed index to
#'   the mean simulated nominal index, averaged over non-NA years.
#' - `Stats`: a data frame computed by [CalcResidualStats()] with columns
#'   `Sim`, `AC` (weighted lag-1 autocorrelation of log-residuals), `SD`
#'   (standard deviation of log-residuals), and `NA_Season` (a list-column
#'   indicating which seasons, if any, have no observations in any year).
#' - `Error`: lognormal error multipliers covering both historical and
#'   projection years, generated by [GenResiduals()] using the estimated `SD`
#'   and `AC`, with projection errors propagated via [ApplyAC()].
#'
#' The `Efficiency`, `Stats`, and `Error` slots are replaced during
#' conditioning regardless of whether the user supplied `Error` directly.
#'
#' ## Selectivity Notes
#'
#' CPUE indices automatically use the fleet selectivity from `OM@Fleet` when
#' `Selectivity` is `NULL`. For survey indices, the selectivity must be
#' specified explicitly via one of the four forms described under the
#' `Selectivity` parameter.
#'
#' ## Attaching to an Obs Object
#'
#' ```
#' CPUE(obs)   <- IndicesObs(CV = 0.3)
#' Survey(obs) <- IndicesObs(CV = 0.2, Selectivity = "SBiomass")
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
#' - [ConditionObs()] for how slots are estimated from real data.
#'
#' @family obs
#'
#' @examples
#' # Biomass CPUE with uniform 30% CV
#' io <- IndicesObs(CV = 0.3, Units = "Biomass")
#'
#' # Stochastic CV — unique CV per simulation drawn from lognormal(mean=0.3, sd=0.05)
#' io <- IndicesObs(CV = c(0.3, 0.05), Units = "Biomass")
#'
#' # Spawning biomass survey using maturity as selectivity
#' survey_obs <- IndicesObs(CV = 0.2, Selectivity = "SBiomass", TruncSD = 3)
#'
#' # Index covering only areas 1 and 2
#' io <- IndicesObs(CV = 0.25, Areas = c(1L, 2L))
#'
#' # Pre-specified error array — bypasses CV entirely
#' err <- array(rlnorm(48 * 20), dim = c(48, 20),
#'              dimnames = list(Sim = 1:48, Year = 2001:2020))
#' io <- IndicesObs(Error = err)
#'
#' # Attach to an obs object
#' obs <- Obs(CPUE   = IndicesObs(CV = 0.3),
#'            Survey = IndicesObs(CV = 0.2, Selectivity = "SBiomass"))
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
  
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}