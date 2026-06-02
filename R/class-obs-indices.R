#' The `indicesobs` S4 Class
#'
#' Defines the observation error structure for abundance or biomass indices
#' (CPUE or survey). Used in the `CPUE` and `Survey` slots of an [obs-class]
#' object. Objects are typically created via [IndicesObs()], which documents
#' all parameters in detail.
#'
#' @slot CV `numeric` or `NULL`. Coefficient of variation of index observation
#'   error. See [IndicesObs()].
#' @slot Error `numeric` array or `NULL`. Lognormal observation error
#'   multipliers (`nSim x nYear`). May be supplied directly via [IndicesObs()],
#'   generated stochastically from `CV`, or
#'   estimated from real data during [ConditionObs()]. See [IndicesObs()].
#' @slot Beta `numeric` array or `NULL`. Hyperstability/hyperdepletion
#'   parameter. Reserved for future use; currently not implemented. See
#'   [IndicesObs()].
#' @slot AC `numeric` array or `NULL`. Lag-1 autocorrelation of index
#'   residuals, one value per simulation. When supplied by the user, overrides
#'   the value estimated during conditioning. See [IndicesObs()].
#' @slot Years `numeric` or `NULL`. Calendar years over which the observation
#'   error is conditioned. See [IndicesObs()].
#' @slot Areas `numeric` or `NULL`. Integer indices of the spatial areas
#'   contributing to this index. See [IndicesObs()].
#' @slot Units `character` or `NULL`. Units of the index (`"Biomass"`,
#'   `"Number"`, or `"Recruitment"`). See [IndicesObs()].
#' @slot Selectivity `array`, `character`, `numeric`, or `list`. Selectivity
#'   specification for this index. See [IndicesObs()].
#' @slot Type `character`. Index type identifier. See [IndicesObs()].
#' @slot Ref `numeric` array or `NULL`. Reference index value, one per
#'   simulation. See [IndicesObs()].
#' @slot Efficiency `numeric` array or `NULL`. Catchability coefficient `q`
#'   relating the nominal index to the population quantity. Calculated
#'   internally during conditioning as the ratio of mean observed index to
#'   mean simulated index; not set by the user.
#' @slot TruncSD `numeric(1)` or `NULL`. Number of standard deviations at
#'   which to truncate the lognormal residual distribution when generating
#'   projection errors. Default `2`. See [IndicesObs()].
#' @slot Stats `data.frame` or `NULL`. Residual statistics computed during
#'   conditioning by [CalcResidualStats()]. Contains columns `Sim`, `AC`
#'   (weighted lag-1 autocorrelation), `SD` (standard deviation), and
#'   `NA_Season` (a list-column of seasons with no observations). Populated
#'   internally; not set by the user.
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @seealso 
#'  - [IndicesObs()] for the constructor and full parameter
#'   documentation. 
#'  - [obs-class] for the enclosing observation model object.
#'  - [CalcResidualStats()] for how `Stats` is computed.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name indicesobs-class
setClass(
  "indicesobs",
  slots = c(
    CV          = "num.array.null",
    Error       = "num.array.null",
    Beta        = "num.array.null",
    AC          = "num.array.null",
    Years       = "num.array.null",
    Areas       = "num.null",
    Units       = "char.null",
    Selectivity = "array.char.num.list",
    Type        = "character",
    Ref         = "num.array.null",
    Efficiency  = "num.array.null",
    TruncSD     = "num.null",
    Stats       = "df.null",
    Misc        = "list"
  )
)