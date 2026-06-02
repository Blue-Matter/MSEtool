#' The `lifehistoryobs` S4 Class
#'
#' Defines the observation error structure for life-history parameters
#' (growth, maturity, natural mortality, etc.). Used in the `LifeHistory` slot
#' of an [obs-class] object. Objects are created via [LifeHistoryObs()].
#'
#' @slot Ages `list`. Observation error specification for age structure
#'   parameters. See [LifeHistoryObs()].
#' @slot Length `list`. Observation error specification for growth parameters.
#'   See [LifeHistoryObs()].
#' @slot Weight `list`. Observation error specification for weight-at-age or
#'   length-weight parameters. See [LifeHistoryObs()].
#' @slot NaturalMortality `list`. Observation error specification for natural
#'   mortality. See [LifeHistoryObs()].
#' @slot Maturity `list`. Observation error specification for maturity
#'   schedules. See [LifeHistoryObs()].
#' @slot Fecundity `list`. Observation error specification for fecundity
#'   parameters. See [LifeHistoryObs()].
#' @slot SRR `list`. Observation error specification for stock-recruitment
#'   parameters. See [LifeHistoryObs()].
#' @slot Spatial `list`. Observation error specification for spatial
#'   parameters. See [LifeHistoryObs()].
#' @slot Depletion `list`. Observation error specification for initial
#'   depletion. See [LifeHistoryObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' This class is a placeholder. Each slot mirrors the corresponding sub-object
#' of [lifehistorydata-class] and will eventually hold an error structure
#' (bias, CV) for the associated life-history parameter. Slots are currently
#' untyped lists and are not populated during model runs. See [LifeHistoryObs()]
#' for further details.
#'
#' @seealso 
#' - [LifeHistoryObs()] for the constructor. 
#' - [obs-class] for the enclosing object. 
#' - [lifehistorydata-class] for the complementary observed-values class.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name lifehistoryobs-class
setClass(
  "lifehistoryobs",
  slots = c(
    Ages             = "list",
    Length           = "list",
    Weight           = "list",
    NaturalMortality = "list",
    Maturity         = "list",
    Fecundity        = "list",
    SRR              = "list",
    Spatial          = "list",
    Depletion        = "list",
    Misc             = "list"
  )
)
