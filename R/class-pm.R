#' `pm` Class
#'
#' Stores the result of evaluating a performance metric (PM) function against
#' an [mse-class] object (or a list of [mse-class] objects, combined via
#' [CombineMSE()]). Replaces the legacy `PMobj` class.
#'
#' @slot Name Character. Short name of the performance metric.
#' @slot Caption Character. Description of the performance metric, used for
#'   plotting/reporting.
#' @slot Stat Array with dimensions `Sim x Stock x MP`. The underlying
#'   statistic (e.g. `SB/SBMSY`), averaged over the `Years` window, on its
#'   natural scale.
#' @slot Ref Numeric. Reference/threshold value the statistic is compared
#'   against.
#' @slot Prob Array with dimensions `Sim x Stock x MP`. Probability (proportion
#'   of years in `Years` meeting the objective) per simulation. Always scaled
#'   so that higher values are better outcomes.
#' @slot Mean Array with dimensions `Stock x MP`. `Prob` averaged over `Sim`.
#' @slot MPs Character vector. Names of the MPs evaluated.
#' @slot Years Numeric vector. Projection years over which the PM was
#'   calculated.
#'
#' @seealso [PM_FFMSY()], [PM_SBSBMSY()], [PM_Status()], [PM_Safety()],
#'   [PM_Rebuild()], [PM_Yield()], [PM_Stability()], [CombineMSE()]
#' @name pm-class
#' @export
setClass("pm",
         slots = c(
           Name    = "character",
           Caption = "character",
           Stat    = "array",
           Ref     = "numeric",
           Prob    = "array",
           Mean    = "array",
           MPs     = "character",
           Years   = "numeric"
         )
)
