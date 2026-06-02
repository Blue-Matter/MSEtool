#' The `effortobs` S4 Class
#'
#' Defines the observation error structure for fishing effort data. Used in the
#' `Effort` slot of an [obs-class] object. Objects are typically created via
#' [EffortObs()], which documents all parameters in detail.
#'
#' @slot CV `numeric` or `NULL`. Coefficient of variation of effort observation
#'   error. See [EffortObs()].
#' @slot Error `numeric` array or `NULL`. Realised lognormal observation error
#'   multipliers (`nSim x nYear`). Populated internally during simulation;
#'   see [EffortObs()].
#' @slot Bias `numeric` or `NULL`. Multiplicative observation bias. See
#'   [EffortObs()].
#' @slot Years `numeric` or `NULL`. Calendar years over which the observation
#'   error applies. See [EffortObs()].
#' @slot Units `character` or `NULL`. Units of fishing effort
#'   (e.g., `"hours"`, `"trips"`). See [EffortObs()].
#' @slot Ref `numeric` array or `NULL`. Reference effort values, one per
#'   simulation. See [EffortObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @seealso [EffortObs()] for the constructor and full parameter documentation.
#'   [obs-class] for the enclosing observation model object.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name effortobs-class
setClass(
  "effortobs",
  slots = c(
    CV    = "num.array.null",
    Error = "num.array.null",
    Bias  = "num.array.null",
    Years = "num.null",
    Units = "char.null",
    Ref   = "num.array.null",
    Misc  = "list"
  )
)