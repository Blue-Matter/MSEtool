#' The `catchobs` S4 Class
#'
#' Defines the observation error structure for landed or discarded catch data.
#' Used in the `Landings` and `Discards` slots of an [obs-class] object.
#' Objects are typically created via [CatchObs()], which documents all
#' parameters in detail.
#'
#' @slot CV `numeric` or `NULL`. Coefficient of variation of catch observation
#'   error. See [CatchObs()].
#' @slot Error `numeric` array or `NULL`. Realised lognormal observation error
#'   multipliers (`nSim x nYear`). Populated internally during conditioning or
#'   simulation; see [CatchObs()].
#' @slot Bias `numeric` or `NULL`. Multiplicative observation bias. See
#'   [CatchObs()].
#' @slot Years `numeric` or `NULL`. Calendar years over which the observation
#'   error is conditioned. See [CatchObs()].
#' @slot Units `character` or `NULL`. Units of catch (`"Biomass"` or
#'   `"Number"`). See [CatchObs()].
#' @slot Ref `numeric` array or `NULL`. Reference catch values, one per
#'   simulation. See [CatchObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @seealso 
#' - [CatchObs()] for the constructor and full parameter documentation.
#' - [obs-class] for the enclosing observation model object.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name catchobs-class
setClass(
  "catchobs",
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