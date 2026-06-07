#' The `catchability` S4 Class
#'
#' Defines gear or vessel efficiency for use in a [fleet-class] object.
#' Objects are typically created via the [Catchability()] constructor, which
#' documents all parameters in detail.
#'
#' @slot Efficiency `numeric`, array, or `NULL`. Gear efficiency (`q`).
#'   See [Catchability()].
#' @slot qCV `numeric` or `NULL`. Coefficient of variation for stochastic
#'   variation in catchability during projected years. See [Catchability()].
#' @slot qInc `numeric` or `NULL`. Annual percentage increase in catchability
#'   during projected years. See [Catchability()].
#' @slot Theta `numeric`, array, or `NULL`. Overdispersion parameter
#'   \eqn{\theta} of the negative binomial within-trip catch distribution
#'   (`Sim`). Used by bag-limit management procedures to compute the
#'   probability that a single trip catches at or above the bag limit. 
#'   See [Catchability()].
#' @slot Misc `list`. Miscellaneous additional inputs. Used internally.
#'
#' @seealso
#' - [Catchability()] for the constructor and full parameter documentation.
#' - [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name catchability-class
setClass(
  "catchability",
  slots = c(
    Efficiency = "num.array.null",
    qCV        = "num.array.null",
    qInc       = "num.array.null",
    Theta      = "num.array.null",
    Misc       = "list"
  )
)

setValidity("catchability", function(object) {
  # TODO: dimension and non-negativity checks
  # TODO: depletion-dependent catchability (gamma != 1) not yet implemented.
  #   Currently the bag-limit model assumes catch rates scale proportionally
  #   with abundance (gamma = 1).
  TRUE
})
