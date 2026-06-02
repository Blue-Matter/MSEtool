#' The `bioeconomic` S4 Class
#'
#' Stores revenue, cost, and investment dynamics for fleet-level or stock-level
#' bioeconomic analyses. Objects are typically created via the [Bioeconomic()]
#' constructor, which documents all parameters in detail.
#'
#' **Note:** The [bioeconomic-class] is not currently used by the MSE
#' framework. It is included for future development.
#'
#' @slot Revenue `numeric` array or `NULL`. Revenue by simulation, fleet, and
#'   year. See [Bioeconomic()].
#' @slot Cost `numeric` or `NULL`. Operating cost per unit of effort. See
#'   [Bioeconomic()].
#' @slot Investment `numeric` or `NULL`. Cost of adding a unit of effort. See
#'   [Bioeconomic()].
#' @slot Disinvestment `numeric` or `NULL`. Cost of removing a unit of effort.
#'   See [Bioeconomic()].
#' @slot Depreciation `numeric` or `NULL`. Depreciation rate of effort units.
#'   See [Bioeconomic()].
#' @slot Discount `numeric` or `NULL`. Discount factor applied to future
#'   values. See [Bioeconomic()].
#' @slot Misc `list`. Miscellaneous additional inputs.
#'
#' @seealso [Bioeconomic()] for the constructor and full parameter
#'   documentation. [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name bioeconomic-class
setClass(
  "bioeconomic",
  slots = c(
    Revenue = "num.array.null",
    Cost = "num.array.null",
    Investment = "num.array.null",
    Disinvestment = "num.array.null",
    Depreciation = "num.array.null",
    Discount = "num.array.null",
    Misc = "list"
  )
)

setValidity("bioeconomic", function(object) {
  # TODO
  TRUE
})
