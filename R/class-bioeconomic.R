#' Bioeconomic Class
#'
#' The `bioeconomic` class stores revenue, cost, and investment information
#' associated with a [Fleet()] and a [Stock()].
#' 
#' See [Bioeconomic()] for details.
#'
#' @slot Revenue Revenue array 
#' @slot Cost Operating cost per unit of effort.
#' @slot Investment Cost of adding a new unit of effort.
#' @slot Disinvestment Cost of removing a unit of effort.
#' @slot Depreciation Depreciation rate of effort units.
#' @slot Discount Discount factor.
#' @slot Misc Miscellaneous list.
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
