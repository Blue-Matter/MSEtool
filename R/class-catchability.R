#' Catchability Class
#'
#' The `catchability` class defines catchability assumptions, including 
#' optional stochastic or incremental changes to gear efficiency over time.
#'
#' @slot Efficiency Gear efficiency (q).
#' @slot qCV Coefficient of variation for catchability.
#' @slot qInc Incremental changes in catchability.
#' @slot Misc Miscellaneous list.
#'
#' @include class-unions.R
setClass(
  "catchability",
  slots = c(
    Efficiency = "num.array.null",
    qCV = "num.array.null",
    qInc = "num.array.null",
    Misc = "list"
  )
)

setValidity("catchability", function(object) {
  # TODO: dimension and non-negativity checks
  TRUE
})
