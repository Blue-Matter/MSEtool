#' Composition Observation Component
#'
#' Internal class describing observation error associated with composition data.
#'
#' @slot SampleSize Sample sizes.
#' @slot ESS Effective sample sizes.
#' @slot Years Observation years.
#' @slot Bias Observation bias.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "CompObs",
  slots = c(
    SampleSize = "num.array.null",
    ESS        = "num.array.null",
    Years      = "num.null",
    Bias       = "num.array.null",
    Misc       = "list"
  )
)