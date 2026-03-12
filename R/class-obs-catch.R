#' Catch Observation Component
#'
#' Internal class describing observation error associated with catch data.
#'
#' @slot CV Coefficient of variation.
#' @slot Error Realized observation error.
#' @slot Bias Observation bias.
#' @slot Years Observation years.
#' @slot Ref Reference values.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
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
