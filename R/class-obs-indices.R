
#' Index Observation Component
#'
#' Internal class describing observation error associated with abundance indices.
#'
#' @slot CV Coefficient of variation.
#' @slot Error Realized observation error.
#' @slot Beta Observation bias parameters.
#' @slot AC Autocorrelation parameters.
#' @slot Years Observation years.
#' @slot Selectivity Observation selectivity definition.
#' @slot Type Index type.
#' @slot Ref Reference values.
#' @slot Efficiency Catchability.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "indicesobs",
  slots = c(
    CV          = "num.array.null",
    Error       = "num.array.null",
    Beta        = "num.array.null",
    AC          = "num.array.null",
    Years       = "num.array.null",
    Areas       = "num.null",
    Units       = "char.null",
    Selectivity = "array.char.num.list",
    Type        = "character",
    Ref         = "num.array.null",
    Efficiency  = "num.array.null",
    TruncSD     = "num.null",   # default 2 
    Stats       = 'df.null',
    Misc        = "list"
  )
)
