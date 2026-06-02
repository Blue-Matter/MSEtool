#' The `effortdata` S4 Class
#'
#' Stores time series observations of fishing effort and associated uncertainty.
#' Used in the `Effort` slot of a [data-class] object. Objects are typically
#' created via [EffortData()], which documents all parameters in detail.
#'
#' @slot Name `character` or `NULL`. Names of the fleets
#' @slot Value `array` or `NULL`. Observed effort values with dimensions
#'   `[nYear x nFleet]`. See [EffortData()].
#' @slot CV `array` or `NULL`. Coefficients of variation matching the
#'   dimensions of `Value`. See [EffortData()].
#' @slot Units `character` or `NULL`. Units of effort measurement per fleet
#'   (e.g., `"days"`, `"trips"`, `"unitless"`). See [EffortData()].
#'
#' @seealso [EffortData()] for the constructor and full parameter documentation.
#'   [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name effortdata-class
NULL

setClass(
  "effortdata",
  slots = c(
    Name  = "char.null",
    Value = "array.null",
    CV    = "array.null",
    Units = "char.null"
  )
)

