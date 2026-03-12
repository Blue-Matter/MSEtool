#' `effortdata` Class
#'
#' Stores time series observations of fishing effort, including optional
#' observation uncertainty. Used in the `Effort` slot of a [data-class] object.
#'
#' 
#' @slot Name Optional character string. Name of the dataset.
#' @slot Value Numeric array of observed effort values, with dimensions
#'   `[nFleet, nYear]`, where `nYear` represents the total number of time steps.
#' @slot CV Numeric array of coefficients of variation (CVs) for the effort
#'   observations, matching the dimensions of `Value`.
#' @slot Units Optional character string. Units of effort measurement
#'   (e.g., `"days"`, `"trips"`).
#'   
#' `EffortData()` creates a new `effortdata` object. 
#' 
#' @return `EffortData()` returns a `effortdata` object.
#' 
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' @name effortdata
#' @aliases effortdata-class
#' @export
setClass(
  "effortdata",
  slots = c(
    Name   = "char.null",
    Value  = "array.null",
    CV     = "array.null",
    Units  = "char.null"
  )
)

#' @rdname effortdata
#' @export
EffortData <- function() {
  new('effortdata')
}