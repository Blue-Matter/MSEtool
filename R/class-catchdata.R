#' `catchdata` Class
#'
#' Stores time series observations of landed or discarded catch, including
#' optional observation uncertainty and reference values. Used in the `Landings`
#' and `Discards` slots of a [data-class] object.
#' 
#' @slot Name Optional character string. Name of the dataset.
#' @slot Value Numeric array of observed catch values, with dimensions
#'   `[nFleet, nYear]`, where `nYear` represents the total number of time steps.
#' @slot CV Numeric array of coefficients of variation (CVs) for the catch
#'   observations, matching the dimensions of `Value`.
#' @slot Units Optional character string. Units of catch measurement
#'   (e.g., `"t"` for tonnes, `"numbers"`).
#' @slot Ref Numeric array of reference catch values (e.g., a historical
#'   baseline), matching the dimensions of `Value`.
#' @slot RefCV Numeric array of CVs for the reference values, matching the
#'   dimensions of `Ref`.
#'
#' `CatchData()` creates a new `catchdata` object. 
#'
#' @return `CatchData()` returns a `catchdata` object.
#' 
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' @name catchdata
#' @export
setClass(
  "catchdata",
  slots = c(
    Name   = "char.null",
    Value  = "array.null",
    CV     = "array.null",
    Units  = "char.null",
    Ref    = "array.null",
    RefCV  = "array.null"
  )
)

#' @rdname catchdata
#' @export
CatchData <- function() {
  new('catchdata')
}
