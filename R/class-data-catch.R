#' The `catchdata` S4 Class
#'
#' Stores time series observations of landed or discarded catch and associated
#' uncertainty. Used in the `Landings` and `Discards` slots of a [data-class]
#' object. Objects are typically created via [CatchData()], which documents all
#' parameters in detail.
#'
#' @slot Name `character` or `NULL`. Fleet names, length `nFleet`.
#' @slot Value `array` or `NULL`. Observed catch values with dimensions
#'   `[nYear x nFleet]`. See [CatchData()].
#' @slot CV `array` or `NULL`. Coefficients of variation matching the
#'   dimensions of `Value`. See [CatchData()].
#' @slot Units `character` or `NULL`. Units of catch measurement per fleet
#'   (e.g., `"t"` for tonnes, `"numbers"`). See [CatchData()].
#' @slot Ref `array` or `NULL`. Reference catch values (e.g., a historical
#'   baseline), matching the dimensions of `Value`. See [CatchData()].
#' @slot RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. See [CatchData()].
#'
#' @seealso [CatchData()] for the constructor and full parameter documentation.
#'   [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name catchdata-class
setClass(
  "catchdata",
  slots = c(
    Name  = "char.null",
    Value = "array.null",
    CV    = "array.null",
    Units = "char.null",
    Ref   = "array.null",
    RefCV = "array.null"
  )
)


