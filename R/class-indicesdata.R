#' `indicesdata` Class
#'
#' Stores abundance or biomass index observations from fishery-dependent
#' (CPUE) or fishery-independent (Survey) sources, including observation
#' uncertainty, reference values, timing, and selectivity mapping. Used in
#' the `CPUE` and `Survey` slots of a [data-class] object.
#'
#' @slot Name Optional character string. Name of the index.
#' @slot Value Numeric array of observed index values, with dimensions
#'   `[nIndex, nYear]`, where `nYear` represents the total number of time steps.
#' @slot CV Numeric array of coefficients of variation (CVs) for the index
#'   observations, matching the dimensions of `Value`.
#' @slot Units Optional character string. Units of the index
#'   (e.g., `"kg/trip"`, `"numbers/tow"`).
#' @slot Ref Numeric vector of length `nIndex` giving a reference value for
#'   each index (e.g., a historical mean or target level).
#' @slot RefCV Numeric array of CVs for the reference values, matching the
#'   dimensions of `Ref`.
#' @slot Timing Numeric vector of length `nIndex` giving the within-year
#'   timing of each observation as a fraction of the year (0–1).
#' @slot Selectivity An array or character specification mapping each index
#'   to a fleet selectivity or defining an independent selectivity curve.
#' @slot Misc A named list for any additional index-level metadata.
#'
#' `IndicesData()` creates a new `indicesdata` object. 
#' 
#' @return `IndicesData()` returns a `indicesdata` object.
#' 
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' @name indicesdata
#' @aliases indicesdata-class
#' @export
setClass(
  "indicesdata",
  slots = c(
    Name        = "char.null",
    Value       = "array.null",
    CV          = "array.null",
    Units       = "char.null",
    Ref         = "num.null",
    RefCV       = "array.null",
    Timing      = "numeric",
    Selectivity = "array.char.num",
    Misc        = "list"
  )
)

#' @rdname indicesdata
#' @export
IndicesData <- function() {
  new('indicesdata')
}