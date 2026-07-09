#' The `indicesdata` S4 Class
#'
#' Stores abundance or biomass index observations from fishery-dependent
#' (CPUE) or fishery-independent (Survey) sources, including observation
#' uncertainty, reference values, timing, and selectivity mapping. Used in
#' the `CPUE` and `Survey` slots of a [data-class] object. Objects are
#' typically created via [IndicesData()], which documents all parameters in
#' detail.
#'
#' @slot Name `character` or `NULL`. Name of the index. See [IndicesData()].
#' @slot Value `array` or `NULL`. Observed index values with dimensions
#'   `[nYear x nIndex]`. See [IndicesData()].
#' @slot CV `array` or `NULL`. Coefficients of variation for the index
#'   observations, matching the dimensions of `Value`. See [IndicesData()].
#' @slot Units `character` or `NULL`. Units of the index (e.g., `"kg/trip"`,
#'   `"numbers/tow"`). See [IndicesData()].
#' @slot Ref `numeric` or `NULL`. Reference value for each index (e.g., a
#'   historical mean or target level). See [IndicesData()].
#' @slot RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. See [IndicesData()].
#' @slot Timing `numeric`. Within-year timing of each observation as a
#'   fraction of the year (0-1). See [IndicesData()].
#' @slot Selectivity An array or character specification mapping each index
#'   to a fleet selectivity or defining an independent selectivity curve.
#'   See [IndicesData()].
#' @slot Misc A named list for any additional index-level metadata.
#'
#' @seealso [IndicesData()] for the constructor and full parameter
#'   documentation. [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name indicesdata-class
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
