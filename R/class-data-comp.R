#' The `compdata` S4 Class
#'
#' Stores age or size composition observations of catch samples.
#' Used in the `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, and
#' `DiscardsAtSize` slots of a [data-class] object. Objects are typically
#' created via [CompData()], which documents all parameters in detail.
#'
#' @slot Name `character` or `NULL`. Fleet names, length `nFleet`.
#' @slot Value `array` or `NULL`. Composition counts with dimensions
#'   `[nYear x nFleet x nClass]`. See [CompData()].
#' @slot Classes `numeric` or `NULL`. Class midpoints — ages in years for
#'   age compositions, or bin midpoints in the appropriate length unit for
#'   size compositions. See [CompData()].
#' @slot Units `character` or `NULL`. Units of the class variable
#'   (e.g., `"years"`, `"cm"`, `"mm"`). See [CompData()].
#' @slot Log `list`. Named list used for diagnostic and audit logging.
#' @slot Misc `list`. Named list for additional composition-level metadata.
#'
#' @seealso 
#' - [CompData()] for the constructor and full parameter documentation.
#' - [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name compdata-class
NULL

setClass(
  "compdata",
  slots = c(
    Name    = "char.null",
    Value   = "array.null",
    Classes = "num.null",
    Units   = "char.null",
    Log     = "list",
    Misc    = "list"
  )
)
