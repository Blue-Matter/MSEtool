#' The `ages` S4 Class
#'
#' Defines the discrete age structure of a [stock-class] object. Objects are
#' typically created via [Ages()], which documents all parameters and validates
#' inputs.
#'
#' @slot MaxAge `numeric(1)`. Maximum age in units of `Units`. When
#'   `PlusGroup = TRUE`, fish older than this age are pooled into the plus
#'   group.
#' @slot MinAge `numeric(1)`. Minimum (youngest) age class in units of
#'   `Units`. Must be non-negative and less than or equal to `MaxAge`.
#' @slot Units `character(1)`. Time unit for `MinAge` and `MaxAge`. Must be
#'   one of the values returned by [ValidUnits()] (e.g., `"year"`).
#' @slot PlusGroup `logical(1)`. If `TRUE`, `MaxAge` is treated as an
#'   open-ended plus group that accumulates all fish at or beyond that age.
#' @slot Classes `numeric`. Vector of age classes in years, derived
#'   automatically from `MinAge`, `MaxAge`, and `Units` by [CalcAgeClasses()].
#'   Not intended to be set directly; use [Ages()] to trigger recalculation.
#'
#' @details
#' An object is considered uninitialised when `MaxAge` or `MinAge` is
#' `numeric(0)`. 
#'
#' Validity is enforced on non-empty objects: `MaxAge` and `MinAge` must be
#' finite scalars, `MinAge` must be non-negative, `MaxAge >= MinAge`, and
#' `Units` must be a scalar string accepted by [ValidUnits()].
#'
#' Direct construction via [methods::new()] is not recommended; use [Ages()]
#' instead, which populates `Classes` automatically.
#'
#' @seealso 
#' - [Ages()] for the constructor and slot-accessor functions.
#' - [stock-class] for the enclosing object.
#' - [ValidUnits()] for accepted unit strings. 
#' - [Classes()] to retrieve the computed age-class vector.
#'
#' @family ages
#'
#' @include class-unions.R
#' @export
#' @rdname ages-class
setClass(
  "ages",
  slots = c(
    MaxAge    = "numeric",
    MinAge    = "numeric",
    Units     = "character",
    PlusGroup = "logical",
    Classes   = "num.null"
  )
)

setValidity("ages", function(object) {
  
  if (!length(object@MaxAge) || !length(object@MinAge))
    return(TRUE)
  
  if (length(object@MaxAge) != 1)
    return("`MaxAge` must be a numeric scalar")
  
  if (length(object@MinAge) != 1)
    return("`MinAge` must be a numeric scalar")
  
  if (!is.finite(object@MaxAge) || !is.finite(object@MinAge))
    return("`MaxAge` and `MinAge` must be finite numeric values")
  
  if (object@MinAge < 0)
    return("`MinAge` must be non-negative")
  
  if (object@MaxAge < object@MinAge)
    return("`MaxAge` must be greater than or equal to `MinAge`")
  
  if (length(object@PlusGroup) && length(object@PlusGroup) != 1)
    return("`PlusGroup` must be a logical scalar")
  
  if (length(object@Units)) {
    if (length(object@Units) != 1)
      return("`Units` must be a character scalar")
    if (!is.character(object@Units))
      return("`Units` must be a character string")
    
    if (!object@Units %in% ValidUnits())
      return("Invalid `Units`. See `ValidUnits()`")
  }
  TRUE
})
