#' Ages Constructor and Accessors
#'
#' Construct an [ages-class] object defining the discrete age structure of a
#' [stock-class], or access and replace the `Ages` slot of a [stock-class]
#' object and its individual slots.
#'
#' @param MaxAge `numeric(1)`. Maximum age in units of `Units`. When
#'   `PlusGroup = TRUE` (the default), fish at or older than this age are
#'   pooled into an open-ended plus group. Required for a non-empty object;
#'   if omitted the constructor returns an uninitialised [ages-class] suitable
#'   for placeholder use inside a [stock-class]. Alternatively, a
#'   [stock-class] object, in which case `Ages()` returns `x@Ages` directly —
#'   see *Pass-Through Access*.
#' @param MinAge `numeric(1)`. Minimum (youngest) age class in units of
#'   `Units`. Must be non-negative and less than or equal to `MaxAge`. Default
#'   `0`.
#' @param Units `character(1)`. Time unit for `MinAge` and `MaxAge`. Must be
#'   one of the strings returned by [ValidUnits()] (e.g., `"year"`,
#'   `"month"`). Default `"year"`.
#' @param PlusGroup `logical(1)`. If `TRUE`, `MaxAge` is an open-ended plus
#'   group that accumulates all fish at or beyond that age. If `FALSE`, the
#'   age classes terminate at exactly `MaxAge` with no pooling. Default `TRUE`.
#' @param x An [ages-class] object for slot accessors (`MaxAge()`,
#'   `MinAge()`, `PlusGroup()`), or a [stock-class] object for `Ages<-`.
#' @param value For `Ages<-`: an [ages-class] object. For `MaxAge<-`,
#'   `MinAge<-`, `PlusGroup<-`: the replacement value for the corresponding
#'   slot (see *Slot Accessors*).
#'
#' @details
#' ## Age Class Calculation
#'
#' The `Classes` slot — a numeric vector of age classes expressed in years —
#' is derived automatically from `MinAge`, `MaxAge`, and `Units` via
#' [CalcAgeClasses()] and cannot be set directly. For seasonal models
#' (`Units != "year"`), fractional year values are produced (e.g., `0`,
#' `0.25`, `0.5`, … for quarterly seasons with `MinAge = 0`). Retrieve the
#' computed vector with `Classes(x)`.
#'
#' ## Plus Group
#'
#' When `PlusGroup = TRUE`, the final age class absorbs all individuals at or
#' beyond `MaxAge`. Mortality, growth, and selectivity at the plus-group age
#' represent the average for that pooled cohort. Setting `PlusGroup = FALSE`
#' treats `MaxAge` as an exact terminal age with no accumulation.
#'
#' ## Uninitialised Objects
#'
#' Calling `Ages()` without `MaxAge` returns an object with empty `MaxAge` and
#' `MinAge` slots (`numeric(0)`). Validity checks are skipped for uninitialised
#' objects, so they can be stored as placeholders inside a [stock-class] before
#' parameters are specified. The model will not run until `MaxAge` is supplied.
#'
#' ## Pass-Through Access from a Stock
#'
#' When `MaxAge` is a [stock-class] object, `Ages()` acts as an accessor:
#'
#' ```r
#' Ages(my_stock)          # returns my_stock@Ages
#' Ages(my_stock) <- a     # replaces my_stock@Ages with ages-class object `a`
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using functions that match the slot
#' names. All replacement functions re-validate the object after assignment:
#'
#' ```r
#' MaxAge(a)        <- 20
#' MinAge(a)        <- 1
#' PlusGroup(a)     <- FALSE
#' ```
#'
#' @return
#' `Ages()` returns an [ages-class] object. If `MaxAge` is a [stock-class],
#' returns `x@Ages`.
#'
#' `Ages<-` returns the [stock-class] `x` with the `Ages` slot replaced by
#' `value` and the object re-validated.
#'
#' `MaxAge()`, `MinAge()`, `PlusGroup()` return the value of the corresponding
#' slot from `x`.
#'
#' `MaxAge<-`, `MinAge<-`, `PlusGroup<-` return `x` with the named slot
#' updated and the object re-validated.
#'
#' @seealso
#' [ages-class] for the class definition and slot-level documentation.
#' [Stock()] for the enclosing stock constructor. [ValidUnits()] for accepted
#' unit strings. [Classes()] to retrieve the derived age-class vector.
#'
#' @family ages
#'
#' @example man-examples/Ages.R
#'
#' @export
Ages <- function(MaxAge,
                 MinAge = 0,
                 Units = "year",
                 PlusGroup = TRUE) {

  if (missing(MaxAge))
    MaxAge <- numeric()
  
  if (.IsStockOrList(MaxAge)) 
    return(.ExtractStockSlot(MaxAge, 'Ages'))
  
  if (is.null(MaxAge))
    return(NULL)
  
  if (length(MaxAge) && is.na(MaxAge)) 
    MaxAge <- numeric()
  
  object <- methods::new(
    "ages",
    MaxAge    = MaxAge,
    MinAge    = MinAge,
    Units     = Units,
    PlusGroup = PlusGroup
  )
  
  object@Classes <- CalcAgeClasses(object)
  methods::validObject(object)
  object
}

#' @rdname Ages
#' @export
`Ages<-` <- function(x, value) {
  .AssignSlotRecursive(x, value, 'Ages')
}

#' @rdname Ages
#' @export
MaxAge <- function(x) {
  .CheckClass(x, "ages", "Ages")
  x@MaxAge
}

#' @rdname Ages
#' @export
`MaxAge<-` <- function(x, value) {
  .CheckClass(x, "ages", "Ages")
  x@MaxAge <- value
  methods::validObject(x)
  x
}

#' @rdname Ages
#' @export
MinAge <- function(x) {
  .CheckClass(x, "ages", "Ages")
  x@MinAge
}

#' @rdname Ages
#' @export
`MinAge<-` <- function(x, value) {
  .CheckClass(x, "ages", "Ages")
  x@MinAge <- value
  methods::validObject(x)
  x
}

#' @rdname Ages
#' @export
PlusGroup <- function(x) {
  .CheckClass(x, "ages", "Ages")
  x@PlusGroup
}

#' @rdname Ages
#' @export
`PlusGroup<-` <- function(x, value) {
  .CheckClass(x, "ages", "Ages")
  x@PlusGroup <- value
  methods::validObject(x)
  x
}

