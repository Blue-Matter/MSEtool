#' Ages
#'
#' Construct and manipulate an `ages` object defining the age structure
#' associated with a [Stock()].
#'
#' @param MaxAge Numeric scalar giving the maximum age. If `PlusGroup == TRUE`,
#'   this represents the plus group age. In units of `Units`.
#' @param MinAge Numeric scalar giving the minimum age. In units of `Units`.
#' @param Units Character string describing the time units (e.g. `"year"`).
#'   See [ValidUnits()].
#' @param PlusGroup Logical; whether the maximum age is treated as a plus group.
#'
#' @details
#' The `Ages` class defines the discrete age structure used by a [Stock()]
#' object. Age classes are derived from `MinAge`, `MaxAge`, and `PlusGroup`.
#'
#' ## Accessors and assignment
#'
#' - `GetAges()` / `SetAges()` retrieve or assign the `Ages` component of a
#'   [Stock()] object.
#' - `MaxAge()`, `MinAge()`, `Units()`, and `PlusGroup()` access individual
#'   components of an `ages` object.
#' - Replacement functions (e.g. `MaxAge<-`) update the corresponding slot
#'   and validate the object.
#'
#' @return An `ages` object.
#'
#'
#' @examples
#' a <- Ages(MaxAge = 20)
#' MaxAge(a)
#'
#' @export
Ages <- function(MaxAge,
                 MinAge = 0,
                 Units = "year",
                 PlusGroup = TRUE) {
  
  if (missing(MaxAge))
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


CalcAgeClasses <- function(Ages) {
  # always in years 
  if (!length(Ages@MaxAge) || !length(Ages@MinAge))
    return(NULL)
  Seasons <- CalcSeasons(Ages@Units)
  seq(from=Ages@MinAge/Seasons, by=1/Seasons, to=Ages@MaxAge/Seasons) |>
    round(3)
}


#' @rdname Ages
#' @export
GetAges <- function(Stock) {
  CheckClass(Stock, "stock", "Stock")
  Stock@Ages
}


#' @rdname Ages
#' @export
SetAges <- function(Stock, Ages) {
  CheckClass(Stock, "stock", "Stock")
  CheckClass(Ages, "ages", "Ages")
  Stock@Ages <- Ages
  methods::validObject(Stock)
  Stock
}


#' @rdname Ages
#' @export
MaxAge <- function(x) {
  CheckClass(x, "ages", "Ages")
  x@MaxAge
}


#' @rdname Ages
#' @export
`MaxAge<-` <- function(x, value) {
  CheckClass(x, "ages", "Ages")
  x@MaxAge <- value
  methods::validObject(x)
  x
}


#' @rdname Ages
#' @export
MinAge <- function(x) {
  CheckClass(x, "ages", "Ages")
  x@MinAge
}


#' @rdname Ages
#' @export
`MinAge<-` <- function(x, value) {
  CheckClass(x, "ages", "Ages")
  x@MinAge <- value
  methods::validObject(x)
  x
}


#' @rdname Ages
#' @export
Units <- function(x) {
  CheckClass(x, "ages", "Ages")
  x@Units
}


#' @rdname Ages
#' @export
`Units<-` <- function(x, value) {
  CheckClass(x, "ages", "Ages")
  x@Units <- value
  methods::validObject(x)
  x
}


#' @rdname Ages
#' @export
PlusGroup <- function(x) {
  CheckClass(x, "ages", "Ages")
  x@PlusGroup
}


#' @rdname Ages
#' @export
`PlusGroup<-` <- function(x, value) {
  CheckClass(x, "ages", "Ages")
  x@PlusGroup <- value
  methods::validObject(x)
  x
}


#