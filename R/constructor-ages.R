#' Ages
#'
#' Construct and manipulate an [ages-class] object defining the age structure
#' associated with a [stock-class] object.
#'
#' @param MaxAge Numeric. Maximum age. If `PlusGroup = TRUE`, this is the plus
#'   group age. In units of `Units`.
#' @param MinAge Numeric. Minimum age. In units of `Units`. Default `0`.
#' @param Units Character. Time units for `MinAge` and `MaxAge`
#'   (e.g., `"year"`). See [ValidUnits()]. Default `"year"`.
#' @param PlusGroup Logical. Whether `MaxAge` is treated as a plus group.
#'   Default `TRUE`.
#' @param x An [ages-class] object, or a [stock-class] object for `Ages<-`.
#' @param value For `Ages<-`: an [ages-class] object. For slot replacement
#'   functions (`MaxAge<-`, `MinAge<-`, `PlusGroup<-`): the new value for
#'   the corresponding slot.
#'   
#'   
#' @details
#' The `ages` class defines the discrete age structure used by a [stock-class]
#' object. Age classes are derived from `MinAge`, `MaxAge`, and `Units`.
#'
#' Although `MinAge` and `MaxAge` are in the units described by `Units`, the
#' resulting age classes are always in years — i.e., fractional year values
#' for seasonal models.
#'
#' Age classes can be accessed with `Classes(x)`.
#'
#' ## Accessing and Assigning Slots
#'
#' All slots in [ages-class] objects can be accessed or assigned new values
#' with functions matching the slot names. See `Examples`.
#'
#' @return
#' - `Ages()` returns an [ages-class] object. If `MaxAge` is a [stock-class] object,
#'   the `Ages` slot of that stock is returned.
#' - `Ages<-` returns `x` with the `Ages` slot replaced.
#' - `MaxAge()`, `MinAge()`, `PlusGroup()` return the corresponding slot value
#'   from `x`.
#' - `MaxAge<-`, `MinAge<-`, `PlusGroup<-` return `x` with the corresponding
#'   slot updated.
#'
#' @seealso [ages-class], [Stock()], [Units()], [Classes()]
#'
#' @examples
#' a <- Ages(MaxAge = 20)
#' MaxAge(a)
#' MaxAge(a) <- 10
#' MaxAge(a)
#'
#' MinAge(a)
#' MinAge(a) <- 1
#'
#' PlusGroup(a)
#' PlusGroup(a) <- FALSE
#'
#' @export
Ages <- function(MaxAge,
                 MinAge = 0,
                 Units = "year",
                 PlusGroup = TRUE) {
  
  if (missing(MaxAge))
    MaxAge <- numeric()
  
  if (inherits(MaxAge, 'stock'))
    return(MaxAge@Ages)
  
  if (!is.finite(MaxAge))
    return(new('ages'))
  
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
  CheckClass(x, "stock", "x")
  CheckClass(value, "ages", "value")
  x@Ages <- value
  methods::validObject(x)
  x
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


CalcAgeClasses <- function(Ages) {
  # always in years 
  if (!length(Ages@MaxAge) || !length(Ages@MinAge))
    return(NULL)
  Seasons <- CalcSeasons(Ages@Units)
  seq(from=Ages@MinAge/Seasons, by=1/Seasons, to=Ages@MaxAge/Seasons) |>
    round(3)
}

