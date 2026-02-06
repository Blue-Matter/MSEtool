#' Ages
#'
#' Construct and manipulate an [ages-class] object defining the age structure
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
#' object. Age classes are derived from `MinAge`, `MaxAge`, and `Units`.
#' 
#' Although `MinAge` and `MaxAge` are defined in the units described in `Units`, 
#' the resulting age classes are always in units of `year`; i.e., fractional
#' year values for seasonal models
#' 
#' The age classes can be accessed with `Classes(MyAges)`
#' 
#' ## Accessing and Assigning Slots
#' 
#' All slots in [ages-class] objects can be accessed or assigned new values
#' with functions matching the slot names. See `Examples`.
#' 
#'
#' @return An [ages-class] object.
#'
#' @seealso [Units()], [`Units<-`], [Classes()]
#' @examples
#' a <- Ages(MaxAge = 20)
#' MaxAge(a)
#' MaxAge(a) <- 10
#' MaxAge(a)
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

