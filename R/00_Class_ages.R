#' `Ages` Object and Functions
#' 
#' An `Ages` object defines the age structure associated with a [Stock()]
#' object. It specifies the minimum and maximum ages, whether a plus group
#' is used, and the resulting vector of age classes.
#' 
#' Age classes are internally represented as numeric values **in units of
#' years**, and may be fractional (e.g. seasonal or sub-annual ages; rounded to 3 digits).
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('ages')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('ages')`
#'
#' @seealso [MaxAge()], [MinAge()]
#' 
#' @name Ages
#' 
NULL

#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' @slot MaxAge Numeric value specifying the maximum age.
#'   If `PlusGroup = TRUE`, this represents the plus group age.
#' @slot MinAge Numeric value specifying the minimum age.
#' @slot Units Character string describing the time units used to define
#'   age classes (e.g. `"year"`).
#' @slot PlusGroup Logical; indicates whether the maximum age is treated
#'   as a plus group.
#' @slot Classes Numeric vector of age classes expressed in **years**.
#'   May include fractional values (e.g. `0.25`, `1.5`).
#' @rdname Ages
setClass('ages',
         slots=c(MaxAge='numeric',
                 MinAge='numeric',
                 Units='character',
                 PlusGroup='logical'),
         contains = c('ClassesClass')
)

#' @describeIn Ages Create a new `ages` object
#'
#' @param MaxAge Either a numeric value specifying the maximum age (in units
#'   corresponding to `Units`), or a [Stock()] object, in which case the
#'   existing `Ages` slot is returned.
#' @param MinAge Numeric value specifying the minimum age. Default is `0`.
#' @param Units Character string specifying the units used to define age
#'   classes. Must be one of `ValidUnits()`.
#' @param PlusGroup Logical; should the maximum age be treated as a plus group?
#'
#' @export
Ages <- function(MaxAge,
                 MinAge=0,
                 Units='year',
                 PlusGroup=TRUE) {
  if (inherits(MaxAge, 'stock'))
    return(MaxAge@Ages)
  
  .Object <- methods::new('ages',
                          MaxAge=MaxAge,
                          MinAge=MinAge,
                          Units=Units,
                          PlusGroup=PlusGroup)
  
  validObject(.Object)
  .Object
}

#' @describeIn Ages Assign an [Ages()] object to a [Stock()] object
#' @param x A [Stock()] object
#' @param value An [Ages()] object to assign
#' 
#' @export
`Ages<-` <- function(x, value) {
  assignSlot(x, value, 'Ages')
}


setValidity('ages', isValidObject)

setMethod("initialize", "ages", function(.Object,
                                         MaxAge=NA_real_,
                                         MinAge=0,
                                         Units='year',
                                         PlusGroup=TRUE) {
  .Object@MinAge <- MinAge
  .Object@Units <- Units
  .Object@PlusGroup <- PlusGroup
  if (!is.na(MaxAge)) {
    .Object@MaxAge <- MaxAge
    .Object@Classes <- CalcAgeClasses(.Object)
  }
  .Object
})



CalcAgeClasses <- function(Ages) {
  # always in years 
  if (!length(Ages@MaxAge) || !length(Ages@MinAge))
    return(NULL)
  Seasons <- CalcSeasons(Ages@Units)
  seq(from=Ages@MinAge/Seasons, by=1/Seasons, to=Ages@MaxAge/Seasons) |>
    round(3)
}


