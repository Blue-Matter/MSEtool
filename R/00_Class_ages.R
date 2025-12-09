#' S4 Object class `ages`
#' 
#' `r S4Description('ages')
#' 
#' ## About the Class
#' The S4 object class `ages` contain information related to the age classes of a [Stock()]
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('ages')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('ages')`
#'
#' @seealso [MaxAge()], [MinAge()], []
#' 
#' @name Ages
#' 
NULL

#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' @slot MaxAge Numeric value specifying the maximum age
#' @slot MinAge Numeric value specifying the minimum age 
#' @slot Units Character string describing the units of `MaxAge` and `MinAge`
#' @slot PlusGroup Logical. Use a plusgroup?
#' @slot Classes Numeric vector of the age classes. **Note:** Age classes are *always* in units of a year
#' @rdname Ages
setClass('ages',
         slots=c(MaxAge='numeric',
                 MinAge='numeric',
                 Units='character',
                 PlusGroup='logical'),
         contains = c('ClassesClass')
)

#' @describeIn Ages Create a new `ages` class object
#' @param MaxAge Either an integer specifying the maximum age in units corresponding to `Units` of a [Stock()], or a [Stock()] class object. If `PlusGroup==TRUE`, `MaxAge` will be a plusgroup. **Required**
#' @param MinAge An integer specifying the minimum age in units corresponding to `Units` for a [Stock()]. Default is 0. 
#' @param Units A character string specifying the units of `MaxAge` and `MinAge`. Must be one of `ValidUnits()`
#' @param PlusGroup Logical. Use a plus group?
#' @export
Ages <- function(MaxAge,
                 MinAge=0,
                 Units='year',
                 PlusGroup=TRUE) {
  if (methods::is(MaxAge, 'stock'))
    return(MaxAge@Ages)
  
  .Object <- methods::new('ages',
                          MaxAge=MaxAge,
                          MinAge=MinAge,
                          Units=Units,
                          PlusGroup=PlusGroup)
  
  validObject(.Object)
  .Object
}

#' @describeIn Ages Assign an `ages` class object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value An `ages` class object to assign to `x`
#' @export
`Ages<-` <- function(x, value) {
  assignSlot(x, value, 'Ages')
}


setValidity('ages', isValidObject)

setMethod("initialize", "ages", function(.Object,
                                         MaxAge=NA,
                                         MinAge=0,
                                         Units='year',
                                         PlusGroup=TRUE) {
  .Object@MinAge <- MinAge
  .Object@Units <- Units
  if (!is.na(MaxAge)) {
    .Object@MaxAge <- MaxAge
    .Object@Classes <- CalcAgeClasses(.Object)
  }
  .Object@PlusGroup <- PlusGroup
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


