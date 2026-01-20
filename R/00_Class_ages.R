#' Ages Class and Constructor
#'
#' The `Ages` class defines the age structure associated with a
#' [Stock()] object. It specifies the minimum and maximum ages,
#' whether a plus group is used, and the resulting vector of age classes.
#'
#' @param MaxAge Either a numeric value specifying the maximum age (in units
#'   corresponding to `Units`, or a [Stock()] object, in which case
#'   the existing `Ages` slot is returned. 
#' @param MinAge Numeric value specifying the minimum age. Default is `0`.
#' @param Units Character string specifying the units used to define age
#'   classes. Must be one of [ValidUnits()].
#' @param PlusGroup Logical; should the maximum age be treated as a plus group?
#' @param x A [Stock()] object.
#' @param value An [Ages()] object to assign.
#'
#' @details
#' 
#' Age classes are internally represented as numeric values in units of **years**. 
#' Fractional ages (e.g. seasonal or sub-annual ages) are supported and are 
#' rounded to three decimal places.
#'
#' The `Ages` generic is used to:
#' * construct new `Ages` objects;
#' * access `Ages` when supplied with a [Stock()] object;
#' * assign an `Ages` object to to a [Stock()] object.
#' 
#' ## Slots
#'
#' Objects of class `"ages"` contain the following slots:
#'
#' * `MaxAge`: Numeric scalar giving the maximum age. If `PlusGroup == TRUE`,
#'   this represents the plus group age.
#' * `MinAge`: Numeric scalar giving the minimum age.
#' * `Units`: Character string describing the time units used to define
#'   age classes (e.g. `"year"`).
#'   
#' * `PlusGroup`: Logical; indicates whether the maximum age is treated
#'   as a plus group.
#' * `Classes`: Numeric vector of age classes expressed in **years**, or
#'   `NULL` if undefined.
#'   
#' @return
#' * `Ages()`: returns an [Ages] class object
#' * `Ages(x)`: returns an `Ages` object from [Stock() object `x`
#' * `Ages<-`: returns the modified [Stock()] object
#'
#' @seealso [Classes()], [MaxAge()], [MinAge()], [Stock()]
#'
#' @name Ages
#' @rdname Ages
#' 
#' @examples 
#' Ages(MaxAge=20)
#' 
#'
#' @include 00_Class_unions.R
NULL


setClass('ages',
         slots=c(MaxAge='numeric',
                 MinAge='numeric',
                 Units='character',
                 PlusGroup='logical',
                 Classes='num.null')
         
)


setValidity("ages", function(object) {
  
  if (length(object@MaxAge) != 1 || !is.finite(object@MaxAge))
    return("MaxAge must be a finite numeric scalar")
  
  if (length(object@MinAge) != 1 || !is.finite(object@MinAge))
    return("MinAge must be a finite numeric scalar")
  
  if (object@MinAge < 0)
    return("MinAge must be non-negative")
  
  if (object@MaxAge <= object@MinAge)
    return("MaxAge must be greater than MinAge")
  
  if (length(object@Units) != 1)
    return("Units must be a single character value")
  
  if (!object@Units %in% ValidUnits())
    return("Units is not a valid time unit")
  
  if (length(object@PlusGroup) != 1)
    return("PlusGroup must be a single logical value")
  
  if (!is.null(object@Classes)) {
    if (!is.numeric(object@Classes))
      return("Classes must be numeric or NULL")
    
    if (any(diff(object@Classes) <= 0))
      return("Classes must be strictly increasing")
  }
  
  TRUE
}
)

CalcAgeClasses <- function(Ages) {
  # always in years 
  if (!length(Ages@MaxAge) || !length(Ages@MinAge))
    return(NULL)
  Seasons <- CalcSeasons(Ages@Units)
  seq(from=Ages@MinAge/Seasons, by=1/Seasons, to=Ages@MaxAge/Seasons) |>
    round(3)
}


setMethod("initialize", "ages", function(.Object,
                                         MaxAge = NA_real_,
                                         MinAge = 0,
                                         Units = "year",
                                         PlusGroup = TRUE) {
  
  .Object@MinAge    <- MinAge
  .Object@Units     <- Units
  .Object@PlusGroup <- PlusGroup
  
  if (!is.na(MaxAge)) {
    .Object@MaxAge  <- MaxAge
    .Object@Classes <- CalcAgeClasses(.Object)
  }
  .Object
}
)
















