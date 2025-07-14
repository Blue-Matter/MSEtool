
## Ages ----

#' Ages Object
#'
#' The `Ages` function is used to create S4 class `ages` objects or to access or
#' assign `ages` objects to [Stock()] class objects.
#' @slot MaxAge `r MaxAge_param()`
#' @slot Units `r Units_param()`
#' @slot PlusGroup `r Plusgroup_param()`
#' @slot Classes `r AgeClasses_param()`
#' @rdname Ages
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
setClass('ages',
         slots=c(MaxAge='numeric',
                 MinAge='numeric',
                 Units='character',
                 PlusGroup='logical'),
         contains = c('ClassesClass')
)

setValidity('ages', isValidObject)

setMethod("initialize", "ages", function(.Object,
                                         MaxAge=NA,
                                         MinAge=0,
                                         Units='year',
                                         PlusGroup=TRUE) {
  if (!is.na(MaxAge)) {
    .Object@MaxAge <- MaxAge
    .Object@Classes <- MinAge:MaxAge
  }
  
  .Object@Units <- Units
  .Object@PlusGroup <- PlusGroup
  .Object
})



#' @describeIn Ages Create a new `ages` class object
#' @param MaxAge `r MaxAge_param()`
#' @param Units `r Units_param()`
#' @param PlusGroup `r Plusgroup_param()`
#' @param Classes `r AgeClasses_param()`
#' @export
Ages <- function(MaxAge=NA,
                 MinAge=0,
                 Units='year',
                 PlusGroup=TRUE
                 ) {
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




#' @details
#' ## About the `ages` Class
#' `ages` is an S4 class used in [Stock()] class objects. It contains information
#' relating to the age classes of the stock.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('ages')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('ages')`
#'
#' @slot MaxAge `r MaxAge_param()`
#' @slot Units `r Units_param()`
#' @slot PlusGroup `r Plusgroup_param()`
#' @slot Classes `r AgeClasses_param()`
#'
#' @seealso `r See_Also('ages', c('ValidUnits', 'Check'))`
#'
#'
#' @example man-examples/Ages-class.R



