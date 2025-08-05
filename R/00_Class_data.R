
#' Class `catch`
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
setClass("catch",
         slots=c(Name='char.null',
                 Value='array.null',
                 CV='array.null',
                 Units='char.null',
                 Type='char.null' # Removals or Landings
         )
)


methods::setClassUnion("catch.list", c("catch", "list", 'NULL'))

# methods::setClassUnion("numeric.selectivity", c("numeric", "selectivity"))

#' Class `Index`
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
setClass("index",
         slots=c(Name='char.null',
                 Value='array.null',
                 CV='array.null',
                 Units='char.null',
                 Timing='numeric',
                 Selectivity='array.char.null',
                 Misc="MiscClass"
         )
)

# Selectivity
# Biomass (total)
# SBiomass
# Numeric array - nrow n age classes - maximum of 1 ncol nfleet
# Numeric array - nrow n length classes - maximum of 1 ncol nfleet
# Fleet Number - map to a fleet
# Character - Obs - 

methods::setClassUnion("index.list", c("index", "list", 'NULL'))


#' Class `composition`
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
setClass("composition",
         slots=c(Name='char.null',
                 Value='array.null',
                 Classes='num.null',
                 Units='char.null',
                 Misc="MiscClass"
         )
)


methods::setClassUnion("comp.list", c("composition", "list", 'NULL'))



#' Data Object
#'
#' 
#' 
#' The `Data` function is used to create S4 class `data` objects or to access or
#' assign `data` or `datalist` objects to [OM()] class objects
#'
#' @details
#' ## About the `data` Class
#' `data` is an S4 class used in [OM()] class objects. It contains either real
#' or simulated fishery data
#'
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('data')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('data')`
#'
#' @slot MaxAge `r MaxAge_param()`
#' @slot Units `r Units_param()`
#'
#' @seealso `r See_Also('data', c('Check'))`
#'
#' @name DataClass
#'
#' @example man-examples/Data-class.R
#' @export
setClass('data',
         slots=c(Name='char.null',
                 CommonName='char.null',
                 Species='char.null',
                 Agency='char.null',
                 Author='char.null',
                 Email='char.null',
                 Region='char.null',
                 Latitude='num.null',
                 Longitude='num.null',
                 
                 TimeSteps='num.null', # should be in fraction of year - or convert Date to fraction year
                 TimeStepLH='num.null', # last historical time step,
                 TimeUnits='char.null', # time units - same as `Ages`
                 TimeStepsPerYear='num.null',
                 nArea='num.null',
                 
                 Catch='catch',  # always is removals
                 Index='index',
                 CAA='comp.list',
                 CAL='comp.list'
         ),
         contains ='MiscClass'
         )




#' @describeIn DataClass Create a new `Data` object
#' @export
Data <- function(Name=NA, ...) {
  if (methods::is(Name, 'om'))
    return(Name@Data)
  
  .Object <- methods::new('data')
  
  validObject(.Object)
  .Object
}

validDataObject <- function(object) {
  TRUE
}
setValidity('data', validDataObject)

setMethod("initialize", "data", function(.Object) {
  # TODO
  .Object@TimeUnits <- 'year'
  .Object
})

#' @describeIn DataClass Assign an `Data` object to a [OM()] object
#' @param x An [OM()] class object
#' @param value An `data` class object to assign to `x`
#' @export
`Data<-` <- function(x, value) {
  assignSlot(x, value, 'Data')
}

