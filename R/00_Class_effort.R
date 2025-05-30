#' Effort Object
#'
#' Historical fishing effort
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Vessels Numeric array. The average number of fishing vessels per time step.
#' @slot Trips Numeric array. The average number of trips per vessel per time step.
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('effort', c('Check'))`
#'
#' @name Effort
#' @rdname Effort
#' @docType class
#' @example man-examples/Effort-class.R
#' @export
setClass('effort',
         slots=c(Effort='num.array.df',
                 Catchability='num.array',
                 qCV='num.array.list',
                 qInc='num.array.list',
                 Vessels='num.array.df',
                 Trips='num.array.list',
                 MaxVessels='num.array.list',
                 MaxTrips='num.array.list',
                 Units='char.null'
         ),
         contains = c('MiscClass')
)

setValidity('effort', isValidObject)

setMethod("initialize", "effort", function(.Object,
                                           Effort=NULL,
                                           Catchability=NULL,
                                           Vessels=NULL,
                                           Trips=NULL,
                                           MaxVessels=NULL,
                                           MaxTrips=NULL,
                                           Units=c('Vessels', 'Trips'),
                                           Misc=list()) {
  .Object@Effort <- Effort
  .Object@Catchability <- Catchability
  .Object@Vessels <- Vessels
  .Object@Trips <- Trips
  .Object@MaxVessels <- MaxVessels
  .Object@MaxTrips <- MaxTrips
  .Object@Units <- Units
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Effort Create a new `effort` class object
#' @export
Effort <- function(Effort=NULL,
                   Catchability=NULL,
                   Vessels=NULL,
                   Trips=NULL,
                   MaxVessels=NULL,
                   MaxTrips=NULL,
                   Units=c('Vessels', 'Trips'),
                   Misc=list()) {
  if (methods::is(Effort, 'fleet'))
    return(Effort@Effort)
  
  methods::new('effort',
               Effort=Effort,
               Catchability=Catchability,
               Vessels=Vessels,
               Trips=Trips,
               MaxVessels=MaxVessels,
               MaxTrips=MaxTrips,
               Units=Units,
               Misc=Misc)
}

#' @describeIn Effort Assign an `Effort` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Effort` object to assign to `x`
#' @export
`Effort<-` <- function(x, value) {
  assignSlot(x, value, 'Effort')
}
