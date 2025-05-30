#' Distribution Object
#'
#' Spatial Distribution of fishing fleet
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('distribution', c('Check'))`
#'
#' @name Distribution
#' @rdname Distribution
#' @docType class
#' @example man-examples/Distribution-class.R
#' @export
setClass('distribution',
         slots=c(Closure='num.array',
                 Cost='num.array.list',
                 EffortArea='num.array.list',
                 Misc='list'
         ))

setValidity('distribution', isValidObject)

setMethod("initialize", "distribution", function(.Object,
                                                 Closure=NULL,
                                                 Cost=NULL,
                                                 EffortArea=NULL,
                                                 Misc=list()) {
  .Object@Closure <- Closure
  .Object@Cost <- Cost
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Distribution Create a new `Distribution` object
#' @export
Distribution <- function(Closure=NULL,
                         Cost=NULL,
                         EffortArea=NULL,
                         Misc=list()) {
  
  if (methods::is(Closure, 'fleet'))
    return(Closure@Distribution)
  
  methods::new('distribution',
               Closure=Closure,
               Cost=Cost,
               EffortArea=EffortArea,
               Misc=Misc)
}

#' @describeIn Distribution Assign an `Distribution` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Distribution` object to assign to `x`
#' @export
`Distribution<-` <- function(x, value) {
  assignSlot(x, value, 'Distribution')
}
