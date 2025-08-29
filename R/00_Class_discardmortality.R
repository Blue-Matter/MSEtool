#' DiscardMortality Object
#'
#' Discard mortality
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('discardmortality', c('Check'))`
#'
#' @name DiscardMortality
#' @rdname DiscardMortality
#' @docType class
#' @example man-examples/DiscardMortality-class.R
#' @export
setClass("discardmortality",
         contains= c(
           'MeanAtAgeClass',
           'MeanAtLengthClass',
           'ClassesClass',
           'MiscClass')
)

setValidity('discardmortality', isValidObject)

setMethod("initialize", "discardmortality", function(.Object,
                                                     MeanAtAge=NULL,
                                                     MeanAtLength=NULL,
                                                     Classes=NULL,
                                                     Misc=list()) {
  
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn DiscardMortality Create a new `DiscardMortality` object
#' @export
DiscardMortality <- function(MeanAtAge=NULL,
                             MeanAtLength=NULL,
                             Classes=NULL,
                             Misc=list()) {
  
  if (methods::is(MeanAtAge, 'fleet'))
    return(MeanAtAge@DiscardMortality)
  
  methods::new('discardmortality',
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Classes=Classes,
               Misc=Misc)
}

#' @describeIn DiscardMortality Assign an `DiscardMortality` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `DiscardMortality` object to assign to `x`
#' @export
`DiscardMortality<-` <- function(x, value) {
  assignSlot(x, value, 'DiscardMortality')
}

