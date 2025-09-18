

#' FishingMortality Object
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#'
#' @seealso `r See_Also('fishingmortality')`
#'
#' @name FishingMortality
#' @rdname FishingMortality
#' @docType class
#' @example man-examples/FishingMortality-class.R
#' @export
setClass('fishingmortality',
         slots=c(ApicalF='array.null',
                 DeadAtAge='array.null',
                 RetainAtAge='array.null'
         ),
         contains = c('MiscClass')
)

setMethod("initialize", "fishingmortality", function(.Object,
                                                     ApicalF=NULL,
                                                     DeadAtAge=NULL,
                                                     RetainAtAge=NULL,
                                                     Misc=list()) {
  .Object@ApicalF <- ApicalF
  .Object@DeadAtAge <- DeadAtAge
  .Object@RetainAtAge <- RetainAtAge
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn FishingMortality Create a new `fishingmortality` class object
#' @export
FishingMortality <- function(ApicalF=NULL,
                             DeadAtAge=NULL,
                             RetainAtAge=NULL,
                             Misc=list()) {
  if (inherits(ApicalF, 'fleet'))
    return(ApicalF@FishingMortality)
  
  methods::new('fishingmortality',
               ApicalF=ApicalF,
               DeadAtAge=DeadAtAge,
               RetainAtAge=RetainAtAge,
               Misc=Misc)
}

#' @describeIn FishingMortality Assign an `FishingMortality` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `FishingMortality` object to assign to `x`
#' @export
`FishingMortality<-` <- function(x, value) {
  assignSlot(x, value, 'FishingMortality')
}
