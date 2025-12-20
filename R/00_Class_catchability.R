#' Catchability Object
#'
#' Historical Catchability
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @name Catchability
#' @export
setClass("catchability",
         slots=c(
           Value='num.array',
           qArea='num.array',
           qCV='num.array',
           qInc='num.array' 
         ),
         contains='MiscClass'
)

setValidity('catchability', isValidObject)


setMethod("initialize", "catchability", function(.Object,
                                           Value=NULL,
                                           qArea=NULL,
                                           qCV=NULL,
                                           qInc=NULL,
                                           Misc=list()) {
  
  .Object@Value <- Value
  .Object@qArea <- qArea
  .Object@qCV <- qCV
  .Object@qInc <- qInc
  .Object@Misc <- Misc
  .Object
})

#' @rdname Catchability
#' @export
Catchability <- function(Value=NULL,
                         qArea=NULL,
                         qCV=NULL,
                         qInc=NULL,
                         Misc=list()) {
  
  methods::new('catchability',
               Value=Value,
               qArea=qArea,
               qCV=qCV,
               qInc=qInc,
               Misc=Misc)
  
}

