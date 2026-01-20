

#' Maturity Class and Constructor
#' 
#' @name Maturity
#' @rdname Maturity
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' @example man-examples/Maturity-class.R
NULL

setClass("maturity",
         slots=c(Pars='list',
                 Model='fun.char',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 MeanAtWeight='num.array.null',
                 Classes='num.null',
                 Semelparous="array.log.null",
                 Misc='list'
         )
)


setValidity('maturity', function(object) {
  # TODO 
  TRUE
})


setMethod("initialize", "maturity", function(.Object,
                                             Model=NULL,
                                             Pars=list(L50=NA, L50_95=NA),
                                             MeanAtAge=NULL,
                                             MeanAtLength=NULL,
                                             MeanAtWeight=NULL,
                                             Classes=NULL,
                                             Semelparous=FALSE,
                                             Misc=list()) {
  .Object@Pars <- Pars
  
  if (!is.null(Model))
    .Object@Model <- Model
  
  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    .Object@Model <- FindModel(.Object)
  
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@MeanAtWeight <- MeanAtWeight
  .Object@Classes <- Classes
  .Object@Semelparous <- Semelparous
  .Object@Misc <- Misc
  .Object
})

