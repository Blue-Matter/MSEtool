
## Fecundity ----

#' Fecundity Class and Constructor
#' 
#' @name Fecundity
#' @rdname Fecundity
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' @example man-examples/Fecundity-class.R
#'
NULL

setClass("fecundity",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 Classes='num.null',
                 Timing='num.array.null',
                 Misc='list'
         )
)


setValidity('fecundity', function(object) {
  # TODO 
  TRUE
})

setMethod("initialize", "fecundity", function(.Object,
                                              Pars=list(L50=NA, L50_95=NA, MaxFec=NA),
                                              Model=NULL,
                                              Units='eggs',
                                              MeanAtAge=NULL,
                                              MeanAtLength=NULL,
                                              Classes=NULL,
                                              Timing=0,
                                              Misc=list()) {
  
  .Object@Pars <- CheckPars(Pars)
  .Object@Model <- Model
  .Object <- PopulateModel(.Object)
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Units <- Units
  .Object@Misc <- Misc
  .Object
})


