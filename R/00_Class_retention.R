
#' Retention Object
#'
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('retention')`
#'
#' @name Retention
#' @rdname Retention
#' @docType class
#' @export
setClass("retention",
         slots=c(Pars='list',
                 Model='fun.char',
                 isRel='char.log.num',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 MeanAtWeight='num.array.null',
                 Classes='num.null',
                 Misc='list')
)
                 
          


setValidity('retention', function(object) {
  #TODO
  TRUE
})



setMethod("initialize", "retention", function(.Object,
                                              Pars=list(),
                                              Model=NULL,
                                              MeanAtAge=NULL,
                                              MeanAtLength=NULL,
                                              MeanAtWeight=NULL,
                                              Classes=NULL,
                                              isRel=FALSE,
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
  .Object@isRel <- isRel
  .Object@Misc <- Misc
  .Object
})

#' @describeIn Retention Create a new `Retention` object
#' @export
Retention <- function(Pars=list(),
                      Model=NULL,
                      MeanAtAge=NULL,
                      MeanAtLength=NULL,
                      MeanAtWeight=NULL,
                      Classes=NULL,
                      isRel=FALSE,
                      Misc=list()) {
  
  if (methods::is(Pars, 'fleet'))
    return(Pars@Retention)
  
  methods::new('retention',
               Pars=Pars,
               Model=Model,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               MeanAtWeight=MeanAtWeight,
               Classes=Classes,
               isRel=isRel,
               Misc=Misc)
}

#' @describeIn Retention Assign an `Retention` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Retention` object to assign to `x`
#' @export
`Retention<-` <- function(x, value) {
  assignSlot(x, value, 'Retention')
}

