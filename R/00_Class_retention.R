
#' Retention Object
#'
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('retention', c('Check'))`
#'
#' @name Retention
#' @rdname Retention
#' @docType class
#' @example man-examples/Retention-class.R
#' @export
setClass("retention",
         slots=c(Pars='list',
                 Model='fun.char'),
         contains= c('MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'ClassesClass',
                     'MiscClass'))

setValidity('retention', isValidObject)

setMethod("initialize", "retention", function(.Object,
                                              Pars=list(),
                                              Model=NULL,
                                              MeanAtAge=NULL,
                                              MeanAtLength=NULL,
                                              Classes=NULL,
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
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Retention Create a new `Retention` object
#' @export
Retention <- function(Pars=list(),
                      Model=NULL,
                      MeanAtAge=NULL,
                      MeanAtLength=NULL,
                      Classes=NULL,
                      Misc=list()) {
  
  if (methods::is(Pars, 'fleet'))
    return(Pars@Retention)
  
  methods::new('retention',
               Pars=Pars,
               Model=Model,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Classes=Classes,
               Misc=Misc)
}

#' @describeIn Retention Assign an `Retention` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Retention` object to assign to `x`
#' @export
`Retention<-` <- function(x, value) {
  assignSlot(x, value, 'Retention')
}

