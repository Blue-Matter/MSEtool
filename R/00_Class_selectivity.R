
#' Selectivity Object
#'
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('selectivity', c('Check'))`
#'
#' @name Selectivity
#' @rdname Selectivity
#' @docType class
#' @example man-examples/Selectivity-class.R
#' @export
setClass("selectivity",
         slots=c(Pars='list',
                 Model='fun.char'),
         contains= c('MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'MeanAtWeightClass',
                     'ClassesClass',
                     'MiscClass'))

setValidity('selectivity', isValidObject)

setMethod("initialize", "selectivity", function(.Object,
                                                Pars=list(),
                                                Model=NULL,
                                                MeanAtAge=NULL,
                                                MeanAtLength=NULL,
                                                MeanAtWeight=NULL,
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
  .Object@MeanAtWeight <- MeanAtWeight
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Selectivity Create a new `Selectivity` object
#' @export
Selectivity <- function(Pars=list(),
                        Model=NULL,
                        MeanAtAge=NULL,
                        MeanAtLength=NULL,
                        MeanAtWeight=NULL,
                        Classes=NULL,
                        Misc=list()) {
  
  if (methods::is(Pars, 'fleet'))
    return(Pars@Selectivity)
  
  methods::new('selectivity',
               Pars=Pars,
               Model=Model,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               MeanAtWeight=MeanAtWeight,
               Classes=Classes,
               Misc=Misc)
}

#' @describeIn Selectivity Assign an `Selectivity` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Selectivity` object to assign to `x`
#' @export
`Selectivity<-` <- function(x, value) {
  assignSlot(x, value, 'Selectivity')
}
