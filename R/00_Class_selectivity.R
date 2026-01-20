
#' Selectivity Object
#'
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('selectivity')`
#'
#' @name Selectivity
#' @rdname Selectivity
#' @docType class
#' @export
setClass("selectivity",
         slots=c(Pars='list',
                 Model='fun.char',
                 isRel='char.log.num',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 MeanAtWeight='num.array.null',
                 Classes='num.null',
                 Misc='list')
)




setValidity('selectivity', function(object) {
  #TODO
  TRUE
})


setMethod("initialize", "selectivity", function(.Object,
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

#' @describeIn Selectivity Create a new `Selectivity` object
#' @export
Selectivity <- function(Pars=list(),
                        Model=NULL,
                        MeanAtAge=NULL,
                        MeanAtLength=NULL,
                        MeanAtWeight=NULL,
                        Classes=NULL,
                        isRel=FALSE,
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
               isRel=isRel,
               Misc=Misc)
}

#' @describeIn Selectivity Assign an `Selectivity` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Selectivity` object to assign to `x`
#' @export
`Selectivity<-` <- function(x, value) {
  assignSlot(x, value, 'Selectivity')
}
