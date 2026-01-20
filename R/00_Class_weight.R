
#' Weight Class and Constructor
#'
#'
#' @seealso [WeightModels()], [Populate()], [Stock()]
#'
#' @name Weight
#' @rdname Weight
#' @example man-examples/class-weight.R
#' 
#'
#' @include 00_Class_unions.R
#' @include 00_Class_stock.R
#' @export
NULL 

setClass("weight",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 CVatAge='num.array.null',
                 Dist='character',
                 TruncSD='num.array.null',
                 Timing='num.array.null',
                 Random='num.array.null',
                 ASK='array.null',
                 Classes='num.null',
                 Misc='list'
         )
)

setValidity("weight", function(object) {
  
  # TODO - update for all legitimate cases
  
  TRUE
})


setMethod("initialize", "weight", function(.Object,
                                           Pars=list(),
                                           Model=NULL,
                                           Units='g',
                                           MeanAtAge=NULL,
                                           MeanAtLength=NULL,
                                           CVatAge=NULL,
                                           Dist='lognormal',
                                           TruncSD=2,
                                           Timing=0,
                                           Random=NULL,
                                           ASK=NULL,
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
  
  .Object@Units <- Units
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@CVatAge <- CVatAge
  .Object@Dist <- Dist
  .Object@TruncSD <- TruncSD
  .Object@Timing <- Timing
  .Object@Random <- Random
  .Object@ASK <- ASK
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  .Object
})
