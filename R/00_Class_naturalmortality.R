


#' NaturalMortality Class and Constructor
#'
#'
#'
#' @name NaturalMortality
#' @rdname NaturalMortality
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @example man-examples/NaturalMortality-class.R
NULL


setClass("naturalmortality",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 Random='num.array.null',
                 Classes='num.null',
                 Misc='list'
         )
)



setValidity('naturalmortality', function(object) {
  # TODO 
  TRUE
})

setMethod("initialize", "naturalmortality", function(.Object,
                                                     Pars=list(M=NA),
                                                     Model=NULL,
                                                     Units='year',
                                                     MeanAtAge=NULL,
                                                     MeanAtLength=NULL,
                                                     Random=NULL,
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
  .Object@Random <- Random
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  .Object
})

