

setClass('CatchObs',
         slots=c(CV='num.array', # numeric length 1, length 2, or length nSim
                 Error='num.array', # numeric array nsim by nTS
                 Bias='num.array',  # numeric length 1, length 2, or length nSim
                 TimeSteps='char.num',
                 Type='character'), # 'removals' or 'landings'
         contains='MiscClass'
)

setMethod("initialize", "CatchObs", function(.Object,
                                         CV=numeric(),
                                         Error=numeric(),
                                         Bias=numeric(),
                                         TimeSteps=NULL,
                                         Type='removals') {
  .Object@CV <- CV
  .Object@Error <- Error
  .Object@Bias <- Bias
  .Object@TimeSteps <- TimeSteps
  .Object@Type <- Type
  .Object
})


setClass('IndexObs',
         slots=c(
           Error='num.array',
           Beta='num.array',
           Time='num.null'
         ),
         contains='MiscClass'
)

setClass('CompObs',
         slots=c(
           ESS='num.array', # nSim, nTS
           Time='num.null'
         ),
         contains='MiscClass'
)


#' Obs Object
#' @include 00_Class_unions.R
#' 
#' @name ObsClass
#'
#' @export
setClass('obs',
         slots=c(Catch='CatchObs',
                 Index='IndexObs',
                 CAA='CompObs',
                 CAL='CompObs'
         ),
         contains='MiscClass')



#' @describeIn ObsClass description
#' @export
Obs <- function(object=NULL) {
  if (inherits(object, 'om'))
    return(object@Obs)
  
  .Object <- methods::new('obs')
  
  validObject(.Object)
  .Object
}

validDataObject <- function(object) {
  TRUE
}
setValidity('obs', validDataObject)

setMethod("initialize", "obs", function(.Object) {

  .Object
})

#' @describeIn ObsClass Assign an `Obs` object to a [OM()] object
#' @param x An [OM()] class object
#' @param value An `obs` class object to assign to `x`
#' @export
`Obs<-` <- function(x, value) {
  assignSlot(x, value, 'Obs')
}

