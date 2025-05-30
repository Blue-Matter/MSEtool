

setClass('CatchObs',
         slots=c(Error='num.array',
                 Bias='num.array',
                 Time='num.null',
                 Misc='MiscClass')
)

setClass('IndexObs',
         slots=c(
           Error='num.array',
           Beta='num.array',
           Time='num.null',
           Misc='MiscClass'
         )
)

setClass('CompObs',
         slots=c(
           ESS='num.array', # nSim, nTS
           Time='num.null',
           Misc='MiscClass'
         )
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
                 CAL='CompObs',
                 Misc='MiscClass' 
         ))




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
  assignSlot(x, value, 'Data')
}

