

setClass('catchobs',
         slots=c(
           CV='num.array.list', # numeric length 1, length 2, or length nSim
           Error='num.array.list', # numeric array nsim by nTS
           Bias='num.array.list',  # numeric length 1, length 2, or length nSim
           TimeSteps='num.list.null',
           Type='char.list', # 'Removals' or 'Landings'
           Ref='num.array.list'
           ), 
         contains='MiscClass'
)

setMethod("initialize", "catchobs", function(.Object,
                                             CV=numeric(),
                                             Error=numeric(),
                                             Bias=numeric(),
                                             TimeSteps=NULL,
                                             Type='Removals',
                                             Ref=numeric()) {
  .Object@CV <- CV
  .Object@Error <- Error
  .Object@Bias <- Bias
  .Object@TimeSteps <- TimeSteps
  .Object@Type <- Type
  .Object@Ref <- Ref
  .Object
})



setClass('effortobs',
         slots=c(
           CV='num.array.list', # numeric length 1, length 2, or length nSim
           Error='num.array.list', # numeric array nsim by nTS
           Bias='num.array.list',  # numeric length 1, length 2, or length nSim
           TimeSteps='num.list.null',
           Ref='num.array.list'
         ), 
         contains='MiscClass'
)


setClass('indicesobs',
         slots=c(
           CV='num.array.list',
           Error='num.array.list',
           Beta='num.array.list',
           AC='num.array.list',
           TimeSteps='num.list.null',
           Selectivity='array.char.num', # Biomass, SBiomass, age classes
           Type='character',
           Ref='num.array.list',
           q='num.array.list' 
         ),
         contains='MiscClass'
)

setClass('CompObs',
         slots=c(
           ESS='num.array', # nSim, nTS
           TimeSteps='num.null',
           Bias='num.array'
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
         slots=c(Name='character',
                 
                 Effort='effortobs',
                 
                 Landings='catchobs',
                 Discards='catchobs',
                 
                 CPUE='indicesobs',
                 Survey='indicesobs',
                 
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

validObsObject <- function(object) {
  TRUE
}
setValidity('obs', validObsObject)

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

