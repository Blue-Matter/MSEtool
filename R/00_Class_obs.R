

setClass('catchobs',
         slots=c(
           CV='num.array.list', # numeric length 1, length 2, or length nSim
           Error='num.array.list', # numeric array nsim by nTS
           Bias='num.array.list',  # numeric length 1, length 2, or length nSim
           Years='num.list.null',
           Type='char.list', # 'Removals' or 'Landings'
           Ref='num.array.list',
           Misc='list'
           )
)

setMethod("initialize", "catchobs", function(.Object,
                                             CV=numeric(),
                                             Error=numeric(),
                                             Bias=numeric(),
                                             Years=NULL,
                                             Type='Removals',
                                             Ref=numeric()) {
  .Object@CV <- CV
  .Object@Error <- Error
  .Object@Bias <- Bias
  .Object@Years <- Years
  .Object@Type <- Type
  .Object@Ref <- Ref
  .Object
})



setClass('effortobs',
         slots=c(
           CV='num.array.list', # numeric length 1, length 2, or length nSim
           Error='num.array.list', # numeric array nsim by nTS
           Bias='num.array.list',  # numeric length 1, length 2, or length nSim
           Years='num.list.null',
           Ref='num.array.list',
           Misc='list'
         )
)


setClass('indicesobs',
         slots=c(
           CV='num.array.list',
           Error='num.array.list',
           Beta='num.array.list',
           AC='num.array.list',
           Years='num.list.null',
           Selectivity='array.char.num', # Biomass, SBiomass, age classes
           Type='character',
           Ref='num.array.list',
           q='num.array.list',
           Misc='list'
         )
)

setClass('CompObs',
         slots=c(
           SampleSize='num.array', 
           ESS='num.array', # nSim, nTS
           Years='num.null',
           Bias='num.array',
           Misc='list'
         )
)



# named parameters matching those in Pars
setClass('lifehistoryobs',
         slots=c(
           Ages='list',
           Length='list',
           Weight='list',
           NaturalMortality='list',
           Maturity='list',
           Fecundity='list',
           SRR='list',
           Spatial='list',
           Depletion='list',
           Misc='list'
         )
)

setClass('exploitationobs',
         slots=c(
           Selectivity='list',
           Retention='list',
           DiscardMortality='list',
           Misc='list'
         )
)

#' Obs Object
#' @include 00_Class_unions.R
#' 
#' @name ObsClass
#'
#' @export
setClass('obs',
         slots=c(Name='character',
                 LifeHistory='lifehistoryobs',
                 Exploitation='exploitationobs',
                 
                 Effort='effortobs',
                 
                 Landings='catchobs',
                 Discards='catchobs',
                 
                 CPUE='indicesobs',
                 Survey='indicesobs',
                 
                 CAA='CompObs',
                 CAL='CompObs',
                 
                 Misc='list'
         )
)



#' @describeIn ObsClass description
#' @export
Obs <- function(object=NULL) {
  if (inherits(object, 'om'))
    return(object@Obs)
  
  .Object <- methods::new('obs')
  
  validObject(.Object)
  .Object
}



setValidity('obs', function(object) {
  #TODO
  TRUE
})



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

