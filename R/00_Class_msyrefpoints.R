#' @include 00_Class_popdynamics.R

setClass("msyrefpoints",
         slots=c(FMSY='array.numeric.null', # Sim, Stock, TimeStep
                 BMSY='array.numeric.null', # Sim, Stock,  TimeStep
                 SBMSY='array.numeric.null', # Sim, Stock, TimeStep
                 SPMSY='array.numeric.null', # Sim, Stock, TimeStep
                 SPRMSY='array.numeric.null', # Sim, Stock, TimeStep
                 MSYRemovals='array.numeric.null', # Sim, Stock, TimeStep
                 MSYLandings='array.numeric.null' # Sim, Stock, TimeStep   
         )
)



MSYRefPoints <- function(MSE=NULL, ...) {
  if (inherits(MSE, 'mse'))
    return(MSE@RefPoints@MSYRefPoints)
  
  ArgList <- list(...)
  nSim <- ArgList$nSim 
  StockNames <- ArgList$StockNames
  TimeSteps <- ArgList$TimeSteps
  
  msyrefpoints <- new('msyrefpoints')
  
  if (is.null(nSim)) {
    Array <- array(NA, dim=c(length(StockNames), length(TimeSteps)),
                   dimnames=list(
                     Stock=StockNames,
                     TimeStep=TimeSteps))
  } else {
    Array <- array(NA, dim=c(nSim, length(StockNames), length(TimeSteps)),
                   dimnames=list(
                     Sim=1:nSim,
                     Stock=StockNames,
                     TimeStep=TimeSteps))
    
  }
  for (sl in slotNames(msyrefpoints)) {
    slot(msyrefpoints, sl) <- Array
  }
  msyrefpoints
}

# # Equilibrium values for a given F 
# make this per-recruit
# setClass("curves",
#          slots=c(FValues='numeric',
#                  NPR='list',
#                  NPRS='list',
#                  SPR='list',
#                  YPR='list',
#                  RPR='list',
#                  RelRec='list',
#                  Recruit='list',
#                  Yield='list',
#                  Removal='list',
#                  Biomass='list',
#                  SBiomass='list',
#                  SP='list',
#                  Misc='list'
#          )
# )
