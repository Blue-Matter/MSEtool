hasSlot <- function(object, slot) {
  slot %in% slotNames(object)
} 

#' @include 00_Class_popdynamics.R

setClass("refpointsMSY",
         slots=c(FMSY='array',
                 BMSY='array',
                 SBMSY='array',
                 SPMSY='array',
                 SPRMSY='array',
                 MSY='array',
                 MSYLandings='array'
         ),
         contains='MiscClass'
)


RefPointsMSY <- function(MSE=NULL, ...) {
  if (inherits(MSE, 'mse')) {
    if (hasSlot(MSE, 'RefPointsMSY')) {
      return(MSE@RefPointsMSY)
    } 
    return(MSE@Reference@MSY)
  }
  
  ArgList <- list(...)
  nSim <- ArgList$nSim
  StockNames <- ArgList$StockNames
  Years <- ArgList$Years

  refpointsMSY <- new('refpointsMSY')

  if (is.null(nSim)) {
    Array <- array(NA, dim=c(length(StockNames), length(Years)),
                   dimnames=list(
                     Stock=StockNames,
                     Year=Years))
  } else {
    Array <- array(NA, dim=c(nSim, length(StockNames), length(Years)),
                   dimnames=list(
                     Sim=1:nSim,
                     Stock=StockNames,
                     Year=Years))

  }
  for (sl in slotNames(refpointsMSY)) {
    if (sl != 'Misc') 
      slot(refpointsMSY, sl) <- Array
  }
  refpointsMSY
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
