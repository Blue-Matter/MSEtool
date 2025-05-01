DoMICE <- function(OMListSim) {
  
  if (!inherits(OMListSim, 'OMListSim'))
    stop('`OMListSim` must be class `OMListSim`')
  
  # Do MICE if applicable
  # update length, weight, fleet weight, natural mortality, maturity, etc 
  
  # TODO confirm timing 
  # - currently done at beginning of time step.
  # - N-at-age at beginning of this TS has been calculated
  # - Biomass etc have not 

  
  OMListSim
  
}