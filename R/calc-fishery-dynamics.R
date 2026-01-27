
CalcFisheryDynamics <- function(Hist, 
                                Years,
                                DoCalcCatch=1,
                                DoCalcaggF=1,
                                IdenticalSim=FALSE) {
  
  nStock <- nStock(Hist)
  nFleet <- nFleet(Hist)
  nArea <- nArea(Hist)
  AllYears <- Years(Hist@OM)
  nSim <- nSim(Hist)
  
  if (IdenticalSim) { 
    # do only for first sim
    Hist_1 <- Subset(Hist, Sim=1)
    Hist_1 <- CalcFisheryDynamics_(HistIn=Hist_1, 
                                   Years=Years,
                                   AllYears=AllYears,
                                   nSim=1,
                                   nStock=nStock,
                                   nFleet=nFleet,
                                   nArea=nArea,
                                   DoCalcCatch=DoCalcCatch,
                                   DoCalcaggF=DoCalcaggF)
    
    # fill nSim in Hist
    slots <- slotNames('timeseries')
    for (sl in slots) {
      if (sl=='Misc')
        next()
      slot(Hist, sl) <- ExtendSims(slot(Hist_1, sl), nSim)
    }
    
    return(Hist)
  }
  
  # sim-dependent
  CalcFisheryDynamics_(HistIn=Hist, 
                       Years=Years,
                       AllYears=AllYears,
                       nSim=nSim,
                       nStock=nStock,
                       nFleet=nFleet,
                       nArea=nArea,
                       DoCalcCatch=DoCalcCatch,
                       DoCalcaggF=DoCalcaggF)
  
  
  
}