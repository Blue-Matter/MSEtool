
CalcFisheryDynamics <- function(Hist, 
                                Years=NULL,
                                Sims=NULL,
                                DoCalcCatch=1,
                                DoCalcaggF=1,
                                IdenticalSim=FALSE) {
  
  nStock <- nStock(Hist)
  nFleet <- nFleet(Hist)
  nArea <- nArea(Hist)
  AllYears <- Years(Hist@OM)
  nSim <- nSim(Hist)
  
  
  if (is.null(Years)) {
    Years <- Years(Hist@OM,'H')
  }
  if (is.null(Sims)) {
    Sims <- 1:nSim
  }
    
  
  if (IdenticalSim) { 
    # do only for first sim
    Hist_1 <- CalcFisheryDynamics_(HistIn=Hist, 
                                   Years=Years,
                                   AllYears=AllYears,
                                   Sims=1,
                                   nSim=nSim,
                                   nStock=nStock,
                                   nFleet=nFleet,
                                   nArea=nArea,
                                   DoCalcCatch=DoCalcCatch,
                                   DoCalcaggF=DoCalcaggF)
    

    for (sl in slotNames('timeseries')) {
      if (sl=='Misc') next()
      slot(Hist, sl) <- CopyFirstSim(x=slot(Hist_1, sl))
      
    }
    return(Hist)
  }
  
  # sim-dependent
  Hist <- CalcFisheryDynamics_(HistIn=Hist, 
                       Years=Years,
                       AllYears=AllYears,
                       Sims=Sims,
                       nSim=nSim,
                       nStock=nStock,
                       nFleet=nFleet,
                       nArea=nArea,
                       DoCalcCatch=DoCalcCatch,
                       DoCalcaggF=DoCalcaggF)
  
  Hist
}
