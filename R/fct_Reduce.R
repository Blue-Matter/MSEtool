
ReduceHist <- function(Hist, Reduce=TRUE) {
  if (!Reduce)
    return(Hist)
  
  # TODO 
  Hist@OM <- ArrayReduceDims(Hist@OM, IncYear=FALSE)
  Hist@Unfished <- ArrayReduceDims(Hist@Unfished, IncYear=FALSE)
  Hist@RefPointsMSY <- ArrayReduceDims(Hist@RefPointsMSY, IncYear=FALSE)
  Hist <- ReduceTimeSeries(Hist)
  
  # Hist@Number <- ArrayReduceDims(Hist@Number, IncYear = FALSE)
  # Hist@Biomass <- ArrayReduceDims(Hist@Biomass, IncYear = FALSE)
  # Hist@SBiomass <- ArrayReduceDims(Hist@SBiomass, IncYear = FALSE)
  # Hist@SProduction <- ArrayReduceDims(Hist@SProduction, IncYear = FALSE)
  # Hist@Landings <- ArrayReduceDims(Hist@Landings, IncYear = FALSE)
  # Hist@Discards <- ArrayReduceDims(Hist@Discards, IncYear = FALSE)
  # Hist@Effort <- ArrayReduceDims(Hist@Effort, IncYear = FALSE)
  # Hist@Distribution <- ArrayReduceDims(Hist@Distribution, IncYear = FALSE)
  # Hist@FDead <- ArrayReduceDims(Hist@FDead, IncYear = FALSE)
  # Hist@FDeadArea <- ArrayReduceDims(Hist@FDeadArea, IncYear = FALSE)
  # Hist@FRetain <- ArrayReduceDims(Hist@FRetain, IncYear = FALSE)
  # Hist@FRetainArea <- ArrayReduceDims(Hist@FRetainArea, IncYear = FALSE)
  Hist
}



ReduceMSE <- function(MSE, Reduce=TRUE) {
  if (!Reduce)
    return(MSE)
  
  MSE@OM <- ArrayReduceDims(MSE@OM)
  MSE@Hist <- ReduceTimeSeries(MSE@Hist)
  MSE <- ReduceTimeSeries(MSE)
  MSE
}

ReduceTimeSeries <- function(object) {
  slots <- slotNames('timeseries')
  slots <- slots[!slots=='Misc']
  
  for (sl in slots) {
    slot(object, sl) <- ArrayReduceDims(slot(object, sl), IncYear = FALSE)
  }
  object
}