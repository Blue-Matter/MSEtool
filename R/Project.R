
ExtendHist <- function(object, TimeSteps) {
  if (isS4(object)) {
    nms <- slotNames(object)
    for (i in seq_along(nms)) {
      slot(object, nms[i]) <- Recall( slot(object, nms[i]), TimeSteps)
    }  
  }
 
  if (is.list(object)) {
    for (i in seq_along(object)) {
      object[[i]] <- Recall(object[[i]], TimeSteps)
    }  
  }
  if (inherits(object,'array'))  {
    dnames <- names(dimnames(object))
    if ("TimeStep" %in% dnames)
      object <- ExpandTimeSteps(object, TimeSteps, default=tiny/2)
  }
  object
}

ProjectDEV <- function(Hist=NULL, MPs=NA, silent=FALSE) {
  
  TimeSteps <- TimeSteps(Hist@OM)
  TimeStepsHist <- TimeSteps(Hist@OM, 'Historical')
  TimeStepsProj <- TimeSteps(Hist@OM, 'Projection')
  
  
  # Extend Arrays for Projection TimeSteps 
  Proj <- ExtendHist(Hist, TimeSteps)
  
  # ---- Populate Number at Beginning of Projection TimeStep ----
  ind <- which(TimeSteps == TimeStepsProj[1])
  
  # TODO - make calc number next a separate function
  # update N at begining of first projection timestep
  Proj@Number[[st]][,,ind,]

  
  # List of `Hist` objects, each with one simulation
  ProjSimList <- Hist2HistSimList(Proj)
  
  # ---- Calculate Reference Catch ----
  # HistSimList <- OptimRefCatch(HistSimList) # TODO
  
  
  # Hist@MPs - need to store in Misc ?
  
  
  ProjectMP 
  
  
}