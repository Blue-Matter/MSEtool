IdenticalSims <- function(SimList, TimeSteps, EditSlots=TRUE) {
  
  if (is.array(SimList))
    return(IdenticalSimsArray(SimList))
  
  SimList <- purrr::map(SimList, \(List) {
    List <-  List |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
    if (EditSlots) 
      List <- EditSlotsForSimCheck(List)
    List
  })
  
  Digest <- vector('character', length(SimList)) 
  for (i in seq_along(SimList)) {
    Digest[i] <- digest::digest(SimList[[i]], algo='spookyhash')
    if (Digest[i] != Digest[1]) {
      return(FALSE)
    }
  }
  TRUE
}



EditSlotsForSimCheck <- function(object) {
  if (!isS4(object)) {
    for (i in seq_along(object))
      object[[i]] <- Recall(object[[i]])
  }

  nms <- slotNames(object)
  for (nm in nms) {
    object2 <- slot(object, nm)
    if (!isS4(object2))
      next()
    if (inherits(object2, 'srr')) {
      dimnames(object2@RecDevInit) <- NULL
      names(object2@RecDevInit) <- NULL
      object2@RecDevInit <- array(object2@RecDevInit)
      object2@RecDevProj <- array()
    }
    
    slots <- slotNames(object2)
    
    if (!'Pars' %in% slots)
      next()
    object2@Pars <- list()
    slot(object, nm) <- object2
  }
  object
}

IdenticalSimsArray <- function(array) {
  if (!is.array(array))
    return(TRUE)
  
  dnames <- dimnames(array)
  SimInd <- which(names(dnames) == 'Sim')
  
  if (!length(SimInd))
    return(TRUE)
  
  dd <- dim(array)    
  if (dd[SimInd]==1)
    return(TRUE)
  
  meanSim <- apply(array, SimInd, mean)
  
  meanmeanSim <- mean(meanSim)
  if (meanmeanSim<=0)
    return(TRUE)
  
  sum(meanSim/meanmeanSim - 1) < 1E-6
  

  
}