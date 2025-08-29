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

CheckIdenticalSims <- function(HistSimList, TimeSteps=NULL, Period='Historical') {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(HistSimList[[1]]@OM, Period)
  Digest <- vector('character', length(HistSimList)) 
  for (i in seq_along(HistSimList)) {
    HistSimList[[i]]@OM@Stock <- lapply(HistSimList[[i]]@OM@Stock, EditSlotsForSimCheck)
    Digest[i] <- digest::digest(HistSimList[[i]], algo='spookyhash')
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

IdenticalSimsArray <- function(array, logical=TRUE) {
  if (!is.array(array))
    return(TRUE)
  
  unique <- UniqueSims(array)
  
  if (!logical) 
    return(unique)
  
  if (is.null(unique))
    return(TRUE)
  
  length(unique)==1
  
}

UniqueSims <- function(array) {
  if (!is.array(array))
    cli::cli_abort('`array` is not an array')
  
  dnames <- dimnames(array)
  SimInd <- which(names(dnames) == 'Sim')
  if (!length(SimInd))
    return(NULL)
  
  dd <- dim(array)    
  if (dd[SimInd]==1)
    return(1)
  
  meanSim <- apply(array, SimInd, mean)
  match(unique(meanSim), meanSim)
}
