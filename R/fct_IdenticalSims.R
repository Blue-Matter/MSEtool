IdenticalSims <- function(SimList, TimeSteps, EditSlots=TRUE) {
  
  if (is.array(SimList))
    return(IdenticalSimsArray(SimList))
  
  if (is.numeric(SimList))
    return(TRUE)

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

# CheckIdenticalSims(SimList, Equilibrium=TRUE)

# FindDiffSlots <- function(object1, object2) {
#   if (class(object1) != class(object2))
#     cli::cli_abort('Both objects must be the same class')
#   
#   digest::digest(object1, algo='spookyhash')
#   digest::digest(object2, algo='spookyhash')
#   
#   
#   digest::digest(object1@OM@Stock[[1]]@SRR, algo='spookyhash')
#   digest::digest(object2@OM@Stock[[1]]@SRR, algo='spookyhash')
#   
#   slots <- slotNames(object1)
#   
#   for (i in seq_along(slots)) {
#     sub1 <- slot(object1, slots[i])
#     sub2 <- slot(object2, slots[i])
#     
#     
#   }
#   
#   
# }




# Equilibrium = ignores recruitment deviations
CheckIdenticalSims <- function(SimList, 
                               TimeSteps=NULL, 
                               Period=c('Historical', 'Projection', 'All'), 
                               Equilibrium=FALSE) {
  
  Period <- match.arg(Period)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(SimList[[1]]@OM, Period)
  Digest <- vector('character', length(SimList)) 
  
  for (i in seq_along(SimList)) {
    if (Equilibrium) {
      SimList[[i]]@OM@Stock <- lapply(SimList[[i]]@OM@Stock, EditSlotsForSimCheck)
    }
     
    Digest[i] <- digest::digest(SimList[[i]], algo='spookyhash')
    if (Digest[i] != Digest[1]) {
      return(FALSE)
    } 
  }
  TRUE
}

# removes rec devs so no differences across sims
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
      object2@RecDevInit <-  array()
      object2@RecDevHist <-  array()
      object2@RecDevProj <- array()
    }
    
    slot(object, nm) <- object2
    
    # slots <- slotNames(object2)
    # if (!'Pars' %in% slots)
    #   next()
    # object2@Pars <- list()
    
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
