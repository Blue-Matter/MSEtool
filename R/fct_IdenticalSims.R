IdenticalSims <- function(SimList, Years, EditSlots=TRUE) {
  
  if (is.array(SimList))
    return(IdenticalSimsArray(SimList))
  
  if (is.numeric(SimList))
    return(TRUE)

  SimList <- purrr::map(SimList, \(List) {
    List <-  List |> SubsetYear(Years, AddPast = FALSE)
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


IdenticalSimsArray <- function(array, logical=TRUE) {
  if (!is.array(array))
    cli::cli_abort('`array` is not an array')
  
  dnames <- dimnames(array)
  SimInd <- which(names(dnames) == 'Sim')
  if (!length(SimInd))
    cli::cli_abort('No `Sim` dimension in this array')
  
  dd <- dim(array)    
  if (dd[SimInd]==1)
    return(TRUE)
  
  Ref <- abind::asub(array, 1, SimInd)
  if (any(is.na(Ref)))
    return(FALSE)
  nSim <- dimnames(array)[[SimInd]] |> length()
  
  for (sim in 2:nSim) {
    Comp1 <- abind::asub(array, sim, SimInd)
    Comp2 <- abind::asub(array, sim-1, SimInd)
    if (any(round(Comp1, 4) != round(Comp2, 4)))
      return(FALSE)
  }
  TRUE
}




# CheckIdenticalSims(SimList, Equilibrium=TRUE)

# FindDiffSlots(SimList[[1]], SimList[[2]])


FindDiffSlots <- function(object1, object2) {

  if (class(object1) != class(object2))
    cli::cli_abort('Both objects must be the same class')

  if (isS4(object1)) {
    slots <- slotNames(object1)
    for (x in seq_along(slots)) {
      obj1 <- slot(object1, slots[x])
      obj2 <- slot(object2, slots[x])
      chk <- digest::digest(obj1, algo='spookyhash') ==  digest::digest(obj2, algo='spookyhash')
      if (!chk) {
        cli::cli_alert_info("{.val {slots[x]}}")
        Recall(obj1, obj2)
      }
    }
  }
  
  if (is.list(object1)) {
    for (x in seq_along(object1)) {
      obj1 <- object1[[x]]
      obj2 <- object2[[x]]
      chk <- digest::digest(obj1, algo='spookyhash') ==  digest::digest(obj2, algo='spookyhash')
      if (!chk) {
        cli::cli_alert_info("{.val {names(object1)[x]}}")
        Recall(obj1, obj2)
      }
    }
  }
  
  
}




# Equilibrium = ignores recruitment deviations
CheckIdenticalSims <- function(SimList, 
                               Years=NULL, 
                               Period=c('Historical', 'Projection', 'All'), 
                               Equilibrium=FALSE) {
  
  # Period <- match.arg(Period)
  # 
  # if (is.null(Years))
  #   Years <- Years(SimList[[1]]@OM, Period)
  
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

