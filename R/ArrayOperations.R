

# TODO - improve and consolidate to generic Subset function


ArraySubsetYear <- function(object, Years=NULL, AddPast=TRUE) {
  
  
  if (is.null(Years))
    return(object)
  
  Years <- as.numeric(Years)
  
  DN <- dimnames(object)
  if (is.null(DN))
    return(object)
  
  DN$Year <- as.numeric(DN$Year)
  TSind <- which(names(DN) == 'Year')
  if (length(TSind)==0)
    cli::cli_abort("`Year` dimension not found in this array", .internal=TRUE)
  
  TSexist <- Years[Years %in% DN$Year]
  TSimpute <- Years[!Years %in% DN$Year]
  
  if (!AddPast & length(TSexist)) {
    TSimpute <- TSimpute[!TSimpute<min(DN$Year)]
  }
  
  if (length(TSimpute)) {
    # need to impute some values
    matchTS <- rep(NA, length(TSimpute))
    for (i in seq_along(TSimpute)) {
      
      FutureImpute <- which(DN$Year < TSimpute[i]) 
      if (length(FutureImpute)>0)
        matchTS[i] <- DN$Year[max(FutureImpute)]
      
      PastImpute <- which(DN$Year >TSimpute[i]) 
      if (length(PastImpute)>0)
        matchTS[i] <- DN$Year[min(PastImpute)]
    }
    
    YearsMod <- c(TSexist, matchTS) |> as.character() 
    array <- abind::asub(object, YearsMod, TSind, drop=FALSE)
    dimnames(array)$Year <- Years
    return(array)
  }
  
  abind::asub(object, (DN[[TSind]] %in% Years), TSind, drop=FALSE)
}

ArraySubsetSim <- function(object, Sims=NULL, drop=FALSE) {
  if (is.null(Sims))
    return(object)
  
  Sims <- Sims |> as.numeric() 
  
  DN <- dimnames(object)
  DN$Sim <- as.numeric(DN$Sim)
  TSind <- which(names(DN) == 'Sim')
  
  if (length(TSind)==0)
    cli::cli_abort("`Sim` dimension not found in this array", .internal=TRUE)
  
  if (any(Sims > max(DN$Sim))) {
    TSexist <- Sims[Sims %in% DN$Sim]
    TSimpute <- Sims[!Sims %in% DN$Sim]
    if (length(TSimpute)) {
      matchTS <- rep(NA, length(TSimpute))
      for (i in seq_along(TSimpute)) {
        matchTS[i] <- DN$Sim[DN$Sim < TSimpute[i]] |> max()
      }
    }
    YearsMod <- c(TSexist, matchTS)
    array <- abind::asub(object, YearsMod, TSind, drop=drop)

    if (!is.null( dimnames(array)))
      dimnames(array)$`Sim` <- Sims
    return(array)
  } 
  if (drop) {
    out <- abind::adrop(abind::asub(object, (DN[[TSind]] %in% Sims), TSind, drop=FALSE), TSind)
    if (is.null(dim(out))) {
      nms <- names(out)
      numericNames <- suppressWarnings(as.numeric(nms))
      if (is.null(nms)) {
        return(out)
      } else  if (any(is.na(numericNames))) {
        out <- array(out, dim=length(out), dimnames=list(Fleet=names(out)))
      } else {
        out <- array(out, dim=length(out), dimnames=list(Year=names(out)))
      }
    }
    return(out)  
  } else {
    out <- abind::asub(object, (DN[[TSind]] %in% Sims), TSind, drop=FALSE)  
    return(out)
  }
  
}

ArraySubsetAge <- function(object, Ages=NULL, drop=FALSE) {
  if (is.null(Ages))
    return(object)
  
  Ages <- Ages |> as.numeric() 
  
  DN <- dimnames(object)
  DN$Age <- as.numeric(DN$Age)
  AgeInd <- which(names(DN) == 'Age')
  
  if (length(AgeInd)==0)
    cli::cli_abort("`Age` dimension not found in this array", .internal=TRUE)
  
  if (any(Ages > max(DN$Age))) {
    cli::cli_abort("`Ages` greater than ages in this array", .internal=TRUE)
  } 
  
  abind::asub(object, (DN[[AgeInd]] %in% Ages), AgeInd, drop=FALSE) 
}





#' @export
ArrayReduceDims <- function(array, 
                            IncSim=TRUE, 
                            IncAge=FALSE, 
                            IncYear=TRUE,
                            debug=FALSE, 
                            silent=FALSE, 
                            id=NULL) {
  
  if (debug)
    print(class(array))
  
  if (!IncSim & !IncYear & !IncAge) 
    return(array)
  
  if (!length(array))
    return(array)
  
  if (!is.array(array)) {
    if (isS4(array)) {
      if (!silent) {
        if (is.null(id)) {
          id <- cli::cli_progress_bar('Reducing dimensions to minimum size', type='tasks')
        } else {
          cli::cli_progress_update(id=id) 
        }
      }
      
      if (inherits(array, 'data'))
        return(array)
      slots <- slotNames(array)
      for (sl in slots) {
        slot(array, sl) <- Recall(slot(array, sl), IncSim, IncAge, IncYear, debug, silent, id)
      }
      return(array)
    }
    if (is.list(array)) {
      if (length(array)) {
        for (i in 1:length(array)) {
          temp <- Recall(array[[i]], IncSim, IncAge, IncYear, debug, silent, id)
          if (!is.null(temp))
            array[[i]] <- temp 
        }
        return(array)
      }
    }
  }
  
  dnames <- array |> dimnames() |> names()
  
  indSim <- which(dnames=='Sim')
  indAge <- which(dnames=='Age')
  indYear <- which(dnames=='Year')
  
  incSim <- length(indSim)
  incAge <- length(indAge)
  incYear <- length(indYear)
  
  if (!IncSim)
    incSim <- 0
  
  if (!IncAge)
    incAge <- 0
  
  if (!IncYear)
    incYear <- 0
  
  if (incSim & incYear & incAge) {
    idenSim <- IdenticalSims(array) 
    idenTime <- IdenticalYears(array)
    idenAge <- IdenticalAge(array)
    
    if (idenSim & idenTime & idenAge) 
      return(abind::asub(array, list(1,1,1), c(indSim, indAge, indYear), drop=FALSE))
    
    if (!idenSim & idenTime & idenAge) 
      return(abind::asub(array, list(1,1), c(indAge, indYear), drop=FALSE))
    
    if (idenSim & !idenTime & idenAge) {
      return(abind::asub(array, list(1,1,UniqueYears(array)), c(indSim, indAge, indYear), drop=FALSE))
    }
    
    if (!idenSim & !idenTime & idenAge) 
      return(abind::asub(array, list(1,UniqueYears(array)), c(indAge, idenTime), drop=FALSE))
    
    if (idenSim & idenTime & !idenAge)
      return(abind::asub(array, list(1,1), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & idenTime & !idenAge) 
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (idenSim & !idenTime & !idenAge) 
      return(abind::asub(array, list(1, UniqueYears(array)), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & !idenTime & !idenAge) {
      return(abind::asub(array, list(UniqueYears(array)), c(indYear), drop=FALSE))
    }
    
  }
  
  if (!incSim & incYear & incAge) {
    idenTime <- IdenticalYears(array)
    idenAge <- IdenticalAge(array)
    
    if (idenTime & idenAge) 
      return(abind::asub(array, list(1,1), c(indAge, indYear), drop=FALSE))
    if (!idenTime & idenAge) 
      return(abind::asub(array, list(1, UniqueYears(array)), c(indAge, indYear), drop=FALSE))
    
    if (idenTime & !idenAge)
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (!idenTime & !idenAge) 
      return(abind::asub(array, list(UniqueYears(array)), c(indYear), drop=FALSE))
  }
  
  if (incSim & !incYear & incAge) {
    idenSim <- IdenticalSims(array) 
    idenAge <- IdenticalAge(array)
    
    if (idenSim  & idenAge) 
      return(abind::asub(array, list(1,1), c(indSim, indAge), drop=FALSE))
    
    if (!idenSim & idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (idenSim & idenAge) 
      return(abind::asub(array, list(1,1), c(indSim, indAge), drop=FALSE))
    
    if (!idenSim & idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (idenSim & !idenAge)
      return(abind::asub(array, list(1,1), c(indSim), drop=FALSE))
    
    if (!idenSim & !idenAge) 
      return(array)
    
  }
  
  if (!incSim & !incYear & incAge) {
    idenAge <- IdenticalAge(array)
    
    if (idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (!idenAge) 
      return(array)
  }
  
  if (incSim & incYear & !incAge) {
    idenSim <- IdenticalSims(array) 
    idenTime <- IdenticalYears(array)
    
    if (idenSim & idenTime) 
      return(abind::asub(array, list(1,1), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & idenTime) 
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (idenSim & !idenTime) 
      return(abind::asub(array, list(1, UniqueYears(array)), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & !idenTime) 
      return(abind::asub(array, list(UniqueYears(array)), c(indYear), drop=FALSE))
    
  }
  
  if (!incSim & incYear & !incAge) {
    idenTime <- IdenticalYears(array)
    if (idenTime) 
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (!idenTime) 
      return(abind::asub(array, UniqueYears(array), indYear, drop=FALSE))
  }
  
  if (incSim & !incYear & !incAge) {
    idenSim <- IdenticalSims(array) 
    if (idenSim) 
      return(abind::asub(array, list(1), c(indSim), drop=FALSE))
    if (!idenSim) 
      return(array)
  }
  
  
  
  array
}

