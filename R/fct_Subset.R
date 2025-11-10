

SubsetSim <- function(object, Sim=1, drop=FALSE, debug=FALSE) {
  # OUT <<- object
  
  if (debug) 
    cli::cli_alert('Class {.val {class(object)}}')
  
  # this is a piece of black magic that subsets all arrays with a Sim
  # dimension
  # Sim is a numeric vector, can be length > 1
  # If Sim includes values greater then the length of the `Sim` dimension in
  # an array, the last value in that dimension will be returned
  # All Sim dimension should be length 1 or length nsim, so that shouldn't be 
  # a problem, but you've been warned!
  if (isS4(object)) {
    if (debug) 
      cli::cli_alert('S4 Object')
    slots <- slotNames(object)
    for (i in seq_along(slots)) {
      if (debug)
        cli::cli_alert('Slot {.val {slots[i]}}')
      
      obj <- slot(object, slots[i])
      
      if (debug)
        cli::cli_alert('Class obj {.val {class(obj)}}')
      
      slot(object, slots[i]) <- Recall(obj, Sim, drop, debug)
    }
    if ('nSim' %in% slotNames(object)) 
      object@nSim <- length(Sim)
    
    return(object)
  }
  
  if (inherits(object, c('StockList',  'StockFleetList', 'FleetList', 'list'))) {
    outlist <- object 
    if (all(Sim %in% names(object))) {
      return(outlist[[Sim]])
    } 
    for (j in seq_along(object)) {
      if (is.null(object[[j]])) {
        outlist[[j]] <- object[[j]]
      } else {
        outlist[[j]] <- Recall(object[[j]], Sim, drop, debug)
      }
      
    }
    if (length(outlist)<1) 
      outlist <- object
    return(outlist)
  }
  
  if (inherits(object, 'array')) {
    dnames <- dimnames(object)
    if ("Sim" %in% names(dnames)) {
      ind <- which(names(dnames)=='Sim')
      maxSim <- max(dnames[[ind]])
      if (maxSim< max(Sim)) {
        object <- ArraySubsetSim(object, maxSim, drop)  
      } else {
        object <- ArraySubsetSim(object, Sim, drop)  
      }
    }
    return(object)
  }
  
  if (inherits(object, 'numeric')) {
    if (!is.null(names(object)))
      return(as.numeric(object[Sim]))
  } 
  
  object
}

SubsetYear <- function(object, Years, AddPast=TRUE, debug=FALSE) {
  
  if (debug) 
    cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    if (debug) 
      cli::cli_alert('S4 Object')
    slots <- slotNames(object)
    for (i in seq_along(slots)) {
      if (debug)
        cli::cli_alert('Slot {.val {slots[i]}}')
      
      obj <- slot(object, slots[i])
      
      if (debug)
        cli::cli_alert('Class obj {.val {class(obj)}}')
      
      slot(object, slots[i]) <- Recall(obj, Years, AddPast, debug)
    }
 
    return(object)
  }
  
  if (inherits(object, c('StockList',  'StockFleetList', 'FleetList', 'list'))) {
    outlist <- object 
    for (j in seq_along(object)) {
      if (is.null(object[[j]])) {
        outlist[[j]] <- object[[j]]
      } else {
        outlist[[j]] <- Recall(object[[j]], Years, AddPast, debug)
      }
      
    }
    if (length(outlist)<1) 
      outlist <- object
    return(outlist)
  }
  
  if (inherits(object, 'array')) {
    dnames <- dimnames(object)
    if ("Year" %in% names(dnames)) {
      ind <- which(names(dnames)=='Year')
      TSValues <- dnames[[ind]]
      if (is.null(TSValues))
        return(object)
      
      object <- ArraySubsetYear(object, Years, AddPast=AddPast)  
      # maxTS <- max(TSValues)
      # if (maxTS< max(Years)) {
      #   object <- ArraySubsetYear(object, maxTS, AddPast=AddPast)  
      # } else {
      #   object <- ArraySubsetYear(object, Years, AddPast=AddPast)  
      # }
    }
    return(object)
  }
  
  # if (inherits(object, 'numeric') | inherits(object, 'integer')) {
  #   if (!is.null(names(object)))
  #     return(as.numeric(object[Years]))
  # } 
  
  object
}

