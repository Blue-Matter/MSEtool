
#' Subset an object by simulation and/or year
#'
#' Recursively subsets an object along the `Sim`, `Year`, and/or `Age` dimensions.
#' The function operates on arbitrarily nested structures including S4 objects,
#' lists, arrays, and named numeric vectors.
#'
#' @param object An object to be subset. Supported types include:
#' * S4 objects (all slots are recursively subset)
#' * Lists (including `StockList`, `FleetList`, etc.)
#' * Arrays with a `Sim`, `Year` and/or `Age` dimension
#' * Named numeric vectors
#'
#' @param Sims Numeric vector of simulation identifiers to retain. 
#' If `NULL`, no subsetting by simulation is performed.
#' @param Years Numeric vector of years to retain. 
#' If `NULL`, no subsetting by year is performed.
#' @param Ages Numeric vector of age classes to retain. 
#' If `NULL`, no subsetting by age is performed.
#' @param Impute Logical; only relevant when subsetting by `Years`. 
#' If `TRUE` (default), years not present in the object may be imputed using the 
#' nearest available past year. 
#' If `FALSE`, years earlier than the earliest available year are not imputed.
#'
#' Subsetting is applied sequentially when multiple dimensions are supplied, allowing users
#' to extract consistent subsets across simulations, years, and age classes using a single call.
#'
#' For array objects, subsetting is performed along dimensions named 
#' `"Sim"`, `"Year"`, and/or `"Age"`.
#'  Year subsetting optionally supports imputation of missing values. 
#'  Age subsetting requires all requested ages to be present.
#'
#' For S4 objects, all slots are recursively subset. 
#' If a slot named `nSim` is present, it is updated to reflect the 
#' number of retained simulations.
#'
#' @return
#' An object of the same class as `object`, subset according to `Sims`, `Years`, and/or `Ages`.
#'
#' @example man-examples/Subset.R
#' @export
Subset <- function(object, Sims=NULL, Years=NULL, Ages=NULL, Impute=TRUE) {
  if (!is.null(Sims)) {
    object <- SubsetSim(object, Sims)
  }
  
  if (!is.null(Years)) {
    object <- SubsetYear(object, Years, Impute)
  }
  
  if (!is.null(Ages)) {
    object <- SubsetAge(object, Ages)
  }
  
  object
}

SubsetSim <- function(object, Sims, debug = FALSE) {
  
  if (debug) {
    cli::cli_alert("Class {.val {class(object)}}")
  }
  
  if (isS4(object)) {
    if (debug) {
      cli::cli_alert("S4 Object")
    }
    slots <- slotNames(object)
    
    for (s in slots) {
      if (debug) {
        cli::cli_alert("Slot {.val {s}}")
      }
      val <- slot(object, s)
      
      if (!is.null(val)) {
        slot(object, s) <- Recall(val, Sims, debug)
      }
    }
    
    if ("nSim" %in% slots) {
      object@nSim <- length(Sims)
    }
    return(object)
  }
  
  if (is.list(object)) {
    if (!is.null(names(object)) && all(Sims %in% names(object))) {
      return(object[Sims])
    }
    
    n <- length(object)
    if (n == 0) {
      return(object)
    }
    
    out <- object
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el)) {
        out[[i]] <- Recall(el, Sims, debug)
      }
    }
    return(out)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Sim" %in% names(dnames)) {
      SimVals <- as.numeric(dnames$Sim)
      
      if (max(SimVals) < max(Sims)) {
        return(object)
      }
      
      object <- ArraySubsetSim(object, Sims)
    }
    return(object)
  }
  
  if (is.numeric(object) && !is.null(names(object))) {
    if (length(object)>1 && "Sim" %in% names(object)) {
      return(as.numeric(object[Sims]))
    } else {
      return(as.numeric(object))
    }
    
  }
  
  object
}

ArraySubsetSim <- function(array, Sims=NULL) {
  CheckClass(array, 'array', 'array')
  CheckClass(Sims, c('numeric', 'integer'), 'Sims')
  
  if (is.null(Sims)) {
    return(array)
  }
    
  # if (!all(diff(Sims) == 1)) {
  #   cli::cli_abort("`Sims` must be sequentially increasing values")
  # }
  
  DN <- dimnames(array)
  if (is.null(DN) || !"Sim" %in% names(DN)) {
    cli::cli_abort("`Sim` dimension not found in this array")
  }
    
  SimVals <- as.numeric(DN$Sim)
 
  if (any(Sims > max(SimVals))) {
    if (max(SimVals)!= 1) {
      Sims <- 1:max(SimVals)
    } else {
      # Impute the sims
      return(ExtendSims(array, max(Sims)))
    }
  }
  
  full_index <- function(i, array) {
    nd <- length(dim(array))
    idx <- vector("list", nd)
    idx[[1]] <- i
    if (nd>1) {
      for (k in 2:nd) {
        idx[[k]] <- seq_len(dim(array)[k])
      } 
    }
    idx
  }
  
  idx <- SimVals %in% Sims
  out <- do.call(`[`, c(list(array), full_index(idx, array), list(drop = FALSE)))
  dimnames(out)$Sim <- seq_along(dimnames(out)$Sim)
  out
}

SubsetYear <- function(object, Years, Impute=TRUE, debug=FALSE) {
  
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
      
      slot(object, slots[i]) <- Recall(obj, Years, Impute, debug)
    }
  
    return(object)
  }
  
  if (inherits(object, c('StockList',  'StockFleetList', 'FleetList', 'list'))) {
    outlist <- object 
    for (j in seq_along(object)) {
      if (is.null(object[[j]])) {
        outlist[[j]] <- object[[j]]
      } else {
        outlist[[j]] <- Recall(object[[j]], Years, Impute, debug)
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
      
      object <- ArraySubsetYear(object, Years, Impute=Impute)  
    }
    return(object)
  }
  
  object
}

ArraySubsetYear<- function(array, Years = NULL, Impute = TRUE) {
  
  if (is.null(Years)) {
    return(array)
  }
    
  Years <- as.numeric(Years)
  
  DN <- dimnames(array)
  if (is.null(DN)) {
    return(array)
  }
    
  TSind <- match("Year", names(DN))
  if (is.na(TSind)) {
    cli::cli_abort("`Year` dimension not found in this array")
  }
    
  YearVals <- as.numeric(DN[[TSind]])

  ## existing vs missing years
  TSexist  <- Years[Years %in% YearVals]
  TSimpute <- Years[!Years %in% YearVals]
  
  ## Impute = FALSE → drop years before earliest available
  if (!Impute && length(TSexist)) {
    TSimpute <- TSimpute[TSimpute >= min(YearVals)]
  }
    
  if (length(TSimpute)) {
    array <- ExtendYears(array, TSimpute)
    YearVals <- as.numeric(dimnames(array)[[TSind]])
  }
  
  ## helper: build index list for arbitrary dimensions
  make_index <- function(i, array) {
    nd <- length(dim(array))
    idx <- vector("list", nd)
    for (k in seq_len(nd))
      idx[[k]] <- if (k == TSind) i else seq_len(dim(array)[k])
    idx
  }
  
  idx <- YearVals %in% Years
  do.call(`[`, c(list(array), make_index(idx, array), list(drop = FALSE)))
}

SubsetAge <- function(object, Ages, debug = FALSE) {
  
  if (debug)
    cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    
    if (debug)
      cli::cli_alert('S4 Object')
    
    slots <- slotNames(object)
    
    for (s in slots) {
      if (debug)
        cli::cli_alert('Slot {.val {s}}')
      
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- Recall(val, Ages, debug)
    }
    
    return(object)
  }
  
  if (is.list(object)) {
    
    n <- length(object)
    if (n == 0)
      return(object)
    
    out <- object
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        out[[i]] <- Recall(el, Ages, debug)
    }
    
    return(out)
  }
  
  if (is.array(object)) {
    
    dnames <- dimnames(object)
    
    if (!is.null(dnames) && "Age" %in% names(dnames)) {
      object <- ArraySubsetAge(object, Ages)
    }
    
    return(object)
  }
  
  if (is.numeric(object) && !is.null(names(object))) {
    age_names <- suppressWarnings(as.numeric(names(object)))
    sel <- age_names %in% Ages
    
    return(as.numeric(object[sel]))
  }
  
  object
}



ArraySubsetAge <- function(array, Ages=NULL) {
  if (is.null(Ages)) {
    return(array)
  }
    
  Ages <-  as.numeric(Ages) 
  DN <- dimnames(array)
  
  if (is.null(DN)) {
    return(array)
  }
   
  AgeInd <- match("Age", names(DN))
  if (is.na(AgeInd)) {
    cli::cli_abort("`Age` dimension not found in this array", .internal = TRUE)
  }
    
  AgeVals <- as.numeric(DN[[AgeInd]])
  
  if (max(Ages) > max(AgeVals)) {
    cli::cli_abort("`Ages` greater than ages in this array", .internal = TRUE)
  }

  ## helper: index list for arbitrary dimensions
  make_index <- function(i, array) {
    nd <- length(dim(array))
    idx <- vector("list", nd)
    for (k in seq_len(nd))
      idx[[k]] <- if (k == AgeInd) i else seq_len(dim(array)[k])
    idx
  }
  
  ## logical selector preserves order of AgeVals
  sel <- AgeVals %in% Ages
  
  do.call(`[`, c(list(array), make_index(sel, array), list(drop = FALSE)))

}