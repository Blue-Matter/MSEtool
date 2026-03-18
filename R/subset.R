#' Subset an object by simulation, year, age, MP, and/or fleet
#'
#' Recursively subsets an object along the `Sim`, `Year`, `Age`, `MP`, and/or
#' `Fleet` dimensions. The function operates on arbitrarily nested structures
#' including S4 objects, lists, arrays, and named numeric vectors.
#'
#' @param object An object to be subset. Supported types include:
#'   * S4 objects (all slots are recursively subset)
#'   * Lists (including `StockList`, `FleetList`, etc.)
#'   * Arrays with a `Sim`, `Year`, `Age`, `MP`, and/or `Fleet` dimension
#'   * Named numeric vectors (for `Sim` and `Age` dimensions)
#'
#' @param Sims   Numeric vector of simulation indices to retain. `NULL` skips.
#' @param Years  Numeric vector of calendar years to retain. `NULL` skips.
#' @param Ages   Numeric vector of age classes to retain. `NULL` skips.
#' @param MPs    Integer or character vector of MPs to retain. `NULL` skips.
#' @param Fleets Character vector of fleets to retain. `NULL` skips.
#' @param Impute Logical; only relevant when subsetting by `Years`. If `TRUE`
#'   (default), years not present in the object are imputed from the nearest
#'   available past year. If `FALSE`, years earlier than the earliest available
#'   year are dropped rather than imputed.
#' 
#' Subsetting is applied sequentially when multiple dimensions are supplied,
#' allowing consistent extraction across simulations, years, and age classes in
#' a single call.
#' 
#' For array objects, subsetting is performed along dimensions named `"Sim"`,
#' `"Year"`, `"Age"`, `"MP"`, and/or `"Fleet"`. Year subsetting optionally
#' supports imputation of missing values. Age subsetting requires all requested
#' ages to be present in the array.
#'
#' For S4 objects, all slots are recursively subset. If a slot named `nSim` is
#' present it is updated to reflect the number of retained simulations.
#'
#' @return An object of the same class as `object`, subset according to the
#'   supplied dimension arguments.
#'   
#' @example man-examples/Subset.R
#' @export
Subset <- function(object,
                   Sims   = NULL,
                   Years  = NULL,
                   Ages   = NULL,
                   MPs    = NULL,
                   Fleets = NULL,
                   Impute = TRUE) {
  
  populated <- try(Populate(object), silent = TRUE)
  
  if (!inherits(populated, "try-error"))
    object <- populated
 
  if (!is.null(Sims))   object <- SubsetSim(object, Sims)
  if (!is.null(Years))  object <- SubsetYear(object, Years, Impute)
  if (!is.null(Ages))   object <- SubsetAge(object, Ages)
  if (!is.null(MPs))    object <- SubsetMP(object, MPs)
  if (!is.null(Fleets)) object <- SubsetFleet(object, Fleets)
  
  object
}

.make_dim_index <- function(i, array, along) {
  nd  <- length(dim(array))
  idx <- vector("list", nd)
  for (k in seq_len(nd))
    idx[[k]] <- if (k == along) i else seq_len(dim(array)[k])
  idx
}

SubsetSim <- function(object, Sims, debug = FALSE) {
  
  if (debug)  cli::cli_alert("Class {.val {class(object)}}")
  
  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- Recall(val, Sims, debug)
    }
    
    if ("nSim" %in% slots) 
      object@nSim <- length(Sims)
    
    return(object)
  }
  
  if (is.list(object)) {
    n <- length(object)
    if (n == 0) return(object)
  
    if (!is.null(names(object)) && all(Sims %in% names(object))) 
      return(object[Sims])
  
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        object[[i]] <- Recall(el, Sims, debug)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Sim" %in% names(dnames)) {
      SimVals <- as.numeric(dnames$Sim)
      
      if (max(SimVals) >= max(Sims)) 
        object <- ArraySubsetSim(object, Sims)
    }
    return(object)
  }
  
  if (is.numeric(object) && !is.null(names(object)) &&
      length(object) > 1 && "Sim" %in% names(object)) {
    return(as.numeric(object[Sims]))
  }
  
  object
}

ArraySubsetSim <- function(array, Sims=NULL) {
  CheckClass(array, 'array', 'array')
  CheckClass(Sims, c('numeric', 'integer'), 'Sims')
  
  if (is.null(Sims)) return(array)
    
  DN <- dimnames(array)
  if (is.null(DN) || !"Sim" %in% names(DN)) 
    cli::cli_abort("`Sim` dimension not found in this array")
  

  SimVals <- as.numeric(DN$Sim)
 
  if (any(Sims > max(SimVals))) {
    if (max(SimVals)==1) {
      return(ExtendSims(array, max(Sims)))
    } else {
      Sims <- seq_len(max(SimVals))
    }
  }
  
  idx <- SimVals %in% Sims
  idx <- SimVals %in% Sims
  out <- do.call(`[`, c(list(array), .make_dim_index(idx, array, 1L),
                        list(drop = FALSE)))
  dimnames(out)$Sim <- seq_along(dimnames(out)$Sim)
  
  out
}

SubsetYear <- function(object, Years, Impute=TRUE, debug=FALSE) {
  
  if (debug) cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (debug) cli::cli_alert('Class val {.val {class(val)}}')
      if (!is.null(val))
        slot(object, s) <- Recall(val, Years, Impute, debug)
    }
    return(object)
  }
  
  if (is.list(object)) {
    for (j in seq_along(object)) {
      if (!is.null(object[[j]]))
        object[[j]] <- Recall(object[[j]], Years, Impute, debug)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Year" %in% names(dnames) &&
        !is.null(dnames[["Year"]])) {
      object <- ArraySubsetYear(object, Years, Impute = Impute)
    }
    return(object)
  }
  
  object
}

ArraySubsetYear<- function(array, Years = NULL, Impute = TRUE) {
  
  if (is.null(Years)) return(array)

  
  Years <- as.numeric(Years)
  DN <- dimnames(array)
  if (is.null(DN)) return(array)
  

  TSind <- match("Year", names(DN))
  if (is.na(TSind)) 
    cli::cli_abort("`Year` dimension not found in this array")
  
  YearVals <- as.numeric(DN[[TSind]])

  TSexist  <- Years[Years %in% YearVals]
  TSimpute <- Years[!Years %in% YearVals]
  
  if (!Impute && length(TSexist)) 
    TSimpute <- TSimpute[TSimpute >= min(YearVals)]
  
    
  if (length(TSimpute)) {
    array <- ExtendYears(array, TSimpute)
    YearVals <- as.numeric(dimnames(array)[[TSind]])
  }
  
  idx <- YearVals %in% Years
  do.call(`[`, c(list(array), .make_dim_index(idx, array, TSind),
                 list(drop = FALSE)))
}

SubsetAge <- function(object, Ages, debug = FALSE) {
  
  if (debug) cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- Recall(val, Ages, debug)
    }
    return(object)
  }
  
  if (is.list(object)) {
    n <- length(object)
    if (n == 0) return(object)
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        object[[i]] <- Recall(el, Ages, debug)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Age" %in% names(dnames))
      object <- ArraySubsetAge(object, Ages)
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
  
  if (is.null(Ages))return(array)
  
  Ages <-  as.numeric(Ages) 
  DN <- dimnames(array)
  if (is.null(DN)) return(array)
  
  AgeInd <- match("Age", names(DN))
  if (is.na(AgeInd)) 
    cli::cli_abort("`Age` dimension not found in this array", .internal = TRUE)
  
    
  AgeVals <- as.numeric(DN[[AgeInd]])
  
  if (max(Ages) > max(AgeVals) && length(AgeVals)>1) 
    cli::cli_abort("`Ages` greater than ages in this array")
  
  sel <- AgeVals %in% Ages
  do.call(`[`, c(list(array), .make_dim_index(sel, array, AgeInd),
                 list(drop = FALSE)))

}

SubsetMP <- function(object, MPs=NULL, debug = FALSE) {
  
  if (debug) cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- Recall(val, MPs, debug)
    }
    return(object)
  }
  
  if (is.list(object)) {
    n <- length(object)
    if (n == 0) return(object)
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        object[[i]] <- Recall(el, MPs, debug)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "MP" %in% names(dnames))
      object <- ArraySubsetMP(object, MPs)
    return(object)
  }
  
  if (is.character(object)) 
    object <- object[object %in% MPs]
  
  object
}

ArraySubsetMP <- function(array, MPs=NULL) {
  
  if (is.null(MPs)) return(array)
  
  DN <- dimnames(array)
  if (is.null(DN)) return(array)
  
  MPInd <- match("MP", names(DN))
  if (is.na(MPInd))
    cli::cli_abort("`MP` dimension not found in this array", .internal = TRUE)
  
  
  MPNames <- DN[[MPInd]]
  
  if (is.numeric(MPs)) {
    MPVals <- as.integer(MPs)
  } else if (is.character(MPs)) {
    MPVals <- match(MPs, MPNames)
    if (any(is.na(MPVals))) {
      bad <- MPs[is.na(MPVals)]
      cli::cli_abort("MP(s) {.val {bad}} not found in this array")
    }
  } else {
    cli::cli_abort("`MPs` must be numeric or character, not {.cls {class(MPs)}}")
  }
  
 
  MPVals <- MPVals[MPVals %in% seq_along(MPNames)]
  sel <- seq_along(MPNames) %in% MPVals 
  
  do.call(`[`, c(list(array), .make_dim_index(sel, array, MPInd),
                 list(drop = FALSE)))
  
}

SubsetFleet <- function(object, Fleets=NULL, debug = FALSE) {
  
  if (debug) cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- Recall(val, Fleets, debug)
    }
    return(object)
  }
  
  if (is.list(object)) {
    n <- length(object)
    if (n == 0) return(object)
    
    if (!is.null(names(object)) && any(Fleets %in% names(object))) {
      Fleets <- Fleets[Fleets %in% names(object)]
      return(object[Fleets])
    }
    
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        object[[i]] <- Recall(el, Fleets, debug)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Fleet" %in% names(dnames))
      object <- ArraySubsetFleet(object, Fleets)
    return(object)
  }
  
  if (is.character(object) && !is.null(Fleets))
    return(object[object %in% Fleets])
  
  object
}

ArraySubsetFleet <- function(array, Fleets=NULL) {
  
  if (is.null(Fleets)) return(array)
  
  DN <- dimnames(array)
  if (is.null(DN)) return(array)
  
  FleetInd <- match("Fleet", names(DN))
  
  if (is.na(FleetInd)) 
    cli::cli_abort("`Fleet` dimension not found in this array", .internal = TRUE)
  
  FleetNames <- DN[[FleetInd]]
  
  if (is.numeric(Fleets)) {
    FleetVals <- as.integer(Fleets)
  } else if (is.character(Fleets)) {
    FleetVals <- match(Fleets, FleetNames)
    if (any(is.na(FleetVals))) 
      FleetVals <- FleetVals[!is.na(FleetVals)]
  } else {
    cli::cli_abort("`Fleets` must be numeric or character, not {.cls {class(Fleets)}}")
  }
  

  FleetVals <- FleetVals[FleetVals %in% seq_along(FleetNames)]
  sel <- seq_along(FleetNames) %in% FleetVals 
  
  do.call(`[`, c(list(array), .make_dim_index(sel, array, FleetInd),
                 list(drop = FALSE)))
  
}
