#' Subset an object by simulation, year, age, MP, and/or fleet
#'
#' Recursively subsets an object along the `Sim`, `Year`, `Age`, `MP`, and/or
#' `Fleet` & `Stock` dimensions. The function operates on arbitrarily nested structures
#' including S4 objects, lists, arrays, and named numeric vectors.
#'
#' @param object An object to be subset. Supported types include:
#'   * S4 objects (all slots are recursively subset)
#'   * Lists (including `StockList`, `FleetList`, etc.)
#'   * Arrays with a `Sim`, `Year`, `Age`, `MP`, and/or `Fleet` dimension
#'   * Named numeric vectors (for `Sim` and `Age` dimensions)
#'   * Data frames with a `Sim` column
#'
#' @param Sims   Numeric vector of simulation indices to retain, in the order
#'   they are returned. `NULL` skips. Subsetting an `om`, `hist`, or `mse`
#'   object records the original simulation numbers, so projections of the
#'   subset reproduce those simulations of the full object.
#' @param Years  Numeric vector of calendar years to retain. `NULL` skips.
#' @param Ages   Numeric vector of age classes to retain. `NULL` skips.
#' @param MPs    Integer or character vector of MPs to retain. `NULL` skips.
#' @param Fleets Character vector of fleets to retain. `NULL` skips.
#' @param Stocks Character vector of stocks to retain. `NULL` skips.
#' @param Impute Logical; only relevant when subsetting by `Years`. If `TRUE`
#'   (default), years not present in the object are imputed from the nearest
#'   available past year. If `FALSE`, years earlier than the earliest available
#'   year are dropped rather than imputed.
#' 
#' Subsetting is applied sequentially when multiple dimensions are supplied,
#' allowing consistent extraction across simulations, years, and age classes in
#' a single call.
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
                   Stocks = NULL,
                   Impute = TRUE) {

  if (!is.null(Sims)) {
    OM     <- .SimOM(object)
    object <- .SubsetSim(object, Sims)
    if (!is.null(OM)) object <- .SetGlobalSims(object, OM, Sims)
  }
  if (!is.null(Years))  object <- .SubsetYear(object, Years, Impute)
  if (!is.null(Ages))   object <- .SubsetAge(object, Ages)
  if (!is.null(MPs))    object <- .SubsetMP(object, MPs)
  if (!is.null(Fleets)) object <- .SubsetFleet(object, Fleets)
  if (!is.null(Stocks)) object <- .SubsetStock(object, Stocks)
  
  object
}

# The `om` that holds an object's nSim and global sim IDs, or NULL
.SimOM <- function(object) {
  if (inherits(object, "om")) return(object)
  if (isS4(object) && "OM" %in% methods::slotNames(object) && inherits(object@OM, "om"))
    return(object@OM)
  NULL
}

# Records which sims of the full run a subset holds, so per-sim seeds follow the sim
.SetGlobalSims <- function(object, OM, Sims) {
  SimIDs  <- .GlobalSim(OM, Sims)
  nGlobal <- .GlobalNSim(OM)
  Identity <- nGlobal == length(Sims) && identical(as.numeric(SimIDs), as.numeric(seq_len(nGlobal)))
  Set <- \(om) {
    om@Misc$SimIDs     <- if (Identity) NULL else SimIDs
    om@Misc$nSimGlobal <- if (Identity) NULL else nGlobal
    om
  }
  if (inherits(object, "om")) return(Set(object))
  object@OM <- Set(object@OM)
  object
}

# nSim of the object a .SubsetSim() call starts from, or NULL if it has none
.TopNSim <- function(object) {
  OM <- .SimOM(object)
  n  <- if (!is.null(OM)) OM@nSim else if (isS4(object) && "nSim" %in% methods::slotNames(object)) object@nSim
  if (is.numeric(n) && length(n) == 1 && !is.na(n)) n else NULL
}

# Names a per-sim vector "1".."nSim" so .SubsetSim() recognizes it
.NameSims <- function(x, nSim) {
  if (!is.null(x) && is.null(dim(x)) && length(x) == nSim)
    names(x) <- seq_len(nSim)
  x
}

# Slots `CalcFisheryDynamics_`/`HistView` read (inst/include/hist_view.h),
# plus what `.CatchMatrix()`
.DynamicsProbeSlots <- c(
  "Number", "Biomass", "SBiomass", "SProduction",
  "Interactions", "Landings", "Discards",
  "InteractAtAge", "LandingsAtAge", "DiscardsAtAge",
  "LandingsAtSize", "DiscardsAtSize",
  "Effort", "Distribution",
  "FInteract", "FDead", "FRetain",
  "FInteractArea", "FDeadArea", "FRetainArea",
  "Misc"
)

# Slice specific slots of an S4 object down to a single simulation.
.SliceSim <- function(object, sim, slots) {
  nSim <- .TopNSim(object)
  if (isS4(object) && "OM" %in% slotNames(object) &&
      "nSim" %in% slotNames(object@OM)) {
    object@OM@nSim <- 1L
    # the solvers index StockTargeting by the slice's sim (1)
    object@OM@StockTargeting <- .SubsetSim(object@OM@StockTargeting, sim, keep_sim_name = FALSE,
                                           nSim = nSim)
  }
  for (s in slots)
    slot(object, s) <- .SubsetSim(slot(object, s), sim, keep_sim_name = FALSE, nSim = nSim)
  object
}

.MakeDimIndex <- function(i, array, along) {
  nd  <- length(dim(array))
  idx <- vector("list", nd)
  for (k in seq_len(nd))
    idx[[k]] <- if (k == along) i else seq_len(dim(array)[k])
  idx
}

.SubsetSim <- function(object, Sims, keep_sim_name = FALSE, debug = FALSE, broadcast = FALSE,
                       nSim = NULL) {
  Sims <- as.numeric(Sims)
  nSim <- nSim %||% .TopNSim(object)
  # names a per-sim vector or list can carry: local sims, or the full run's sims in a chunk
  SimKeys <- if (!is.null(nSim)) {
    Keys <- list(as.character(seq_len(nSim)))
    OM <- .SimOM(object)
    if (!is.null(OM) && !is.null(OM@Misc$SimIDs))
      Keys <- c(Keys, list(as.character(.GlobalSim(OM, seq_len(nSim)))))
    Keys
  }
  .SubsetSimNode(object, Sims, keep_sim_name, debug, broadcast, nSim, SimKeys)
}

.IsSimKeyed <- function(object, nSim, SimKeys) {
  nms <- names(object)
  if (is.null(nms)) return(FALSE)
  if (is.null(nSim))
    return(identical(nms, as.character(seq_along(object))))
  length(object) == nSim && any(vapply(SimKeys, identical, logical(1), nms))
}

.SimLabels <- function(Sims, keep_sim_name) {
  if (keep_sim_name) Sims else seq_along(Sims)
}

.SubsetSimNode <- function(object, Sims, keep_sim_name, debug, broadcast, nSim, SimKeys) {

  if (debug)  cli::cli_alert("Class {.val {class(object)}}")

  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- .SubsetSimNode(val, Sims, keep_sim_name, debug, broadcast, nSim, SimKeys)
    }

    if ("nSim" %in% slots)
      object@nSim <- length(Sims)

    return(object)
  }

  if (is.data.frame(object))
    return(.DFSubsetSim(object, Sims, keep_sim_name))

  if (is.list(object)) {
    n <- length(object)
    if (n == 0) return(object)

    if (.IsSimKeyed(object, nSim, SimKeys)) {
      .CheckSimsAvailable(Sims, n)
      object <- object[Sims]
      names(object) <- .SimLabels(Sims, keep_sim_name)
      return(object)
    }

    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        object[[i]] <- .SubsetSimNode(el, Sims, keep_sim_name, debug, broadcast, nSim, SimKeys)
    }
    return(object)
  }

  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Sim" %in% names(dnames)) {
      if (length(dnames$Sim) == 1L && !broadcast)
        return(object)
      object <- .ArraySubsetSim(object, Sims, keep_sim_name = keep_sim_name)
    }
    return(object)
  }

  # per-sim vectors named by sim (e.g. Obs@Survey@Efficiency)
  if (is.atomic(object) && is.null(dim(object)) && !is.null(nSim) && nSim > 1 &&
      .IsSimKeyed(object, nSim, SimKeys)) {
    .CheckSimsAvailable(Sims, nSim)
    object <- object[Sims]
    names(object) <- .SimLabels(Sims, keep_sim_name)
    return(object)
  }

  object
}

.CheckSimsAvailable <- function(Sims, n) {
  if (any(Sims < 1 | Sims > n | Sims != round(Sims)))
    cli::cli_abort("`Sims` ({.val {Sims}}) must be within 1:{n}.")
}

# Rows of a data frame with a `Sim` column, in the order of `Sims`; one-sim frames apply to all sims
.DFSubsetSim <- function(df, Sims, keep_sim_name = FALSE) {
  if (!"Sim" %in% names(df) || !nrow(df) || length(unique(df$Sim)) == 1L)
    return(df)
  Pos  <- lapply(Sims, \(s) which(df$Sim == s))
  Rows <- unlist(Pos)
  out  <- df[Rows, , drop = FALSE]
  out$Sim <- rep(.SimLabels(Sims, keep_sim_name), lengths(Pos))
  rownames(out) <- NULL
  out
}

.ArraySubsetSim <- function(array, Sims = NULL, keep_sim_name = FALSE) {
  .CheckClass(array, 'array', 'array')
  .CheckClass(Sims, c('numeric', 'integer'), 'Sims')

  if (is.null(Sims)) return(array)

  DN <- dimnames(array)
  if (is.null(DN) || !"Sim" %in% names(DN))
    cli::cli_abort("`Sim` dimension not found in this array")

  SimVals <- as.numeric(DN$Sim)
  Along   <- match("Sim", names(DN))

  if (length(SimVals) == 1L) {
    idx <- rep(1L, length(Sims))
  } else {
    idx <- match(Sims, SimVals)
    if (anyNA(idx)) {
      Missing <- Sims[is.na(idx)]
      cli::cli_abort("`Sims` {.val {Missing}} not found in an array with {length(SimVals)} sims.")
    }
  }

  out <- if (identical(idx, seq_along(SimVals))) {
    array
  } else {
    do.call(`[`, c(list(array), .MakeDimIndex(idx, array, Along),
                   list(drop = FALSE)))
  }
  dimnames(out)[[Along]] <- .SimLabels(Sims, keep_sim_name)
  out
}

.SubsetYear <- function(object, Years, Impute=TRUE, debug=FALSE) {
  
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
      object <- .ArraySubsetYear(object, Years, Impute = Impute)
    }
    return(object)
  }
  
  if (is.numeric(object)) {
    if (!is.null(names(object))) {
      ind <- which(names(object) %in% Years)
      if (any(ind))
        return(object[ind])
    }
    
    if (any(is.na(object)))
      return(object)
    
    if (all(object > 1000) && all(object < 3000)) {
      ind <- object %in% Years 
      if (any(ind))
        return(object[ind])  
    }
    
  }
  
  object
}

.ArraySubsetYear<- function(array, Years = NULL, Impute = TRUE) {
  
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
  if (all(idx)) return(array)
  do.call(`[`, c(list(array), .MakeDimIndex(idx, array, TSind),
                 list(drop = FALSE)))
}

.DropYearsAfter <- function(array, LastYear) {
  TSind <- match("Year", names(dimnames(array)))
  if (is.na(TSind)) return(array)
  idx <- as.numeric(dimnames(array)[[TSind]]) <= LastYear
  if (all(idx)) return(array)
  do.call(`[`, c(list(array), .MakeDimIndex(idx, array, TSind),
                 list(drop = FALSE)))
}

.SubsetAge <- function(object, Ages, debug = FALSE) {
  
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
      object <- .ArraySubsetAge(object, Ages)
    return(object)
  }
  
  if (is.numeric(object) && !is.null(names(object))) {
    age_names <- suppressWarnings(as.numeric(names(object)))
    sel <- age_names %in% Ages
    return(as.numeric(object[sel]))
  }
  
  object
}

.ArraySubsetAge <- function(array, Ages=NULL) {
  
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
  if (all(sel)) return(array)
  do.call(`[`, c(list(array), .MakeDimIndex(sel, array, AgeInd),
                 list(drop = FALSE)))

}

.SubsetMP <- function(object, MPs=NULL, debug = FALSE) {
  
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

    if (!is.null(names(object))) {
      if (is.numeric(MPs)) {
        idx <- as.integer(MPs)
        idx <- idx[idx %in% seq_len(n)]
        return(object[idx])
      }
      if (is.character(MPs) && any(MPs %in% names(object))) {
        MPs <- MPs[MPs %in% names(object)]
        return(object[MPs])
      }
    }

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
      object <- .ArraySubsetMP(object, MPs)
    return(object)
  }
  
  if (is.character(object) && !is.null(MPs)) {
    ind <- object %in% MPs
    if (any(ind))
      return(object[ind])
    return(object)
  }
  
  
  object
}

.ArraySubsetMP <- function(array, MPs=NULL) {
  
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

  if (all(sel)) return(array)
  do.call(`[`, c(list(array), .MakeDimIndex(sel, array, MPInd),
                 list(drop = FALSE)))

}

.SubsetFleet <- function(object, Fleets=NULL, debug = FALSE) {
  
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
      object <- .ArraySubsetFleet(object, Fleets)
    return(object)
  }
  
  if (is.character(object) && !is.null(Fleets)) {
    ind <- object %in% Fleets
    if (any(ind))
      return(object[ind])
    return(object)
  }
    
  
  object
}

.ArraySubsetFleet <- function(array, Fleets=NULL) {
  
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

  if (all(sel)) return(array)
  do.call(`[`, c(list(array), .MakeDimIndex(sel, array, FleetInd),
                 list(drop = FALSE)))

}

.SubsetStock <- function(object, Stocks=NULL, debug=FALSE) {
  
  if (debug) cli::cli_alert('Class {.val {class(object)}}')
  
  if (isS4(object)) {
    if (debug) cli::cli_alert("S4 Object")
    slots <- slotNames(object)
    for (s in slots) {
      if (debug) cli::cli_alert("Slot {.val {s}}")
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- Recall(val, Stocks, debug)
    }
    return(object)
  }
  
  if (is.list(object)) {
    n <- length(object)
    if (n == 0) return(object)
    
    if (!is.null(names(object)) && any(Stocks %in% names(object))) {
      Stocks <- Stocks[Stocks %in% names(object)]
      return(object[Stocks])
    }
    
    for (i in seq_len(n)) {
      el <- object[[i]]
      if (!is.null(el))
        object[[i]] <- Recall(el, Stocks, debug)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Stock" %in% names(dnames))
      object <- .ArraySubsetStock(object, Stocks)
    return(object)
  }
  
  if (is.character(object) && !is.null(Stocks)) {
    ind <- object %in% Stocks
    if (any(ind))
      return(object[ind])
    return(object)
  }
  
  object
}

.ArraySubsetStock <- function(array, Stocks=NULL) {
  
  if (is.null(Stocks)) return(array)
  
  DN <- dimnames(array)
  if (is.null(DN)) return(array)
  
  StockInd <- match("Stock", names(DN))
  if (is.na(StockInd))
    cli::cli_abort("`Stock` dimension not found in this array", .internal = TRUE)
  
  StockNames <- DN[[StockInd]]
  
  if (is.numeric(Stocks)) {
    StockVals <- as.integer(Stocks)
  } else if (is.character(Stocks)) {
    StockVals <- match(Stocks, StockNames)
    if (any(is.na(StockVals))) {
      bad <- Stocks[is.na(StockVals)]
      cli::cli_abort("Stock(s) {.val {bad}} not found in this array")
    }
  } else {
    cli::cli_abort("`Stocks` must be numeric or character, not {.cls {class(Stocks)}}")
  }
  
  StockVals <- StockVals[StockVals %in% seq_along(StockNames)]
  sel <- seq_along(StockNames) %in% StockVals

  if (all(sel)) return(array)
  do.call(`[`, c(list(array), .MakeDimIndex(sel, array, StockInd),
                 list(drop = FALSE)))
}
