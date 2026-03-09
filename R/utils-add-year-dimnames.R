#' Recursively assign year dimnames to arrays within an object
#'
#' Traverses S4 objects, lists, and arrays recursively. For any array with a
#' `"Year"` dimension whose dimnames contain `NA`s, replaces those dimnames
#' with values from `Years`. Leaves all other objects unchanged.
#'
#' @param object An S4 object, list, array, or atomic vector.
#' @param Years Numeric vector of year values to assign to `"Year"` dimnames.
#' @return `object` with `"Year"` dimnames populated wherever they were `NA`.
#' @keywords internal
AddYearDimnames <- function(object, Years) {
  
  if (isS4(object)) {
    slots <- slotNames(object)
    
    for (s in slots) {
      val <- slot(object, s)
      
      if (!is.null(val)) {
        slot(object, s) <- Recall(val, Years)
      }
    }
    
    return(object)
  }
  
  if (is.list(object)) {
    out <- object
    for (i in seq_along(out)) {
      el <- object[[i]]
      if (!is.null(el)) {
        out[[i]] <- Recall(el, Years)
      }
    }
    return(out)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Year" %in% names(dnames)) {
      YearVals <- dnames$Year
      if (is.null(YearVals) || any(is.na(YearVals)) || any(nchar(YearVals)<1)) {
        dimnames(object)$Year <- Years[seq_along(dimnames(object)$Year )]
      }
    }
    return(object)
  }
  object
}

#' Recursively assign fleet dimnames to arrays within an object
#'
#' Traverses S4 objects, lists, and arrays recursively. For any array with a
#' `"Fleet"` dimension whose dimnames contain `NA`s or are missing, replaces those dimnames
#' with values from `FleetNames`. Leaves all other objects unchanged.
#'
#' @param object An S4 object, list, array, or atomic vector.
#' @param FleetNames Character vector of names to assign to `"Fleet"` dimension.
#' @return `object` with `"Fleet"` dimnames populated wherever they were `NA`.
#' @keywords internal
AddFleetDimnames <- function(object, FleetNames) {
  
  if (isS4(object)) {
    slots <- slotNames(object)
    
    for (s in slots) {
      val <- slot(object, s)
      
      if (!is.null(val)) {
        slot(object, s) <- Recall(val, FleetNames)
      }
    }
    return(object)
  }
  
  if (is.list(object)) {
    out <- object
    for (i in seq_along(out)) {
      el <- object[[i]]
      if (!is.null(el)) {
        out[[i]] <- Recall(el, FleetNames)
      }
    }
    return(out)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Fleet" %in% names(dnames)) {
      FleetVals <- dnames$Fleet
      if (is.null(FleetVals) || any(is.na(FleetVals)) || any(nchar(FleetVals)<1)) {
        dimnames(object)$Fleet <- FleetNames
      }
    }
    return(object)
  }
  object
}