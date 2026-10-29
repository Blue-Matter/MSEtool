#' Reduce redundant Sim, Year, and Age dimensions in arrays
#'
#' Reduce the size of array-like objects by collapsing dimensions
#' (`Sim`, `Year`, and/or `Age`) when their values are identical across
#' that dimension. The function works recursively on S4 objects and lists,
#' applying the reduction to all array-valued components.
#'
#' @param array An array, list, or S4 object. For non-array objects, the
#'   function is applied recursively to all components or slots.
#' @param IncSim Logical; reduce the `Sim` dimension if present and
#'   all simulations are identical.
#' @param IncAge Logical; reduce the `Age` dimension if present and
#'   all ages are identical.
#' @param IncYear Logical; reduce the `Year`dimension if present and
#'   all years are identical or by keeping only unique years.
#' @param debug Logical. Print debug messages?
#'    
#' @details
#' Reduction is controlled independently for each supported dimension.
#' If a dimension is excluded or not present, it is left unchanged.
#' 
#' For arrays, the function detects dimensions named `Sim`,
#' `Age`, and `Year`. If enabled, dimensions
#' are reduced as follows:
#' * `Sim`: kept at length 1 when all simulations are identical.
#' * `Age`:  kept at length 1 when all ages are identical.
#' * `Year`: kept at length 1 when all years are identical; otherwise,
#'     only unique years are retained.
#'    
#' @return An object of the same class as `array`, with reduced dimensions
#'   where applicable. 
#' @seealso [ReduceNSim()]
#' @export
ReduceDims <- function(array,
                       IncSim  = TRUE,
                       IncAge  = FALSE,
                       IncYear = FALSE,
                       debug   = FALSE) {
  
  if (!IncSim && !IncYear && !IncAge) 
    return(array)
  
  if (!length(array)) 
    return(array)
  
  if (debug)
    cli::cli_alert_info(class(array))
  
  # Recall for S4
  if (isS4(array)) {
    if (inherits(array, "data")) {
      return(array)
    }
    
    for (sl in slotNames(array)) {
      if (debug)
        cli::cli_alert('Slot {sl}')
      slot(array, sl) <- Recall(slot(array, sl),IncSim, IncAge, IncYear)
    }
    
    return(array)
  }
  
  # Recall for list
  if (is.list(array)) {
    for (i in seq_along(array)) {
      tmp <- Recall(array[[i]], IncSim, IncAge, IncYear)
      if (!is.null(tmp)) {
        array[[i]] <- tmp
      }
    }
    return(array)
  }
  
  DN <- dimnames(array)
  if (is.null(DN)) 
    return(array)
  
  dnm <- names(DN)
  
  indSim <- match("Sim", dnm)
  indAge <- match("Age", dnm)
  indYear <- match("Year", dnm)
  
  hasSim <- !is.na(indSim) && IncSim
  hasAge <- !is.na(indAge) && IncAge
  hasYear <- !is.na(indYear) && IncYear
  
  if (!hasSim && !hasAge && !hasYear) 
    return(array)
  
  idSim <- hasSim && .IdenticalSims(array)
  idAge <- hasAge && .IdenticalAge(array)
  idYear <- hasYear && .IdenticalYears(array)
  
  uniqYears <- if (hasYear && !idYear) .UniqueYears(array) else NULL
  
  nd <- length(dim(array))
  idx <- vector("list", nd)
  
  for (k in seq_len(nd)) 
    idx[[k]] <- seq_len(dim(array)[k])
  
  
  ## ---- Sim ----
  if (hasSim && idSim) 
    idx[[indSim]] <- 1
  
  
  ## ---- Age ----
  if (hasAge && idAge) 
    idx[[indAge]] <- 1
  
  
  ## ---- Year ----
  if (hasYear) {
    if (idYear) {
      idx[[indYear]] <- 1
    } else {
      idx[[indYear]] <- uniqYears
    }
  }
  
  do.call(`[`, c(list(array), idx, list(drop = FALSE)))
}




.ReduceHist <- function(Hist, Reduce=TRUE) {
  if (!Reduce)
    return(Hist)
  
  ReduceDims(Hist)
}


.ReduceMSE <- function(MSE, Reduce=TRUE) {
  if (!Reduce)
    return(MSE)
  
  ReduceDims(MSE)
}
