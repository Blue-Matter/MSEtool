#' Reduce redundant Sim, Year, and Age dimensions in arrays
#'
#' Reduce the size of array-like objects by collapsing dimensions
#' (`Sim`, `Year`, and/or `Age`) when their values are identical across
#' that dimension. The function works recursively on S4 objects and lists,
#' applying the reduction to all array-valued components.
#'
#' @param array An array, list, or S4 object. For non-array objects, the
#'   function is applied recursively to all components or slots.
#' @param IncSim Logical; reduce the \code{"Sim"} dimension if present and
#'   all simulations are identical.
#' @param IncAge Logical; reduce the \code{"Age"} dimension if present and
#'   all ages are identical.
#' @param IncYear Logical; reduce the \code{"Year"} dimension if present and
#'   all years are identical or by keeping only unique years.
#'   
#' @details
#' Reduction is controlled independently for each supported dimension.
#' If a dimension is excluded or not present, it is left unchanged.
#' 
#' For arrays, the function detects dimensions named \code{"Sim"},
#' \code{"Age"}, and \code{"Year"} via \code{dimnames}. If enabled, dimensions
#' are reduced as follows:
#' \itemize{
#'   \item \code{Sim}: kept at length 1 when all simulations are identical.
#'   \item \code{Age}: kept at length 1 when all ages are identical.
#'   \item \code{Year}: kept at length 1 when all years are identical; otherwise,
#'     only unique years are retained.
#' }
#'
#' The function relies on the internal helpers \code{IdenticalSims()},
#' \code{IdenticalAge()}, \code{IdenticalYears()}, and \code{UniqueYears()}.
#'
#' @return An object of the same class as \code{array}, with reduced dimensions
#'   where applicable. Dimensions are never dropped entirely; all subsetting
#'   uses \code{drop = FALSE}.
#'
#' @export
ReduceDims <- function(array,
                            IncSim = TRUE,
                            IncAge = FALSE,
                            IncYear = TRUE) {
  if (!IncSim && !IncYear && !IncAge) {
    return(array)
  }
  
  if (!length(array)) {
    return(array)
  }
  
  # Recall for S4
  if (isS4(array)) {
    if (inherits(array, "data")) {
      return(array)
    }
    
    for (sl in slotNames(array)) {
      slot(array, sl) <-
        Recall(
          slot(array, sl),
          IncSim, IncAge, IncYear, debug
        )
    }
    
    return(array)
  }
  
  # Recall for list
  if (is.list(array)) {
    for (i in seq_along(array)) {
      tmp <- Recall(
        array[[i]],
        IncSim, IncAge, IncYear, debug
      )
      if (!is.null(tmp)) {
        array[[i]] <- tmp
      }
    }
    return(array)
  }
  
  DN <- dimnames(array)
  if (is.null(DN)) {
    return(array)
  }
  
  dnm <- names(DN)
  
  indSim <- match("Sim", dnm)
  indAge <- match("Age", dnm)
  indYear <- match("Year", dnm)
  
  hasSim <- !is.na(indSim) && IncSim
  hasAge <- !is.na(indAge) && IncAge
  hasYear <- !is.na(indYear) && IncYear
  
  if (!hasSim && !hasAge && !hasYear) {
    return(array)
  }
  
  idSim <- hasSim && IdenticalSims(array)
  idAge <- hasAge && IdenticalAge(array)
  idYear <- hasYear && IdenticalYears(array)
  
  uniqYears <- if (hasYear && !idYear) UniqueYears(array) else NULL
  
  nd <- length(dim(array))
  idx <- vector("list", nd)
  
  for (k in seq_len(nd)) {
    idx[[k]] <- seq_len(dim(array)[k])
  }
  
  ## ---- Sim ----
  if (hasSim && idSim) {
    idx[[indSim]] <- 1
  }
  
  ## ---- Age ----
  if (hasAge && idAge) {
    idx[[indAge]] <- 1
  }
  
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




ReduceHist <- function(Hist, Reduce=TRUE) {
  if (!Reduce)
    return(Hist)
  
  # TODO 
  Hist@OM <- ReduceDims(Hist@OM, IncYear=FALSE)
  Hist@Unfished <- ReduceDims(Hist@Unfished, IncYear=FALSE)
  Hist@Reference <- ReduceDims(Hist@Reference, IncYear=FALSE)
  Hist <- ReduceTimeSeries(Hist)
  
  # Hist@Number <- ReduceDims(Hist@Number, IncYear = FALSE)
  # Hist@Biomass <- ReduceDims(Hist@Biomass, IncYear = FALSE)
  # Hist@SBiomass <- ReduceDims(Hist@SBiomass, IncYear = FALSE)
  # Hist@SProduction <- ReduceDims(Hist@SProduction, IncYear = FALSE)
  # Hist@Landings <- ReduceDims(Hist@Landings, IncYear = FALSE)
  # Hist@Discards <- ReduceDims(Hist@Discards, IncYear = FALSE)
  # Hist@Effort <- ReduceDims(Hist@Effort, IncYear = FALSE)
  # Hist@Distribution <- ReduceDims(Hist@Distribution, IncYear = FALSE)
  # Hist@FDead <- ReduceDims(Hist@FDead, IncYear = FALSE)
  # Hist@FDeadArea <- ReduceDims(Hist@FDeadArea, IncYear = FALSE)
  # Hist@FRetain <- ReduceDims(Hist@FRetain, IncYear = FALSE)
  # Hist@FRetainArea <- ReduceDims(Hist@FRetainArea, IncYear = FALSE)
  Hist
}



ReduceMSE <- function(MSE, Reduce=TRUE) {
  if (!Reduce)
    return(MSE)
  
  MSE@OM <- ReduceDims(MSE@OM)
  MSE@Hist <- ReduceTimeSeries(MSE@Hist)
  MSE <- ReduceTimeSeries(MSE)
  MSE
}

ReduceTimeSeries <- function(object) {
  slots <- slotNames('timeseries')
  slots <- slots[!slots=='Misc']
  
  for (sl in slots) {
    slot(object, sl) <- ReduceDims(slot(object, sl), IncYear = FALSE)
  }
  object
}