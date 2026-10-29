
#' Get maximum number of simulations
#'
#' Recursively searches an object for simulation dimension names and returns
#' the highest value found, or `nSim` if none are found.
#'
#' @param object An S4 object, list, or numeric array with a `Sim` dimname.
#' @param nSim Integer or `NULL`. A baseline value to compare against.
#'
#' @return The maximum number of simulations found, or `nSim` if none.
#' @keywords internal
.GetNSim <- function(object, nSim = NULL) {
  if (is.null(object))
    return(nSim)
  
  if (isS4(object)) {
    results <- lapply(slotNames(object), function(sl) .GetNSim(slot(object, sl), nSim))
    results <- Filter(Negate(is.null), results)
    if (length(results) > 0) return(max(unlist(results)))
  }
  
  if (is.list(object)) {
    results <- lapply(object, .GetNSim, nSim = nSim)
    results <- Filter(Negate(is.null), results)
    if (length(results) > 0) return(max(unlist(results)))
  }
  
  if (is.numeric(object)) {
    sims <- dimnames(object)$Sim
    if (!is.null(sims)) return(max(as.numeric(sims), nSim))
  }
  
  nSim
}
