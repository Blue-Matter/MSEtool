#' Reduce the number of simulations 
#' 
#' Subsets an [OM()] or [Hist()] object by reducing number
#' of simulations to `nSim`
#' 
#' @param object An [OM()] or [Hist()] object
#' @param nSim Integer. The maximum number of simulations
#' 
#' @return The [OM()] or [Hist()] with reduced `nSim`
#' 
#' @example man-examples/ReduceNSim.R
#' @seealso [Reduce()]
#' @export
ReduceNSim <- function(object, nSim = NULL) {
  if (is.null(nSim)) {
    return(object)
  }
  
  CheckClass(object, c("om", "hist"), "object")
  CheckClass(nSim, c("numeric", "integer"), "nSim")
  
  if (length(nSim) > 1) {
    cli::cli_abort("`nSim` ({.val {nSim}}) must be length 1")
  }
  
  if (nSim < 1) {
    cli::cli_abort("`nSim` ({.val {nSim}}) must be >= 1")
  }
  
  if (nSim(object) == nSim) {
    return(object)
  }
  
  if (nSim > nSim(object)) {
    cli::cli_alert_warning("Argument `nSim` ({.val {nSim}}) is greater than `nSim(OM)` ({.val {nSim(object)}}). Ignoring argument `nSim`  ")
    nSim <- nSim(object)
  }
  
  SubsetSim(object, Sim = 1:nSim)
}