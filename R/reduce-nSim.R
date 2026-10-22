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
#' @export
#' @seealso [Reduce()]
ReduceNSim <- function(object, nSim = NULL) {
  if (is.null(nSim)) {
    return(object)
  }
  
  .CheckClass(object, c("om", "hist"), "object")
  .CheckClass(nSim, c("numeric", "integer"), "nSim")
  
  if (length(nSim) > 1) {
    cli::cli_abort("`nSim` ({.val {nSim}}) must be length 1")
  }
  
  if (nSim < 1) {
    cli::cli_abort("`nSim` ({.val {nSim}}) must be >= 1")
  }
  
  if (nSim > nSim(object)) {
    cli::cli_alert_warning("Argument `nSim` ({.val {nSim}}) is greater than {.run nSim(OM)} ({.val {nSim(object)}}). Ignoring argument `nSim`  ")
    nSim <- nSim(object)
  }
  
  .SubsetSim(object, Sims = 1:nSim)
}

# Trims arrays and per-sim vectors populated for more sims than `OM@nSim`;
# aborts if a populated OM holds fewer, since drawn values replace the sampling inputs
.DropStaleSims <- function(OM) {
  .CheckNSimNotIncreased(OM)
  nOld <- .GetNSim(OM)
  if (is.null(nOld) || nOld <= OM@nSim) return(OM)
  .SubsetSim(OM, Sims = seq_len(OM@nSim), nSim = nOld)
}

.CheckNSimNotIncreased <- function(OM) {
  if (is.null(attr(OM, "digest"))) return(invisible())
  # StockTargeting is rebuilt for the current nSim
  Slots <- setdiff(methods::slotNames(OM), "StockTargeting")
  nPop  <- max(0, vapply(Slots, \(s) .LongestSimDim(methods::slot(OM, s)), numeric(1)))
  if (nPop <= 1 || nPop >= OM@nSim) return(invisible())
  cli::cli_abort(c(
    "x" = "{.code nSim} cannot be increased on a populated OM: it was populated with {nPop} simulation{?s}, but {.code nSim} is now {OM@nSim}.",
    "i" = "Populating replaces sampling inputs (e.g. parameter bounds) with the drawn values.",
    "i" = "Set {.code nSim} on the OM before calling {.fn Populate} or {.fn Simulate}."
  ), call = NULL)
}

.LongestSimDim <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(0)
  if (isS4(x))
    return(max(0, vapply(methods::slotNames(x), \(s) .LongestSimDim(methods::slot(x, s)), numeric(1))))
  if (is.list(x) && !is.data.frame(x))
    return(max(0, vapply(x, .LongestSimDim, numeric(1))))
  if (is.array(x)) length(dimnames(x)$Sim) else 0
}
