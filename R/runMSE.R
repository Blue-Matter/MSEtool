#' Run a Management Strategy Evaluation
#' 
#' A function that runs a Management Strategy Evaluation (closed-loop
#' simulation) for a specified operating model
#' 
#' @param OM An operating model object (class `OM` or class `MOM`)
#' @param MPs A vector of methods (character string) of class MP
#' @param Hist Should model stop after historical simulations? Returns an object of 
#' class 'Hist' containing all historical data
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical. Should the MSE be run using parallel processing?
#' 
#' @return An object of class \linkS4class{MSE}
#' @export
#' 
runMSE <- function(OM=NULL, 
                   MPs = NA, 
                   Hist=FALSE, 
                   silent=FALSE, 
                   parallel=FALSE) {
  
  # ---- Initial Checks and Setup ----
  if (class(OM) == 'OM') {
    if (OM@nsim <=1) stop("OM@nsim must be > 1", call.=FALSE)
    
  } else if (class(OM) == 'Hist') {
    if (!silent) message("Using `Hist` object to reproduce historical dynamics")
    
    # --- Extract cpars from Hist object ----
    cpars <- list() 
    cpars <- c(OM@SampPars$Stock, OM@SampPars$Fleet, OM@SampPars$Obs,
               OM@SampPars$Imp, OM@OMPars, OM@OM@cpars)
    
    # --- Populate a new OM object ----
    newOM <- OM@OM
    newOM@cpars <- cpars
    OM <- newOM
  } else {
    stop("You must specify an operating model")
  } 
  
  HistSims <- Simulate(OM, parallel, silent) 
  if (Hist) return(HistSims)
  
  MSEout <- Project(Hist=HistSims, MPs, parallel, silent)
  MSEout
    
}






 



 
  
  






