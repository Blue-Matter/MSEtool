
#' @describeIn runMSE Run the Historical Simulations from an object of class `OM` or class `om`
#' @export
Simulate <- function(OM=NULL, ...) {
  
  if (inherits(OM, 'om'))
    return(Simulate_om(OM, ...))
  
  SimulateOM(OM, , ...)
}