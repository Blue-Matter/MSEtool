
#' Return the Number of Stocks or Fleets
#' 
#' @export
nStock <- function(object) {
  
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    return(length(object@Stock))
  }
    
  if (inherits(object,'hist')) {
    return(length(object@OM@Stock))
  }
  
  if (inherits(object,'mse')) {
    return(length(object@OM@Stock))
  }
  
}

#' @rdname object
#' @export
nFleet <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    fleet <- object@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if(isS4(fleet[[1]])) {
      dd <- dim(object@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  return(dim(object@Landings)[4])
  
}
