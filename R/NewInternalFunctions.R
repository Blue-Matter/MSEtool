


SetHistRel <- function(OM) {
  # Ignore MICE in historical period
  if (isFALSE(OM@Control$HistRel))
    return(list())
  Relations(OM) 
}





ConvertToList <- function(x) {
  # TODO add names
  if (methods::is(x, 'om')) {
    if (methods::is(x@Stock, 'stock')) {
      x@Stock <- list(x@Stock)
      class(x@Stock) <- 'StockList'
    }
      
    if (methods::is(x@Fleet, 'fleet')) {
      x@Fleet <- list(list(x@Fleet))
      class(x@Fleet) <- 'StockFleetList'
      class(x@Fleet[[1]]) <- 'FleetList'
    }
      
  }
  if (methods::is(x, 'stock')) {
    x <- list(x)
    class(x) <- 'StockList'
  }
    
  if (methods::is(x, 'fleet')) {
    x <- list(list(x))   
    class(x) <- 'StockFleetList'
    class(x[[1]]) <- 'FleetList'
  }
    
  x
}




