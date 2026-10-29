

.ConvertToList <- function(x) {
  # TODO add names
  if (inherits(x, 'om')) {
    if (inherits(x@Stock, 'stock')) {
      x@Stock <- list(x@Stock)
      class(x@Stock) <- 'StockList'
    }
    
    if (inherits(x@Fleet, 'fleet')) {
      x@Fleet <- list(list(x@Fleet))
      class(x@Fleet) <- 'StockFleetList'
      class(x@Fleet[[1]]) <- 'FleetList'
    }
    
  }
  if (inherits(x, 'stock')) {
    x <- list(x)
    class(x) <- 'StockList'
  }
  
  if (inherits(x, 'fleet')) {
    x <- list(list(x))   
    class(x) <- 'StockFleetList'
    class(x[[1]]) <- 'FleetList'
  }
  
  x
}









