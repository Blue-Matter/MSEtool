ProcessCatchFrac <- function(object) {
  if (length(object@CatchFrac)<1) {
    object@CatchFrac <- MakeNamedList(StockNames(object))
  }
  
  nStock <- nStock(object)
  nFleet <- nFleet(object)
  nSim <- nSim(object)
  
  names(object@CatchFrac) <- StockNames(object)
  
  if (length(object@CatchFrac)!= nStock)
    cli::cli_abort('`OM@CatchFrac` must be a list length 0 or length `nStock(OM)` ')
  
  for (st in 1:nStock) {
    CatchFracFleet <- object@CatchFrac[[st]]
    if (is.null(CatchFracFleet)) {
      CatchFracFleet <- matrix(1/nFleet, nSim, nFleet)
      if (nFleet>1) 
        cli::cli_alert_warning("`OM@CatchFrac` must be specified if there are multiple fleets... ")
      
    } 
    dd <- dim(CatchFracFleet)
    
    if (dd[1]>nSim)
      cli::cli_abort('`OM@CatchFrac` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (dd[2]!=nFleet)
      cli::cli_abort('`OM@CatchFrac` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (any(CatchFracFleet<0) || any(!is.finite(CatchFracFleet)))
      cli::cli_abort('Values in `OM@CatchFrac` must be positive')
    
    rsum <- rowSums(CatchFracFleet)
    if (any(rsum!=1))
      cli::cli_abort('Values in `OM@CatchFrac` sum to 1 across rows')
    
    dimnames(CatchFracFleet) <- list("Sim"=1:dd[1],
                                     "Fleet"=FleetNames(object))
    
    object@CatchFrac[[st]] <- CatchFracFleet
  }
  
  object
  
}
