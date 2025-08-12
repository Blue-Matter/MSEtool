CheckCatchFrac <- function(OM) {
  StockNames <- StockNames(OM)
  if (length(OM@CatchFrac)<1) {
    OM@CatchFrac <- MakeNamedList(StockNames)
  }
  
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nSim <- nSim(OM)
  
  names(OM@CatchFrac) <- StockNames
  
  if (nFleet==1)
    return(OM)
  
  if (length(OM@CatchFrac)!= nStock)
    cli::cli_abort('`OM@CatchFrac` must be a list length 0 or length `nStock(OM)` ')
  
  for (st in 1:nStock) {
    CatchFracFleet <- OM@CatchFrac[[st]]
    
    if (is.null(CatchFracFleet)) {
      CatchFracFleet <- matrix(1/nFleet, nSim, nFleet)
      cli::cli_warn(c("`CatchFrac(OM)` has not been specified for Stock {.val {StockNames[st]}} ",
                      'i'="Assuming catches equally distributed across fleets"))
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
                                     "Fleet"=FleetNames(OM))
    
    OM@CatchFrac[[st]] <- CatchFracFleet
  }
  OM
}