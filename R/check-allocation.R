CheckAllocation <- function(OM) {
  StockNames <- StockNames(OM)
  if (length(OM@Allocation)<1) {
    OM@Allocation <- MakeNamedList(StockNames)
  }
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nSim <- nSim(OM)
  
  names(OM@Allocation) <- StockNames
  
  if (length(OM@Allocation)!= nStock)
    cli::cli_abort('`OM@Allocation` must be a list length 0 or length `nStock(OM)` ')
  
  for (st in 1:nStock) {
    AllocationFleet <- OM@Allocation[[st]] 
    
    if (is.null(AllocationFleet)) {
      
      if (!is.null(OM@CatchFrac[[st]])) {
        AllocationFleet <- OM@CatchFrac[[st]]
        cli::cli(c(
          cli::cli_alert_info("`Allocation(OM)` has not been specified for Stock {.val {StockNames[st]}}"),
          cli::cli_alert("Assuming distribution of TAC in projections is the same as `CatchFrac`")
        ))
      } else {
        AllocationFleet <- matrix(1/nFleet, nSim, nFleet)
        cli::cli(c(
          cli::cli_alert_info("`Allocation(OM)` has not been specified for Stock {.val {StockNames[st]}}"),
          cli::cli_alert("Assuming TAC in projections equally distributed across fleets")
        ))
      }
    } 
    dd <- dim(AllocationFleet)
    if (dd[1]>nSim)
      cli::cli_abort('`OM@Allocation` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (dd[2]!=nFleet)
      cli::cli_abort('`OM@Allocation` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (any(AllocationFleet<0) || any(!is.finite(AllocationFleet)))
      cli::cli_abort('Values in `OM@Allocation` must be positive')
    
    rsum <- rowSums(AllocationFleet)
    if (any(rsum!=1))
      cli::cli_abort('Values in `OM@Allocation` sum to 1 across rows')
    
    dimnames(AllocationFleet) <- list("Sim"=1:dd[1],
                                      "Fleet"=FleetNames(OM))
    
    OM@Allocation[[st]] <- AllocationFleet
    
  }
  OM
}