ProcessEFactor <- function(OM) {
  if (!length(OM@EFactor)) {
    FleetNames <- FleetNames(OM)
    if (is.list(FleetNames))
      FleetNames <- FleetNames[[1]]
    nFleet <- length(FleetNames)
    
    OM@EFactor <- MakeNamedList(StockNames(OM), 
                                array(1, dim=c(1,nFleet),
                                      dimnames = list(
                                        Sim=1,
                                        Fleet=FleetNames
                                      )
                                ))
    return(OM)
  }
  
  # TODO
  # check EFactor
  
}