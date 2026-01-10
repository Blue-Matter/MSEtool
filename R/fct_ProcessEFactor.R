ProcessEFactor <- function(OM) {
  if (!length(OM@EFactor)) {
    OM@EFactor <- MakeNamedList(StockNames(OM), 
                                array(1, dim=c(1, nFleet(OM)),
                                      dimnames = list(
                                        Sim=1,
                                        Fleet=FleetNames(OM)[[1]]
                                      )
                                ))
    return(OM)
  }
  
  # TODO
  # check EFactor
  
}