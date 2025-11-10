ProcessEFactor <- function(om) {
  if (!length(om@EFactor)) {
    om@EFactor <- MakeNamedList(StockNames(om), 
                                array(1, dim=c(1, nFleet(om)),
                                      dimnames = list(
                                        Sim=1,
                                        Fleet=FleetNames(om)
                                      )
                                ))
    return(om)
  }
  
  # TODO
  # check EFactor
  
}