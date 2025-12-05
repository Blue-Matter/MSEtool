SimulateDynamics <- function(SimList,
                             HistYears) {
  
  
  if (CheckIdenticalSims(SimList, HistYears)) {
    # stop('not done yet!')
    
    # if (IdenticalAcrossSims) {
    #   # TODO - only run SimulateDynamics_ once and copy across SimList
    #   # need to make sure to update all historical dynamics - eg Stock@Length for each sim
    #   # if MICE is used
    # } 
    
  }
  
  SimListOut <- purrr::map(SimList, \(HistSim) 
                        SimulateDynamics_(HistSim, HistYears),
                        .progress = list(
                          type = "iterator", 
                          format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))

  # populate CatchFrac if needed
  SimListOut <- purrr::map(SimListOut, \(HistSim) {
    if (length(HistSim@OM@CatchFrac))
      return(HistSim)
    
    HistSim@OM@CatchFrac <- purrr::map2(HistSim@LandingsAtAge, HistSim@DiscardsAtAge, \(landings, discards) {
      removals <- landings[[length(landings)]] + discards[[length(discards)]]
      fleetCatch <- apply(removals,2, sum)
      fleetCatch/sum(fleetCatch)
    })
    HistSim
  })
  
  # Add dimensiom names back
  SimListOut <- purrr::map2(SimList, SimListOut, AddDimensionNames)
    
  
  class(SimListOut) <- 'simlist'
  SimListOut
}

AddDimensionNames <- function(Hist, HistOut) {
  slots <- slotNames('timeseries')
  for (sl in slots) { 
    object <- slot(HistOut, sl) 
    Namedobject <- slot(Hist, sl)
    if (is.list(object)) {
      object <- purrr::map2(object, Namedobject, \(x,y) {
        dimnames(x) <- dimnames(y)
        x 
      })
    } else {
      dimnames(object) <- dimnames(Namedobject)
    }
    slot(HistOut, sl) <- object
  }
  HistOut
}