SimulateDynamics <- function(SimList,
                             HistTimeSteps) {
  
  
  
  if (CheckIdenticalSims(SimList, HistTimeSteps)) {
    stop('not done yet!')
    
    # if (IdenticalAcrossSims) {
    #   # TODO - only run SimulateDynamics_ once and copy across SimList
    #   # need to make sure to update all historical dynamics - eg Stock@Length for each sim
    #   # if MICE is used
    # } 
    
  }
  
  SimList <- purrr::map(SimList, \(HistSim) 
                        SimulateDynamics_(HistSim, HistTimeSteps),
                        .progress = list(
                          type = "iterator", 
                          format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))
  
  # populate CatchFrac if needed
  SimList <- purrr::map(SimList, \(HistSim) {
    if (length(HistSim@OM@CatchFrac))
      return(HistSim)
    
    HistSim@OM@CatchFrac <- purrr::map2(HistSim@Landings, HistSim@Discards, \(landings, discards) {
      removals <- landings[[length(landings)]] + discards[[length(discards)]]
      fleetCatch <- apply(removals,2, sum)
      fleetCatch/sum(fleetCatch)
    })
    HistSim
  })
  
  class(SimList) <- 'simlist'
  SimList
}