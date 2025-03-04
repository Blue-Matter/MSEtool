
# Sim, Stock, Age 
ArraySimStockAge <- function(OM, Period=c('Historical', 'Projection', 'All'),
                             default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  array(default, dim=c(meta$nSim,
                    length(meta$StockNames),
                    meta$nAges),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=meta$StockNames,
                      Age=0:(meta$nAges-1))
        )  
}

# Sim, Stock, Age, Time Step
ArraySimStockAgeTime <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                 default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
 
  array(default, dim=c(meta$nSim,
                    length(meta$StockNames),
                    meta$nAges,
                    length(meta$TimeSteps)),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=meta$StockNames,
                      Age=0:(meta$nAges-1),
                      `Time Step`=meta$TimeSteps)
        )  
}

# Sim, Stock, Age, Time Step, Area
ArraySimStockAgeTimeArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  array(default, dim=c(meta$nSim,
                    length(meta$StockNames),
                    meta$nAges,
                    length(meta$TimeSteps),
                    meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=meta$StockNames,
                      Age=0:(meta$nAges-1),
                      `Time Step`=meta$TimeSteps,
                      Area=1:meta$nAreas)
  )  
}


# Sim, Stock, Age, Time Step, Fleet
ArraySimStockAgeTimeFleet <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                      default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  array(default, dim=c(meta$nSim,
                    length(meta$StockNames),
                    meta$nAges,
                    length(meta$TimeSteps),
                    length(meta$FleetNames)),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=meta$StockNames,
                      Age=0:(meta$nAges-1),
                      `Time Step`=meta$TimeSteps,
                      Fleet=meta$FleetNames)
  )  
}


# Sim, Stock, Age, Time Step, Fleet, Area
ArraySimStockAgeTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                      default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
 
  array(default, dim=c(meta$nSim,
                       length(meta$StockNames),
                       meta$nAges,
                       length(meta$TimeSteps),
                       length(meta$FleetNames),
                       meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=meta$StockNames,
                      Age=0:(meta$nAges-1),
                      `Time Step`=meta$TimeSteps,
                      Fleet=meta$FleetNames,
                      Area=1:meta$nAreas)
  )  
}
