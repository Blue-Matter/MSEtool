

# ---- Sim, Age ----
ArraySimAge <- function(OM, Period=c('Historical', 'Projection', 'All'),
                        stock=1, default=tiny/2) {
  
  meta <- GetMetaData(OM, Period)
  nAges <-  meta$nAges[[stock]]
  array(default, dim=c(meta$nSim,
                       nAges),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1))
        )  
}

ListArraySimAge <- function(OM, Period=c('Historical', 'Projection', 'All'),
                            default=tiny/2) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAge(OM, Period, stock=st, default)
  } 
  List
}



# ---- Sim, Age, Time Step ----

ArraySimAgeTime <- function(OM, Period=c('Historical', 'Projection', 'All'),
                        stock=1, default=tiny/2, TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nAges <-  meta$nAges[[stock]]
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$TimeSteps)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      `Time Step`=meta$TimeSteps)
  ) 
    
}
  
ListArraySimAgeTime <- function(OM, Period=c('Historical', 'Projection', 'All'),
                            default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTime(OM, Period, stock=st, default, TimeSteps)
  } 
  List
}

# ---- Sim, Age, Time Step, Area ----
ArraySimAgeTimeArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  nAges <- meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$TimeSteps),
                       meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      `Time Step`=meta$TimeSteps,
                      Area=1:meta$nAreas)
  )
}

ListArraySimAgeTimeArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeArea(OM, Period, stock=st, default, TimeSteps)
  } 
  List
}

# ---- Sim, Age, Time Step, Fleet ----
ArraySimAgeTimeFleet <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                 stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                    nAges,
                    length(meta$TimeSteps),
                    length(meta$FleetNames)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      `Time Step`=meta$TimeSteps,
                      Fleet=meta$FleetNames)
  )  
}

ListArraySimAgeTimeFleet <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                    default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleet(OM, Period, stock=st, default, TimeSteps)
  } 
  List
}


# ---- Sim, Age, Time Step, Fleet, Area ----
ArraySimAgeTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$TimeSteps),
                       length(meta$FleetNames),
                       meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      `Time Step`=meta$TimeSteps,
                      Fleet=meta$FleetNames,
                      Area=1:meta$nAreas)
  )  
}

ListArraySimAgeTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetArea(OM, Period, stock=st, default, TimeSteps)
  } 
  List
}
