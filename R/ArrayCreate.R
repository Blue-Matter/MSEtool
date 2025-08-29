
ArraySimStockAgeTimeArea <- function(Sims, Stocks, Ages, TimeSteps, Areas, default=tiny/2) {
  
  array(default, dim=c(length(Sims),
                       length(Stocks),
                       length(Ages),
                       length(TimeSteps),
                       length(Areas)),
        dimnames=list(Sim=Sims,
                      Stock=Stocks,
                      Age=Ages,
                      TimeStep=TimeSteps,
                      Area=Areas)
  )
  
  
}


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
                      TimeStep=meta$TimeSteps)
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
                      TimeStep=meta$TimeSteps,
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
                      TimeStep=meta$TimeSteps,
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
                      TimeStep=meta$TimeSteps,
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



# ---- Sim, Age, Time Step, Area, MP ----
ArraySimAgeTimeMPArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$TimeSteps),
                       meta$nAreas,
                       length(MPs)
                       ),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      TimeStep=meta$TimeSteps,
                      Area=1:meta$nAreas,
                      MP=MPs)
  )  
}

ListArraySimAgeTimeAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           MPs=NULL,
                                           default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeMPArea(OM, Period, MPs, stock=st, default, TimeSteps)
  } 
  List
}


# ---- Sim, Age, Time Step, Fleet, Area, MP ----
ArraySimAgeTimeFleetAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$TimeSteps),
                       length(FleetNames),
                       meta$nAreas,
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      TimeStep=meta$TimeSteps,
                      Fleet=FleetNames,
                      Area=1:meta$nAreas,
                      MP=MPs)
  )  
}

ListArraySimAgeTimeFleetAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           MPs=NULL,
                                           default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetAreaMP(OM, Period, MPs, stock=st, default, TimeSteps)
  } 
  List
}



# ---- Sim, Stock, Time Step, MP ----

ArraySimStockTimeMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                  MPs=NULL,
                                  stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  StockNames <- meta$StockNames
  array(default, dim=c(meta$nSim,
                       length(StockNames),
                       length(meta$TimeSteps),
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=StockNames,
                      TimeStep=meta$TimeSteps,
                      MP=MPs)
  )  
}
# ---- Sim, Stock, Time Step, Fleet MP ----

ArraySimStockTimeFleetMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                MPs=NULL,
                                stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  
  StockNames <- meta$StockNames
  array(default, dim=c(meta$nSim,
                       length(StockNames),
                       length(meta$TimeSteps),
                       length(FleetNames),
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=StockNames,
                      TimeStep=meta$TimeSteps,
                      Fleet=FleetNames,
                      MP=MPs)
  )  
}

# ---- Sim, Age, Timestep, Fleet, MP ----

ArraySimAgeTimeFleetMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       stock=1, default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period, TimeSteps)
  
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$TimeSteps),
                       length(FleetNames),
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      TimeStep=meta$TimeSteps,
                      Fleet=FleetNames,
                      MP=MPs)
  )  
}

ListArraySimAgeTimeFleetMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           MPs=NULL,
                                           default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetMP(OM, Period, MPs, stock=st, default, TimeSteps)
  } 
  List
}


# ---- Sim, Timestep, Fleet, Area, MP ----

ListArraySimTimeFleetAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       default=tiny/2, TimeSteps=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetAreaMP(OM, Period, MPs, stock=st, default, TimeSteps) |>
      DropDimension('Age', FALSE)
  } 
  List
}





