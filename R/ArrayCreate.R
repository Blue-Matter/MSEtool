
ArraySimStockAgeTimeArea <- function(Sims, Stocks, Ages, Years, Areas, default=0) {
  
  array(default, dim=c(length(Sims),
                       length(Stocks),
                       length(Ages),
                       length(Years),
                       length(Areas)),
        dimnames=list(Sim=Sims,
                      Stock=Stocks,
                      Age=Ages,
                      Year=Years,
                      Area=Areas)
  )
  
  
}


# ---- Sim, Age ----
ArraySimAge <- function(OM, Period=c('Historical', 'Projection', 'All'),
                        stock=1, default=0) {
  
  meta <- GetMetaData(OM, Period)
  nAges <-  meta$nAges[[stock]]
  array(default, dim=c(meta$nSim,
                       nAges),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1))
        )  
}

ListArraySimAge <- function(OM, Period=c('Historical', 'Projection', 'All'),
                            default=0) {
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
                        stock=1, default=0, Years=NULL) {
  
  meta <- GetMetaData(OM, Period, Years)
  nAges <-  meta$nAges[[stock]]
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$Years)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      Year=meta$Years)
  ) 
    
}
  
ListArraySimAgeTime <- function(OM, Period=c('Historical', 'Projection', 'All'),
                            default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTime(OM, Period, stock=st, default, Years)
  } 
  List
}

# ---- Sim, Age, Time Step, Area ----
ArraySimAgeTimeArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  AgeClasses <-  meta$AgeClasses[[stock]]
  nAges <- length(AgeClasses)
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$Years),
                       meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Age=AgeClasses,
                      Year=meta$Years,
                      Area=1:meta$nAreas)
  )
}

ListArraySimAgeTimeArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeArea(OM, Period, stock=st, default, Years)
  } 
  List
}

# ---- Sim, Age, Time Step, Fleet ----
ArraySimAgeTimeFleet <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                 stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  AgeClasses <-  meta$AgeClasses[[stock]]
  nAges <- length(AgeClasses)
  
  array(default, dim=c(meta$nSim,
                    nAges,
                    length(meta$Years),
                    length(meta$FleetNames)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=AgeClasses,
                      Year=meta$Years,
                      Fleet=meta$FleetNames)
  )  
}

ListArraySimAgeTimeFleet <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                    default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames

  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleet(OM, Period, stock=st, default, Years)
  } 
  List
}




# ---- Sim, Age, Time Step, Fleet, Area ----
ArraySimAgeTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  AgeClasses <-  meta$AgeClasses[[stock]]
  nAges <- length(AgeClasses)
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$Years),
                       length(meta$FleetNames),
                       meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Age=AgeClasses,
                      Year=meta$Years,
                      Fleet=meta$FleetNames,
                      Area=1:meta$nAreas)
  )  
}

ListArraySimAgeTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetArea(OM, Period, stock=st, default, Years)
  } 
  List
}



ListArraySimClassTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  FleetNames <- meta$FleetNames
  
  List <- MakeNamedList(stocknames,
                        MakeNamedList(FleetNames)
                        )
  
  for (st in 1:length(List)) {
    for (fl in seq_along(FleetNames))
      List[[st]][[fl]] <- ArraySimClassYearArea(OM, Period, stock=st, fleet=fl, default, Years)
  } 
  List
}


ArraySimClassYearArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     stock=1, fleet=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  
  Classes <- OM@Fleet[[stock]][[fleet]]@Selectivity@Classes
  
  array(default, dim=c(meta$nSim,
                       length(Classes),
                       length(meta$Years),
                       meta$nAreas),
        dimnames=list(Sim=1:meta$nSim,
                      Class=Classes,
                      Year=meta$Years,
                      Area=1:meta$nAreas)
  )  
}


# ---- Sim, Age, Time Step, Area, MP ----
ArraySimAgeTimeMPArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$Years),
                       meta$nAreas,
                       length(MPs)
                       ),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      Year=meta$Years,
                      Area=1:meta$nAreas,
                      MP=MPs)
  )  
}

ListArraySimAgeTimeAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           MPs=NULL,
                                           default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeMPArea(OM, Period, MPs, stock=st, default, Years)
  } 
  List
}


# ---- Sim, Age, Time Step, Fleet, Area, MP ----
ArraySimAgeTimeFleetAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$Years),
                       length(FleetNames),
                       meta$nAreas,
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      Year=meta$Years,
                      Fleet=FleetNames,
                      Area=1:meta$nAreas,
                      MP=MPs)
  )  
}

ListArraySimAgeTimeFleetAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           MPs=NULL,
                                           default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetAreaMP(OM, Period, MPs, stock=st, default, Years)
  } 
  List
}



# ---- Sim, Stock, Time Step, MP ----

ArraySimStockTimeMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                  MPs=NULL,
                                  stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  
  StockNames <- meta$StockNames
  array(default, dim=c(meta$nSim,
                       length(StockNames),
                       length(meta$Years),
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=StockNames,
                      Year=meta$Years,
                      MP=MPs)
  )  
}
# ---- Sim, Stock, Time Step, Fleet MP ----

ArraySimStockTimeFleetMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                MPs=NULL,
                                stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  
  FleetNames <- meta$FleetNames
  StockNames <- meta$StockNames
  array(default, dim=c(meta$nSim,
                       length(StockNames),
                       length(meta$Years),
                       length(FleetNames),
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Stock=StockNames,
                      Year=meta$Years,
                      Fleet=FleetNames,
                      MP=MPs)
  )  
}

# ---- Sim, Age, Year, Fleet, MP ----

ArraySimAgeTimeFleetMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       stock=1, default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period, Years)
  
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  nAges <-  meta$nAges[[stock]]
  
  array(default, dim=c(meta$nSim,
                       nAges,
                       length(meta$Years),
                       length(FleetNames),
                       length(MPs)),
        dimnames=list(Sim=1:meta$nSim,
                      Age=0:(nAges-1),
                      Year=meta$Years,
                      Fleet=FleetNames,
                      MP=MPs)
  )  
}

ListArraySimAgeTimeFleetMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                           MPs=NULL,
                                           default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetMP(OM, Period, MPs, stock=st, default, Years)
  } 
  List
}


# ---- Sim, Year, Fleet, Area, MP ----

ListArraySimTimeFleetAreaMP <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                       MPs=NULL,
                                       default=0, Years=NULL) {
  meta <- GetMetaData(OM, Period)
  stocknames <- meta$StockNames
  FleetNames <- as.vector(OM@Fleet[[1]]@Name)
  
  List <- MakeNamedList(stocknames)
  for (st in 1:length(List)) {
    List[[st]] <- ArraySimAgeTimeFleetAreaMP(OM, Period, MPs, stock=st, default, Years) |>
      DropDimension('Age', FALSE)
  } 
  List
}





