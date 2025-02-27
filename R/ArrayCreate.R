
# Sim, Stock, Age 
ArraySimStockAge <- function(OM, Period=c('Historical', 'Projection', 'All'),
                             default=tiny/2) {
  Period <- match.arg(Period)
  
  nSim <- nSim(OM)
  StockNames <- StockNames(OM)
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()
  
  array(default, dim=c(nSim,
                    length(StockNames),
                    nAges),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1))
        )  
}

# Sim, Stock, Age, Time Step
ArraySimStockAgeTime <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                 default=tiny/2) {
  Period <- match.arg(Period)
  
  nSim <- nSim(OM)
  StockNames <- StockNames(OM)
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()
  TimeSteps <- TimeSteps(OM, Period)
 
  array(default, dim=c(nSim,
                    length(StockNames),
                    nAges,
                    length(TimeSteps)),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1),
                      `Time Step`=TimeSteps)
        )  
}

# Sim, Stock, Age, Time Step, Area
ArraySimStockAgeTimeArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                     default=tiny/2) {
  Period <- match.arg(Period)
  
  nSim <- nSim(OM)
  StockNames <- StockNames(OM)
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()
  TimeSteps <- TimeSteps(OM, Period)
  nAreas <- unlist(purrr::map(OM@Stock, nArea)) |> unique()
  if (length(nAreas)>1) 
    cli::cli_abort('All Stocks must have the same number of areas')
  
  array(default, dim=c(nSim,
                    length(StockNames),
                    nAges,
                    length(TimeSteps),
                    nAreas),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1),
                      `Time Step`=TimeSteps,
                      Area=1:nAreas)
  )  
}


# Sim, Stock, Age, Time Step, Fleet
ArraySimStockAgeTimeFleet <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                      default=tiny/2) {
  Period <- match.arg(Period)
  
  nSim <- nSim(OM)
  StockNames <- StockNames(OM)
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()
  TimeSteps <- TimeSteps(OM, Period)
  FleetNames <- FleetNames(OM)
  array(default, dim=c(nSim,
                    length(StockNames),
                    nAges,
                    length(TimeSteps),
                    length(FleetNames)),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1),
                      `Time Step`=TimeSteps,
                      Fleet=FleetNames)
  )  
}


# Sim, Stock, Age, Time Step, Fleet, Area
ArraySimStockAgeTimeFleetArea <- function(OM, Period=c('Historical', 'Projection', 'All'),
                                      default=tiny/2) {
  Period <- match.arg(Period)
  
  nSim <- nSim(OM)
  StockNames <- StockNames(OM)
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()
  TimeSteps <- TimeSteps(OM, Period)
  FleetNames <- FleetNames(OM)
  nAreas <- unlist(purrr::map(OM@Stock, nArea)) |> unique()
  if (length(nAreas)>1) 
    cli::cli_abort('All Stocks must have the same number of areas')
  
  array(default, dim=c(nSim,
                       length(StockNames),
                       nAges,
                       length(TimeSteps),
                       length(FleetNames),
                       nAreas),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1),
                      `Time Step`=TimeSteps,
                      Fleet=FleetNames,
                      Area=1:nAreas)
  )  
}

# 
# # sim, age, timestep, fleet
# CreateArraySATF <- function(Stock, nsim=NULL, timesteps=NULL, fleetnames=NULL) {
#   
#   array <- CreateArraySATR(Stock, nsim, timesteps)  |> DropDimension(warn=FALSE)
#   
#   arrayFleet <- replicate(length(fleetnames), array)
#   
#   l <- dimnames(array)
#   l$Fleet <- fleetnames
#   dimnames(arrayFleet) <- l
#   arrayFleet 
# }
# 
# # sim, age, time step, fleet, area
# 
# CreateArraySATFR <- function(Stock, nsim=NULL, timesteps=NULL, fleetnames=NULL) {
#   
#   array <- CreateArraySATR(Stock, nsim, timesteps) 
#   arrayFleet <- replicate(length(fleetnames), array)
#   
#   l <- dimnames(array)
#   l$Fleet <- fleetnames
#   dimnames(arrayFleet) <- l
#   
#   arrayFleet |> aperm(c(1,2,3,5,4))
#   
# }
# 
# # sim, age, time step, region (area)
# CreateArraySATR <- function(Stock, nsim=NULL, timesteps=NULL) {
#   # if (inherits(Stock, 'om')) {
#   #   return(purrr::map(Stock@Stock, CreateArraySATR, nsim, timesteps))
#   # }
#   if (is.null(nsim))
#     nsim <- nSim(Stock)
#   if (is.null(timesteps))
#     timesteps <- TimeSteps(Stock, 'Historical')
#   # sim, age, time step, region (area)
#   array <- array(tiny, dim=c(nsim,nAge(Stock), length(timesteps), nArea(Stock)))
#   AddDimNames(array, names=c("Sim", "Age", "Time Step", 'Area'),
#               TimeSteps = timesteps)
#   
# }
