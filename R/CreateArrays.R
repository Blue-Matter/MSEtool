
# Sim, Stock, Age, Time Step
ArraySimStockAgeTime <- function(OM, Period=c('Historical', 'Projection', 'All')) {
  Period <- match.arg(Period)
  
  nSim <- nSim(OM)
  StockNames <- StockNames(OM)
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()
  TimeSteps <- TimeSteps(OM, Period)
 
  array(tiny, dim=c(nSim,
                    length(StockNames),
                    nAges,
                    length(TimeSteps)),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1),
                      `Time Step`=TimeSteps)
        )  
  
  
  nSim <- 200
  StockNames <- c('A', 'B', 'C')
  nAges <- 50
  TimeSteps <- 1950:2050
  
  array <- array(tiny, dim=c(nSim,
                    length(StockNames),
                    nAges,
                    length(TimeSteps)),
        dimnames=list(Sim=1:nSim,
                      Stock=StockNames,
                      Age=0:(nAges-1),
                      `Time Step`=TimeSteps)
  )  
  
  
  List <- Array2List(array, 'Sim')
  
  object.size(array)/1e6
  object.size(List)/1e6
  
  apply(array, 1, object.size)
  obj <- abind::adrop(array[1,,,, drop=FALSE],1)
  
  object.size(obj)
  
  object.size(List[[1]]) 
  lapply(List, object.size)
  
  11624*20
  
  # m <- apply(t, 1:4, max)
  # 
  l <- Array2List(t, 'Age')
  # 
  # l <- Array2List(t, 'Sim')
  # object.size(l)/1E6
  
}

CreateArrayAT <- function(nAge, TimeSteps) {
  array(tiny, TimeSteps, dim=c(nAge,
                               length(TimeSteps)),
        dimnames=list(Age=0:(nAge-1),
                      `Time Step`=TimeSteps)
  )
}


# Sim, Stock, Age 

# Sim, Stock, Age, Time Step

# Sim, Stock, Age, Time Step, Fleet

# Sim, Stock, Age, Time Step, Fleet, Area



# sim, age, timestep, fleet
CreateArraySATF <- function(Stock, nsim=NULL, timesteps=NULL, fleetnames=NULL) {
  
  array <- CreateArraySATR(Stock, nsim, timesteps)  |> DropDimension(warn=FALSE)
  
  arrayFleet <- replicate(length(fleetnames), array)
  
  l <- dimnames(array)
  l$Fleet <- fleetnames
  dimnames(arrayFleet) <- l
  arrayFleet 
}

# sim, age, time step, fleet, area

CreateArraySATFR <- function(Stock, nsim=NULL, timesteps=NULL, fleetnames=NULL) {
  
  array <- CreateArraySATR(Stock, nsim, timesteps) 
  arrayFleet <- replicate(length(fleetnames), array)
  
  l <- dimnames(array)
  l$Fleet <- fleetnames
  dimnames(arrayFleet) <- l
  
  arrayFleet |> aperm(c(1,2,3,5,4))
  
}

# sim, age, time step, region (area)
CreateArraySATR <- function(Stock, nsim=NULL, timesteps=NULL) {
  # if (inherits(Stock, 'om')) {
  #   return(purrr::map(Stock@Stock, CreateArraySATR, nsim, timesteps))
  # }
  if (is.null(nsim))
    nsim <- nSim(Stock)
  if (is.null(timesteps))
    timesteps <- TimeSteps(Stock, 'Historical')
  # sim, age, time step, region (area)
  array <- array(tiny, dim=c(nsim,nAge(Stock), length(timesteps), nArea(Stock)))
  AddDimNames(array, names=c("Sim", "Age", "Time Step", 'Area'),
              TimeSteps = timesteps)
  
}
