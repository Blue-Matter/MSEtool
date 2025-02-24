
CreateArrayAT <- function(nAge, TimeSteps) {
  array(tiny, TimeSteps, dim=c(nAge,
                               length(TimeSteps)),
        dimnames=list(Age=0:(nAge-1),
                      `Time Step`=TimeSteps)
  )
}


# Sim, Age 

# Sim, Age, Time Step

# Sim Age, Time Step, Fleet

# Sim, Age, Time Step, Fleet, Area  


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
