# sim, age, time step, region (area), fleet

CreateArraySATRF <- function(Stock, nsim=NULL, timesteps=NULL, fleetnames=NULL) {
  # if (inherits(Stock, 'om')) {
  #   return(purrr::map(Stock@Stock, CreateArraySATRF, nsim, timesteps, fleetnames))
  # }
  
  array <- CreateArraySATR(Stock, nsim, timesteps) |> 
    AddDimension('Fleet')
  dimnames(array)$Fleet <- fleetnames
  array
  
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
  array <- array(NA, dim=c(nsim,nAge(Stock), length(timesteps), nArea(Stock)))
  AddDimNames(array, names=c("Sim", "Age", "Time Step", 'Area'),
              TimeSteps = timesteps)
  
}

InitNumber <- function(Stock, UnfishedNumber) {
  HistTimeSteps <- TimeSteps(Stock, 'Historical')
  RecDevInit <- GetRecDevInit(Stock)
  RecDevHist <- GetRecDevHist(Stock)
  InitAgeClassRecDevs <- cbind(RecDevHist[,1, drop=FALSE], RecDevInit) |>
    AddDimension('Time Step') |> 
    AddDimension('Area') 
  dd <- dim(InitAgeClassRecDevs)
  dimnames(InitAgeClassRecDevs) <- list(Sim=1:dd[1],
                                        Age=0:(dd[2]-1),
                                        `Time Step`=HistTimeSteps[1:dd[3]],
                                        Area=1:dd[4])
  
  NumberHist <- CreateArraySATR(Stock,
                                dd[1],
                                HistTimeSteps)
  
  NumberHist[,,1,] <- ArrayMultiply(UnfishedNumber[, ,1,, drop=FALSE], InitAgeClassRecDevs)
  NumberHist
}