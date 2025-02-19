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

CalcPopDynamics <- function(Hist, TimeSteps, MP=NULL, silent=FALSE) {
  on.exit(cli::cli_progress_done())
  
  progress <- seq_along(TimeSteps)
  
  if (!silent) {
    progress <- cli::cli_progress_along(TimeSteps,
                                        'Calculating Population Dyamics')
  } 
 
  # tictoc::tic()
  for (ts in progress) {
    
    thisTimeStep <- TimeSteps[ts]
    
    # ---- Do MICE stuff during this Time Step (if applicable) -----
    # TODO
    Hist <- CalcMICE(Hist, TimeStep=thisTimeStep)
    
    # ---- Update Biomass At Age ----
    # done after MICE to account for changes
    Hist <- UpdateArrays(Hist, thisTimeStep)
    
    
  
    
    
    # for MPs - Calculate Effort, Selectivity, etc
    
    # ---- Calculate Catch by Area this Time Step ----
    Hist <- CatchByArea(Hist, TimeSteps=thisTimeStep)
   
    # ---- Calculate Recruitment  Time Step ----
    Hist <- CalcRecruitment(Hist, TimeStep=thisTimeStep)
    
    # ---- Number, Biomass at beginning of Next Time Step and Move ----
    Hist <- CalcNumberNext(Hist, thisTimeStep)
    
  }
  # tictoc::toc()
  
  
  
  Hist
}