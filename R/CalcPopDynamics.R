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

CalcPopDynamics <- function(Hist, TimeSteps=NULL, MP=NULL, silent=FALSE) {
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist, 'Historical')
  
  progress <- seq_along(TimeSteps)
  
  if (!silent)  {
    progress <- cli::cli_progress_along(TimeSteps,
                                        'Calculating Population Dyamics')
    on.exit(cli::cli_progress_done())
  }
  

  # Arrays to create to convert the whole process to C++
  # Set Up Arrays
  # For each Stock:
  # nsim, nage, nts, narea
  # Number <- Hist@Number 
  # Biomass <- Hist@Biomass
  # SBiomass <- Hist@SBiomass
  # LengthAtAge
  # WeightAtAge
  # MaturityAtAge
  
  # # nsim, nts
  # SProduction <- Hist@SProduction
  # 
  # # nsim, nage, nts, nfleet
  # Selectivity
  # Retention
  # DiscardMortality
  # 
  # 
  # # nsim, nts, nfleet, narea
  # 
  # 
  # VBiomass
  # 
  # # nsim, narea
  # AreaSize 
  # 

  
  for (ts in progress) {
    
    TimeStep <- TimeSteps[ts]

    
    # ---- Do MICE stuff during this Time Step (if applicable) -----
    # TODO
    Hist <- CalcMICE(Hist, TimeStep=TimeStep)
    
    # ---- Update Biomass At Age etc ----
    # done after MICE to account for changes
    Hist <- UpdateBioArrays(Hist, TimeStep)
  
    # for MPs - Calculate Effort, Selectivity, etc
    # update fishery data 
    # these two steps should be done first
  
    # ---- Distribute Effort across Areas ----
    Hist <- DistributeEffort(Hist, TimeStep)
    
    # ---- Calculate Catch and Fishing Mortality ----
    Hist <- CalcCatch(Hist, TimeStep)
    
    # ---- Calculate Recruitment  Time Step ----
    Hist <- CalcRecruitment(Hist, TimeStep=TimeStep)
    
    # ---- Number, Biomass at beginning of Next Time Step and Move ----
    Hist <- CalcNumberNext(Hist, TimeStep)
    
    # print(sum(Hist@Number[[1]][1,,ts+1,]))
    
  }
  # tictoc::toc()
  

  
  
  Hist
}