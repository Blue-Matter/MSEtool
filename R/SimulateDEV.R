
CheckSimsUnique <- function(List) {
  l1 <- MakeSimList(List, 1)
  l2 <- MakeSimList(List, 2)
  digest::digest(l1, algo='spookyhash') != digest::digest(l2, algo='spookyhash')
}

ConvertToSimList <- function(PopulationList, FleetList, OM) {
  
  SimsUnique <- FALSE
  if (OM@nSim>1) {
    SimsUnique <- CheckSimsUnique(PopulationList) | CheckSimsUnique(FleetList)
  }
  
  if (SimsUnique) {
    uniqueSims <- 1:OM@nSim
    PopulationListSim <- lapply(1:OM@nSim, function(x) MakeSimList(PopulationList, x))
    names(PopulationListSim) <- 1:OM@nSim
    FleetListSim <- lapply(1:OM@nSim, function(x) MakeSimList(FleetList, x)) 
    names(FleetListSim) <- 1:OM@nSim
  } else {
    uniqueSims <- 1
    PopulationListSim <- list(MakeSimList(PopulationList, 1))
    names(PopulationListSim) <- 1
    FleetListSim <- list(MakeSimList(FleetList, 1) )
    names(FleetListSim) <- 1
  }
  
  list(PopulationListSim=PopulationListSim,
       FleetListSim=FleetListSim)
}

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        messages='default',
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  MSEtool:::CheckClass(OM)
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- Check(OM) # TODO OM checks
  
  OM <- MSEtool:::StartUp(OM, messages) 
  
  # ---- Calculate Unfished Dynamics ----
  Unfished <- CalcUnfishedDynamics(OM)
  
  # ---- Make Lists of Arrays ----
  # Convert values from Stock and Fleet objects to named lists
  # Expands simulations, ages, and time steps as needed
  
  PopulationList <- MakePopulationList(OM, Unfished=Unfished)
  FleetList <- MakeFleetList(OM) # everything with a fleet dimension

  # Calculate Unfished 
  
  # Calculate Reference Points
  
  # Add to Pop List 
  
  # Convert to list by simulation
  # length OM@nSim or length 1 if all values identical across simulations
  SimList <- ConvertToSimList(PopulationList, FleetList, OM)
  PopulationListSim <- SimList$PopulationListSim
  FleetListSim <- SimList$FleetListSim
  

  # ---- Calculate Reference Points ----
  # RefPoints <- CalcRefPoints(OM)
  
  # ---- Number-at-Age at Beginning of Initial Time Step ----
  PopulationList <- CalcInitialTimeStep(PopulationList, Unfished) 
  
  # ---- Optimize for Final Depletion ----
  
  OptimCatchability(PopulationListSim, FleetListSim)
  

  # ---- Historical Population Dynamics ----
  PopDynamicsHistorical <- purrr::map2(PopulationListSim, FleetListSim, \(x,y)
                                       CalcPopDynamics_(x,
                                                        y,
                                                        TimeSteps=TimeSteps(OM, 'Historical'))
  ) 
  
  
 

  # ---- Project with an MP ----
  
  
 
  # TODO
  # - project with catch, effort, spatial, size limits
  # - make MICE model 
  # - make data 
  # - make obs
  # - make imp
  
  
  
  


  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  # make Hist object
}




ProjectDEV <- function(Hist, MPs=NULL, parallel = FALSE, silent = FALSE, options=NULL) {
  
  
} 

# TODO Pre-MP function



setClass('advice',
         slots=c(Name='character',
                 TimeStep='numeric',
                 Removal='numeric',
                 Retain='numeric',
                 Spatial='numeric',
                 Selectivity='list',
                 Retention='list',
                 DiscardMortality='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


Advice <- function() new('advice')


MP <- function(Data=NULL) {
  Advice <- Advice()
  Advice@Spatial <- c(0,1)
  Advice
}
class(MP) <- 'mp'


ProjectMP <- function(Hist, MP, parallel = FALSE, silent = FALSE, options=NULL) {
  
  AllTimeSteps <- TimeSteps(Hist)
  TimeSteps <- TimeSteps(Hist, 'Projection')
  
  progress <- seq_along(TimeSteps)

  # TODO create the arrays
  ts <- progress[1]
  
  for (ts in progress) {
    
    TSInd <- match(TimeStep, AllTimeSteps)
    
    TimeStep <- TimeSteps[ts]
    
    # ---- Apply MP ----
    
    ApplyMP <- function(Hist, DataList, MP, TimeStep) {
      
      
      
      
      # Apply the MP
      Advice <- MP()
      
      # update Selectivity, Retention, Spatial Closures based on MP Advice
      Hist@Fleet[[1]][[1]]@Distribution@Closure
      
      
      # Calculate Effort from Removal or Retain if necessary
      
      # Update Effort 
      
      
      # Return Hist
      Hist
      
    }
    
    # ---- Do MICE stuff during this Time Step (if applicable) -----
    # TODO
    Hist <- CalcMICE(Hist, TimeStep=TimeStep)
    
    # ---- Update Biomass At Age etc ----
    # done after MICE to account for changes
    Hist <- UpdateBioArrays(Hist, TimeStep)
    
  

    
    Hist@EffortArea
    
    # ---- Distribute Effort across Areas ----
    Hist <- DistributeEffort(Hist, TimeStep)
    
    # ---- Calculate Catch and Fishing Mortality ----
    Hist <- CalcCatch(Hist, TimeStep)
    
    Hist@Removal[[1]][1,,1,1,]
    
    
    # ---- Calculate Recruitment  Time Step ----
    Hist <- CalcRecruitment(Hist, TimeStep=TimeStep)
    
    # ---- Number, Biomass at beginning of Next Time Step and Move ----
    Hist <- CalcNumberNext(Hist, TimeStep)
    
    # print(sum(Hist@Number[[1]][1,,ts+1,]))
    
  }
  # tictoc::toc()
  
}

