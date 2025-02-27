
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
  
  OM <- MSEtool:::StartUp(OM, messages, nSim) 
  
  # ---- Make Lists of Arrays ----
  StockParsList <- MakeStockParsList(OM)
  FleetParsList <- MakeFleetParsList(OM)
  
  # object too big for NSWO
  
  # ---- Calculate Unfished Dynamics ----
  Unfished <- MSEtool:::CalcUnfishedDynamics(OM)
  
  
  # ---- Calculate Reference Points ----
  RefPoints <- MSEtool:::CalcRefPoints(OM)
  
  
  # ---- Optimize for Initial Depletion ----
  # Hist <- MSEtool:::OptimInitDepletion(OM)
  
  # ---- Number-at-Age at Beginning of Initial Time Step ----
  abind::afill(StockParsList$NumberAtAgeArea) <- CalcInitialTimeStep(StockParsList, Unfished) 
  
  dd <- dim(StockParsList$NumberAtAgeArea)
  
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM@Stock[[1]])
  nAges <- 21
  
  ArgList <- list(
    NumberAtAgeArea=Array2List(StockParsList$NumberAtAgeArea, 'Sim'),
    BiomassAtAgeArea=Array2List(StockParsList$BiomassAtAgeArea, 'Sim'),
    WeightAtAge=Array2List(StockParsList$WeightMeanAtAge, 'Sim'),
    Effort=Array2List(FleetParsList$Effort, 'Sim') 
    )
  
  Test <- purrr::pmap(ArgList, CalcPopDynamics_,
                     TimeStep=as.character(TimeSteps[1]),
                     nStock=nStock,
                     nAge=nAges,
                     nFleet=nFleet,
                     nArea=nArea)
  
  
  


  
  # R-Version
  
  CalcPopDynamics <- function(NumberAtAgeArea=StockParsList$NumberAtAgeArea,
                              BiomassAtAgeArea=StockParsList$BiomassAtAgeArea) {
    
  }
  
  
  # ---- Historical Population Dynamics ----
  
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
  
  
  Hist <- MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist, 'Historical'))

  # TODO Make C++ version for increased speed
  # TODO Make C++ versions for Ref Point Calcs
  # profvis::profvis(
  #   postHist <- MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist,'Historical'), silent=T)
  # )
  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  Hist
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

