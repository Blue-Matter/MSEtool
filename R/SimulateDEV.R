
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
  
  Hist <- MSEtool:::Hist(OM)

  # ---- Calculate Unfished Dynamics ----
  Hist@Unfished <- MSEtool:::CalcUnfishedDynamics(OM)

  # ---- Calculate Reference Points ----
  #Hist@RefPoints <- MSEtool:::CalcRefPoints(Hist)

  # ---- Optimize for Initial Depletion ----
  Hist <- MSEtool:::OptimInitDepletion(Hist)
  
  # ---- Number and Biomass in Initial Time Step ----
  Hist <- MSEtool::: CalcInitialTimeStep(Hist)
  
  # ---- Make All Array Lists ----
  
  NaturalMortalityAtAge <- purrr::map(OM@Stock,                           
                                      \(x) Array2List(x@NaturalMortality@MeanAtAge, 'Sim')
  ) |> purrr::transpose()
  
  
    purrr::transpose()
  
  LengthAtAge <- purrr::map(GetLengthAtAge(OM), 
                                      Array2List, 'Sim') |>
    purrr::transpose()
  
  
  
  
  
  SimList <- MakeNamedList(1:nSim(OM))
  StockList <- MakeNamedList(StockNames(OM))
  
  
  
  
  NumberAtAge <- 
  BiomassAtAge <- array()
  
  
  array <- array(1:5, c(20,3,5), dimnames=list('Sim'=1:20,
                                               'n'=1:3,
                                               'p'=1:5))
  List <- list(one=array,
               two=array)

  tt <- lapply(List, SubsetSim, 3, drop=TRUE)
  tt$one |> dim()
  
  
  # ---- Optimize Catchability for Terminal Depletion ----
  # TODO:  Rarely used. Slooooow now, C++ later.
  # Hist <- MSEtool::: OptimCatchability(Hist)
  
  nsims <- nSim(Hist)
  Hist@Fleet[[1]][[1]]@Effort@Catchability <- array(15,
                                                    dim=c(nsims,1),
                                                    dimnames = list(Sim=1:nsims,
                                                                    `Time Step`=TimeSteps(Hist, 'Historical')[1]))
  
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

