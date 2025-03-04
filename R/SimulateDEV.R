
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
  
  # ---- Calculate Unfished Dynamics ----
  Unfished <- CalcUnfishedDynamics(OM)
  
  # ---- Calculate Reference Points ----
  # RefPoints <- CalcRefPoints(OM)
  
  # ---- Optimize for Initial Depletion ----
  # Hist <- MSEtool:::OptimInitDepletion(OM)
  
  # ---- Number-at-Age at Beginning of Initial Time Step ----
  abind::afill(StockParsList$NumberAtAgeArea) <- CalcInitialTimeStep(StockParsList, Unfished) 
  


  NumberAtAgeArea <- Array2List(StockParsList$NumberAtAgeArea, 'Sim')[[1]]
  BiomassAtAgeArea <- Array2List(StockParsList$BiomassAtAgeArea, 'Sim')[[1]]
  WeightAtAge <- Array2List(StockParsList$WeightMeanAtAge, 'Sim')[[1]]
  NaturalMortalityAtAge <- Array2List(StockParsList$NaturalMortalityMeanAtAge, 'Sim')[[1]]
  FecundityAtAge <- Array2List(StockParsList$FecundityMeanAtAge, 'Sim')[[1]]
  
  SProduction <- Array2List(StockParsList$SProduction, 'Sim')[[1]]
  
  SRRModel <- StockParsList$SRRModel
  
  R0 <- Array2List(StockParsList$R0, 'Sim')[[1]]
  
  SP0 <- purrr::map(Unfished@Equilibrium@SProduction, \(x)
                    apply(x, c('Sim', 'Time Step'), sum)) |>
    List2Array('Stock') |>
    Array2List(pos=1)
  
  SP0 <- SP0[[1]]
  
  
  SpawnTimeFrac <- List2Array(StockParsList$SpawnTimeFrac, 'Stock')
  Semelparous <- Array2List(StockParsList$MaturitySemelparous, 'Sim')[[1]]

  RecDevHist <- Array2List(StockParsList$RecDevHist, 'Sim')[[1]]
  
  SRRPars <- StockParsList$SRRPars
  
  
  RelativeSize <- Array2List(StockParsList$RelativeSize, 'Sim')[[1]]
  
  
  Effort <- Array2List(FleetParsList$Effort, 'Sim')[[1]]
  EffortArea <- Array2List(FleetParsList$EffortArea, 'Sim')[[1]]
  
  Catchability <- Array2List(FleetParsList$Catchability, 'Sim')[[1]]
  
  
  SelectivityAtAge <- Array2List(FleetParsList$SelectivityMeanAtAge, 'Sim')[[1]]
  RetentionAtAge <- Array2List(FleetParsList$RetentionMeanAtAge, 'Sim')[[1]]
  DiscardMortalityAtAge <- Array2List(FleetParsList$DiscardMortalityMeanAtAge, 'Sim')[[1]]
  
  FDeadArea <- Array2List(FleetParsList$FDeadArea, 'Sim')[[1]]
  FRetainArea <- Array2List(FleetParsList$FRetainArea, 'Sim')[[1]]
  
  RemovalArea <- Array2List(FleetParsList$RemovalArea, 'Sim')[[1]]
  RetainArea <- Array2List(FleetParsList$RetainArea, 'Sim')[[1]]
  
  FDeadAtAge <- Array2List(FleetParsList$FDeadAtAge, 'Sim')[[1]]
  FRetainAtAge <- Array2List(FleetParsList$FRetainAtAge, 'Sim')[[1]]
  
  RemovalNAtAge <- Array2List(FleetParsList$RemovalNAtAge, 'Sim')[[1]]
  RetainNAtAge <- Array2List(FleetParsList$RetainNAtAge, 'Sim')[[1]]
  
  RemovalBAtAge <- Array2List(FleetParsList$RemovalBAtAge, 'Sim')[[1]]
  RetainBAtAge <- Array2List(FleetParsList$RetainBAtAge, 'Sim')[[1]]
  
  VBiomassAtAgeArea <- Array2List(FleetParsList$VBiomass, 'Sim')[[1]]
  FleetWeightAtAge <- Array2List(FleetParsList$FleetWeightAtAge, 'Sim')[[1]]
  DensityArea <- Array2List(FleetParsList$DensityArea, 'Sim')[[1]] 
  
  Sim <- StockParsList$Sim[[1]]
  
  ## 
  NumberAtAge <- apply(NumberAtAgeArea, c('Stock', 'Age', 'Time Step'), sum)
  CalcDims_(NumberAtAgeArea)
  t = CalcDims_(NumberAtAge)
  
  
  t = CalcBiomass_(BiomassAtAgeArea,
               NumberAtAgeArea,
               WeightAtAge,
               TSindex=0)
  
  SATR = CalcDims_(NumberAtAgeArea);
  nStock <- 1
  c(0:nStock, 0:nAge)
  GetIndex_(SATR, SATRindex)
  ###
  
  
  TimeSteps <- TimeSteps(OM,'Historical')
  
  Test <- CalcPopDynamics_(NumberAtAgeArea, 
                           BiomassAtAgeArea,
                           
                           
                           WeightAtAge,
                           NaturalMortalityAtAge,
                           FecundityAtAge,
                           
                           R0,
                           RecDevHist,
                           SRRModel,
                           SRRPars,
                           SProduction,
                           
                           
                           RelativeSize,
                           
                           Effort,
                           EffortArea,
                           Catchability,
                           
                           SelectivityAtAge,
                           RetentionAtAge,
                           DiscardMortalityAtAge,
                           
                           VBiomassAtAgeArea,
                           FleetWeightAtAge,
                           DensityArea, 
                           
                           FDeadArea, 
                           FRetainArea, 
                           RemovalArea, 
                           RetainArea,
                           
                           FDeadAtAge, 
                           FRetainAtAge,
                           RemovalNAtAge,
                           RetainNAtAge,
                           RemovalBAtAge,
                           RetainBAtAge,
                           Sim,
                           
                           TimeStep=as.character(TimeSteps[1]))
  
  Test$NumberAtAgeArea[1,,1,]
  Test$BiomassAtAge[1,,1,]
  Test$VBiomassAtAge[1,,1,1,]
  Test$DensityArea[1,1,1,]
  Test$EffortArea[1,1,1,] 
  Test$FDeadArea[1,,1,1,]
  Test$RemovalArea[1,,1,1,]
  Test$RetainArea[1,,1,1,]
  Test$FDeadAtAge[1,,1,1]
  
  
  ArgList <- list(NumberAtAgeArea=NumberAtAgeArea,
                  BiomassAtAgeArea=BiomassAtAgeArea,
                  WeightAtAge=WeightAtAge,
                  Effort=Effort,
                  SelectivityAtAge=SelectivityAtAge,
                  SelectivityAtAge=SelectivityAtAge,
                  TimeStep=as.character(TimeSteps[1]))
  
  
  

  
  # ---- Historical Population Dynamics ----
  # 
  # for (ts in progress) {
  #   
  #   TimeStep <- TimeSteps[ts]
  #   
  #   # ---- Do MICE stuff during this Time Step (if applicable) -----
  #   # TODO
  #   Hist <- CalcMICE(Hist, TimeStep=TimeStep)
  #   
  #   # ---- Update Biomass At Age etc ----
  #   # done after MICE to account for changes
  #   Hist <- UpdateBioArrays(Hist, TimeStep)
  #   
  #   # for MPs - Calculate Effort, Selectivity, etc
  #   # update fishery data 
  #   # these two steps should be done first
  #   
  #   # ---- Distribute Effort across Areas ----
  #   Hist <- DistributeEffort(Hist, TimeStep)
  #   
  #   # ---- Calculate Catch and Fishing Mortality ----
  #   Hist <- CalcCatch(Hist, TimeStep)
  #   
  #   # ---- Calculate Recruitment  Time Step ----
  #   Hist <- CalcRecruitment(Hist, TimeStep=TimeStep)
  #   
  #   # ---- Number, Biomass at beginning of Next Time Step and Move ----
  #   Hist <- CalcNumberNext(Hist, TimeStep)
  #   
  #   # print(sum(Hist@Number[[1]][1,,ts+1,]))
  #   
  # }
  # 
  # 
  # Hist <- MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist, 'Historical'))

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

