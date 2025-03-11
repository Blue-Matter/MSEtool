
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
  
  # ---- Calculate Reference Points ----
  RefPoints <- CalcRefPoints(OM, Unfished)
  
  # ---- Make OM List ----
  OMList <- MakeOMList(OM, Unfished)
  
  # ---- Number-at-Age at Beginning of Initial Time Step ----
  OMList <- CalcInitialTimeStep(OMList, Unfished) 
  
  # Convert to list by simulation
  OMListSim <- ConvertToSimList(OMList) 
  

  AgesList <- OMListSim[[1]]$Ages
  LengthList <- OMListSim[[1]]$Length$MeanAtAge
  WeightList <- OMListSim[[1]]$Weight$MeanAtAge
  NaturalMortalityList <- OMListSim[[1]]$NaturalMortality$MeanAtAge
  MaturityList <- OMListSim[[1]]$Maturity$MeanAtAge
  FecundityList <- OMListSim[[1]]$Fecundity$MeanAtAge
  # SRRList <- OMListSim[[1]]$SRR
  
  RelativeSizeList <- OMListSim$`1`$Spatial$RelativeSize
  DepletionList <- OMListSim[[1]]$Depletion
  
  NumberAtAgeAreaList <- OMListSim[[1]]$NumberAtAgeArea
  BiomassAreaList <- OMListSim[[1]]$BiomassArea
  

  
  
  DiscardMortalityAtAgeList <- OMListSim$`1`$DiscardMortality$MeanAtAge
  EffortList <- OMListSim$`1`$Effort$Effort
  CatchabilityList <- OMListSim$`1`$Effort$Catchability
  
  SelectivityAtAgeList <- OMListSim$`1`$Selectivity$MeanAtAge
  RetentionAtAgeList <- OMListSim$`1`$Retention$MeanAtAge
  ClosureAreaList <- OMListSim$`1`$Distribution$Closure
  
  EffortAreaList <- OMListSim$`1`$EffortArea
  DensityAreaList <- OMListSim$`1`$DensityArea
  VBiomassAreaList <- OMListSim$`1`$VBiomassArea
  
  FDeadAtAgeList <- OMListSim$`1`$FishingMortality$DeadAtAge
  FRetainAtAgeList <- OMListSim$`1`$FishingMortality$RetainAtAge
 
  FDeadAtAgeAreaList <- OMListSim$`1`$FDeadAtAgeArea
  FRetainAtAgeAreaList <- OMListSim$`1`$FRetainAtAgeArea
 
  RemovalAtAgeAreaList <- OMListSim$`1`$RemovalAtAgeArea
  RetainAtAgeAreaList <- OMListSim$`1`$RetainAtAgeArea
  
  RemovalNumberAtAgeList <- OMListSim$`1`$RemovalNumberAtAge
  RetainNumberAtAgeList  <- OMListSim$`1`$RetainNumberAtAge
  RemovalBiomassAtAgeList <- OMListSim$`1`$RemovalBiomassAtAge
  RetainBiomassAtAgeList <- OMListSim$`1`$RetainBiomassAtAge
  
  
  TSindex = 0;
  
  OMListSim$`1`$BiomassArea$Albacore |> dimnames()
  
  OMListSim$`1`$BiomassArea = CalcBiomass_(
    OMListSim$`1`$BiomassArea,
    OMListSim$`1`$NumberAtAgeArea,
    OMListSim$`1`$Weight$MeanAtAge,
    TSindex
  );
  

  OMListSim$`1`$VBiomassArea = CalcVBiomass_(
    OMListSim$`1`$VBiomassArea,
    OMListSim$`1`$NumberAtAgeArea,
    OMListSim$`1`$FleetWeightAtAge,
    OMListSim$`1`$Selectivity$MeanAtAge,
    OMListSim$`1`$Distribution$Closure,
    TSindex
  );
  
  OMListSim$`1`$DensityArea = CalcDensity_(
    OMListSim$`1`$DensityArea,
    OMListSim$`1`$VBiomassArea,
    OMListSim$`1`$Spatial$RelativeSize,
    TSindex
  );
  
  OMListSim$`1`$EffortArea = DistEffort_(
    OMListSim$`1`$EffortArea,
    OMListSim$`1`$DensityArea,
    OMListSim$`1`$Effort$Effort,
    TSindex
  );
  OMListSim$`1`$EffortArea$Albacore[1,,]
  

  
  St <- Sys.time()
  for (i in 1:1) {
    Test <- CalcFfromCatch_(FDeadAtAgeList,
                            FRetainAtAgeList,
                            NumberAtAgeAreaList,
                            RemovalNumberAtAgeList,
                            NaturalMortalityList,
                            SelectivityAtAgeList,
                            RetentionAtAgeList,
                            DiscardMortalityAtAgeList,
                            FDeadAtAgeAreaList,
                            FRetainAtAgeAreaList,
                            TSindex=0)
  }
  Sys.time() - St
  Test$RemovalAtAgeArea$Albacore$`1976`[,1,]
  
  Test$FRetainAtAgeArea$Albacore$`1976`[,1,]
  Test$FDeadAtAgeArea$Albacore$`1976`[,1,]

  DensityAreaList$Albacore[1,,]
  EffortAreaList$Albacore[1,,]
  
  
  # ---- Optimize for Final Depletion ----
  
  OptimCatchability(PopulationListSim, FleetListSim, OM)
  

  # ---- Historical Population Dynamics ----
  tictoc::tic()
  PopDynamicsHistorical <- purrr::map2(PopulationListSim, FleetListSim, \(x,y)
                                       CalcPopDynamics_(x,
                                                        y,
                                                        TimeSteps=TimeSteps(OM, 'Historical'))
  ) 
  tictoc::toc()
  
  PopDynamicsHistorical$`1`$PopulationList$NaturalMortality$MeanAtLength
  N <- PopDynamicsHistorical$`1`$PopulationList$NumberAtAgeArea$Albacore |> apply("Time Step", sum)
  plot(N, type='l') 
  
  B <- PopDynamicsHistorical$`3`$PopulationList$BiomassArea$Albacore |> rowSums()
  plot(B, type='l')

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

