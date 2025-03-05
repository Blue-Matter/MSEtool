
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
  
  # ---- Calculate Unfished Dynamics ----
  Unfished <- CalcUnfishedDynamics(OM)
  
  
  # ---- Make Lists of Arrays ----
  # expands all arrays to include all sims, and maximum `nage` across stocks, and all Historical time steps
  PopulationList <- MakePopulationList(OM, Unfished=Unfished)
  FleetList <- MakeFleetList(OM) # everything with a fleet dimension
  
  # ---- Calculate Reference Points ----
  # RefPoints <- CalcRefPoints(OM)
  
  # ---- Optimize for Initial Depletion ----
  # Hist <- MSEtool:::OptimInitDepletion(OM)
  
  # ---- Number-at-Age at Beginning of Initial Time Step ----
  abind::afill(PopulationList$NumberAtAgeArea) <- CalcInitialTimeStep(PopulationList, Unfished) 
 
  sim <- 1
  
  PopulationListsim <- MakeSimList(PopulationList, sim)
  FleetListsim <- MakeSimList(FleetList, sim)
  TimeSteps <- TimeSteps(OM, 'Historical')
  
  Test <- CalcPopDynamics_(PopulationListsim,
                           FleetListsim,
                           TimeSteps=as.character(TimeSteps))
  
  
  # TODO
  # - check plus group
  # - check recruits
  # - check N-at-age match BAMdata
  # - finish movement
  Test$PopulationList$SP0
  Test$PopulationList$SProduction
  
  Test$PopulationList$NumberAtAgeArea[1,,1,1]
  Test$PopulationList$NumberAtAgeArea[1,,2,1]

  
  
  
  n <- NumberAtAgeArea[1,,1,] |> rowSums()
  n2 <- NumberAtAgeArea[1,,2,] |> rowSums()
  
  plot(n, type='l')
  lines(n2, col='blue')
  
  plot(n, type='l')
  n0 <- rowSums(Unfished@Equilibrium@Number$`Day octopus`[1,,1,])
  sum(n * FecundityAtAge[1,,1])
  
  sum(n0 * FecundityAtAge[1,,1])
  SP0
  
  plot(n0)
  lines(rowSums(SNatAge$`Day octopus`[1,,1,]))
  
  
  sum(Unfished@Equilibrium@SProduction$`Day octopus`[1,,1,])
  
  Test$NumberAtAgeArea[1,,2,]
  
  Test$SProduction[1]
  
  36693921/5207933
  
  CalcRecruitment_(SProduction,
                   R0,
                   SP0,
                   RecDevHist,
                   SRRModel,
                   SRRPars,
                   Sim,
                   TSindex=0);
  
  
  rdat <- bamExtras::rdat_RedSnapper
  
  cbind(Test$NumberAtAgeArea[1,,1,], c(rdat$N.age[1,1], rdat$N.age[1,]))
  
  Test$NumberAtAgeArea[1,,2,]
  c(rdat$N.age[2,1], rdat$N.age[2,])
  
  Test$BiomassAtAge[1,,1,]
  Test$VBiomassAtAge[1,,1,1,]
  Test$DensityArea[1,1,1,]
  Test$EffortArea[1,1,1,] 
  Test$FDeadArea[1,,1,1,]
  Test$RemovalArea[1,,1,1,]
  Test$RetainArea[1,,1,1,]
  
  
 
  
  


  
  ArgList <- list(NumberAtAgeArea=NumberAtAgeArea,
                  BiomassAtAgeArea=BiomassAtAgeArea,
                  WeightAtAge=WeightAtAge,
                  Effort=Effort,
                  SelectivityAtAge=SelectivityAtAge,
                  SelectivityAtAge=SelectivityAtAge,
                  TimeStep=as.character(TimeSteps[1]))
  
  
  

  
  # ---- Historical Population Dynamics ----
  progress = seq_along(TimeSteps)
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

