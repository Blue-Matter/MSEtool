#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, parallel=FALSE, silent=FALSE, nSim=NULL) {

  if (!is.null(nSim) && (nSim<OM@nSim)) 
    OM@nSim <- nSim
  
  
  OMCheck <- Check(OM)
  
  OM <- Populate(OM, silent=silent)
  
  
  # TODO
  # - MICE models and OM@Rel
  
  TimeSteps <- TimeSteps(OM)
  
  nStocks <- OM@Stock |> length()
  if (methods::is(OM@Stock, 'stock'))
    OM@Stock <- list(OM@Stock)
  if (methods::is(OM@Fleet, 'fleet'))
    OM@Fleet <- list(list(OM@Fleet))
  nFleets <- length(OM@Fleet[[1]])
  
  # ---- Unfished Equilibrium ----
  UnfishedSurvival <- list()
  # spawning population; SO = Spawning Output
  UnfishedSurvival_SO <- list()
  
  for (st in 1:nStocks) {
    M_at_Age <- OM@Stock[[st]]@NaturalMortality@MeanAtAge
    PlusGroup <- OM@Stock[[st]]@Ages@PlusGroup
    SpawnTimeFrac <- OM@Stock[[st]]@SRR@SpawnTimeFrac
    
    UnfishedSurvival[[st]] <- CalcSurvival(M_at_Age, PlusGroup)
    if (any(SpawnTimeFrac!=0) && length(SpawnTimeFrac)>0) 
      UnfishedSurvival_SO[[st]] <- CalcSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
    
  }
  
  
  
  
                           
 

  
  M_at_Age <- getR6(M_at_Age)
  PlusGroup <- getR6(PlusGroup)
  SpawnTimeFrac <- getR6(SpawnTimeFrac)
  
 
  

  
 
  
  
  M_at_Age <- ListSimStock(Stock=OM@Stock, slots=c('NaturalMortality', 'MeanAtAge'))
  PlusGroup <- ListStock(OM@Stock, slots=c('Ages', 'PlusGroup'))
  SpawnTimeFrac <- ListStock(OM@Stock, slots=c('SRR', 'SpawnTimeFrac'))
  
  Unfished_Survival <- CalcUnfishedSurvival(M_at_Age, PlusGroup)
  # spawning population; SO = Spawning Output
  Unfished_Survival_SO <- CalcUnfishedSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
  


  # nested lists nsim by nstock
  # final array is an R6 object; data in `value`
  # number at beginning of time-step
  N_at_Age <- SimStockList(OM)
  # number spawning (same as N_at_Age if  `Spawn_Time_Frac=0`)
  NS_at_Age <- SimStockList(OM)
  
  
  
  
  
  
}


