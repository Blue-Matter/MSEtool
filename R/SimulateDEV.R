#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, parallel=FALSE, silent=FALSE, nSim=NULL) {

  if (!is.null(nSim) & (nSim<OM@nSim)) 
    OM@nSim <- nSim
  
  
  OMCheck <- Check(OM)
  
  OM <- Populate(OM, silent=silent)
  
  # TODO
  # - MICE models and OM@Rel
  
  # ---- Unfished Equilibrium ----
  
  
  
  
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


