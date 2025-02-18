CalcSpawnBiomass <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist) 
  sbiomass <- GetSpawnBiomassAtAge(Hist, TimeSteps=TimeSteps)
  sproduction <- GetSProductionAtAge(Hist, TimeSteps=TimeSteps)
  
  
  for (i in 1:nStock(Hist)) {
    ArrayFill(Hist@SBiomass[[i]]) <-  sbiomass[[i]]
    ArrayFill(Hist@SProduction[[i]]) <-  sproduction[[i]]
  }
  Hist
}