UpdateEffort <- function(ProjSim, MPAdviceList, MPAdviceList_Previous, Year, YearsHist, YearsProj) {
  
  FleetNames <- FleetNames(ProjSim@OM)
  Complexes <- ProjSim@OM@Complexes
  YearsAll <- c(YearsHist, YearsProj)
  TSIndex <- match(Year, YearsAll)
  LastHistIndex <- match(max(YearsHist), YearsAll)
  
  nArea <- nArea(ProjSim@OM)
  nStock <- nStock(ProjSim@OM)
  StockNames <- StockNames(ProjSim@OM)
  
  for (complex in seq_along(MPAdviceList)) {
    stocks <- Complexes[[complex]]
    MPAdvice <- MPAdviceList[[complex]]
    MPAdvicePrevious <- MPAdviceList_Previous[[complex]]
    
    if (EmptyObject(MPAdvice@Effort)) {
      if (!is.null(MPAdvicePrevious) && !EmptyObject(MPAdvicePrevious@Effort)) {
        MPAdvice@Effort <- MPAdvicePrevious@Effort
      } else {
        next()
      }
    }
    ProjSim <- DistributeEffort(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex)
  }
  ProjSim
}

