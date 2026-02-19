
library(MSEtool)


testOM@nsim <- 2
OM <- Convert(testOM)

Name(OM) <- 'testOM'
Hist <- Simulate(OM)


CurrentCatch <- function(Data) {
  LHind <- match(Data@YearLH,Data@Years)
  HistLandings <- Data@Landings@Value[LHind, ]
  HistDiscards <- Data@Discards@Value[LHind,]
  Advice(TAC=HistLandings+HistDiscards)
}
class(CurrentCatch) <- 'mp'

CurrentCatchArea <- function(Data) {
  LHind <- match(Data@YearLH,Data@Years)
  HistLandings <- Data@Landings@Value[LHind, ]
  HistDiscards <- Data@Discards@Value[LHind,]
  removals <- HistLandings + HistDiscards
  
  nFleet <- nFleet(Data)
  TAC_by_Fleet <- rep(removals/nFleet,nFleet)
  nArea <- nArea(Data)
  
  TAC <- matrix(TAC_by_Fleet/nArea, nFleet, nArea)
  Advice(TAC=TAC)
}
class(CurrentCatchArea) <- 'mp'

MPs <- c('CurrentCatch', 'CurrentCatchArea')

MSE <- Project(Hist, MPs)




la()
LoadArgs(Project_hist)


CloseArea1 <- function(Data) {
  Advice(Closure=c(0,1))
}
class(CloseArea1) <- 'mp'

DiscMort <- function(Data) {
  Advice(DiscardMortality=DiscardMortality(0.5))
}
class(DiscMort) <- 'mp'

MPs <- c('DiscMort', 'CloseArea1')

MSE <- Project(Hist, MPs)




EffortbyFleet <- function(Data) {
  
  OM_Effort <- Misc(Data)$DataOM@Effort 
  # `hist` class object with all historical and projection data - all stocks
  # might have to drop stock out of this for class `mp`
  # TODO - add dimension names to PopDyn arrays
  
  LHYearindex <- GetYearLH(Data)
 
  st <- 1 # hard code stock 
  realEffortFleet <- apply(PopDyn@Effort[st,(LHYearindex-4):LHYearindex, ,drop=FALSE], 3, mean) # historical effort by fleet 

  advice <- Advice()
  advice@Effort <- realEffortFleet
  advice
}
class(EffortbyFleet) <- 'mp'

TACbyFleet <- function(Data) {
  PopDyn <- Misc(Data)$PopDyn 
  LHYearindex <- GetYearLH(Data)
  st <- 1 # hard code stock 
  realCatchFleet <- apply(PopDyn@Landings[st,(LHYearindex-4):LHYearindex, ,drop=FALSE], 3, mean) 
  
  advice <- Advice()
  advice@TAC <- realCatchFleet
  advice
}
class(TACbyFleet) <- 'mp'

MPs <- c('EffortbyFleet', 'TACbyFleet')

MSE <- Project(Hist, MPs)

MSE@Landings[1,1,,1,]
MSE@Effort[1,1,,1,]


#  ---- Dev -----

LoadArgs(Project_hist)
