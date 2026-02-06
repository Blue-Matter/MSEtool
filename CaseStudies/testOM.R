
library(MSEtool)

testOM@nsim <- 10
OM <- Convert(testOM)
OM@Control$DataOM <- list(Effort=TRUE)
Hist <- Simulate(OM)


Data <- Hist@Data$`1`$Albacore

# TODO - up to here - finish developing Test MPs



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
