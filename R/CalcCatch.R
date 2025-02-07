
# returns catch-at-age by fleet
# TODO - add time-step argument
# TODO - add spatial dimension
# TODO - replace CalcFishedSurvival - needs to be actual n-at-age by area
# TODO - functions for numbers and for weight
# TODO - add option for alternative catch equation
CalcCatch <- function(Stock, Fleet, NatAge=NULL, TimeSteps=NULL) {
  FDead <- GetFatAgeArray(Fleet, TimeSteps) # need to add spatial dimension for pop dynamics
  FRetain <- GetFatAgeArray(Fleet, TimeSteps, 'Retain')
  FDeadTotal <- apply(FDead, 1:3, sum)
  
  
  MatAge <- GetNMortalityAtAge(Stock, TimeSteps)
  ZDead <- ArrayAdd(MatAge, FDeadTotal)
  
  
  FleetWeightatAge <- GetFleetWeightAtAge(Stock, Fleet)
  

  
  # use the empirical weight at age if available, otherwise stock

  
  NatAge <- AddDimension(NatAge, 'Fleet')
  ZDead <- AddDimension(ZDead, 'Fleet')
  
  RemovalNumber <- ArrayMultiply(NatAge, (1-exp(-ZDead))) |>
    ArrayMultiply(ArrayDivide(FDead,ZDead))
  RemovalBiomass <- ArrayMultiply(RemovalNumber, FleetWeightatAge)
  
  if (prod(FRetain, FDead)==1) {
    RetainNumber <- RemovalNumber
    RetainBiomass <- RemovalBiomass
  } else {
    RetainNumber <- ArrayMultiply(NatAge, (1-exp(-ZDead))) |>
      ArrayMultiply(ArrayDivide(FRetain,ZDead))
    RetainBiomass <- ArrayMultiply(RetainNumber, FleetWeightatAge)
  }

  list(RemovalNumber=RemovalNumber,
       RemovalBiomass=RemovalBiomass,
       RetainNumber=RetainNumber,
       RetainBiomass=RetainBiomass)
  
}