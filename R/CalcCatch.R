
# returns catch-at-age by fleet
# TODO - add time-step argument
# TODO - add spatial dimension
# TODO - replace CalcFishedSurvival - needs to be actual n-at-age by area
# TODO - functions for numbers and for weight
# TODO - add option for alternative catch equation
CalcCatch <- function(Stock, Fleet, NatAge=NULL) {
  FDead <- GetFatAgeArray(Fleet) # need to add spatial dimension for pop dynamics
  FRetain <- GetFatAgeArray(Fleet, 'Retain')
  FDeadTotal <- apply(FDead, 1:3, sum)
  
  timesteps <- dim(FDeadTotal)[3] # TODO 
  ZDead <- ArrayAdd(Stock@NaturalMortality@MeanAtAge[,,timesteps, drop=FALSE], FDeadTotal)
  
  FleetWeightatAge <- FDead
  FleetWeightatAge[] <- NA
  
  FleetEmpiricalWeightatAge <- GetFleetWeightAtAge(Fleet) 
  # use the empirical weight at age if available, otherwise stock
  for (fl in seq_along(FleetEmpiricalWeightatAge)) {
    if (is.null(FleetEmpiricalWeightatAge[[fl]])) {
      FleetWeightatAge[,,,fl] <- ArrayFill(Array=abind::adrop(FleetWeightatAge[,,,fl, drop=FALSE],4),
                                           FillValue=GetWeightAtAge(Stock))
    } else {
      FleetWeightatAge[,,,fl] <- ArrayFill(Array=abind::adrop(FleetWeightatAge[,,,fl, drop=FALSE],4),
                                           FillValue=FleetEmpiricalWeightatAge[[fl]])
    }
  }
  
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