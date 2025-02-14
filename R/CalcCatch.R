
CalcCatchN <- function(FDeadAtAge, FRetainAtAge, NatAge, NMortAtAge) {
  # calculate removals and retained number at age
  
  # NatAge is number at begining of time-step before mortality in this time step
  # FDeatAtAge is retained + dead discards
  
  # Add Area Dimension if Missing
  FDeadAtAge <- AddDimension(FDeadAtAge, 'Area')
  FRetainAtAge <- AddDimension(FRetainAtAge, 'Area')
  NatAge <- AddDimension(NatAge, 'Area')
  NMortAtAge <- AddDimension(NMortAtAge, 'Area')
  
  FDeadTotal <- apply(FDeadAtAge, c('Sim', 'Age', 'Time Step', 'Area'), sum)
  ZDeadTotal <- ArrayAdd(NMortAtAge, FDeadTotal)
  
  NDead <- ArrayMultiply(NatAge, (1-exp(-ZDeadTotal)))

   
  ZDeadTotal <- AddDimension(ZDeadTotal, 'Fleet') |> aperm(c(1,2,3,5,4))
  FishingDead <- ArrayDivide(FDeadAtAge, ZDeadTotal)
  FishingRetain <- ArrayDivide(FRetainAtAge, ZDeadTotal)

  NDead <- AddDimension(NDead, 'Fleet') |> aperm(c(1,2,3,5,4))
  
  # drops the Area dimension if it isn't used
  Removal <- ArrayMultiply(FishingDead, NDead) |> DropDimension()
  Retain <- ArrayMultiply(FishingRetain, NDead)|> DropDimension()
    
  list(Removal= Removal,
       Retain= Retain
  )
}

# returns catch-at-age by fleet
# TODO - add spatial dimension
# TODO - functions for numbers and for weight
# TODO - add option for alternative catch equation
# CalcCatch <- function(Stock, Fleet, NatAge=NULL, TimeSteps=NULL) {
#   
#   FDead <- GetFatAgeArray(Fleet, TimeSteps) # need to add spatial dimension for pop dynamics
#   FRetain <- GetFatAgeArray(Fleet, TimeSteps, 'Retain')
#   FDeadTotal <- apply(FDead, 1:3, sum)
#   
#   MatAge <- GetNMortalityAtAge(Stock, TimeSteps)
#   ZDead <- ArrayAdd(MatAge, FDeadTotal)
#   
#   FleetWeightatAge <- GetFleetWeightAtAge(Stock, Fleet, TimeSteps)
# 
#   NatAge <- AddDimension(NatAge, 'Fleet')
#   dimnames(NatAge)$Fleet <- names(Fleet)
#   
#   ZDead <- AddDimension(ZDead, 'Fleet')
#   dimnames(ZDead)$Fleet <- names(Fleet)
#   
#   RemovalNumber <- ArrayMultiply(NatAge, (1-exp(-ZDead))) |>
#     ArrayMultiply(ArrayDivide(FDead,ZDead))
#   RemovalBiomass <- ArrayMultiply(RemovalNumber, FleetWeightatAge)
#   
#   if (prod(FRetain/FDead)==1) {
#     RetainNumber <- RemovalNumber
#     RetainBiomass <- RemovalBiomass
#   } else {
#     RetainNumber <- ArrayMultiply(NatAge, (1-exp(-ZDead))) |>
#       ArrayMultiply(ArrayDivide(FRetain,ZDead))
#     RetainBiomass <- ArrayMultiply(RetainNumber, FleetWeightatAge)
#   }
# 
#   list(RemovalNumber=RemovalNumber,
#        RemovalBiomass=RemovalBiomass,
#        RetainNumber=RetainNumber,
#        RetainBiomass=RetainBiomass)
#   
# }


# CalcCatch <- function(Stock, Fleet, NatAge=NULL, TimeSteps=NULL) {
#   
#   FDead <- GetFatAgeArray(Fleet, TimeSteps) # need to add spatial dimension for pop dynamics
#   FRetain <- GetFatAgeArray(Fleet, TimeSteps, 'Retain')
#   FDeadTotal <- apply(FDead, 1:3, sum)
#   
#   MatAge <- GetNMortalityAtAge(Stock, TimeSteps)
#   ZDead <- ArrayAdd(MatAge, FDeadTotal)
#   
#   FleetWeightatAge <- GetFleetWeightAtAge(Stock, Fleet, TimeSteps)
#   
#   NatAge <- AddDimension(NatAge, 'Fleet')
#   dimnames(NatAge)$Fleet <- names(Fleet)
#   
#   ZDead <- AddDimension(ZDead, 'Fleet')
#   dimnames(ZDead)$Fleet <- names(Fleet)
#   
#   RemovalNumber <- ArrayMultiply(NatAge, (1-exp(-ZDead))) |>
#     ArrayMultiply(ArrayDivide(FDead,ZDead))
#   RemovalBiomass <- ArrayMultiply(RemovalNumber, FleetWeightatAge)
#   
#   if (prod(FRetain/FDead)==1) {
#     RetainNumber <- RemovalNumber
#     RetainBiomass <- RemovalBiomass
#   } else {
#     RetainNumber <- ArrayMultiply(NatAge, (1-exp(-ZDead))) |>
#       ArrayMultiply(ArrayDivide(FRetain,ZDead))
#     RetainBiomass <- ArrayMultiply(RetainNumber, FleetWeightatAge)
#   }
#   
#   list(RemovalNumber=RemovalNumber,
#        RemovalBiomass=RemovalBiomass,
#        RetainNumber=RetainNumber,
#        RetainBiomass=RetainBiomass)
#   
# }
