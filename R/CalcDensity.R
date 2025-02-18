

CalcVBiomassArea <- function(Stock, FleetList, NatAgeArea, TimeSteps=NULL, Rel=TRUE) {
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Stock, 'Historical')
  
  # Calculates the relative density of vulnerable biomass in each area
  fleetweight <- GetFleetWeightAtAge(Stock, FleetList, TimeSteps) |>
    AddDimension('Area')
  
  NatAgeArea <- AddDimension(NatAgeArea, 'Fleet') |> aperm(c(1,2,3,5,4))
  BatAgeArea <- ArrayMultiply(NatAgeArea, fleetweight)
  
  selectivity <- GetSelectivityAtAge(FleetList, TimeSteps)
  retention <- GetRetentionAtAge(FleetList, TimeSteps)
  SelectRetain <- ArrayMultiply(retention, selectivity) |>
    AddDimension('Area')
  VBatAgeArea <- ArrayMultiply(BatAgeArea, SelectRetain)
  
  VBatArea <- apply(VBatAgeArea, c(1,3,4,5), sum)
  if (!Rel) 
    return(VBatArea)
       
  Total <- apply(VBatArea, c('Sim', 'Time Step', 'Fleet'), sum) |> AddDimension('Area')
  
  RelVBatArea <- ArrayDivide(VBatArea,Total)
  RelVBatArea[!is.finite(RelVBatArea)] <- tiny
  RelVBatArea
} 

CalcDensity <- function(Stock, FleetList, NatAgeArea, TimeSteps=NULL, Rel=TRUE) {
  
  VBatArea <- CalcVBiomassArea(Stock, FleetList, NatAgeArea, TimeSteps, Rel=FALSE)
  
  RelativeSize <- GetRelativeSize(Stock) |>
    AddDimension('Time Step') |>
    AddDimension('Fleet') |>
    aperm(c(1,3,4,2))
  
  Density <- ArrayDivide(VBatArea,RelativeSize)
  if (!Rel) 
    return(Density)
  
  Total <- apply(Density, c('Sim', 'Time Step', 'Fleet'), sum) |> AddDimension('Area')
  RelDensity <- ArrayDivide(Density,Total)
  RelDensity[!is.finite(RelDensity)] <- tiny
  RelDensity
}

