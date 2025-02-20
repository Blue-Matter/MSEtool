
# number and biomass at beginning of time step
CalcInitialTimeStep <- function(Hist) {

  if (EmptyObject(Hist@Unfished))
    Hist@Unfished <- CalcUnfishedDynamics(Hist)
  
  for (st in 1:nStock(Hist)) {
    ArrayFill(Hist@Number[[st]]) <- InitNumber(Hist@Stock[[st]], Hist@Unfished@Equilibrium@Number[[st]])
  }
  
  Hist
}

# assumes number at age, area is populated 
UpdateBioArrays <- function(Hist, TimeStep) {
  
  fleetnames <- FleetNames(Hist)
  
  NumberAtAge <- GetNumberAtAge(Hist, TimeSteps=TimeStep) 
  WeightAtAge <- GetWeightAtAge(Hist@Stock, TimeSteps=TimeStep) |>
    purrr::map(AddDimension, 'Area')
  
  MaturityAtAge <- GetMaturityAtAge(Hist@Stock, TimeSteps=TimeStep) |>
    purrr::map(AddDimension, 'Area')
  
  SProductionAtAge <- GetSProductionAtAge(Hist, TimeSteps=TimeStep) |>
    purrr::map(AddDimension, 'Area')
  
  
  FleetWeight <- purrr::map2(Hist@Stock, Hist@Fleet, \(x,y) {
    GetFleetWeightAtAge(x, y, TimeStep) |>
      AddDimension('Area')
  })
  
  selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeStep)
  retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeStep) 
  
  SelectRetain <- purrr::map2(selectivity, retention, \(x,y) {
    ArrayMultiply(x, y) |>  AddDimension('Area')
  })
  
  BatAgeArea <- purrr::map2(NumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x)
               AddDimension(x, 'Fleet', val=fleetnames[1]) |>
                 aperm(c('Sim', 'Age', 'Time Step', 'Fleet', 'Area'))
    )
  
  VBatArea <- purrr::map2(BatAgeArea, SelectRetain,  \(x,y) {
    ArrayMultiply(x, y) 
  }) 
  
  VBTotalatArea <- purrr::map(VBatArea, \(x) {
    apply(x, c('Sim', 'Time Step', 'Fleet', 'Area'), sum)
  }) 
  
  RelativeSize <- purrr::map(Hist@Stock, \(x) {
    GetRelativeSize(x) |>
    AddDimension('Time Step') |>
    AddDimension('Fleet') |>
      aperm(c('Sim', 'Time Step', 'Fleet', 'Area'))
  })


  Density <- purrr::map2(VBTotalatArea,RelativeSize, \(x,y) {
    Density <- ArrayDivide(x,y)
    Total <- apply(Density, c('Sim', 'Time Step', 'Fleet'), sum) |> AddDimension('Area')
    RelDensity <- ArrayDivide(Density,Total)
    RelDensity[!is.finite(RelDensity)] <- tiny
    RelDensity
  })
  
  BatAgeArea <- purrr::map(BatAgeArea, DropDimension, name='Fleet', warn=FALSE) 
 
  for (st in 1:nStock(Hist)) {
   
    ArrayFill(Hist@Biomass[[st]]) <- BatAgeArea[[st]]
    ArrayFill(Hist@SBiomass[[st]]) <- ArrayMultiply(BatAgeArea[[st]],MaturityAtAge[[st]])
    ArrayFill(Hist@SProduction[[st]]) <- ArrayMultiply(NumberAtAge[[st]], SProductionAtAge[[st]])
    
    ArrayFill(Hist@VBiomass[[st]]) <- VBatArea[[st]]
    
    # ArrayFill(Hist@EffortArea[[st]]) <- VBatArea[[st]]
    # ArrayFill(Hist@FDeadArea[[st]]) <- VBatArea[[st]]
    # ArrayFill(Hist@FRetainArea[[st]]) <- VBatArea[[st]]
    
    ArrayFill(Hist@Density[[st]]) <- Density[[st]]
    
  }
  
  Hist
}