
# number and biomass at beginning of time step
CalcInitialTimeStep <- function(Hist) {

  if (is.null(Hist@Unfished))
    Hist@Unfished <- CalcUnfishedDynamics(Hist)
  
  Hist@Number <- purrr::map2(Hist@Stock,
                             Hist@Unfished@Equilibrium@Number, 
                             InitNumber)
                             
  WeightAtAge <- purrr::map(Hist@Stock, GetWeightAtAge, TimeSteps=TimeSteps(Hist)[1]) |>
    purrr::map(AddDimension, 'Area')
  
  Hist@Biomass <- purrr::map2(Hist@Number, WeightAtAge, ArrayMultiply)
  
  Hist
}