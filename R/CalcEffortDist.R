# Calculate effort distribution over areas
CalcEffortDist <- function(Hist, TimeSteps=NULL) {
  # Distributes Effort over Areas 
  # Proportional to Relative Density of Vulnerable Biomass
  
  # Note: calculated independently by stock to deal with 
  # multi-stock OMs imported from single-stock, non-spatial assessments
  
  # TODO: calculate utility by area accounting for age/length value and
  #       cost by area in Fleet@Distribution
  

 
  EffortDist <- CalcDensity(Hist, TimeSteps=TimeSteps)
                            
  Effort <- purrr::map(Hist@Fleet, GetEffort, TimeSteps=TimeSteps)
  Effort <- purrr::map(Effort, AddDimension, 'Area')
  purrr::map2(Effort, EffortDist, ArrayMultiply)
}