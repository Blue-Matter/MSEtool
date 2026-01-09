# TODO - use IsIdenticalSim and IdenticalYears to speed up if identical over sims and/or years

#' Calculate Survival 
#' 
#' @param NaturalMortality Natural Mortality by Age. A 3D array with dimensions `Sim`, `Age`, and `Year`, 
#' or a 2D array with dimensions `Age`, and `Year`
#' @param FishingMortality Optional. Fishing Mortality by Age. A 3D array with dimensions `Sim`, `Age`, and `Year`, 
#' or a 2D array with dimensions `Age`, and `Year`
#' @param PlusGroup Logical. Use a Plus Group? Default `TRUE`
#' @param SpawnTimeFrac Numeric length 1 or length `nSim`. 
#' Spawn timing within a given timestep. Default is 0 (beginning of time step) 
#' @param Semelparous Semelparous mortality. Either logical (`FALSE`; default) to ignore, or 
#' a 3D array with named dimensions `Sim`, `Age`, and `Year`, 
#' or a 2D array with named dimensions `Age`, and `Year`
#' 
#' @return An array with the same dimensions as `NaturalMortality` with the 
#' equilibrium survival from recruitment (first age class) to each age  
#' 
#' @export
CalcSurvival <- function(NaturalMortality, 
                         FishingMortality = NULL, 
                         PlusGroup = TRUE, 
                         SpawnTimeFrac = 0, 
                         Semelparous = FALSE) {

  d <- dim(NaturalMortality)
  if (!is.array(NaturalMortality) | length(d)!=2 & length(d)!=3) {
    cli::cli_abort("`NaturalMortality` must be either a 2D or 3D array")
  }
  
  Years <- dimnames(NaturalMortality)[['Year']] |> as.numeric()
  AgeClasses <- dimnames(NaturalMortality)[['Age']] |> as.numeric() 
  
  bySim <- TRUE
  if (length(d)==2) {
    bySim <- FALSE  
   # Temporary add sim dimension to NaturalMortality and others as needed
    NaturalMortality <- AddDimension(NaturalMortality, 'Sim') |> aperm(c('Sim', 'Age', 'Year'))
    FishingMortality <- AddDimension(FishingMortality, 'Sim') |> aperm(c('Sim', 'Age', 'Year'))
  }
  
  d <- dim(NaturalMortality)
  nSim <- d[1]
  nAge <- d[2]
  nYear <- d[3]
  
  # Create array if needed
  Semelparous <- ProcessSemelparuous(Semelparous, nSim, AgeClasses, Years) 
  
  if (!bySim) {
    Semelparous <- AddDimension(Semelparous, 'Sim') |> aperm(c('Sim', 'Age', 'Year'))
  }
  
  # Create output array
  Survival <- array(0, 
                    dim=dim(NaturalMortality),
                    dimnames = dimnames(NaturalMortality))
  
  # Create vector if needed
  if (length(SpawnTimeFrac) != nSim) {
    SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[1:nSim]
  }
  
  # Check FishingMortality
  if (!is.null(FishingMortality)) {
    ArrayList <- ArrayExtend(NaturalMortality, FishingMortality)
    NaturalMortality <- ArrayList[[1]]
    NaturalMortality <- ArrayList[[2]]
  }

  # Sum if FishingMortality exists, otherwise NaturalMortality
  Z <- ArrayAdd(NaturalMortality, FishingMortality)

  # Age index 1
  Survival[, 1, ] <- exp(-Z[, 1, ] * SpawnTimeFrac)
  
  # 2+ 
  for (a in 2:nAge) {
    Survival[, a, ] <- Survival[, a - 1, ] *
      exp(-(Z[, a - 1, ] * (1 - SpawnTimeFrac) + Z[, a, ] * SpawnTimeFrac)) *
      (1 - Semelparous[, a - 1, ])
  }
  
  if (PlusGroup) {
    Survival[, nAge, ] <- survival[, nAge, ] / (1 - exp(-Z[, nAge, ]))
  }
  if (!bySim) {
    Survival <- DropDimension(Survival, 'Sim')
  }
  Survival
}

ProcessSemelparuous <- function(Semelparous, nSim=NULL, AgeClasses=NULL, Years=NULL) {
  if (inherits(Semelparous, "logical")) {
    Semelparous <- array(0, dim=c(nSim, length(AgeClasses), length(Years)),
                         dimnames=list(Sim=1:nSim,
                                       Age=AgeClasses,
                                       Year=Years)
    )  
  }
  Semelparous |> Extend(nSim, AgeClasses, Years)
  
}

