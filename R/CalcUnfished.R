
not <- function(val) !val

DistributeStock <- function(AtAge, UnfishedDist) {
  # adds an area dimension and distributes -at-age arrays
  # according to Spatial@UnfishedDist
  
  if (is.null(UnfishedDist)) {
    # no spatial structure
    # just add spatial dimension
    out <- replicate(1, AtAge)
    l <- dimnames(AtAge)
    l$Area <- 1
    dimnames(out) <- l
    return(out)
  }
    
  # because UnfishedDist has different order in dimensions
  # need to re-order here. Could fix this by restructiing UnfishedDist
  # TODO
  
  UnfishedDist <- aperm(UnfishedDist, c(1,3,4,2)) # sim, age, time step, area
  
  DimUnfishedDist <- dim(UnfishedDist)
  narea <- DimUnfishedDist[4]
  DimAtAge <- dim(AtAge)
  if (length(DimAtAge)==3) {
    l <- dimnames(AtAge)
    AtAge <- replicate(narea, AtAge) 
    l$Area <- 1:narea
    dimnames(AtAge) <- l
  }
  
  DimAtAge <- dim(AtAge)
  if (DimAtAge[4] !=narea) 
    cli::cli_abort('Mismatch in number of areas')
  
  ArrayMultiply(array1=UnfishedDist, array2=AtAge)
}


# TODO Dynamic Unfished

# Calculates Equilibrium  Population Dynamics
# across spatial areas
CalcEquilibriumUnfished <- function(OM,
                                 messages='default',
                                 nSim=NULL,
                                 parallel=FALSE, 
                                 ...) {
  OM <- StartUp(OM, messages, nSim) 

  Unfished <- new('unfished') 
  
  # cli::cli_progress_step(
  #   'Calculating Unfished Dynamics',
  #   msg_done = 'Calculated Unfished Dynamics',
  #   spinner = TRUE)
  
  # ---- Equilibrium ----
  # NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
  # but does NOT account for expected recruitment from the stock-recruit relationship.
  # i.e., an expected change in recruitment if Fecundity-at-Age changes over time 
  # e.g., change in Weight-at-Age etc
  
  UnfishedSurvival <- UnfishedSurvivalSP <- purrr::map(OM@Stock, CalcUnfishedSurvival)
  
  SpawnTimeFrac <- purrr::map(OM@Stock, GetSpawnTimeFrac)
  for (i in seq_along(nStock(OM))) {
    if (SpawnTimeFrac[[i]] !=0)
      UnfishedSurvivalSP[[i]] <- CalcUnfishedSurvival(OM@Stock[[i]], SP=TRUE)
  }
  R0 <- purrr::map(OM@Stock, GetR0) |>
    purrr::map(Structure) |> 
    purrr::map(AddDimNames, TimeSteps=TimeSteps(OM))
 
  NatAge <- purrr::map2(UnfishedSurvival, R0, ArrayMultiply)
  Unfished@Equilibrium@Number <- purrr::map2(NatAge, 
                                             UnfishedDist(OM), 
                                             DistributeStock)
  
  WeightatAge <- purrr::map(OM@Stock, GetWeightAtAge) |> 
    purrr::map(AddAreaDimension)
  
  Unfished@Equilibrium@Biomass <- purrr::map2(WeightatAge, 
                                              Unfished@Equilibrium@Number, 
                                              ArrayMultiply)

  SNatAge <- purrr::map2(R0, UnfishedSurvivalSP, ArrayMultiply) |>
    purrr::map2(purrr::map(OM@Stock, GetMaturityAtAge), ArrayMultiply) |>
    purrr::map2(UnfishedDist(OM), DistributeStock)

  
  Unfished@Equilibrium@SBiomass <- purrr::map2(WeightatAge, SNatAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum))

  FecundityatAge <- purrr::map(OM@Stock, GetFecundityAtAge) |> purrr::map(AddAreaDimension)
  # NOTE: not sure if this will work for all cases 
  ind <- lapply(FecundityatAge, is.null) |> unlist() |> not() |> which() 
  
  SNatAge <- purrr::map2(R0, UnfishedSurvivalSP, ArrayMultiply) |> # already accounts for maturity in FecundityatAge
    purrr::map2(UnfishedDist(OM), DistributeStock)
  
  Unfished@Equilibrium@SProduction <- purrr::map2(SNatAge[ind], FecundityatAge[ind], ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum))
  
  Unfished
}
