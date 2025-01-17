# --- New Classes (temp) ----
setClass("popdynamics",
         slots=c(Number='list',
                 Biomass='list',
                 SBiomass='list',
                 SProduction='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)

setClass("unfished",
         slots=c(Equilibrium='popdynamics',
                 Dynamic='popdynamics',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


AddStockNames <- function(list, Names) {
  names(list) <- Names
  list
}


CalcUnfishedSurvival <- function(Stock, SP=FALSE) {
  M_at_Age <- Stock@NaturalMortality@MeanAtAge
  PlusGroup <- Stock@Ages@PlusGroup
  if (SP) {
    SpawnTimeFrac <- Stock@SRR@SpawnTimeFrac  
  } else {
    SpawnTimeFrac <- NULL
  }
  CalcSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
}

not <- function(val) !val

MakeAtAgeArray <- function(nSim, nAge, nTS, nArea) {
  lens <- list(nSim=nSim, nAge=nAge, nTS=nTS, nArea=nArea) |> 
    lapply(length) |> unlist()
  
  nout <- vector('list', max(lens))
  for (i in seq_along(nout)) {
    nout[[i]] <- array(NA, dim=c(nSim[GetIndex(i, length(lens[1]))],
                                 nAge[GetIndex(i, length(lens[2]))],
                                 nTS[GetIndex(i, length(lens[3]))],
                                 nArea[GetIndex(i, length(lens[4]))]
    )
    ) 
    nout[[i]] <- AddDimNames(nout[[i]], c('Sim', 'Age', 'Time Step', 'Area'))
  }
  names(nout) <- names(nAge)
  nout
}

DistributeStock <- function(AtAge, UnfishedDist) {
  # adds an area dimension and distributes -at-age arrays
  # according to Spatial@UnfishedDist
  
  if (is.null(UnfishedDist)) {
    # no spatial structure
    # just add spatial dimension
    return(
      replicate(1, AtAge) |>
      AddDimNames(names=c('Sim', 'Age', 'Time Step', 'Area'))
    )
  }
    
  # because UnfishedDist has different order in dimensions
  # need to re-order here. Could fix this by restructiing UnfishedDist
  # TODO
  
  UnfishedDist <- aperm(UnfishedDist, c(1,3,4,2)) # sim, age, time step, area
  
  DimUnfishedDist <- dim(UnfishedDist)
  narea <- DimUnfishedDist[4]
  DimAtAge <- dim(AtAge)
  if (length(DimAtAge)==3) {
    AtAge <- replicate(narea, AtAge) |>
      AddDimNames(names=c('Sim', 'Age', 'Time Step', 'Area'))
  }
  
  DimAtAge <- dim(AtAge)
  if (DimAtAge[4] !=narea) 
    cli::cli_abort('Mismatch in number of areas')
  
  MultiplyArrays(UnfishedDist, AtAge)
}



# Calculates Equilibrium and Dynamic Unfished Population Dynamics
CalcUnfishedDynamics <- function(OM,
                                 parallel=FALSE, 
                                 messages='default',
                                 nSim=NULL,
                                 ...) {
  OM <- OM |> 
    nSimUpdate(nSim, messages) |>
    Populate(messages=messages) |>
    ConvertToList()
  

  # TODO                    
  if (!is.null(OM@SexPars@Herm))
    stop('Herm not done yet!')
  
  
  Unfished <- new('unfished') 
  
  # ---- Equilibrium ----
  
  # NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
  # but does NOT account for expected recruitment from the stock-recruit relationship.
  # i.e., an expected change in recruitment if Fecundity-at-Age changes over time 
  
  UnfishedSurvival <- purrr::map(OM@Stock, CalcUnfishedSurvival)
  UnfishedSurvivalSP <- purrr::map(OM@Stock, CalcUnfishedSurvival, SP=TRUE)
  R0 <- purrr::map(OM@Stock, GetR0)
  WeightatAge <- purrr::map(OM@Stock, GetWeightAtAge)
  MaturityatAge <- purrr::map(OM@Stock, GetMaturityAtAge)
  FecundityatAge <- purrr::map(OM@Stock, GetFecundityAtAge)
  # NOTE: not sure if this will work for all cases 
  ind <- lapply(FecundityatAge, is.null) |> unlist() |> not() |> which() 
  
  NatAge <- purrr::map2(R0, UnfishedSurvival, MultiplyArrays, structure=TRUE)
  BatAge <- purrr::map2(NatAge, WeightatAge, MultiplyArrays)
  
  SNatAge <- purrr::map2(R0, UnfishedSurvivalSP, MultiplyArrays, 
                         structure=TRUE) |>
    purrr::map2(MaturityatAge, MultiplyArrays)
  SBatAge <- purrr::map2(SNatAge, WeightatAge, MultiplyArrays)
  
  # TODO 
  # need to be consistent if FecundityAtAge already accounts for maturity-at-age or not!
  SPatAge <- purrr::map2(SNatAge[ind], FecundityatAge[ind], MultiplyArrays)
  
  # Distribute over areas
  Unfished@Equilibrium@Number <- purrr::map2(NatAge, 
                                             UnfishedDist(OM), 
                                             DistributeStock) |>
    AddStockNames(StockNames(OM))
  
  Unfished@Equilibrium@Biomass <- purrr::map2(BatAge, UnfishedDist(OM), DistributeStock) |>
    AddStockNames(StockNames(OM))
  Unfished@Equilibrium@SBiomass <- purrr::map2(SBatAge, UnfishedDist(OM), DistributeStock) |>
    AddStockNames(StockNames(OM))
  Unfished@Equilibrium@SProduction <- purrr::map2(SPatAge[ind], UnfishedDist(OM)[ind], DistributeStock) |>
    AddStockNames(StockNames(OM))
  
  # TODO `SProduction` will be identical to `SBiomass` if spawning biomass 
  # is used for fecundity. Can make it NULL here to save memory

  t = AddStockNames(list= Unfished@Equilibrium@Number, Names=StockNames(OM))
  
  # ---- Dynamic ----
  
  # RecDevInit <- purrr::map(OM@Stock, GetRecDevInit)
  # RecDevHist <- purrr::map(OM@Stock, GetRecDevHist)
  # RecDevProj <- purrr::map(OM@Stock, GetRecDevProj)
  
  # TODO - simulate population with no recruitment deviations
  
  Unfished
}
