CalcEquilibriumUnfished <- function(OM) {
  OM <- Populate(OM)
  
  # NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
  # but does NOT account for expected recruitment from the stock-recruit relationship.
  # i.e., an expected change in recruitment if Fecundity-at-Age changes over time 
  # e.g., change in Weight-at-Age etc
  
  UnfishedSurvival <- CalcUnfishedSurvival(OM)
  UnfishedSurvivalSP <- CalcUnfishedSurvival(OM, TRUE)
  
  EquilibriumUnfished <- new('popdynamics')
  
  R0List <- purrr::map(OM@Stock, \(x) {
    x@SRR@R0 |> 
      AddDimension('Age') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
  })
               
  UnfishedNumberAtAge <- purrr::map2(UnfishedSurvival, R0List, ArrayMultiply)
  UnfishedSpawnNumberAtAge <- purrr::map2(UnfishedSurvival, R0List, ArrayMultiply)
  
  WeightAtAge <- purrr::map(OM@Stock, \(x) {
    x@Weight@MeanAtAge 
  })
  
  MaturityAtAge <- purrr::map(OM@Stock, \(x) {
    x@Maturity@MeanAtAge 
  })
  
  FecundityAtAge <- purrr::map(OM@Stock, \(x) {
    x@Fecundity@MeanAtAge 
  })
  
  EquilibriumUnfished@Number <- UnfishedNumberAtAge
  
  EquilibriumUnfished@Biomass <- purrr::map2(UnfishedNumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum)) |> 
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'TimeStep'))
  
  EquilibriumUnfished@SBiomass <- purrr::map2(UnfishedSpawnNumberAtAge, WeightAtAge, ArrayMultiply) |> 
    purrr::map2(MaturityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'TimeStep'))
  
  EquilibriumUnfished@SProduction <- purrr::map2(UnfishedSpawnNumberAtAge, FecundityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'TimeStep'))
  
  EquilibriumUnfished
}


CalcDynamicUnfished <- function(HistSimList, silent=FALSE) {
  
  if (inherits(HistSimList, 'om')) 
    HistSimList <- OM2Hist(HistSimList, silent) |> Hist2HistSimList()
  
  if (inherits(HistSimList, 'hist')) 
    HistSimList <- Hist2HistSimList(HistSimList)
  
  HistSimList <- purrr::map(HistSimList, \(x) {
    nStock <- nStock(x@OM)
    for (st in 1:nStock) {
      x@OM@Fleet[[st]]@Effort@Catchability[] <- tiny
    }
    x
  })
  
  TimeSteps <- TimeSteps(HistSimList[[1]]@OM, 'Historical')
  StockNames <- StockNames(HistSimList[[1]]@OM)
  out <- purrr::map(HistSimList, \(HistSim) {
    
    unfished <- SimulateDynamics_(HistSim, TimeSteps)
    
    HistSim@Unfished@Dynamic@Number <- lapply( unfished@Number, AddDimNames, c("Age", "TimeStep", "Area"), TimeSteps)
    
    HistSim@Unfished@Dynamic@Biomass  <- AddDimNames(unfished@Biomass, 
                                                      c('Stock', 'TimeStep'), 
                                                      TimeSteps=TimeSteps, values=list(StockNames))
    
    HistSim@Unfished@Dynamic@SBiomass  <- AddDimNames(unfished@SBiomass, 
                                                      c('Stock', 'TimeStep'), 
                                                      TimeSteps=TimeSteps, values=list(StockNames))
    
    HistSim@Unfished@Dynamic@SProduction  <- AddDimNames(unfished@SProduction, 
                                                       c('Stock', 'TimeStep'), 
                                                       TimeSteps=TimeSteps, values=list(StockNames))
    HistSim
    
  }, .progress = 'Calculating Dynamic Unfished')
  out
}

# CalcUnfished <- function(OM, Hist=NULL, silent=FALSE) {
#   OM <- Populate(OM)
#   Unfished <- new('unfished')
#   Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
#   if (is.null(Hist))
#     Hist <- Hist(OM, silent)
#   Unfished@Dynamic <- CalcDynamicUnfished(Hist)
#   Unfished
#  
# }

# 
# nsim <- length(OMList) 
# stocks <- names(OMList[[1]]$Ages)
# nstock <- length(stocks)
# TimeSteps <- OMList[[1]]$TimeSteps
# nTS <- length(TimeSteps)
# 
# Unfished@Dynamic@Number <- Unfished@Dynamic@Biomass <- 
#   Unfished@Dynamic@SBiomass <-
#   Unfished@Dynamic@SProduction <- MakeNamedList(stocks)
# 
# for (s in 1:nstock) {
#   Unfished@Dynamic@Number[[s]] <- array(NA, dim=c(nsim, nTS))
#   Unfished@Dynamic@Biomass[[s]] <- array(NA, dim=c(nsim, nTS))
#   Unfished@Dynamic@SBiomass[[s]] <- array(NA, dim=c(nsim, nTS)) 
#   Unfished@Dynamic@SProduction[[s]] <- array(NA, dim=c(nsim, nTS))  
#   
#   for (x in 1:nsim) {
#     Unfished@Dynamic@Number[[s]][x,] <- apply(OMListUnfished[[x]]$NumberAtAgeArea[[s]], 2, sum)
#     Unfished@Dynamic@Biomass[[s]][x,] <- OMListUnfished[[x]]$Biomass[s,]
#     Unfished@Dynamic@SBiomass[[s]][x,] <- OMListUnfished[[x]]$SBiomass[s,]
#     Unfished@Dynamic@SProduction[[s]][x,] <- OMListUnfished[[x]]$SProduction[s,]
#   }
# }
# Unfished

# 
# 
# DistributeStock <- function(AtAge, UnfishedDist) {
#   # adds an area dimension and distributes -at-age arrays
#   # according to Spatial@UnfishedDist
#   
#   if (is.null(UnfishedDist)) {
#     # no spatial structure
#     # just add spatial dimension
#     out <- replicate(1, AtAge)
#     l <- dimnames(AtAge)
#     l$Area <- 1
#     dimnames(out) <- l
#     return(out)
#   }
#     
#   # because UnfishedDist has different order in dimensions
#   # need to re-order here. Could fix this by restructiing UnfishedDist
#   # TODO
#   
#   UnfishedDist <- aperm(UnfishedDist, c(1,3,4,2)) # sim, age, time step, area
#   
#   DimUnfishedDist <- dim(UnfishedDist)
#   narea <- DimUnfishedDist[4]
#   DimAtAge <- dim(AtAge)
#   if (length(DimAtAge)==3) {
#     l <- dimnames(AtAge)
#     AtAge <- replicate(narea, AtAge) 
#     l$Area <- 1:narea
#     dimnames(AtAge) <- l
#   }
#   
#   DimAtAge <- dim(AtAge)
#   if (DimAtAge[4] !=narea) 
#     cli::cli_abort('Mismatch in number of areas')
#   
#   ArrayMultiply(array1=UnfishedDist, array2=AtAge)
# }


# TODO Dynamic Unfished


# CalcEquilibriumUnfished <- function(OM,
#                                  messages='default',
#                                  nSim=NULL,
#                                  parallel=FALSE, 
#                                  ...) {
#   OM <- StartUp(OM, messages, nSim) 
# 
#   Unfished <- new('unfished') 
#   
#   # cli::cli_progress_step(
#   #   'Calculating Unfished Dynamics',
#   #   msg_done = 'Calculated Unfished Dynamics',
#   #   spinner = TRUE)
#   
#   
#   
#  
#   WeightatAge <- purrr::map(OM@Stock, GetWeightAtAge) |> 
#     purrr::map(AddAreaDimension)
#   
#   Unfished@Equilibrium@Biomass <- purrr::map2(WeightatAge, 
#                                               Unfished@Equilibrium@Number, 
#                                               ArrayMultiply)
# 
#   SNatAge <- purrr::map2(R0, UnfishedSurvivalSP, ArrayMultiply) |>
#     purrr::map2(purrr::map(OM@Stock, GetMaturityAtAge), ArrayMultiply) |>
#     purrr::map2(UnfishedDist(OM), DistributeStock)
# 
#   
#   Unfished@Equilibrium@SBiomass <- purrr::map2(WeightatAge, SNatAge, ArrayMultiply) |>
#     purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum))
# 
#   FecundityatAge <- purrr::map(OM@Stock, GetFecundityAtAge) |> purrr::map(AddAreaDimension)
#   # NOTE: not sure if this will work for all cases 
#   ind <- lapply(FecundityatAge, is.null) |> unlist() |> not() |> which() 
#   
#   SNatAge <- purrr::map2(R0, UnfishedSurvivalSP, ArrayMultiply) |> # already accounts for maturity in FecundityatAge
#     purrr::map2(UnfishedDist(OM), DistributeStock)
#   
#   Unfished@Equilibrium@SProduction <- purrr::map2(SNatAge[ind], FecundityatAge[ind], ArrayMultiply) |>
#     purrr::map(\(x) apply(x, c('Sim', 'TimeStep'), sum))
#   
#   Unfished
# }



