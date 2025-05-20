CalcEquilibriumUnfished <- function(OMListSim) {
  
  # NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
  # but does NOT account for expected recruitment from the stock-recruit relationship.
  # i.e., an expected change in recruitment if Fecundity-at-Age changes over time 
  # e.g., change in Weight-at-Age etc
  
  NaturalMortalityAtAge <- OMListSim$NaturalMortalityMeanAtAge
  PlusGroup <- lapply(OMListSim$Ages, slot, 'PlusGroup')
  SpawnTimeFrac <- as.list(OMListSim$SpawnTimeFrac)
  Semelparous <- OMListSim$MaturitySemelparous
  
  UnfishedSurvival <- purrr::pmap(list(NaturalMortalityAtAge,
                                       PlusGroup,
                                       SpawnTimeFrac,
                                       Semelparous),
                                  CalcUnfishedSurvival)
  
  UnfishedSurvivalSP <- purrr::pmap(list(NaturalMortalityAtAge,
                                         PlusGroup,
                                         SpawnTimeFrac,
                                         Semelparous,
                                         SP=TRUE),
                                    CalcUnfishedSurvival)
  

  # Include spatial distribution
  # NumberAtAgeArea <- purrr::pmap(list(UnfishedSurvival, 
  #                                 Array2List(OMListSim$R0, 1),
  #                                 OMListSim$UnfishedDist),
  #                            \(x,y,z) {
  #                              NatAge <- x * matrix(y, nrow=nrow(x), ncol=length(y))   
  #                              out <- replicate(dim(z)[3], NatAge) *  z
  #                              dimnames(out) <- dimnames(z)
  #                              out
  #                            }
  #                            
  # ) 
  # 
  # BiomassAtAgeArea = purrr::map2(NumberAtAgeArea, OMListSim$WeightMeanAtAge,
  #                                \(x,y) 
  #                                x * replicate(dim(x)[3],y)
  # )
  
  
  UnfishedNumberAtAge <- purrr::pmap(list(UnfishedSurvival,
                                          Array2List(OMListSim$R0, 1)),  
                                     \(surv, r0) 
                                     surv * matrix(r0, nrow=nrow(surv), ncol=length(r0))
  )
  
  UnfishedSpawnNumberAtAge <- purrr::pmap(list(UnfishedSurvivalSP,
                                               Array2List(OMListSim$R0, 1),
                                               OMListSim$MaturityMeanAtAge),  
                                          \(surv, r0, maturity) 
                                          surv * matrix(r0, nrow=nrow(surv), ncol=length(r0)) * maturity
  )
  
  
  UnfishedBiomass <- purrr::map2(UnfishedNumberAtAge, OMListSim$WeightMeanAtAge,
                                 \(number,weight)
                                 apply(number * weight, 'TimeStep', sum)
  ) |> List2Array('Stock', 'TimeStep') |> aperm(c('Stock', 'TimeStep'))
  dimnames(UnfishedBiomass)$TimeStep <- OMListSim$TimeSteps
  
  UnfishedSBiomass <- purrr::map2(UnfishedSpawnNumberAtAge, OMListSim$WeightMeanAtAge,
                                  \(number,weight)
                                  apply(number * weight, 'TimeStep', sum)
  ) |> List2Array('Stock', 'TimeStep') |> aperm(c('Stock', 'TimeStep'))
  dimnames(UnfishedSBiomass)$TimeStep <- OMListSim$TimeSteps
  
  # already accounts for maturity in FecundityatAge
  UnfishedSpawnNumberAtAge2 <- purrr::pmap(list(UnfishedSurvivalSP,
                                                Array2List(OMListSim$R0, 1)),  
                                           \(surv, r0) 
                                           surv * matrix(r0, nrow=nrow(surv), ncol=length(r0)) 
  ) 
  
  UnfishedSProduction <- purrr::map2(UnfishedSpawnNumberAtAge2, OMListSim$FecundityMeanAtAge,
                                     \(number,weight)
                                     apply(number * weight, 'TimeStep', sum)
  ) |> List2Array('Stock', 'TimeStep') |> aperm(c('Stock', 'TimeStep'))
  dimnames(UnfishedSProduction)$TimeStep <- OMListSim$TimeSteps
  
  
  OMListSim$N0atAge <- UnfishedNumberAtAge
  OMListSim$B0 <- UnfishedBiomass
  OMListSim$SB0 <- UnfishedSBiomass
  OMListSim$SP0 <- UnfishedSProduction
  OMListSim
}

CalcDynamicUnfished <- function(OMListSim) {
  OMListSim$Catchability[] <- tiny
  out <- SimulateFisheryDynamics_(OMListSim, TimeSteps=OMListSim$TimeSteps, MP=NULL, CalcCatch=0)  
  AddDimNamesOMListSim(out)
  
}

CalcUnfished <- function(OMList) {
  if (inherits(OMList, 'om'))
    OMList <- MakeOMList(OMList)
  
  if (is.null(OMList[[1]]$N0atAge)) {
    if (CheckSimsUnique(OMList, ignore=c('Sim', 'RecDevs'))) {
      # identical across Sims
      temp <- CalcEquilibriumUnfished(OMList[[1]])
      for (i in seq_along(OMList)) {
        OMList[[i]]$N0atAge <- temp$N0atAge
        OMList[[i]]$B0 <- temp$B0
        OMList[[i]]$SB0 <- temp$SB0
        OMList[[i]]$SP0 <- temp$SP0
      }
    } else {
      OMList <- purrr::map(OMList, CalcEquilibriumUnfished)  
    }
  }
    
  Unfished <- new('unfished')
  
  # Equilibrium 
  Unfished@Equilibrium@Number <- purrr::map(OMList, \(x) {
    array <- lapply(x$N0atAge, apply, 2, sum) |>
      List2Array('Stock', 'TimeStep') |> aperm(c('Stock', 'TimeStep'))
    dimnames(array)$TimeStep <- x$TimeSteps
    array
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  Unfished@Equilibrium@Biomass <- purrr::map(OMList, \(x) {
    x$B0
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  Unfished@Equilibrium@SBiomass <- purrr::map(OMList, \(x) {
    x$SB0
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  Unfished@Equilibrium@SProduction <- purrr::map(OMList, \(x) {
    x$SP0
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  
  # Dynamic - accounts for rec devs AND SRR 
  DynamicUnfished <- purrr::map(OMList, CalcDynamicUnfished,
                                .progress = list(
                                  type = "iterator", 
                                  format = "Calculating Dynamic Unfished {cli::pb_bar} {cli::pb_percent}",
                                  clear = TRUE))
 
  Unfished@Dynamic@Number <- purrr::map(DynamicUnfished, \(x) {
    array <- lapply(x$NumberAtAgeArea, apply, 2, sum) |>
      List2Array('Stock', 'TimeStep') |> aperm(c('Stock', 'TimeStep'))
    dimnames(array)$TimeStep <- x$TimeSteps
    array
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  Unfished@Dynamic@Biomass <- purrr::map(DynamicUnfished, \(x) {
    x$Biomass
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  
  Unfished@Dynamic@SBiomass <- purrr::map(DynamicUnfished, \(x) {
    x$SBiomass
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  Unfished@Dynamic@SProduction <- purrr::map(DynamicUnfished, \(x) {
    x$SProduction
  }) |> List2Array('Sim') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  
  Unfished
}

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



