CalcEquilibriumUnfished <- function(OM) {
  OM <- PopulateOM(OM)
  
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
  
  # apply SPFrom
  stockNames <- StockNames(OM)
  for (st in seq_along(stockNames)) {
    SPFrom <- OM@Stock[[st]]@SRR@SPFrom
    if (!is.null(SPFrom)) {
      ind <- match(SPFrom, stockNames)
      EquilibriumUnfished@SProduction[,st,] <- EquilibriumUnfished@SProduction[,ind,]
    }
  }
  
  
  EquilibriumUnfished
}


CalcDynamicUnfished <- function(HistSimList, silent=FALSE) {
  
  if (inherits(HistSimList, 'om')) 
    HistSimList <- OM2Hist(HistSimList, silent) |> Hist2HistSimList()
  
  if (inherits(HistSimList, 'hist')) 
    HistSimList <- Hist2HistSimList(HistSimList)
  
  HistSimListCopy <- purrr::map(HistSimList, \(x) {
    nStock <- nStock(x@OM)
    for (st in 1:nStock) {
      x@OM@Fleet[[st]]@Effort@Catchability[] <- tiny
    }
    x
  })
  
  TimeSteps <- TimeSteps(HistSimList[[1]]@OM, 'Historical')
  StockNames <- StockNames(HistSimList[[1]]@OM)

  if (CheckIdenticalSims(HistSimListCopy)) {
    # identical historical period across all sims
    HistSim <- HistSimListCopy[[1]]
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
    
    
    HistSimListOut <- purrr::map(HistSimListCopy, \(HistSim) {
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
      
    }) 
  } else {
    HistSimListOut <- purrr::map(HistSimListCopy, \(HistSim) {
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
  }

  
  HistSimListOut <- purrr::map2(HistSimListOut, HistSimList, \(x,y) {
    nStock <- nStock(x@OM)
    for (st in 1:nStock) {
      x@OM@Fleet[[st]]@Effort@Catchability[] <- y@OM@Fleet[[st]]@Effort@Catchability[]
    }
    x
  })
  HistSimListOut
}

