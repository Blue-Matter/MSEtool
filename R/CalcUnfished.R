CalcEquilibriumUnfished <- function(OM) {
  OM <- PopulateOM(OM)
  
  # NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
  # but does NOT account for expected recruitment from the stock-recruit relationship.
  # i.e., an expected change in recruitment if Fecundity-at-Age changes over time 
  # e.g., change in Weight-at-Age etc
  
  UnfishedSurvival_List <- CalcUnfishedSurvival(OM) 
  UnfishedSurvivalSP_List <- CalcUnfishedSurvival(OM, TRUE)
  
  EquilibriumUnfished <- new('popdynamics')
  
  R0List <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0 |> 
      AddDimension('Age') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
  })
  
  # if (OM@TSperYear==1) {
    UnfishedNumberAtAge <- purrr::map2(UnfishedSurvival_List, R0List, ArrayMultiply)
    UnfishedSpawnNumberAtAge <- purrr::map2(UnfishedSurvivalSP_List, R0List, ArrayMultiply)
  # } else {
  #   
  #   # ---------------------- DEBUG ----------------------
  #   # seasonal model 
  #   
  #   UnfishedSurvival <- UnfishedSurvival_List$Female
  #   R0 <- R0List$Female
  #   CalcSeasonalUnfishedNumber <- function(UnfishedSurvival, R0) {
  #     
  #     d1 <- dim(UnfishedSurvival)
  #     d2 <- dim(R0)
  #     
  #     nSim <- max(d1[1], d2[1])
  #     R0 <- ExpandSims(R0, nSim)
  #     
  #     
  #     UnfishedNumberAtAge <- UnfishedSurvival
  #     UnfishedNumberAtAge[] <- 0 
  #     
  #     UnfishedNumberAtAge[,3,] <- R0[,1,]
  #     
  #     for (i in 3:nrow(UnfishedNumberAtAge))
  #     
  #     UnfishedNumberAtAge[1,,1:4]
  #     
  #     Classes <- OM@Stock$Female@Ages@Classes
  #     nClasses <- length(Classes)
  #     
  #     r0 <- R0[1,1,]
  #     eqAge <- r0[1:4] * UnfishedSurvival[1,,1:4]
  #     
  #     InitialAgeStructure <- array(0, dim=c(nClasses, OM@TSperYear))
  #     for (a in 1:OM@TSperYear) {
  #       InitialAgeStructure[a,a] <- r0[a]
  #     }
  #     
  #     
  #   }
    
  # }
               
  
  # -------------------- END DEBUG --------------------
  
  
  

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


CalcDynamicUnfished <- function(SimList, silent=FALSE) {
  
  if (inherits(SimList, 'om')) 
    SimList <- OM2Hist(SimList, silent) |> Hist2SimList()
  
  if (inherits(SimList, 'hist')) 
    SimList <- Hist2SimList(SimList)
  
  SimListCopy <- purrr::map(SimList, \(x) {
    nStock <- nStock(x@OM)
    for (st in 1:nStock) {
      x@OM@Fleet[[st]]@Catchability[] <- tiny
      x@OM@Fleet[[st]]@qArea[] <- tiny
    }
    x
  })
  
  TimeSteps <- TimeSteps(SimList[[1]]@OM, 'Historical')
  StockNames <- StockNames(SimList[[1]]@OM)

  if (CheckIdenticalSims(SimListCopy)) {
    # identical historical period across all sims
    HistSim <- SimListCopy[[1]]
    unfished <- SimulateDynamics_(HistSim, TimeSteps)

    HistSim@Unfished@Dynamic@Number <- lapply(unfished@Number, 
                                              AddDimNames, 
                                              c("Age", "TimeStep", "Area"), 
                                              TimeSteps)
    
    HistSim@Unfished@Dynamic@Biomass  <- AddDimNames(unfished@Biomass, 
                                                     c('Stock', 'TimeStep'), 
                                                     TimeSteps=TimeSteps, values=list(StockNames))
    
    HistSim@Unfished@Dynamic@SBiomass  <- AddDimNames(unfished@SBiomass, 
                                                      c('Stock', 'TimeStep'), 
                                                      TimeSteps=TimeSteps, values=list(StockNames))
    
    HistSim@Unfished@Dynamic@SProduction  <- AddDimNames(unfished@SProduction, 
                                                         c('Stock', 'TimeStep'), 
                                                         TimeSteps=TimeSteps, values=list(StockNames))
    
    
    SimListOut <- purrr::map(SimListCopy, \(HistSim) {
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
    SimListOut <- purrr::map(SimListCopy, \(HistSim) {
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

  
  SimListOut <- purrr::map2(SimListOut, SimList, \(x,y) {
    nStock <- nStock(x@OM)
    for (st in 1:nStock) {
      x@OM@Fleet[[st]]@Catchability[] <- y@OM@Fleet[[st]]@Catchability[]
    }
    x
  })
  class(SimListOut) <- 'simlist'
  SimListOut
}

