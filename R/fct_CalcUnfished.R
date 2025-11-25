CalcEquilibriumUnfished <- function(OM) {
  OM <- PopulateOM(OM)

  EquilibriumUnfished <- new('popdynamics')
  
  UnfishedNumberAtAge <- CalcUnfishedNumber(OM)
  UnfishedSpawnNumberAtAge <- CalcUnfishedNumber(OM, SP=TRUE)
  
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
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |> 
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year'))
  
  EquilibriumUnfished@SBiomass <- purrr::map2(UnfishedSpawnNumberAtAge, WeightAtAge, ArrayMultiply) |> 
    purrr::map2(MaturityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year'))
  
  EquilibriumUnfished@SProduction <- purrr::map2(UnfishedSpawnNumberAtAge, FecundityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year'))
  
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
      x@OM@Stock[[st]]@SRR@RecDevProj[] <- tiny
      x@OM@Fleet[[st]]@Catchability[] <- tiny
      x@OM@Fleet[[st]]@qArea[] <- tiny
    }
    x
  })
  
  Years <- Years(SimList[[1]]@OM, 'Historical')
  StockNames <- StockNames(SimList[[1]]@OM)

  # TODO - this should only check for historical years
  if (CheckIdenticalSims(SimListCopy)) {
    # identical historical period across all sims
    HistSim <- SimListCopy[[1]]
    unfished <- SimulateDynamics_(HistSim, Years, CalcCatch=0)

    HistSim@Unfished@Dynamic@Number <- lapply(unfished@Number, 
                                              AddDimNames, 
                                              c("Age", "Year", "Area"), 
                                              Years)
    
    HistSim@Unfished@Dynamic@Biomass  <- AddDimNames(unfished@Biomass, 
                                                     c('Stock', 'Year'), 
                                                     Years=Years, values=list(StockNames))
    
    HistSim@Unfished@Dynamic@SBiomass  <- AddDimNames(unfished@SBiomass, 
                                                      c('Stock', 'Year'), 
                                                      Years=Years, values=list(StockNames))
    
    HistSim@Unfished@Dynamic@SProduction  <- AddDimNames(unfished@SProduction, 
                                                         c('Stock', 'Year'), 
                                                         Years=Years, values=list(StockNames))
    
    
    SimListOut <- purrr::map(SimListCopy, \(HistSim) {
      HistSim@Unfished@Dynamic@Number <- lapply( unfished@Number, AddDimNames, c("Age", "Year", "Area"), Years)
      
      HistSim@Unfished@Dynamic@Biomass  <- AddDimNames(unfished@Biomass, 
                                                       c('Stock', 'Year'), 
                                                       Years=Years, values=list(StockNames))
      
      HistSim@Unfished@Dynamic@SBiomass  <- AddDimNames(unfished@SBiomass, 
                                                        c('Stock', 'Year'), 
                                                        Years=Years, values=list(StockNames))
      
      HistSim@Unfished@Dynamic@SProduction  <- AddDimNames(unfished@SProduction, 
                                                           c('Stock', 'Year'), 
                                                           Years=Years, values=list(StockNames))
      HistSim
      
    }) 
  } else {
    SimListOut <- purrr::map(SimListCopy, \(HistSim) {
      
      unfished <- SimulateDynamics_(HistSim, Years)
      
      HistSim@Unfished@Dynamic@Number <- lapply(unfished@Number, AddDimNames, c("Age", "Year", "Area"), Years)
      
      HistSim@Unfished@Dynamic@Biomass  <- AddDimNames(unfished@Biomass, 
                                                       c('Stock', 'Year'), 
                                                       Years=Years, values=list(StockNames))
      
      HistSim@Unfished@Dynamic@SBiomass  <- AddDimNames(unfished@SBiomass, 
                                                        c('Stock', 'Year'), 
                                                        Years=Years, values=list(StockNames))
      
      HistSim@Unfished@Dynamic@SProduction  <- AddDimNames(unfished@SProduction, 
                                                           c('Stock', 'Year'), 
                                                           Years=Years, values=list(StockNames))
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

