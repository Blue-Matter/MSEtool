
# TODO - include complexes & female/male 

CalcPerRecruit <- function(Hist, apicalF=0.1, Years=NULL) {

  if (inherits(Hist, 'om')) {
    Hist <- OM2Hist(Hist, silent=TRUE)
  }
    
  CheckClass(Hist, 'hist', 'Hist')
  
  nArea <- nArea(Hist)

  if (is.null(Years))
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)

  StockList <- Hist@OM@Stock

  NaturalMortalityList <- purrr::map(StockList, \(Stock)
                                     Stock@NaturalMortality@MeanAtAge |> ArraySubsetYear(Years))

  StockFleetAllocation <- purrr::map(Hist@OM@Fleet, \(FleetList)
                                     CalcFleetAllocationF(FleetList, Years)
  ) |> List2Array('Stock', pos=2)

  PlusGroupList <- purrr::map(StockList, \(Stock) Stock@Ages@PlusGroup)

  MaturityList <- purrr::map(StockList, \(Stock) Stock@  Maturity@MeanAtAge |> ArraySubsetYear(Years))
  SemelparousList <- purrr::map(StockList, \(Stock)
                                Stock@Maturity@Semelparous |> ArraySubsetYear(Years))
  WeightList <- purrr::map(StockList, \(Stock) Stock@Weight@MeanAtAge |>
                             ArraySubsetYear(Years))
  SpawnTimeFracList <- purrr::map(StockList, \(Stock) Stock@SRR@SpawnTimeFrac)
  SPFrom <- purrr::map(StockList, \(stock) stock@SRR@SPFrom) |> unlist()
  if (is.null(SPFrom))
    SPFrom <- 1:length(StockList)

  if (is.null(Hist@Reference@SPR0))
    Hist@Reference@SPR0 <- CalcSPR0(Hist)

  SPR0List <- Array2List(Hist@Reference@SPR0)

  FecundityList <- purrr::map(StockList, \(Stock) Stock@Fecundity@MeanAtAge |>
                                ArraySubsetYear(Years))
  
  
  StockFleetList <- Hist@OM@Fleet
  
  WeightFleetList <- purrr::map(StockFleetList, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@WeightFleet |>
        ArraySubsetYear(Years)
    }) |>
      List2Array(pos=4)
  })
  
  Selectivity <- purrr::map(StockFleetList, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@Selectivity@MeanAtAge |>
        ArraySubsetYear(Years)
      }) |>
      List2Array(pos=4)
    }) |> List2Array('Stock', pos=2) |>
    CheckSpatial('Selectivity')
  

  Retention <- purrr::map(StockFleetList, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@Retention@MeanAtAge |>
        ArraySubsetYear(Years)
    }) |>
      List2Array(pos=4)
  }) |> List2Array('Stock', pos=2) |>
    CheckSpatial('Retention')

  DiscardMortality <- purrr::map(StockFleetList, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@DiscardMortality@MeanAtAge |>
        ArraySubsetYear(Years)
    }) |>
      List2Array(pos=4)
  }) |> List2Array('Stock', pos=2) |>
    CheckSpatial('DiscardMortality')


  FleetNames <- FleetNames(Hist)
  
  
}

CalcFleetAllocationF <- function(FleetList, Years) {

  FDistribution <- purrr::map(FleetList, \(Fleet) {
    ArrayMultiply(Fleet@Effort@Effort |>  ArraySubsetYear(Years),
                  Fleet@Catchability@Efficiency |>  ArraySubsetYear(Years))
  }) |>
    List2Array('Fleet', pos=3)

  FDistributionTotal <- SumOverFleet(FDistribution)
  FDistributionTotal <- List2Array(replicate(length(FleetList), FDistributionTotal, simplify = FALSE), pos=3)
  dimnames(FDistributionTotal)[['Fleet']] <- names(FleetList)

  ArrayDivide(FDistribution, FDistributionTotal)
}

# Not correct for complexes with different growth/selectivity curves

CalcPerRecruit_F <- function(apicalF = 0.1,
                             StockFleetAllocation,
                             NaturalMortalityList,
                             PlusGroupList,
                             MaturityList,
                             SemelparousList,
                             WeightList,
                             SpawnTimeFracList,
                             SPFrom,
                             SPR0List,
                             FecundityList,
                             WeightFleetList,
                             Selectivity,
                             Retention,
                             DiscardMortality,
                             FleetNames,
                             Years) {

  apicalFAge <- apicalF * StockFleetAllocation  |>
    AddDimension("Age", pos=3) 
  
  FInteract <- ArrayMultiply(apicalFAge, Selectivity)
  FRetain <- ArrayMultiply(FInteract, Retention)
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
  FDead <- FRetain + FDiscardDead
  FDeadTotal <- SumOverFleet(FDead)
  ActualApicalF <- apply(FDeadTotal, setdnames('Year'), max)
  
  if (apicalF>0 & any(abs(ActualApicalF/apicalF - 1) > 1E-2)) {
    # adjust for retention and discard mortality & different selectivity patterns by fleet
    apicalFSimTS <- array(apicalF, dim=dim(ActualApicalF), dimnames = dimnames(ActualApicalF))
    
    adjust <- ArrayDivide(apicalFSimTS,ActualApicalF)
    adjust <- adjust |> AddDimension("Stock", pos=2) |> AddDimension("Age", pos=3) |> AddDimension("Fleet", pos=5) |>
      ExtendFleets(Fleets=FleetNames)
  
    FInteract <- ArrayMultiply(adjust, FInteract)
    
    FRetain <- ArrayMultiply(FInteract, Retention)
    FDiscardTotal <- ArraySubtract(FInteract, FRetain)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
    FDead <- FRetain + FDiscardDead
    FDeadTotal <- apply(FDead, setdnames(c('Stock', 'Age', 'Year'), BySim), sum)
    ActualApicalF <- apply(FDeadTotal, setdnames('Year', BySim), max)
  }
  
  stockInd <- which(names(dimnames(FDeadTotal)) == 'Stock')
  FDeadTotalList <- FDeadTotal |> Array2List(stockInd)
  ZDeadTotalList <- purrr::map2(FDeadTotalList, NaturalMortalityList, ArrayAdd)
  
  NPRFList <- purrr::pmap(list(NaturalMortalityList, FDeadTotalList, PlusGroupList, SemelparousList),
                          \(NaturalMortality, FishingMortalityAtAge, PlusGroup, Semelparous)
                          CalcSurvival(NaturalMortality,
                                       FishingMortalityAtAge,
                                       PlusGroup,
                                       SpawnTimeFrac=0,
                                       Semelparous)
  )
  
  IsSpawnTimeFrac <- any(unlist(SpawnTimeFracList)!=0)
  if (IsSpawnTimeFrac) {
    # per recruit spawning
    NPRF_SPList <- purrr::pmap(list(NaturalMortalityList, FDeadTotalList, PlusGroupList, SemelparousList, SpawnTimeFracList),
                               \(NaturalMortality, FishingMortalityAtAge, PlusGroup, Semelparous, SpawnTimeFrac)
                               CalcSurvival(NaturalMortality,
                                            FishingMortalityAtAge,
                                            PlusGroup,
                                            SpawnTimeFrac,
                                            Semelparous)
    )
  } else {
    NPRF_SPList <- NPRFList
  }
  
  # SPR
  SPRFList <- purrr::map2(NPRF_SPList, FecundityList, \(NPRF_SP, Fecundity) {
    SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
    if (!is.array(SPRF))
      SPRF <- array(SPRF, length(SPRF), dimnames = list(Year=Years))
    SPRF
  })
  
  SPRFList <- SPRFList[SPFrom]
  names(SPRFList) <- names(NPRFList)
  SPR <- purrr::map2(SPRFList, SPR0List, \(SPRF, SPR0) ArrayDivide(SPRF, SPR0)) |>
    List2Array('Stock') |>
    ArraySubsetYear(Years)
 
  
  # Removals and Landings
  stockInd <- which(names(dimnames(FDead)) == 'Stock')
  FDeadList <- FDead |> Array2List(stockInd)
  FishingDeadList <- purrr::map2(FDeadList, ZDeadTotalList, \(FDead, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    ArrayDivide(FDead, ZDeadTotalFleet)
  })
  names(FishingDeadList) <- names(NaturalMortalityList)
  
  NDeadList <- purrr::map2(NPRFList, ZDeadTotalList, \(NPRF, ZDeadTotal)
                           ArrayMultiply(NPRF, (1-exp(-ZDeadTotal))))
  
  
  Removals <- purrr::pmap(list(FishingDeadList, NDeadList, WeightFleetList), \(FishingDead, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    removals <- ArrayMultiply(FishingDead, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      SumOverFleet() |> SumOverAge()
    removals
  }) |>
    List2Array('Stock', pos=2) 
  
  stockInd <- which(names(dimnames(FRetain)) == 'Stock')
  FRetainList <- FRetain |> Array2List(stockInd)
  FishingRetainList <- purrr::map2(FRetainList, ZDeadTotalList, \(FRetain, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    ArrayDivide(FRetain, ZDeadTotalFleet)
  })
  names(FishingRetainList) <- names(NaturalMortalityList)
  
  Landings <- purrr::pmap(list(FishingRetainList, NDeadList, WeightFleetList), \(FishingRetain, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    removals <- ArrayMultiply(FishingRetain, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      SumOverFleet() |> SumOverAge()
    removals
  }) |> List2Array('Stock', pos=2) 
    
  Biomass <- purrr::map2(NPRFList, WeightList, \(NPRF, Weight) {
    ArrayMultiply(NPRF, Weight) |> SumOverAge()
  }) |> List2Array("Stock", pos=2)
  
  SBiomass <- purrr::pmap(list(NPRF_SPList, WeightList, MaturityList), \(NPRF_SP, Weight, Maturity) {
    ArrayMultiply(NPRF_SP, Weight) |> ArrayMultiply(Maturity) |> SumOverAge()
  }) |> List2Array("Stock", pos=2)
  
  SProduction <- purrr::map2(NPRF_SPList,FecundityList, \(NPRF_SP, Fecundity) {
    ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
  }) |> List2Array("Stock", pos=2)
  
  
  PerRecruit <- new('perrecruit')
  PerRecruit@SPR0 <- SPR0List |> List2Array("Stock", pos=2) 
  PerRecruit@apicalF <- apicalF
  PerRecruit@NPRF <- NPRFList |> List2Array("Stock", pos=2) 
  if (IsSpawnTimeFrac)
    PerRecruit@NPRF_SP <- NPRF_SPList |> List2Array("Stock", pos=2) 
  PerRecruit@SPRF <- SPRFList |>  List2Array("Stock", pos=2) 
  PerRecruit@SPR <- SPR
  PerRecruit@Biomass <- Biomass
  PerRecruit@SBiomass <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals <- Removals
  PerRecruit@Landings <- Landings
  PerRecruit
  
}

