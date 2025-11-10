
CalcFleetAllocationF <- function(FleetList, Years) {
  
  BySim <- "Sim" %in% (FleetList[[1]]@Effort |> dimnames() |> names())
  FDistribution <- purrr::map(FleetList, \(Stock) {
    ArrayMultiply(Stock@Effort |>  ArraySubsetYear(Years),
                  Stock@Catchability |>  ArraySubsetYear(Years))
  }) |> 
    List2Array('Stock') |>
    aperm(setdnames(c('Stock', 'Year', 'Fleet'), BySim))
  
  FDistributionTotal <- apply(FDistribution, setdnames('Year', BySim), sum) 
  if (!is.array(FDistributionTotal)) {
    FDistributionTotal <- array(FDistributionTotal, length(FDistributionTotal),
                                dimnames = list(Year=Years))
  }
  
  FDistributionTotal <- FDistributionTotal |>
    AddDimension("Fleet") |> AddDimension("Stock") |>
    aperm(setdnames(c('Stock', 'Year', 'Fleet'), BySim))
  
  ArrayDivide(FDistribution, FDistributionTotal) 
}

CalcPerRecruit <- function(apicalF, OM, Years=NULL) {

  if (is.null(Years))
    Years <- OM |> Years('Historical') |> tail(1)

  StockList <- PopulateStockList(OM) |> SubsetYear(Years, AddPast = FALSE)
  StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetYear(Years)
  nAgesList <- purrr::map(StockList, \(Stock)
                          length(Stock@Ages@Classes))

  FleetList <- purrr::map2(StockFleetList, nAgesList, \(FleetList,nAges)
                           Fleet2Hist(FleetList, nAges,
                                      nSim=nSim(OM),
                                      Years=Years,
                                      nArea(StockList[[1]]),
                                      silent=TRUE)
  )
  
  CalcPerRecruit_StockList(apicalF, StockList, FleetList, Years)
}

CalcPerRecruit_StockList <- function(apicalF, StockList, FleetList, Years) {

  NaturalMortalityList <- purrr::map(StockList, \(Stock) 
                                     Stock@NaturalMortality@MeanAtAge |> ArraySubsetYear(Years))
  
  BySim <- "Sim" %in% (NaturalMortalityList[[1]] |> dimnames() |> names())
  
  StockFleetAllocation <- CalcFleetAllocationF(FleetList, Years)

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
  
  SPR0List <- purrr::map(StockList, \(Stock) CalcSPR0_Stock(Stock, Years))
  
  SPR0List <- SPR0List[SPFrom]
  names(SPR0List) <- names(NaturalMortalityList)
  
  FecundityList <- purrr::map(StockList, \(Stock) Stock@Fecundity@MeanAtAge |> 
                                ArraySubsetYear(Years))
  WeightFleetList <-purrr::map(FleetList, \(Fleet) Fleet@WeightFleet |>
                                 ArraySubsetYear(Years))
  
  Selectivity <- purrr::map(FleetList, \(Stock) Stock@Selectivity@MeanAtAge |> 
                              ArraySubsetYear(Years)) |> 
    List2Array('Stock') |> aperm(setdnames(c('Stock', 'Age', 'Year', 'Fleet'), BySim))
  
  Retention <- purrr::map(FleetList, \(Stock) Stock@Retention@MeanAtAge |> 
                            ArraySubsetYear(Years)) |> 
    List2Array('Stock') |> aperm(setdnames(c('Stock', 'Age', 'Year', 'Fleet'), BySim))
  
  DiscardMortality <- purrr::map(FleetList, \(Stock) Stock@DiscardMortality@MeanAtAge |> 
                                   ArraySubsetYear(Years)) |> 
    List2Array('Stock') |> aperm(setdnames(c('Stock', 'Age', 'Year', 'Fleet'), BySim))
  
  PerRecruitF <- purrr::map(apicalF, \(F)
                            CalcPerRecruit_StockList_F(F, 
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
                                                       Years,
                                                       BySim)
  )
  names(PerRecruitF) <- apicalF
 
  PerRecruit <- new('perrecruit')
  PerRecruit@SPR0 <- purrr::map(PerRecruitF, slot, 'SPR0')[[1]] 
  
  PerRecruit@NPRF <- purrr::map(PerRecruitF, slot, 'NPRF') |> List2Array("F")
  PerRecruit@NPRF_SP <- purrr::map(PerRecruitF, slot, 'NPRF_SP') |> List2Array("F")
  PerRecruit@SPRF <- purrr::map(PerRecruitF, slot, 'SPRF') |> List2Array("F")
  PerRecruit@SPR <- purrr::map(PerRecruitF, slot, 'SPR') |> List2Array("F")
  PerRecruit@Biomass <- purrr::map(PerRecruitF, slot, 'Biomass') |> List2Array("F")
  PerRecruit@SBiomass <- purrr::map(PerRecruitF, slot, 'SBiomass') |> List2Array("F")
  PerRecruit@SProduction <- purrr::map(PerRecruitF, slot, 'SProduction') |> List2Array("F")
  PerRecruit@Removals <- purrr::map(PerRecruitF, slot, 'Removals') |> List2Array("F")
  PerRecruit@Landings <- purrr::map(PerRecruitF, slot, 'Landings') |> List2Array("F")
  PerRecruit
  
  
}

CalcPerRecruit_StockList_F <- function(apicalF,
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
                                       Years,
                                       BySim) {
  

  apicalFAge <- apicalF * StockFleetAllocation  |> 
    AddDimension("Age") |> 
    aperm(setdnames(c('Stock', 'Age', 'Year', 'Fleet'), BySim))
  
  FInteract <- ArrayMultiply(apicalFAge, Selectivity)
  FRetain <- ArrayMultiply(FInteract, Retention)  
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
  FDead <- FRetain + FDiscardDead
  # FDeadStock <- apply(FDead, setdnames(c('Stock', 'Age', 'Year'), BySim), sum) 
  FDeadTotal <- apply(FDead, setdnames(c('Stock', 'Age', 'Year'), BySim), sum) 
  ActualApicalF <- apply(FDeadTotal, setdnames('Year', BySim), max)  
  if (!is.array(ActualApicalF)) {
    ActualApicalF <- array(ActualApicalF, length(ActualApicalF),
                           dimnames = list(Year=Years))
  }
  
  if (apicalF>0 & any(abs(ActualApicalF/apicalF - 1) > 1E-2)) {
    # adjust for retention and discard mortality & different selectivity patterns by fleet
    apicalFSimTS <- array(apicalF, dim=dim(ActualApicalF), dimnames = dimnames(ActualApicalF))
    
    adjust <- ArrayDivide(apicalFSimTS,ActualApicalF)
    adjust <- adjust |> AddDimension("Age") |> AddDimension("Fleet") |> AddDimension("Stock")
    adjust <- aperm(adjust, setdnames(c('Stock', 'Age', 'Year', 'Fleet'), BySim))
    
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
    SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> apply(setdnames(c('Year'), BySim), sum)
    if (!is.array(SPRF)) 
      SPRF <- array(SPRF, length(SPRF), dimnames = list(Year=Years))
    SPRF
  })
  
  SPRFList <- SPRFList[SPFrom]  
  names(SPRFList) <- names(NPRFList)
  SPR <- purrr::map2(SPRFList, SPR0List, \(SPRF, SPR0) ArrayDivide(SPRF, SPR0)) |> List2Array('Stock') |>
    aperm(setdnames(c("Stock", "Year"), BySim))
  
  
  # Removals and Landings
  stockInd <- which(names(dimnames(FDead)) == 'Stock')
  FDeadList <- FDead |> Array2List(stockInd)
  FishingDeadList <- purrr::map2(FDeadList, ZDeadTotalList, \(FDead, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet')
    ArrayDivide(FDead, ZDeadTotalFleet)
  })
  names(FishingDeadList) <- names(NaturalMortalityList)
  
  NDeadList <- purrr::map2(NPRFList, ZDeadTotalList, \(NPRF, ZDeadTotal)
                           ArrayMultiply(NPRF, (1-exp(-ZDeadTotal))))
  
  
  Removals <- purrr::pmap(list(FishingDeadList, NDeadList, WeightFleetList), \(FishingDead, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet')
    removals <- ArrayMultiply(FishingDead, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      apply(setdnames('Year', BySim), sum)
    if (!is.array(removals))
      removals <- array(removals, length(removals), dimnames = list(Year=Years))
    removals
  }) |> 
    List2Array('Stock') |>
    aperm(setdnames(c('Stock', 'Year'), BySim))
  
  stockInd <- which(names(dimnames(FRetain)) == 'Stock')
  FRetainList <- FRetain |> Array2List(stockInd)
  FishingRetainList <- purrr::map2(FRetainList, ZDeadTotalList, \(FRetain, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet')
    ArrayDivide(FRetain, ZDeadTotalFleet)
  })
  names(FishingRetainList) <- names(NaturalMortalityList)
  
  Landings <- purrr::pmap(list(FishingRetainList, NDeadList, WeightFleetList), \(FishingRetain, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet')
    removals <- ArrayMultiply(FishingRetain, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      apply(setdnames('Year', BySim), sum)
    if (!is.array(removals))
      removals <- array(removals, length(removals), dimnames = list(Year=Years))
    removals
  }) |> List2Array('Stock') |>
    aperm(setdnames(c('Stock', 'Year'), BySim))
  
  Biomass <- purrr::map2(NPRFList, WeightList, \(NPRF, Weight) {
    biomass <- ArrayMultiply(NPRF, Weight) |> apply(setdnames('Year', BySim), sum)
    if (!is.array(biomass))
      biomass <- array(biomass, length(biomass), dimnames = list(Year=Years))
    biomass
  }) |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Year'), BySim)) 
  
  SBiomass <- purrr::pmap(list(NPRF_SPList, WeightList, MaturityList), \(NPRF_SP, Weight, Maturity) {
    biomass <- ArrayMultiply(NPRF_SP, Weight) |>
      ArrayMultiply(Maturity) |>
      apply(setdnames('Year', BySim), sum)
    if (!is.array(biomass))
      biomass <- array(biomass, length(biomass), dimnames = list(Year=Years))
    biomass
  }) |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Year'), BySim)) 
  
  SProduction <- purrr::map2(NPRF_SPList,FecundityList, \(NPRF_SP, Fecundity) {
    biomass <- ArrayMultiply(NPRF_SP, Fecundity) |>
      apply(setdnames('Year', BySim), sum)
    if (!is.array(biomass))
      biomass <- array(biomass, length(biomass), dimnames = list(Year=Years))
    biomass
  }) |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Year'), BySim)) 
  
  
  PerRecruit <- new('perrecruit')
  PerRecruit@SPR0 <- SPR0List |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Year'), BySim))
  PerRecruit@apicalF <- apicalF
  PerRecruit@NPRF <- NPRFList |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Age', 'Year'), BySim))  
  if (IsSpawnTimeFrac)
    PerRecruit@NPRF_SP <- NPRF_SPList |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Age', 'Year'), BySim)) 
  PerRecruit@SPRF <- SPRFList |>  List2Array("Stock") |> aperm(setdnames(c('Stock', 'Year'), BySim)) 
  PerRecruit@SPR <- SPR
  PerRecruit@Biomass <- Biomass
  PerRecruit@SBiomass <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals <- Removals
  PerRecruit@Landings <- Landings
  PerRecruit
}
