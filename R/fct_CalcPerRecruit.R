# TODO make compatible with OM 


# CalcPerRecruit <- function(apicalF, OM, TimeSteps=NULL) {
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
#   
#   StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
#   StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
#   CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
#   
#   nAgesList <- purrr::map(StockList, \(Stock) 
#                           length(Stock@Ages@Classes))
#   
#   FleetList <- purrr::map2(StockFleetList, nAgesList, \(FleetList,nAges)
#                            Fleet2Hist(FleetList, nAges,
#                                       nSim=nSim(OM), 
#                                       TimeSteps=TimeSteps,
#                                       nArea(StockList[[1]]),
#                                       silent=TRUE)
#   )
#   
#   PerRecruitSim <- purrr::pmap(list(StockList, FleetList, CatchFracList),
#                                CalcPerRecruitStock, apicalF=apicalF, TimeSteps=TimeSteps)
#   names(PerRecruitSim) <- StockNames(OM)
#   
#   PerRecruit <- new('perrecruit')
#   PerRecruit@apicalF <- apicalF
#   PerRecruit@NPR0 <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPR0')) 
#   PerRecruit@NPR0_SP <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPR0_SP')) 
#   PerRecruit@NPRF <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPRF')) 
#   PerRecruit@NPRF_SP <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPRF_SP')) 
#   PerRecruit@SPR0 <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SPR0')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep'))
#   
#   PerRecruit@SPRF <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SPRF')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit@SPR <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SPR')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit@Biomass <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'Biomass')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit@SBiomass <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SBiomass')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit@SProduction <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SProduction')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit@Removals <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'Removals')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit@Landings <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'Landings')) |> 
#     List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
#   
#   PerRecruit
# }

CalcPerRecruit_StockList <- function(apicalF, StockList, FleetList, TimeSteps) {

  NaturalMortalityList <- purrr::map(StockList, \(Stock) 
                                     Stock@NaturalMortality@MeanAtAge |> ArraySubsetTimeStep(TimeSteps))
  
  BySim <- "Sim" %in% (NaturalMortalityList[[1]] |> dimnames() |> names())
  
  # TODO - should account for different units of Effort ...
  StockAllocation <- purrr::map(FleetList, \(Stock) Stock@Effort@Catchability |> 
                                  ArraySubsetTimeStep(TimeSteps)) |> 
    List2Array('Stock') |>
    aperm(setdnames(c('Stock', 'TimeStep', 'Fleet'), BySim))
  
  StockAllocationTotal <- apply(StockAllocation, setdnames('TimeStep', BySim), sum) 
  if (!is.array(StockAllocationTotal)) {
    StockAllocationTotal <- array(StockAllocationTotal, length(StockAllocationTotal),
                                  dimnames = list(TimeStep=TimeSteps))
  }
  
  StockAllocationTotal <- StockAllocationTotal |>
    AddDimension("Fleet") |> AddDimension("Stock") |>
    aperm(setdnames(c('Stock', 'TimeStep', 'Fleet'), BySim))
  
  StockAllocation <- ArrayDivide(StockAllocation, StockAllocationTotal) 
  

  PlusGroupList <- purrr::map(StockList, \(Stock) Stock@Ages@PlusGroup)
  MaturityList <- purrr::map(StockList, \(Stock) Stock@  Maturity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps))
  SemelparousList <- purrr::map(StockList, \(Stock) 
                                Stock@Maturity@Semelparous |> ArraySubsetTimeStep(TimeSteps))
  WeightList <- purrr::map(StockList, \(Stock) Stock@Weight@MeanAtAge |> 
                             ArraySubsetTimeStep(TimeSteps))
  SpawnTimeFracList <- purrr::map(StockList, \(Stock) Stock@SRR@SpawnTimeFrac)
  SPFrom <- purrr::map(StockList, \(stock) stock@SRR@SPFrom) |> unlist()
  if (is.null(SPFrom))
    SPFrom <- 1:length(StockList)
  
  SPR0List <- purrr::map(StockList, \(Stock) CalcSPR0_Stock(Stock, TimeSteps))
  SPR0List <- SPR0List[SPFrom]
  names(SPR0List) <- names(NaturalMortalityList)
  
  FecundityList <- purrr::map(StockList, \(Stock) Stock@Fecundity@MeanAtAge |> 
                                ArraySubsetTimeStep(TimeSteps))
  WeightFleetList <-purrr::map(FleetList, \(Fleet) Fleet@WeightFleet |>
                                 ArraySubsetTimeStep(TimeSteps))
  
  Selectivity <- purrr::map(FleetList, \(Stock) Stock@Selectivity@MeanAtAge |> 
                              ArraySubsetTimeStep(TimeSteps)) |> 
    List2Array('Stock') |> aperm(setdnames(c('Stock', 'Age', 'TimeStep', 'Fleet'), BySim))
  
  Retention <- purrr::map(FleetList, \(Stock) Stock@Retention@MeanAtAge |> 
                            ArraySubsetTimeStep(TimeSteps)) |> 
    List2Array('Stock') |> aperm(setdnames(c('Stock', 'Age', 'TimeStep', 'Fleet'), BySim))
  
  DiscardMortality <- purrr::map(FleetList, \(Stock) Stock@DiscardMortality@MeanAtAge |> 
                                   ArraySubsetTimeStep(TimeSteps)) |> 
    List2Array('Stock') |> aperm(setdnames(c('Stock', 'Age', 'TimeStep', 'Fleet'), BySim))
  
  
  PerRecruitF <- purrr::map(apicalF, \(F)
                            CalcPerRecruit_StockList_F(F, 
                                                       StockAllocation, 
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
                                                       TimeSteps,
                                                       BySim)
  )
  names(PerRecruitF) <- apicalF
 
  PerRecruit <- new('perrecruit')
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
                                       StockAllocation, 
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
                                       TimeSteps,
                                       BySim) {
  
  apicalFAge <- apicalF * StockAllocation  |> 
    AddDimension("Age") |> 
    aperm(setdnames(c('Stock', 'Age', 'TimeStep', 'Fleet'), BySim))
  
  FInteract <- ArrayMultiply(apicalFAge, Selectivity)
  FRetain <- ArrayMultiply(FInteract, Retention)  
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
  FDead <- FRetain + FDiscardDead
  # FDeadStock <- apply(FDead, setdnames(c('Stock', 'Age', 'TimeStep'), BySim), sum) 
  FDeadTotal <- apply(FDead, setdnames(c('Stock', 'Age', 'TimeStep'), BySim), sum) 
  ActualApicalF <- apply(FDeadTotal, setdnames('TimeStep', BySim), max)  
  if (!is.array(ActualApicalF)) {
    ActualApicalF <- array(ActualApicalF, length(ActualApicalF),
                           dimnames = list(TimeStep=TimeSteps))
  }
  
  if (any(abs(ActualApicalF/apicalF - 1) > 1E-2)) {
    # adjust for retention and discard mortality & different selectivity patterns by fleet
    apicalFSimTS <- array(apicalF, dim=dim(ActualApicalF), dimnames = dimnames(ActualApicalF))
    
    adjust <- ArrayDivide(apicalFSimTS,ActualApicalF)
    adjust <- adjust |> AddDimension("Age") |> AddDimension("Fleet") |> AddDimension("Stock")
    adjust <- aperm(adjust, setdnames(c('Stock', 'Age', 'TimeStep', 'Fleet'), BySim))
    
    FInteract <- ArrayMultiply(adjust, FInteract)
    FRetain <- ArrayMultiply(FInteract, Retention)  
    FDiscardTotal <- ArraySubtract(FInteract, FRetain)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
    FDead <- FRetain + FDiscardDead
    FDeadTotal <- apply(FDead, setdnames(c('Stock', 'Age', 'TimeStep'), BySim), sum) 
    ActualApicalF <- apply(FDeadTotal, setdnames('TimeStep', BySim), max) 
  }
  
  
  FDeadTotalList <- FDeadTotal |> Array2List(1)
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
    SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> apply(setdnames(c('TimeStep'), BySim), sum)
    if (!is.array(SPRF)) 
      SPRF <- array(SPRF, length(SPRF), dimnames = list(TimeStep=TimeSteps))
    SPRF
  })
  
  SPRFList <- SPRFList[SPFrom]  
  names(SPRFList) <- names(NPRFList)
  SPR <- purrr::map2(SPRFList, SPR0List, \(SPRF, SPR0) ArrayDivide(SPRF, SPR0)) |> List2Array('Stock') |>
    aperm(setdnames(c("Stock", "TimeStep"), BySim))
  
  
  # Removals and Landings
  FDeadList <- FDead |> Array2List(1)
  FishingDeadList <- purrr::map2(FDeadList, ZDeadTotalList, \(FDead, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet')
    ArrayDivide(FDead, ZDeadTotalFleet)
  })
  
  NDeadList <- purrr::map2(NPRFList, ZDeadTotalList, \(NPRF, ZDeadTotal)
                           ArrayMultiply(NPRF, (1-exp(-ZDeadTotal))))
  
  
  Removals <- purrr::pmap(list(FishingDeadList, NDeadList, WeightFleetList), \(FishingDead, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet')
    removals <- ArrayMultiply(FishingDead, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      apply(setdnames('TimeStep', BySim), sum)
    if (!is.array(removals))
      removals <- array(removals, length(removals), dimnames = list(TimeStep=TimeSteps))
    removals
  }) |> List2Array('Stock') |>
    aperm(setdnames(c('Stock', 'TimeStep'), BySim))
  
  FRetainList <- FRetain |> Array2List(1)
  FishingRetainList <- purrr::map2(FRetainList, ZDeadTotalList, \(FRetain, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet')
    ArrayDivide(FRetain, ZDeadTotalFleet)
  })
  
  Landings <- purrr::pmap(list(FishingRetainList, NDeadList, WeightFleetList), \(FishingRetain, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet')
    removals <- ArrayMultiply(FishingRetain, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      apply(setdnames('TimeStep', BySim), sum)
    if (!is.array(removals))
      removals <- array(removals, length(removals), dimnames = list(TimeStep=TimeSteps))
    removals
  }) |> List2Array('Stock') |>
    aperm(setdnames(c('Stock', 'TimeStep'), BySim))
  
  Biomass <- purrr::map2(NPRFList, WeightList, \(NPRF, Weight) {
    biomass <- ArrayMultiply(NPRF, Weight) |> apply(setdnames('TimeStep', BySim), sum)
    if (!is.array(biomass))
      biomass <- array(biomass, length(biomass), dimnames = list(TimeStep=TimeSteps))
    biomass
  }) |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'TimeStep'), BySim)) 
  
  SBiomass <- purrr::pmap(list(NPRF_SPList, WeightList, MaturityList), \(NPRF_SP, Weight, Maturity) {
    biomass <- ArrayMultiply(NPRF_SP, Weight) |>
      ArrayMultiply(Maturity) |>
      apply(setdnames('TimeStep', BySim), sum)
    if (!is.array(biomass))
      biomass <- array(biomass, length(biomass), dimnames = list(TimeStep=TimeSteps))
    biomass
  }) |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'TimeStep'), BySim)) 
  
  SProduction <- purrr::pmap(list(NPRF_SPList, WeightList,FecundityList), \(NPRF_SP, Weight, Fecundity) {
    biomass <- ArrayMultiply(NPRF_SP, Weight) |>
      ArrayMultiply(Fecundity) |>
      apply(setdnames('TimeStep', BySim), sum)
    if (!is.array(biomass))
      biomass <- array(biomass, length(biomass), dimnames = list(TimeStep=TimeSteps))
    biomass
  }) |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'TimeStep'), BySim)) 
  
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF <- apicalF
  PerRecruit@NPRF <- NPRFList |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Age', 'TimeStep'), BySim))  
  if (IsSpawnTimeFrac)
    PerRecruit@NPRF_SP <- NPRF_SPList |> List2Array("Stock") |> aperm(setdnames(c('Stock', 'Age', 'TimeStep'), BySim)) 
  PerRecruit@SPRF <- SPRFList |>  List2Array("Stock") |> aperm(setdnames(c('Stock', 'TimeStep'), BySim)) 
  PerRecruit@SPR <- SPR
  PerRecruit@Biomass <- Biomass
  PerRecruit@SBiomass <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals <- Removals
  PerRecruit@Landings <- Landings
  PerRecruit
}
