CalcPerRecruit <- function(apicalF, OM, TimeSteps=NULL) {
  
  if (is.null(TimeSteps))
    TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
  
  StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
  StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
  CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
  
  nAgesList <- purrr::map(StockList, \(Stock) 
                          length(Stock@Ages@Classes))
  
  FleetList <- purrr::map2(StockFleetList, nAgesList, \(FleetList,nAges)
                           Fleet2Hist(FleetList, nAges,
                                      nSim=nSim(OM), 
                                      TimeSteps=TimeSteps,
                                      nArea(StockList[[1]]),
                                      silent=TRUE)
  )
  
  PerRecruitSim <- purrr::pmap(list(StockList, FleetList, CatchFracList),
                               CalcPerRecruitStock, apicalF=apicalF, TimeSteps=TimeSteps)
  names(PerRecruitSim) <- StockNames(OM)
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF <- apicalF
  PerRecruit@NPR0 <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPR0')) 
  PerRecruit@NPR0_SP <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPR0_SP')) 
  PerRecruit@NPRF <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPRF')) 
  PerRecruit@NPRF_SP <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'NPRF_SP')) 
  PerRecruit@SPR0 <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SPR0')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep'))
  
  PerRecruit@SPRF <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SPRF')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit@SPR <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SPR')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit@Biomass <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'Biomass')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit@SBiomass <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SBiomass')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit@SProduction <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'SProduction')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit@Removals <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'Removals')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit@Landings <- purrr::map(PerRecruitSim, \(PerRecruit) slot(PerRecruit, 'Landings')) |> 
    List2Array("Stock") |> aperm(c("Sim", 'Stock', 'TimeStep', 'F'))
  
  PerRecruit
}



CalcPerRecruitStock <- function(apicalF, Stock, Fleet, CatchFrac, TimeSteps) {
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF <- apicalF
  
  NaturalMortality <- Stock@NaturalMortality@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 


  NPR0 <- CalcUnfishedSurvival(Stock, TimeSteps=TimeSteps)
  PerRecruit@NPR0 <- NPR0
  
  NPR0_SP <- CalcUnfishedSurvival(Stock, SP=TRUE, TimeSteps=TimeSteps)
  
  if (!identical(NPR0, NPR0_SP))
    PerRecruit@NPR0_SP <- NPR0_SP
  
  Fecundity <- Stock@Fecundity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps)
  PerRecruit@SPR0 <- CalcSPR0_Stock(Stock, TimeSteps)
  
  
  Selectivity <- Fleet@Selectivity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  Retention <- Fleet@Retention@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  DiscardMortality <- Fleet@DiscardMortality@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  
  
  PerRecruitF <- purrr::map(apicalF, \(F) 
                            CalcPerRecruitStock_F(F, Stock, Fleet, CatchFrac, TimeSteps,
                                                  NaturalMortality,
                                                  Selectivity, Retention, DiscardMortality)
                  )
   names(PerRecruitF) <- apicalF
   
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

CalcPerRecruitStock_F <- function(apicalF, Stock, Fleet, CatchFrac, TimeSteps,
                                  NaturalMortality,
                                  Selectivity, Retention, DiscardMortality) {
  
  dd <- dim(Selectivity)
  dnames <- dimnames(Selectivity)
  BySim <- "Sim" %in% names(dnames)
  AgeInd <- which(names(dnames)=='Age')
  nAge <- dd[AgeInd]
  FleetInd <- which(names(dnames)=='Fleet')
  
  apicalFAge <- apicalF * CatchFrac |> AddDimension("Age")
  if (BySim) {
    apicalFAge <- aperm(apicalFAge, c('Sim', 'Age', 'Fleet'))
    dimnames(apicalFAge) <- list(Sim=dnames[[1]],
                                 Age=dnames[[AgeInd]][1],
                                 Fleet=dnames[[FleetInd]])
    
    apicalFAgeFleet <- AddDimension(apicalFAge, 'TimeStep', TimeSteps[1]) |>
      aperm(c('Sim', 'Age', 'TimeStep', 'Fleet'))
    
  } else {
    apicalFAge <- aperm(apicalFAge, c('Age', 'Fleet'))
    dimnames(apicalFAge) <- list(Age=dnames[[AgeInd]][1],
                                 Fleet=dnames[[FleetInd]])
    
    apicalFAgeFleet <- AddDimension(apicalFAge, 'TimeStep', TimeSteps[1]) |>
      aperm(c('Age', 'TimeStep', 'Fleet'))
    
  }
  
  FInteract <- ArrayMultiply(apicalFAgeFleet, Selectivity)
  FRetain <- ArrayMultiply(FInteract, Retention)  
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
  FDead <- FRetain + FDiscardDead
  
  if (BySim) {
    FDeadTotal <- apply(FDead, c('Sim', 'Age', 'TimeStep'), sum)
    ActualApicalF <- apply(FDeadTotal, c('Sim', 'TimeStep'), max)  
  } else {
    FDeadTotal <- apply(FDead, c('Age', 'TimeStep'), sum)
    ActualApicalF <- apply(FDeadTotal, c('TimeStep'), max)  
  }
  
  updateF <- which(ActualApicalF/apicalF - 1 > 1E-2)
  
  # adjust for retention and discard mortality & different selectivity patterns by fleet
  if (length(updateF)) {
    adjust <- apicalF/ActualApicalF
    adjust <- adjust |> AddDimension("Age") |> AddDimension("Fleet")
    
    if (BySim) {
      adjust <- aperm(adjust, c('Sim', 'Age', 'TimeStep', 'Fleet'))
    } else {
      adjust <- aperm(adjust, c('Age', 'TimeStep', 'Fleet'))
    }
  
    FInteract <- ArrayMultiply(adjust, FInteract)
    FRetain <- ArrayMultiply(FInteract, Retention)  
    FDiscardTotal <- ArraySubtract(FInteract, FRetain)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
    FDead <- FRetain + FDiscardDead
    if (BySim) {
      FDeadTotal <- apply(FDead, c('Sim', 'Age', 'TimeStep'), sum)
      ActualApicalF <- apply(FDeadTotal, c('Sim', 'TimeStep'), max)  
    } else {
      FDeadTotal <- apply(FDead, c('Age', 'TimeStep'), sum)
      ActualApicalF <- apply(FDeadTotal, c('TimeStep'), max)  
    }
  }
  
  ZDeadTotal <- ArrayAdd(NaturalMortality, FDeadTotal)
  Semelparous <- Stock@Maturity@Semelparous |> ArraySubsetTimeStep(TimeSteps)
  
  NPRF <- CalcSurvival(NaturalMortalityAtAge=NaturalMortality, 
                       FishingMortalityAtAge=FDeadTotal, 
                       PlusGroup=Stock@Ages@PlusGroup,
                       SpawnTimeFrac=0,
                       Semelparous=Semelparous)
  
  NPRF_SP <- CalcSurvival(NaturalMortalityAtAge=NaturalMortality, 
                          FishingMortalityAtAge=FDeadTotal, 
                          PlusGroup=Stock@Ages@PlusGroup,
                          SpawnTimeFrac=Stock@SRR@SpawnTimeFrac,
                          Semelparous=Semelparous)
  
  
  NDead <- ArrayMultiply(NPRF, (1-exp(-ZDeadTotal)))
  
  # Calc SPR
  Fecundity <- Stock@Fecundity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps)
  
  SPR0 <- CalcSPR0_Stock(Stock, TimeSteps)
  
  if (BySim) {
    SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> 
      apply(c('Sim', 'TimeStep'), sum)
    
  } else {
    SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> 
      apply('TimeStep', sum)
    SPRF <- array(SPRF, dimnames=list(TimeStep=TimeSteps))
  }
 
  SPR <- ArrayDivide(SPRF, SPR0)
  
  # Calc Removals and Landings
  ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet')
  FishingDead <- ArrayDivide(FDead, ZDeadTotalFleet)
  FishingRetain <- ArrayDivide(FRetain, ZDeadTotalFleet)
  
  NDeadFleet <- AddDimension(NDead, 'Fleet') 
  WeightFleet <- Fleet@WeightFleet |> ArraySubsetTimeStep(TimeSteps)
  
  Removals <- ArrayMultiply(FishingDead, NDeadFleet) |>
    ArrayMultiply(WeightFleet) 
  
  Landings <- ArrayMultiply(FishingRetain, NDeadFleet) |>
    ArrayMultiply(WeightFleet) 
  
  if (BySim) {
    Removals <- apply(Removals, c('Sim', 'TimeStep'), sum)
    Landings <- apply(Landings, c('Sim', 'TimeStep'), sum)
  } else {
    Removals <- apply(Removals, c('TimeStep'), sum) |>
      array(dimnames=list(TimeStep=TimeSteps))
    Landings <- apply(Landings, c('TimeStep'), sum) |>
      array(dimnames=list(TimeStep=TimeSteps))
  }
 
  Weight <- Stock@Weight@MeanAtAge |> ArraySubsetTimeStep(TimeSteps)
  Biomass <- ArrayMultiply(NPRF, Weight)
  SBiomass <- ArrayMultiply(NPRF_SP, Weight)
  SProduction <- ArrayMultiply(NPRF_SP, Fecundity)
  
  
  if (BySim) {
    Biomass <-  apply(Biomass, c('Sim', 'TimeStep'), sum)
    SBiomass <-  apply(SBiomass, c('Sim', 'TimeStep'), sum)
    SProduction <-  apply(SProduction, c('Sim', 'TimeStep'), sum)
  } else {
    Biomass <- apply(Biomass, c('TimeStep'), sum) |>
      array(dimnames=list(TimeStep=TimeSteps))
    SBiomass <- apply(SBiomass, c('TimeStep'), sum) |>
      array(dimnames=list(TimeStep=TimeSteps))
    SProduction <- apply(SProduction, c('TimeStep'), sum) |>
      array(dimnames=list(TimeStep=TimeSteps))
    
  }
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF <- apicalF
  PerRecruit@NPRF <- NPRF
  if (!identical(NPRF, NPRF_SP))
    PerRecruit@NPRF_SP <- NPRF_SP
  PerRecruit@SPRF <- SPRF
  PerRecruit@SPR <- SPR
  PerRecruit@Biomass <- Biomass
  PerRecruit@SBiomass <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals <- Removals
  PerRecruit@Landings <- Landings
  PerRecruit
}
