CalcReferencePoints <- function(OM, TimeSteps=NULL, silent=FALSE) {
  
  if (is.null(TimeSteps))
    TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
  
  StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
  StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
  CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
  
  StockNames <- StockNames(OM)
  nAgesList <- purrr::map(StockList, \(Stock) 
                          length(Stock@Ages@Classes))
  
  FleetList <- purrr::map2(StockFleetList, nAgesList, \(FleetList,nAges)
                               Fleet2Hist(FleetList, nAges,
                                          nSim=nSim(OM), 
                                          TimeSteps=TimeSteps,
                                          nArea(StockList[[1]]),
                                          silent=TRUE)
  )
  
  StockSimList <- purrr::map(1:nSim(OM), \(x) {
    SubsetSim(StockList, Sim=x, drop=TRUE)
  }, .progress = 'Building StockList')
  names(StockSimList) <- 1:nSim(OM)
  
  FleetSimList <- purrr::map(1:nSim(OM), \(x) {
    SubsetSim(FleetList, Sim=x, drop=TRUE)
  }, .progress = 'Building FleetList')
  names(FleetSimList) <- 1:nSim(OM)
  
  CatchFracSimList <- purrr::map(1:nSim(OM), \(x) {
    SubsetSim(CatchFracList, Sim=x, drop=TRUE)
  })
  names(CatchFracSimList) <- 1:nSim(OM)
  

 
}

CalcRefPointsSim <- function(StockSimList, FleetSimList, CatchFracSimList, TimeSteps) {
  IdenticalAcrossSims <- IdenticalSims(StockSimList, TimeSteps) &
    IdenticalSims(FleetSimList, TimeSteps) &
    IdenticalSims(CatchFracSimList, TimeSteps, EditSlots=FALSE)
  

  RefPoints <- new('refpoints')
  stop()
  # TODO - up to here - refine refpoints object, finiish code, update Simulate_om
  
}



CalculateMSY <- function(OM, TimeSteps=NULL) {
  
  
}


CalcRefPointsHistSim <- function(HistSimList, TimeSteps=NULL) {
  
  if (is.null(TimeSteps))
    TimeSteps <- HistSimList[[1]] |> TimeSteps() |> tail(1)
  
  IdenticalAcrossSims <- CheckIdenticalSims(HistSimList, TimeSteps)
  
  RefPoints <- new('refpoints')
  
  # ---- MSY Ref Points ----
  if (IdenticalAcrossSims) {
    HistSim <- HistSimList[[1]]
    RefPoints <- CalculateMSYHistSim(HistSim, TimeSteps)
    HistSimList <- purrr::map(HistSimList, \(histsim) {
      histsim@RefPoints <-RefPoints
      histsim
    })
    
  } else {
    HistSimList <- purrr::map(HistSimList, \(HistSim) 
                              CalculateMSYHistSim(HistSim, TimeSteps))  
  }
  

  
  
  # ---- SPR0 ----
  # RefPoints@SPR0 <- purrr::map(HistSimList, \(HistSim) 
  #                              CalcSPR0(HistSim, TimeSteps)) |>
  #   List2Array('Sim', 'Stock') |> 
  #   aperm(c('Sim', 'Stock', 'TimeStep'))
  

  # ---- MSY Ref Points ----
  
  # HistSim <- HistSimList$`1`
  # 
  # Stock <- HistSim@OM@Stock$Female
  # Fleet <- HistSim@OM@Fleet$Female
  # Allocation <- HistSim@OM@Allocation$Female
  # 
  # st1 <- OptimizeMSY(HistSim@OM@Stock[[1]], 
  #                    HistSim@OM@Fleet[[1]],
  #                    HistSim@OM@Allocation[[1]], 
  #                    TimeSteps)
  # 
  # st2 <- OptimizeMSY(HistSim@OM@Stock[[2]], 
  #                    HistSim@OM@Fleet[[2]],
  #                    HistSim@OM@Allocation[[2]], 
  #                    TimeSteps)
  # 
  # st1$Landings
  # st2$Landings
  
  # TODO - calculate FMSY for stock complexes or SPFrom
  # - NSWO
  
  
  # RefPoints@Curves <- CalcCurves(OM, TimeSteps=TimeSteps)
  
  HistSimList
  
}

CalculateMSYHistSim <- function(HistSim, TimeSteps) {
  # TODO - add Frange to OM@control 
  logApicalFRange <- log(c(0.01, 3))
  
  StockList <- HistSim@OM@Stock
  FleetList <- HistSim@OM@Fleet
  Allocation <- HistSim@OM@Allocation

  # if Complexes or SPFrom another stock - calculate overall MSY
  Complexes <- HistSim@OM@Complexes
  SPFrom <- purrr::map(StockList, \(stock) stock@SRR@SPFrom) |> unlist()
  
  if (length(Complexes)>0) {
    # stock complex - calculate MSY for complex TODO
    cli::cli_alert_warning('MSY calculations for Stock Complexes not complete. Calculating MSY ref points by stock')
  }
  
  
  # TODO - fix for complexes, multi-sex, etc
  if (all(SPFrom==SPFrom[1]) & length(SPFrom)>1) {
    # stock complex - calculate MSY for complex
    opt <- optimize(CalculateMSY_Complex, 
                    logApicalFRange, 
                    StockList, 
                    FleetList, 
                    Allocation, 
                    TimeSteps=TimeSteps)
    
    MSYRefs <- CalculateMSY_Complex(opt$minimum,
                         StockList, 
                         FleetList, 
                         Allocation, 
                         TimeSteps=TimeSteps,2)
  } else {
    # By Stock
    MSYRefs <- purrr::pmap(list(StockList, FleetList, Allocation), \(stock, fleet, allocation) {
      opt <- optimize(CalculateMSY_SingleStock, 
                      logApicalFRange, 
                      Stock=stock, 
                      Fleet=fleet, 
                      Allocation=allocation, 
                      TimeSteps=TimeSteps)
      CalculateMSY_SingleStock(opt$minimum,
                               stock, fleet, allocation, TimeSteps,2)
    })
  }

  RefPoints@SPR0 <- List2Array(purrr::map(MSYRefs, \(stock) stock$SPR0), 'Stock') |> t()
  RefPoints@RemovalsMSY <- List2Array(purrr::map(MSYRefs, \(stock) stock$Removals), 'Stock') |> t()
  RefPoints@LandingsMSY <- List2Array(purrr::map(MSYRefs, \(stock) stock$Landings), 'Stock')|> t()
  RefPoints@FMSY <- List2Array(purrr::map(MSYRefs, \(stock) stock$F), 'Stock', 'TimeStep')|> t()
  dimnames(RefPoints@FMSY)[[2]] <- TimeSteps
  RefPoints@BMSY <- List2Array(purrr::map(MSYRefs, \(stock) stock$Biomass), 'Stock') |> t()
  RefPoints@SBMSY <- List2Array(purrr::map(MSYRefs, \(stock) stock$SBiomass), 'Stock') |> t()
  RefPoints@SPRMSY <- List2Array(purrr::map(MSYRefs, \(stock) stock$SPR), 'Stock') |> t()
  RefPoints
}


CalculateMSY_SingleStock <- function(logApicalF, Stock, Fleet, Allocation, TimeSteps, option=1) {
  
  apicalF <- exp(logApicalF)
  
  PerRecruit <- CalcPerRecruit(apicalF, 
                               Stock, 
                               Fleet, 
                               Allocation,
                               TimeSteps)
  
  RecPars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetTimeStep(pars,TimeSteps))
  RelRecruits <- Stock@SRR@RelRecFun(Pars=RecPars, SPR=PerRecruit$SPR)
  
  RelRecruits[RelRecruits<0] <- 0
  
  R0 <- Stock@SRR@R0 |> ArraySubsetTimeStep(TimeSteps)
  Recruits <- ArrayMultiply(R0, RelRecruits)
  
  Removals <- ArrayMultiply(PerRecruit$RemovalsPR, Recruits)
  Landings <- ArrayMultiply(PerRecruit$LandingsPR, Recruits)
  SBiomass <- ArrayMultiply(PerRecruit$SBiomassPR, Recruits)
  Biomass <- ArrayMultiply(PerRecruit$BiomassPR, Recruits)
  
  if (option==1) {
    # TODO add option to calculate MSY in terms of removals or landings
    return(-Removals)
  }
  
  PerRecruit$Removals <- Removals
  PerRecruit$Landings <- Landings
  PerRecruit$SBiomass <- SBiomass
  PerRecruit$Biomass <- Biomass
  PerRecruit$Recruits <- Recruits
  PerRecruit$RelRecruits <- RelRecruits
  PerRecruit$Recruits <- Recruits
  PerRecruit
  
}


CalculateMSY_Complex <- function(logApicalF, StockList, FleetList, Allocation,TimeSteps, option=1) {
  
  apicalF <- exp(logApicalF)
  
  PerRecruit <- MakeNamedList(names(StockList))
  for (st in seq_along(StockList)) {
    PerRecruit[[st]] <- CalcPerRecruit(apicalF, 
                                       StockList[[st]], 
                                       FleetList[[st]], 
                                       Allocation[[st]], 
                                       TimeSteps)
  }
  
  # TODO - this approach could be problematic if there are different 
  # apical Fs by sex or stocks within complex ...
  
  for (st in seq_along(StockList)) {
    Stock <- StockList[[st]]
    
    SPFrom <- Stock@SRR@SPFrom 
    SPR <- PerRecruit[[SPFrom]]$SPR
    RecPars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetTimeStep(pars,TimeSteps))
    RelRecruits <- Stock@SRR@RelRecFun(Pars=RecPars, SPR=SPR)
    RelRecruits[RelRecruits<0] <- 0
    
    R0 <- Stock@SRR@R0 |> ArraySubsetTimeStep(TimeSteps)
    Recruits <- ArrayMultiply(R0, RelRecruits)
    
    Removals <- ArrayMultiply(PerRecruit[[st]]$RemovalsPR, Recruits)
    Landings <- ArrayMultiply(PerRecruit[[st]]$LandingsPR, Recruits)
    SBiomass <- ArrayMultiply(PerRecruit[[st]]$SBiomassPR, Recruits)
    Biomass <- ArrayMultiply(PerRecruit[[st]]$BiomassPR, Recruits)
    
    PerRecruit[[st]]$Removals <- Removals
    PerRecruit[[st]]$Landings <- Landings
    PerRecruit[[st]]$SBiomass <- SBiomass
    PerRecruit[[st]]$Biomass <- Biomass
    PerRecruit[[st]]$Recruits <- Recruits
    PerRecruit[[st]]$RelRecruits <- RelRecruits
    PerRecruit[[st]]$Recruits <- Recruits
  }
  
  if (option==1) {
    # TODO add option to calculate MSY in terms of removals or landings
    Removals <- purrr::map(PerRecruit, \(stock) stock$Removals) |> unlist() |> sum()
    return(-Removals)  
  }
  PerRecruit
  
}


# CalculateMSY <- function(logApicalF, StockList, FleetList, Allocation, SPR0=NULL, TimeSteps, option=1) {
#   
#   apicalF <- exp(logApicalF)
#   
#   # Loop over Stocks
#   for (st in seq_along(StockList)) {
#     Stock <- StockList[[st]]
#     
#     SPFrom <- Stock@SRR@SPFrom 
#     SPR <- PerRecruit[[SPFrom]]$SPR
#     RecPars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetTimeStep(pars,TimeSteps))
#     RelRecruits <- Stock@SRR@RelRecFun(Pars=RecPars, SPR=SPR)
#     RelRecruits[RelRecruits<0] <- 0
#     
#     R0 <- Stock@SRR@R0 |> ArraySubsetTimeStep(TimeSteps)
#     Recruits <- ArrayMultiply(R0, RelRecruits)
#     
#     Removals <- ArrayMultiply(PerRecruit[[st]]$RemovalsPR, Recruits)
#     Landings <- ArrayMultiply(PerRecruit[[st]]$LandingsPR, Recruits)
#     SBiomass <- ArrayMultiply(PerRecruit[[st]]$SBiomassPR, Recruits)
#     Biomass <- ArrayMultiply(PerRecruit[[st]]$BiomassPR, Recruits)
#     
#     PerRecruit[[st]]$Removals <- Removals
#     PerRecruit[[st]]$Landings <- Landings
#     PerRecruit[[st]]$SBiomass <- SBiomass
#     PerRecruit[[st]]$Biomass <- Biomass
#     PerRecruit[[st]]$Recruits <- Recruits
#     PerRecruit[[st]]$RelRecruits <- RelRecruits
#     PerRecruit[[st]]$Recruits <- Recruits
#   }
#   
#   if (option==1) {
#     # TODO add option to calculate MSY in terms of removals or landings
#     return(-Removals)
#   }
#   
# 
#   
#   
#   # PerRecruit <- CalcPerRecruit(apicalF, Stock, Fleet, Allocation, SPR0, TimeSteps)
#   
#   RecPars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetTimeStep(pars,TimeSteps))
#   RelRecruits <- Stock@SRR@RelRecFun(Pars=RecPars, SPR=PerRecruit$SPR)
#   
#   RelRecruits[RelRecruits<0] <- 0
#   
#   R0 <- Stock@SRR@R0 |> ArraySubsetTimeStep(TimeSteps)
#   Recruits <- ArrayMultiply(R0, RelRecruits)
#   
#   Removals <- ArrayMultiply(PerRecruit$RemovalsPR, Recruits)
#   Landings <- ArrayMultiply(PerRecruit$LandingsPR, Recruits)
#   SBiomass <- ArrayMultiply(PerRecruit$SBiomassPR, Recruits)
#   Biomass <- ArrayMultiply(PerRecruit$BiomassPR, Recruits)
# 
#   if (option==1) {
#     # TODO add option to calculate MSY in terms of removals or landings
#     return(-Removals)
#   }
#   PerRecruit$Removals <- Removals
#   PerRecruit$Landings <- Landings
#   PerRecruit$SBiomass <- SBiomass
#   PerRecruit$Biomass <- Biomass
#   PerRecruit$Recruits <- Recruits
#   PerRecruit$RelRecruits <- RelRecruits
#   PerRecruit$Recruits <- Recruits
#   PerRecruit
# }





