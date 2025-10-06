setdnames <- function(dnames, BySim=TRUE) {
  if (BySim & !"Sim" %in% dnames) 
    dnames <- c('Sim', dnames)
  dnames
}


# TODO - update for new OptMSY and CalculateMSYSim

# CalculateMSY <- function(OM, TimeSteps=NULL) {
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
#   
#   StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
#   StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
#   CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
#   
#   Complexes <- OM@Complexes
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
#   StockSimList <- purrr::map(1:nSim(OM), \(x) {
#     SubsetSim(StockList, Sim=x, drop=TRUE)
#   }, .progress = 'Building StockList')
#   names(StockSimList) <- 1:nSim(OM)
#   
#   FleetSimList <- purrr::map(1:nSim(OM), \(x) {
#     SubsetSim(FleetList, Sim=x, drop=TRUE)
#   }, .progress = 'Building FleetList')
#   names(FleetSimList) <- 1:nSim(OM)
#   
#   CatchFracSimList <- purrr::map(1:nSim(OM), \(x) {
#     SubsetSim(CatchFracList, Sim=x, drop=TRUE)
#   })
#   names(CatchFracSimList) <- 1:nSim(OM)
#   
#   CalculateMSYSimList(StockSimList, FleetSimList, CatchFracSimList, Complexes, TimeSteps)
#   
# }


# CalculateMSYSimList <- function(StockSimList, FleetSimList, CatchFracSimList, Complexes, TimeSteps) {
#   IdenticalAcrossSims <- IdenticalSims(StockSimList, TimeSteps) &
#     IdenticalSims(FleetSimList, TimeSteps) &
#     IdenticalSims(CatchFracSimList, TimeSteps, EditSlots=FALSE)
#   
#   if (IdenticalAcrossSims) {
#     MSYRefPointsList <- CalculateMSYSim(StockSimList[[1]], 
#                                         FleetSimList[[1]], 
#                                         CatchFracSimList[[1]],
#                                         Complexes,
#                                         TimeSteps)
#     
#     MSYRefPointsList <- replicate(OM@nSim, MSYRefPointsList, simplify = FALSE)
#     names(MSYRefPointsList) <- 1:OM@nSim
#   } else {
#     MSYRefPointsList <- purrr::pmap(list(StockSimList, FleetSimList, CatchFracSimList), 
#                                     CalculateMSYSim, Complexes=Complexes, TimeSteps=TimeSteps,
#                                     .progress = list(
#                                       type = "iterator",
#                                       format = "Calculating MSY Reference Points {cli::pb_bar} {cli::pb_percent}",
#                                       clear = TRUE))
#   }
#   
#   # StockList <- StockSimList$`1`
#   # FleetList <- FleetSimList$`1`
#   # CatchFracList <- CatchFracSimList$`1`
#   
#   MSYRefPoints <- new("msyrefpoints")
#   slots <- slotNames(MSYRefPoints)
#   for (sl in slots) {
#     slot(MSYRefPoints, sl) <- purrr::map(MSYRefPointsList, slot, sl) |> 
#       List2Array("Sim") |> 
#       aperm(c('Sim', 'Stock', 'TimeStep'))
#   }
#   MSYRefPoints
# }



CalculateMSYSim <- function(StockList, FleetList, Complexes, TimeSteps=NULL, maxF=3) {
  logApicalFRange <- log(c(0.01, maxF))
  
  MSYRefPoints <- MSYRefPoints(StockNames=names(StockList), TimeSteps=TimeSteps)
  for (st in seq_along(Complexes)) {
    StockInd <- Complexes[[st]]
    StockList_ <- StockList[StockInd]
    FleetList_ <- FleetList[StockInd]
    
    for (ts in seq_along(TimeSteps)) {
      opt <- optimize(OptMSY, 
                      logApicalFRange, 
                      StockList_, 
                      FleetList_, 
                      TimeSteps=TimeSteps[ts])
      MSYRefs <- OptMSY(opt$minimum, StockList_, FleetList_, TimeSteps[ts],2)
      
      for (sl in slotNames(MSYRefs)) {
        val <- slot(MSYRefs,sl)
        if (!is.null(val))
          ArrayFill(slot(MSYRefPoints,sl)) <- val
          
      }
    }
  }
  MSYRefPoints
}


OptMSY <- function(logApicalF, StockList, FleetList, TimeSteps, option=1) {
  
  if (length(logApicalF)>1) {
    cli::cli_alert_danger('{.var logApicalF} must be length 1. Using first value {.val {logApicalF[1]}}')
    logApicalF <- logApicalF[1]
  }
  
  if (length(TimeSteps)>1) {
    cli::cli_alert_danger('{.var TimeSteps} must be length 1. Using last value {.val {tail(TimeSteps,1)}}')
    TimeSteps <- tail(TimeSteps,1)
  }
  
  apicalF <- exp(logApicalF)
  
  PerRecruit <- CalcPerRecruit_StockList(apicalF, 
                                         StockList, 
                                         FleetList, 
                                         TimeSteps)
  SPFrom <- purrr::map(StockList, \(stock) stock@SRR@SPFrom) |> unlist()
  if (is.null(SPFrom))
    SPFrom <- 1:length(StockList)
  
  SPR0List <- PerRecruit@SPR0 |> Array2List(1)
  
  RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
    Pars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetTimeStep(pars,TimeSteps))
    Pars$R0 <- ArraySubsetTimeStep(Stock@SRR@R0, TimeSteps)
    Pars$SPR0 <- ArraySubsetTimeStep(SPR0, TimeSteps)
    Pars
  })
  
  RecParsList <- RecParsList[SPFrom]
  names(RecParsList) <-  names(SPFrom)
  
  RelRecFunList <- purrr::map(StockList, \(Stock) {
    if (!is.null(Stock@SRR@Model) && inherits(Stock@SRR@Model, 'character')) {
      if (is.null(Stock@SRR@RelRecFun)) {
        mod <- get(paste0(Stock@SRR@Model, 'RelRec'))
        class(mod) <- 'function'
        Stock@SRR@RelRecFun <- mod
      }
    }
    Stock@SRR@RelRecFun 
  })
  
  SPRList <- PerRecruit@SPR |> Array2List(1)
  RelRecruits <- purrr::pmap(list(RecParsList, SPRList, RelRecFunList), \(RecPars, SPR, RelRecFun) {
    RelRecruits <- RelRecFun(Pars=RecPars, SPR=PerRecruit@SPR[1])
    RelRecruits[RelRecruits<0] <- 0
    RelRecruits
  }) |>
    List2Array('Stock') |> aperm(c('Stock', 'TimeStep'))
  
  
  R0 <- purrr::map(StockList, \(Stock) Stock@SRR@R0 |> ArraySubsetTimeStep(TimeSteps)) |>
    List2Array('Stock') |> aperm(c('Stock', 'TimeStep'))
  

  Recruits <- ArrayMultiply(R0, RelRecruits) |>  AddDimension("F")
  Removals <- ArrayMultiply(PerRecruit@Removals, Recruits) |> DropDimension("F")
  
  if (option==1) {
    # TODO add option to calculate MSY in terms of removals or landings
    return(-sum(Removals))
  }
  
  Biomass <- ArrayMultiply(PerRecruit@Biomass, Recruits) |> DropDimension("F")
  SBiomass <- ArrayMultiply(PerRecruit@SBiomass, Recruits) |> DropDimension("F")
  SProduction <- ArrayMultiply(PerRecruit@SProduction, Recruits) |> DropDimension("F")
  SPR <- PerRecruit@SPR |> DropDimension("F")
  Landings <- ArrayMultiply(PerRecruit@Landings, Recruits) |> DropDimension("F")
  
  
  MSYRefPoints <- new("msyrefpoints")
  MSYRefPoints@FMSY <- array(apicalF, dim(Biomass), dimnames=dimnames(Biomass))
  MSYRefPoints@BMSY <- Biomass 
  MSYRefPoints@SBMSY <- SBiomass 
  MSYRefPoints@SPMSY <- SProduction 
  MSYRefPoints@SPRMSY <- SPR 
  MSYRefPoints@MSYRemovals <- Removals 
  # if (!all(Landings == Removals))
  MSYRefPoints@MSYLandings <- Landings 
  
  MSYRefPoints
}



