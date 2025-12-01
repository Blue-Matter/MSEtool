setdnames <- function(dnames, BySim=TRUE) {
  if (BySim & !"Sim" %in% dnames) 
    dnames <- c('Sim', dnames)
  dnames
}


# TODO - update for new OptMSY and CalculateMSYSim

# CalculateMSY <- function(OM, Years=NULL) {
#   
#   if (is.null(Years))
#     Years <- OM |> Years('Historical') |> tail(1)
#   
#   StockList <- PopulateStockList(OM) |> SubsetYear(Years, AddPast = FALSE)
#   StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetYear(Years)
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
#                                       Years=Years,
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
#   CalculateMSYSimList(StockSimList, FleetSimList, CatchFracSimList, Complexes, Years)
#   
# }


# CalculateMSYSimList <- function(StockSimList, FleetSimList, CatchFracSimList, Complexes, Years) {
#   IdenticalAcrossSims <- IdenticalSims(StockSimList, Years) &
#     IdenticalSims(FleetSimList, Years) &
#     IdenticalSims(CatchFracSimList, Years, EditSlots=FALSE)
#   
#   if (IdenticalAcrossSims) {
#     MSYRefPointsList <- CalculateMSYSim(StockSimList[[1]], 
#                                         FleetSimList[[1]], 
#                                         CatchFracSimList[[1]],
#                                         Complexes,
#                                         Years)
#     
#     MSYRefPointsList <- replicate(OM@nSim, MSYRefPointsList, simplify = FALSE)
#     names(MSYRefPointsList) <- 1:OM@nSim
#   } else {
#     MSYRefPointsList <- purrr::pmap(list(StockSimList, FleetSimList, CatchFracSimList), 
#                                     CalculateMSYSim, Complexes=Complexes, Years=Years,
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
#       aperm(c('Sim', 'Stock', 'Year'))
#   }
#   MSYRefPoints
# }

CalcMSYRefPoints <- function(SimList, RefPointYears, RefPointsMSY=TRUE) {
  
  if (inherits(RefPointsMSY, 'logical') && RefPointsMSY) {
    if (CheckIdenticalSims(SimList, Equilibrium=TRUE)) {
      SimOne <- SimList[[1]]
      SimOne@RefPointsMSY <- CalculateMSYSim(StockList=SimOne@OM@Stock,
                                             FleetList=SimOne@OM@Fleet,                                  
                                             Complexes=SimOne@OM@Complexes,
                                             Years = RefPointYears,
                                             maxF=SimOne@OM@maxF)
      
      SimList <- purrr::map(SimList, \(HistSim) {
        HistSim@RefPointsMSY <- SimOne@RefPointsMSY
        HistSim
      })
    } else {
      SimList <- purrr::map(SimList, \(HistSim) {
        HistSim@RefPointsMSY <- CalculateMSYSim(StockList=HistSim@OM@Stock,
                                                FleetList=HistSim@OM@Fleet,                                  
                                                Complexes=HistSim@OM@Complexes,
                                                Years = RefPointYears,
                                                maxF=HistSim@OM@maxF)
        HistSim
      }, .progress = list(
        type = "iterator",
        format = "Calculating MSY Reference Points {cli::pb_bar} {cli::pb_percent}",
        clear = TRUE))
    }
  } 
  class(SimList) <- 'simlist'
  SimList
}


CalculateMSYSim <- function(StockList, FleetList, Complexes, Years=NULL, maxF=3) {
  logApicalFRange <- log(c(1E-5, maxF))
  
  MSYRefPoints <- RefPointsMSY(StockNames=names(StockList), Years=Years)
  for (st in seq_along(Complexes)) {
    StockInd <- Complexes[[st]]
    StockList_ <- StockList[StockInd]
    FleetList_ <- FleetList[StockInd]
    
    for (ts in seq_along(Years)) {
      opt <- optimize(OptMSY, 
                      logApicalFRange, 
                      StockList_, 
                      FleetList_, 
                      Years=Years[ts])
      MSYRefs <- OptMSY(opt$minimum, StockList_, FleetList_, Years[ts],2)
      
      for (sl in slotNames(MSYRefs)) {
        val <- slot(MSYRefs,sl)
        if (!is.null(val))
          ArrayFill(slot(MSYRefPoints,sl)) <- val
          
      }
    }
  }
  MSYRefPoints
}


OptMSY <- function(logApicalF, StockList, FleetList, Years, option=1) {
  
  if (length(logApicalF)>1) {
    cli::cli_alert_danger('{.var logApicalF} must be length 1. Using first value {.val {logApicalF[1]}}')
    logApicalF <- logApicalF[1]
  }
  
  if (length(Years)>1) {
    cli::cli_alert_danger('{.var Years} must be length 1. Using last value {.val {tail(Years,1)}}')
    Years <- tail(Years,1)
  }
  
  apicalF <- exp(logApicalF)
  
  PerRecruit <- CalcPerRecruit_StockList(apicalF, 
                                         StockList, 
                                         FleetList, 
                                         Years)
  
  SPFrom <- purrr::map(StockList, \(stock) stock@SRR@SPFrom) |> unlist()
  if (is.null(SPFrom))
    SPFrom <- 1:length(StockList)
  
  SPR0List <- PerRecruit@SPR0 |> Array2List(1)
  
  RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
    Pars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetYear(pars,Years))
    Pars$R0 <- ArraySubsetYear(Stock@SRR@R0, Years)
    Pars$SPR0 <- ArraySubsetYear(SPR0, Years)
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
    List2Array('Stock') |> aperm(c('Stock', 'Year'))
  
  
  R0 <- purrr::map(StockList, \(Stock) Stock@SRR@R0 |> ArraySubsetYear(Years)) |>
    List2Array('Stock') |> aperm(c('Stock', 'Year'))
  
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
  
  
  MSYRefPoints <- new("refpointsMSY")
  MSYRefPoints@FMSY <- array(apicalF, dim(Biomass), dimnames=dimnames(Biomass))
  MSYRefPoints@BMSY <- Biomass 
  MSYRefPoints@SBMSY <- SBiomass 
  MSYRefPoints@SPMSY <- SProduction 
  MSYRefPoints@SPRMSY <- SPR 
  MSYRefPoints@MSY <- Removals 
  # if (!all(Landings == Removals))
  MSYRefPoints@MSYLandings <- Landings 
  MSYRefPoints
}



