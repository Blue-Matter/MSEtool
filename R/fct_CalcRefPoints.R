CalcReferencePoints <- function(OM, TimeSteps=NULL, silent=FALSE) {
  
  if (is.null(TimeSteps))
    TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
  
  StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
  StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
  CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
  
  Complexes <- OM@Complexes
  
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

CalcRefPointsSim <- function(StockSimList, FleetSimList, CatchFracSimList, Complexes, TimeSteps) {
  IdenticalAcrossSims <- IdenticalSims(StockSimList, TimeSteps) &
    IdenticalSims(FleetSimList, TimeSteps) &
    IdenticalSims(CatchFracSimList, TimeSteps, EditSlots=FALSE)
  

  RefPoints <- new('refpoints')
  

  
  

  
}


#' @export
CalculateMSY <- function(OM, TimeSteps=NULL) {
  
  if (is.null(TimeSteps))
    TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
  
  StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
  StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
  CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
  
  Complexes <- OM@Complexes
  
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
  
  # TODO identical Sims
  IdenticalAcrossSims <- IdenticalSims(StockSimList, TimeSteps) &
    IdenticalSims(FleetSimList, TimeSteps) &
    IdenticalSims(CatchFracSimList, TimeSteps, EditSlots=FALSE)
  
  if (IdenticalAcrossSims) {
    MSYRefPointsList <- CalculateMSYSim(StockSimList[[1]], 
                                        FleetSimList[[1]], 
                                        CatchFracSimList[[1]],
                                        Complexes,
                                        TimeSteps)
    
    MSYRefPointsList <- replicate(OM@nSim, MSYRefPointsList, simplify = FALSE)
    names(MSYRefPointsList) <- 1:OM@nSim
  } else {
    MSYRefPointsList <- purrr::pmap(list(StockSimList, FleetSimList, CatchFracSimList), 
                                    CalculateMSYSim, Complexes=Complexes, TimeSteps=TimeSteps)
  }
  
  
  MSYRefPoints <- new("msyrefpoints")
  slots <- slotNames(MSYRefPoints)
  for (sl in slots) {
    slot(MSYRefPoints, sl) <- purrr::map(MSYRefPointsList, slot, sl) |> 
      List2Array("Sim") |> 
      aperm(c('Sim', 'Stock', 'TimeStep'))
  }
  MSYRefPoints
}


CalculateMSYSim <- function(StockList, FleetList, CatchFracList, Complexes, TimeSteps=NULL) {
  # TODO - add Frange to OM@control 
  logApicalFRange <- log(c(0.01, 3))
  
  # if Complexes or SPFrom another stock - calculate overall MSY
  SPFrom <- purrr::map(StockList, \(stock) stock@SRR@SPFrom) |> unlist()
  if (is.null(SPFrom))
    SPFrom <- 1:length(StockList)
  
  if (length(Complexes)>0) {
    # stock complex - calculate MSY for complex TODO
    cli::cli_alert_warning('MSY calculations for Stock Complexes not complete. Calculating MSY ref points by stock')
  }
  

  
  # By Stock
  MSYRefs <- purrr::pmap(list(StockList, FleetList, CatchFracList), \(Stock, Fleet, CatchFrac) {
    opt <- optimize(CalculateMSY_SingleStock, 
                    logApicalFRange, 
                    Stock=Stock, 
                    Fleet=Fleet, 
                    CatchFrac=CatchFrac, 
                    TimeSteps=TimeSteps)
    CalculateMSY_SingleStock(opt$minimum,
                             Stock, Fleet, CatchFrac, TimeSteps,2)
  })
  
  MSYRefPoints <- new("msyrefpoints")
  MSYRefPoints@FMSY <- purrr::map(MSYRefs, slot, 'FMSY') |> List2Array('Stock') 
  MSYRefPoints@BMSY <- purrr::map(MSYRefs, slot, 'BMSY') |>
    List2Array('Stock') |>
    aperm(c('Stock', 'TimeStep'))
  
  MSYRefPoints@SBMSY <- purrr::map(MSYRefs, slot, 'SBMSY') |>
    List2Array('Stock') |>
    aperm(c('Stock', 'TimeStep'))
  
  MSYRefPoints@SPMSY <- purrr::map(MSYRefs, slot, 'SPMSY') |>
    List2Array('Stock') |>
    aperm(c('Stock', 'TimeStep'))
  
  
  MSYRefPoints@MSYRemovals <- purrr::map(MSYRefs, slot, 'MSYRemovals') |>
    List2Array('Stock') |>
    aperm(c('Stock', 'TimeStep'))
  
  MSYRefPoints@MSYLandings <- purrr::map(MSYRefs, slot, 'MSYLandings') |>
    List2Array('Stock') |>
    aperm(c('Stock', 'TimeStep'))
  
  return(MSYRefPoints)
  
  # Female/Male & Stock Complex 
  # TODO 
  
}


CalculateMSY_SingleStock <- function(logApicalF, Stock, Fleet, CatchFrac, TimeSteps, option=1) {
  
  if (length(logApicalF)>1) {
    cli::cli_alert_danger('{.var logApicalF} must be length 1. Using first value {.val {logApicalF[1]}}')
    logApicalF <- logApicalF[1]
  }
  if (length(TimeSteps)>1) {
    cli::cli_alert_danger('{.var TimeSteps} must be length 1. Using last value {.val {tail(TimeSteps,1)}}')
    TimeSteps <- tail(TimeSteps,1)
  }
    
  apicalF <- exp(logApicalF)
  
  PerRecruit <- CalcPerRecruitStock(apicalF, 
                                    Stock, 
                                    Fleet, 
                                    CatchFrac,
                                    TimeSteps)
  
  
  RecPars <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetTimeStep(pars,TimeSteps))
  
  if (!is.null(Stock@SRR@Model) && inherits(Stock@SRR@Model, 'character')) {
    if (is.null(Stock@SRR@RelRecFun)) {
      mod <- get(paste0(Stock@SRR@Model, 'RelRec'))
      class(mod) <- 'function'
      Stock@SRR@RelRecFun <- mod
    }
  }

  RelRecruits <- Stock@SRR@RelRecFun(Pars=RecPars, SPR=PerRecruit@SPR[1])
  RelRecruits[RelRecruits<0] <- 0
  
  R0 <- Stock@SRR@R0 |> ArraySubsetTimeStep(TimeSteps)
  Recruits <- ArrayMultiply(R0, RelRecruits) |>
    AddDimension("F")
  
  Removals <- ArrayMultiply(PerRecruit@Removals, Recruits) |> DropDimension("F")
  
  if (option==1) {
    # TODO add option to calculate MSY in terms of removals or landings
    return(-Removals[1])
  }
  
  Biomass <- ArrayMultiply(PerRecruit@Biomass, Recruits) |> DropDimension("F")
  SBiomass <- ArrayMultiply(PerRecruit@SBiomass, Recruits) |> DropDimension("F")
  SProduction <- ArrayMultiply(PerRecruit@SProduction, Recruits) |> DropDimension("F")
  Landings <- ArrayMultiply(PerRecruit@Landings, Recruits) |> DropDimension("F")
  
  MSYRefPoints <- new("msyrefpoints")
  MSYRefPoints@FMSY <- array(apicalF, length(apicalF), dimnames=list(TimeStep=TimeSteps))
  MSYRefPoints@BMSY <- Biomass 
  MSYRefPoints@SBMSY <- SBiomass 
  MSYRefPoints@SPMSY <- SProduction 
  MSYRefPoints@MSYRemovals <- Removals 
  if (!all.equal(Landings,Removals))
    MSYRefPoints@MSYLandings <- Landings 
  
  MSYRefPoints
  
}

