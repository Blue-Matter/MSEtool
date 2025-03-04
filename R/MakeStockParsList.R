
GetMetaData <- function(OM, Period=c('Historical', 'Projection', 'All'), TimeSteps=NULL) {
  
  Period <- match.arg(Period)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
  
  nAges <- unlist(purrr::map(OM@Stock, nAge)) |> max()

  
  nAreas <- unlist(purrr::map(OM@Stock, nArea)) |> unique()
  if (length(nAreas)>1) 
    cli::cli_abort('All Stocks must have the same number of areas')
  
  list(nSim=nSim(OM),
       nAges=nAges,
       nAreas=nAreas,
       StockNames=StockNames(OM),
       TimeSteps=TimeSteps,
       Period=Period,
       FleetNames=FleetNames(OM))
  
}

MakeSimList <- function(List, Sim=1) {
  nms <- names(List)
  for (i in seq_along(nms)) {
    if (inherits(List[[i]], 'array')) {
      if ("Sim" %in% names(dimnames(List[[i]]))) {
        List[[i]] <- Array2List(List[[i]], 'Sim')[[Sim]]
      }
    } else if (inherits(List[[i]], 'list')) {
      List[[i]] <- Recall(List[[i]])
    }
  } 
  
  List
}


MakePopulationList <- function(OM, Period='Historical') {
  
  
  List <- list()
  
  List$Ages <- purrr::map(OM@Stock, slot, 'Ages')
  List$Length <- MakeStockSlotList(OM, 'Length', Period)
  List$Weight <- MakeStockSlotList(OM, 'Weight', Period)
  List$NaturalMortality <- MakeStockSlotList(OM, 'NaturalMortality', Period)
  List$Maturity <- MakeStockSlotList(OM, 'Maturity', Period)
  List$Fecundity <- MakeStockSlotList(OM, 'Fecundity', Period)
  List$SRR <- MakeSRRList(OM, Period)
  List$Spatial <- MakeSpatialList(OM, Period)
  List$Depletion <- MakeDepletionList(OM, Period)
  
  # Arrays to be  filled 
  if (Period=='Historical') {
    # include first projection time step for historical numbers
    TSNumber <- c(TimeSteps(OM, 'Historical'), 
                  TimeSteps(OM, 'Projection')[1])
  } else {
    TSNumber <- TimeSteps(OM, 'Projection')
  }
  
  List$NumberAtAgeArea <- ArraySimStockAgeTimeArea(OM, TimeSteps=TSNumber) 
  List$BiomassArea <- ArraySimStockAgeTimeArea(OM, Period) |>
    DropDimension('Age', warn=FALSE) 
  List$SBiomassArea <- List$BiomassArea 
  List$SProduction <-  List$BiomassArea |>
    DropDimension('Area', warn=FALSE)
  
  List$CurrentYear <- purrr::map(OM@Stock, \(x) slot(x, 'CurrentYear'))
  List$TimeUnits <- purrr::map(OM@Stock, \(x) slot(x, 'TimeUnits'))
  List$TimeSteps <- purrr::map(OM@Stock, \(x) slot(x, 'TimeSteps'))
  List$TimeStepsHist <- purrr::map(OM@Stock, \(x) TimeSteps(x, 'Historical'))
  List$TimeStepsProj <- purrr::map(OM@Stock, \(x) TimeSteps(x, 'Projection'))
  List$TimeStepsPerYear <- purrr::map(OM@Stock, \(x) slot(x, 'TimeStepsPerYear'))
  List$Misc <- purrr::map(OM@Stock, \(x) slot(x, 'Misc'))
  List$Sim <- as.list(1:nSim(OM))
  
  List
}

MakeStockSlotList <- function(OM, slot='Length', Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas

  sNames <- slotNames(slot(OM@Stock[[1]], slot))
  
  List <- list()
  
  if ('Pars' %in% sNames) {
    List[['Pars']] <- purrr::map(OM@Stock, \(x)
                                               x |> 
                                                 slot(slot) |>
                                                 slot('Pars'))
    
    for (i in seq_along(List[['Pars']])) {
      stList <-  List[['Pars']][[i]]
      for (j in seq_along(stList)) {
        List[['Pars']][[i]][[j]] <-  List[['Pars']][[i]][[j]] |>  ArrayExpand(nSim, nAges, TimeSteps)
      }
    }
    
  }
    
  if ('Model' %in% sNames) 
    List[['Model']] <- purrr::map(OM@Stock, \(x)
                                               x |> 
                                                 slot(slot) |>
                                                 slot('Model'))
  
  if ('MeanAtAge' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtAge'))
    List[['MeanAtAge']] <- fun(OM@Stock, TimeSteps) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
   
  if ('MeanAtLength' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtLength'))
    List[['MeanAtLength']] <- fun(OM@Stock, TimeSteps) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Class', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  
  if ('CVatAge' %in% sNames) {
    fun <- get('GetCVAtAge')
    List[['CVAtAge']] <- fun(OM@Stock, TimeSteps, slot) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  if ('Dist' %in% sNames)
    List[['Dist']] <- purrr::map(OM@Stock, \(x) x |> 
                                                 slot(slot) |>
                                                 slot('Dist'))
  
  if ('TruncSD' %in% sNames)
    List[['TruncSD']] <- purrr::map(OM@Stock, \(x) x |> 
                                                 slot(slot) |>
                                                 slot('TruncSD'))
  
  if ('Timing' %in% sNames)
    List[[paste0(slot, 'Timing')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                    slot(slot) |>
                                                    slot('Timing'))
  
  # TODO
  if ('Random' %in% sNames)
    List[['Random']] <- purrr::map(OM@Stock, \(x) x |> 
                                                   slot(slot) |>
                                                   slot('Random'))
  
 # TODO
  if ('ASK' %in% sNames)
    List[['ASK']] <- purrr::map(OM@Stock, \(x) x |> 
                                                   slot(slot) |>
                                                   slot('ASK'))
  
  if ('Classes' %in% sNames)
    List[['Classes']] <- purrr::map(OM@Stock, \(x) x |> 
                                                slot(slot) |>
                                                slot('Classes'))
  
  if ('Semelparous' %in% sNames) {
    List[['Semelparous']] <- GetSemelparous(OM@Stock, TimeSteps) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  
  if ('Misc' %in% sNames)
    List[['Misc']] <- purrr::map(OM@Stock, \(x) x |> 
                                                    slot(slot) |>
                                                    slot('Misc'))
  List
}


MakeSRRList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  
  List <- list()
  
  List$SRRPars <- purrr::map(OM@Stock, \(x)
                          x |> 
                            slot('SRR') |>
                            slot('Pars')) 
  
  for (i in seq_along(List$SRRPars)) {
    stList <- List$SRRPars[[i]]
    for (j in seq_along(stList)) {
      List$SRRPars[[i]][[j]] <- List$SRRPars[[i]][[j]] |>  ArrayExpand(nSim, nAges, TimeSteps)
    }
  }
                              
  List$SRRModel <- purrr::map(OM@Stock, \(x)
                          x |> 
                            slot('SRR') |>
                            slot('Model') |>
                            get()) 
 
  List$R0 <- purrr::map(OM@Stock, \(x) 
                        x |> slot('SRR') |> slot('R0')
                        ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 
  
  List$SD <- purrr::map(OM@Stock, \(x) 
                        x |> slot('SRR') |> slot('SD')
                        ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 
  
  List$AC <- purrr::map(OM@Stock, \(x) 
                        x |> slot('SRR') |> slot('AC')
                        ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 
  
  List$SPFrom <- purrr::map(OM@Stock, \(x) 
    x |> slot('SRR') |> slot('SPFrom')) 
  
  for (i in 1:nStock(OM)) {
    SPFrom <- List$SPFrom[[i]]
    if (is.null(SPFrom)) {
      List$SPFrom[[i]] <- i
    } else {
      List$SPFrom[[i]] <- match(SPFrom, StockNames(OM))
    }
     
  }
  
  List$RecDevInit <- purrr::map(OM@Stock, \(x) 
                                x |> slot('SRR') |> slot('RecDevInit')
                                ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age')) |>
    ExpandSims(nSim) 
  
  List$RecDevHist <- purrr::map(OM@Stock, \(x) 
                                x |> slot('SRR') |> slot('RecDevHist')
                                ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step')) |>
    ExpandSims(nSim) 
  
  List$RecDevProj <- purrr::map(OM@Stock, \(x) 
                                x |> slot('SRR') |> slot('RecDevProj')
                                ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step')) |>
    ExpandSims(nSim) 
  
  List$SpawnTimeFrac <- purrr::map(OM@Stock, \(x) 
                                   x |> slot('SRR') |> slot('SpawnTimeFrac')
                                   )
  
  List$RelRecFun <- purrr::map(OM@Stock, \(x) 
                               x |> slot('SRR') |> slot('RelRecFun')
                               ) 
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('SRR') |>
                            slot('Misc'))
  
  List
}

MakeSpatialList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  List <- list()
  
  List$UnfishedDist <- GetUnfishedDist(OM, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area', 'Age', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps, AgeOpt=3)
  

  List$ProbStaying <- GetProbStaying(OM, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area', 'Age', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps, AgeOpt=3) 
  
  List$RelativeSize <- GetRelativeSize(OM) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area')) |>
    ExpandSims(nSim) 
  
  List$Movement <- GetMovementAtAge(OM) |>
    List2Array('Stock') |>
    aperm(c(1, 6, 2, 3, 4, 5)) |>
    ArrayExpand(nSim, nAges, TimeSteps, AgeOpt=3)
  
  # List$FracOther <- purrr::map(OM@Stock, \(x) 
  #                              x@Spatial@FracOther
  #                              ) |>
  #   List2Array('Stock') 
  # 
  #   aperm(c(1, 6, 2, 3, 4, 5)) |>
  #   ArrayExpand(nSim, nAges, TimeSteps) |
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('Spatial') |>
                            slot('Misc'))
  
  
  
  
  List
  
}

MakeDepletionList <- function(OM, Period='Historical', TimeSteps=NULL) {

  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
    
  List <- list()
  
  List$Initial <- purrr::map(OM@Stock, \(x)
                           x@Depletion@Initial) |>
    List2Array('Stock')
  
  if (length(List$Initial)>0) {
    List$Initial <- List$Initial  |>
      ExpandSims(nSim)
  } else {
    List$Initial <- MakeNamedList(1:nSim)
  }
  
  List$Final <- purrr::map(OM@Stock, \(x)
                             x@Depletion@Final) |>
    List2Array('Stock')
  
  if (length(List$Final)>0) {
    List$Final <- List$Final  |>
      ExpandSims(nSim) 
  } else {
    List$Final <- MakeNamedList(1:nSim)
  }
  
  List$DepletionReference <- purrr::map(OM@Stock, \(x)
                                        x@Depletion@Reference)
  
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('Depletion') |>
                            slot('Misc'))
 
  List
}