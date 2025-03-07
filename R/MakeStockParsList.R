
GetMetaData <- function(OM, Period=c('Historical', 'Projection', 'All'), TimeSteps=NULL) {
  
  Period <- match.arg(Period)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
  
  nAges <- purrr::map(OM@Stock, nAge)

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
        List[[i]] <- Array2List(List[[i]], 'Sim', Sim)[[1]]
      }
    } else if (inherits(List[[i]], 'list')) {
      List[[i]] <- Recall(List[[i]], Sim)
    } else if (inherits(List[[i]], 'numeric')) {
      List[[i]] <- List[[i]][Sim]

    }
  } 
  
  List
}


MakePopulationList <- function(OM, Period='Historical', Unfished=NULL) {
  
  if (is.null(Unfished)) 
    Unfished <- CalcUnfishedDynamics(OM)
  
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
 
  List$NumberAtAgeArea <- ListArraySimAgeTimeArea(OM, Period) 
  List$BiomassArea <- ListArraySimAgeTimeArea(OM, Period) |>
    purrr::map(\(x)  DropDimension(x, 'Age', warn=FALSE))
  
  List$SProduction <-  List$BiomassArea |>
    purrr::map(\(x)  DropDimension(x, 'Area', warn=FALSE))
  
  List$SP0 <- Unfished@Equilibrium@SProduction |>
    purrr::map(\(x) {
      x |> ExpandSims(OM@nSim) |> ExpandTimeSteps(TimeSteps=TimeSteps(OM, Period))
        
    })
  
  List$CurrentYear <- purrr::map(OM@Stock, \(x) slot(x, 'CurrentYear'))
  List$TimeUnits <- purrr::map(OM@Stock, \(x) slot(x, 'TimeUnits'))
  List$TimeSteps <- purrr::map(OM@Stock, \(x) slot(x, 'TimeSteps'))
  List$TimeStepsHist <- purrr::map(OM@Stock, \(x) TimeSteps(x, 'Historical'))
  List$TimeStepsProj <- purrr::map(OM@Stock, \(x) TimeSteps(x, 'Projection'))
  List$TimeStepsPerYear <- purrr::map(OM@Stock, \(x) slot(x, 'TimeStepsPerYear'))
  List$Misc <- purrr::map(OM@Stock, \(x) slot(x, 'Misc'))
  
  List
}

MakeStockSlotList <- function(OM, slot='Length', Period='Historical', TimeSteps=NULL) {
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
  
  sNames <- slotNames(slot(OM@Stock[[1]], slot))
  
  meta <- GetMetaData(OM)
  
  List <- list()
  
  if ('Pars' %in% sNames) {
    List[['Pars']] <- purrr::map(OM@Stock, \(x)
                                               x |> 
                                                 slot(slot) |>
                                                 slot('Pars'))
    
    for (i in seq_along(List[['Pars']])) {
      stList <-  List[['Pars']][[i]]
      for (j in seq_along(stList)) {
        List[['Pars']][[i]][[j]] <-  List[['Pars']][[i]][[j]] |> ArrayExpand(OM@nSim, 
                                                                             nAge(OM@Stock[[j]]),
                                                                             TimeSteps)
      }
    }
    
  }
    
  if ('Model' %in% sNames) 
    List$Model <- purrr::map(OM@Stock, \(x)
                             x |> 
                               slot(slot) |>
                               slot('Model')
    ) |> purrr::map(\(x) if(!is.null(x) && is.character(x))
      get(x))
  
  
  
  if ('MeanAtAge' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtAge'))
    List$MeanAtAge <- fun(OM@Stock, TimeSteps) |>
      purrr::imap(\(x, idx) {
        ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                    TimeSteps)
      })
  }
   
  if ('MeanAtLength' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtLength'))
    List$MeanAtLength <- fun(OM@Stock, TimeSteps) |>
      purrr::imap(\(x, idx) {
        ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                    TimeSteps)
      })
    }
  
  
  if ('CVatAge' %in% sNames) {
    List[['CVAtAge']] <- GetCVAtAge(OM@Stock, TimeSteps, slot) |>
      purrr::imap(\(x, idx) {
        ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                    TimeSteps)
      })
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
    List[['Timing']] <- purrr::map(OM@Stock, \(x) x |> 
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
      purrr::imap(\(x, idx) {
        ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                    TimeSteps)
      })
  }
  
  
  if ('Misc' %in% sNames)
    List[['Misc']] <- purrr::map(OM@Stock, \(x) x |> 
                                                    slot(slot) |>
                                                    slot('Misc'))
  List
}


MakeSRRList <- function(OM, Period='Historical', TimeSteps=NULL) {
  meta <- GetMetaData(OM)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
  
  List <- list()
  
  List$SRRPars <- purrr::map(OM@Stock, \(x)
                          x |> 
                            slot('SRR') |>
                            slot('Pars')) 
  
  for (i in seq_along(List$SRRPars)) {
    stList <- List$SRRPars[[i]]
    for (j in seq_along(stList)) {
      List$SRRPars[[i]][[j]] <- List$SRRPars[[i]][[j]] |> ArrayExpand(OM@nSim, 
                                                                      nAge(OM@Stock[[j]]),
                                                                      TimeSteps)
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
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps)
    })
  
  
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
  List$SPFrom <- unlist(List$SPFrom)
  
  List$RecDevInit <- purrr::map(OM@Stock, \(x)
                                x |> slot('SRR') |> slot('RecDevInit')
                                ) |>
    purrr::map(ExpandSims, OM@nSim)
  
  if (Period=='Historical') {
    List$RecDevs <- purrr::map(OM@Stock, \(x) 
                                  x |> slot('SRR') |> slot('RecDevHist')
    ) |>
      purrr::map(ExpandSims, OM@nSim)
    
  } else {
    List$RecDevs <- purrr::map(OM@Stock, \(x) 
                                  x |> slot('SRR') |> slot('RecDevProj')
    ) |>
      purrr::map(ExpandSims, OM@nSim)
  }
 

  List$SpawnTimeFrac <- purrr::map(OM@Stock, \(x) 
                                   x |> slot('SRR') |> slot('SpawnTimeFrac')
                                   ) |> unlist()
  
  List$RelRecFun <- purrr::map(OM@Stock, \(x)
                               x |> slot('SRR') |> slot('RelRecFun')
                               )
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('SRR') |>
                            slot('Misc'))
  
  List
}

MakeSpatialList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)

  List <- list()
  
  List$UnfishedDist <- GetUnfishedDist(OM, TimeSteps) |>
    purrr::map(\(x) aperm(x, c('Sim', 'Age', 'Time Step', 'Area'))) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps)
    })
  

  List$ProbStaying <- GetProbStaying(OM, TimeSteps) |>
    purrr::map(\(x) aperm(x, c('Sim', 'Age', 'Time Step', 'Area'))) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps)
    })
  
  List$RelativeSize <- GetRelativeSize(OM) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps)
    })
  
  List$Movement <- GetMovementAtAge(OM) |>
    purrr::imap(\(x, idx) ArrayExpand(x, OM@nSim, meta$nAges[[idx]], TimeSteps)) |>
    purrr::map(\(x) aperm(x, c('Sim', 'Age', 'Time Step', 'FromArea', 'ToArea'))) 

  
  List$FracOther <- purrr::map(OM@Stock, \(x)
                               x@Spatial@FracOther
                               ) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps)
    })
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('Spatial') |>
                            slot('Misc'))

  List
  
}

MakeDepletionList <- function(OM, Period='Historical', TimeSteps=NULL) {

  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
    
  List <- list()
  
  List$Initial <- purrr::map(OM@Stock, \(x)
                           x@Depletion@Initial) 
  
  if (length(List$Initial)>0) {
    List$Initial <- List$Initial  |>
      purrr::map(ExpandSims,OM@nSim)
  } else {
    List$Initial <- MakeNamedList(1:OM@nSim)
  }
  
  List$Final <- purrr::map(OM@Stock, \(x)
                             x@Depletion@Final) 
  
  if (length(List$Final)>0) {
    List$Final <- List$Final  |>
      purrr::map(ExpandSims,OM@nSim)
  } else {
    List$Final <- MakeNamedList(1:OM@nSim)
  }
  
  List$DepletionReference <- purrr::map(OM@Stock, \(x)
                                        x@Depletion@Reference)
  
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('Depletion') |>
                            slot('Misc'))
 
  List
}
