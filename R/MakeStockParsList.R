
OMListSimSubsetTimeStep <- function(List, TimeSteps=NULL) {
  nms <- names(List)
  if (is.null(nms)) {
    if (inherits(List, 'array')) {
      if ("TimeStep" %in% names(dimnames(List))) {
        List <- ArraySubsetTimeStep(List, TimeSteps)
      }
    }
  } else {
    for (i in seq_along(nms)) {
      nmsList <- names(List[[i]]) 
      if (isS4(List[[i]])) {
        return(List[[i]])
      }
      
      hasTS <- "TimeStep" %in% nmsList
      if (all(is.na(nmsList)))
        nmsList <- NULL 
      if (inherits(List[[i]], 'array')) {
        if ("TimeStep" %in% names(dimnames(List[[i]]))) {
          List[[i]] <- ArraySubsetTimeStep(List[[i]], TimeSteps)
        }
      } else if (is.list(List[[i]])) {
        for (j in seq_along(List[[i]])) {
          obj <- Recall(List[[i]][[j]], TimeSteps)
          if (is.null(obj))
            next()
          List[[i]][[j]] <- Recall(List[[i]][[j]], TimeSteps)
        }
      } else if (inherits(List[[i]], 'numeric')) {
        if (hasTS)
          List[[i]] <- ArraySubsetTimeStep(List[[i]], TimeSteps)
      } else if (inherits(List[[i]], 'integer')) {
        if (hasTS)
          List[[i]] <-  ArraySubsetTimeStep(List[[i]], TimeSteps)
      }
    }
  }
  List
}


CheckSimsUnique <- function(OMList, TimeSteps=NULL, ignore=c('Sim')) {
  if (is.null(TimeSteps)) {
    l1 <- OMList[[1]]
    l2 <- OMList[[2]]
  } else {
    l1 <- OMListSimSubsetTimeStep(OMList[[1]], TimeSteps)
    l2 <- OMListSimSubsetTimeStep(OMList[[2]], TimeSteps)  
  }
  
  for (nm in ignore) {
    l1[[nm]] <- NULL
    l2[[nm]] <- NULL
  }
  digest::digest(l1, algo='spookyhash') == digest::digest(l2, algo='spookyhash')
}

# ConvertToSimList <- function(OMList) {
#   
#   sims <- OMList$Sim
#   nsim <- max(sims)
#   
#   SimsUnique <- FALSE
#   if (nsim) {
#     SimsUnique <- CheckSimsUnique(OMList) 
#   }
#   
#   if (SimsUnique) {
#     # tictoc::tic()
#     OMListSim <- lapply(sims, function(x) MakeSimList(OMList, x))
#     # tictoc::toc()
#     names(OMListSim) <-sims
#   } else {
#     OMListSim <- list(MakeSimList(OMList, 1))
#     names(OMListSim) <- 1
#   }
#   
#   
#   OMListSim
# }


# https://stackoverflow.com/questions/15263146/revert-list-structure
#' @export
ReverseList <- function(ls) {
  if (all(lapply(ls, is.null) |> unlist()))
    return(ls)
  x <- lapply(ls, `[`, names(ls[[1]]))
  apply(do.call(rbind, x), 2, as.list) 
}

MakeOMList <- function(OM, Period="All") {
  id <- cli::cli_progress_bar("Creating Internal OM List Object")
  
  OMList <- MakePopulationList(OM, Period)

  OMList <- c(OMList, MakeFleetList(OM, Period))

  if (length(OM@CatchFrac)>0)
    OMList$CatchFrac <- List2Array(OM@CatchFrac, 'Stock') |> aperm(c('Sim', 'Stock', 'Fleet'))
  
  OMList$Sim <- 1:OM@nSim
  OMList$CurvesFSearch <- OM@Control$Curves$FSearch
  
  names(OMList$Sim) <- rep("Sim", OM@nSim)
  
  OMList <- purrr::map(1:OM@nSim, function(x) {
    cli::cli_progress_update(id=id)
    MakeSimList(OMList, x)
  })
  names(OMList) <- 1:OM@nSim
  
  cli::cli_progress_done()
 
  class(OMList) <- "OMList"
 
  OMList
}

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
    nmsList <- names(List[[i]]) 
    hasSim <- "Sim" %in% nmsList
    if (all(is.na(nmsList)))
      nmsList <- NULL 
    if (inherits(List[[i]], 'array')) {
      if ("Sim" %in% names(dimnames(List[[i]]))) {
        List[[i]] <- Array2List(List[[i]], 'Sim', Sim)[[1]]
      }
    } else if (inherits(List[[i]], 'list')) {
      List[[i]] <- Recall(List[[i]], Sim)
    } else if (inherits(List[[i]], 'numeric')) {
      if (hasSim)
        List[[i]] <- List[[i]][Sim]
    } else if (inherits(List[[i]], 'integer')) {
      if (hasSim)
        List[[i]] <- List[[i]][Sim]
    }
  } 
 
  class(List) <- "OMListSim"
  List
}


MakePopulationList <- function(OM, Period='All') {
  
  List <- list()
  
  List$Ages <-  purrr::map(OM@Stock, methods::slot, 'Ages')
  List$Length <- MakeStockSlotList(OM, 'Length', Period)
  
  weight <- MakeStockSlotList(OM, 'Weight', Period)
  List$WeightMeanAtAge <- weight$MeanAtAge
  
  natmort <- MakeStockSlotList(OM, 'NaturalMortality', Period)
  List$NaturalMortalityMeanAtAge <- natmort$MeanAtAge
  
  maturity <- MakeStockSlotList(OM, 'Maturity', Period)
  List$MaturityMeanAtAge <- maturity$MeanAtAge 
  List$MaturitySemelparous <- maturity$Semelparous
  
  fecundity <- MakeStockSlotList(OM, 'Fecundity', Period)
  List$FecundityMeanAtAge <- fecundity$MeanAtAge
  
  srr <- MakeSRRList(OM, Period)
  List$SRRPars <- srr$SRRPars
  List$SRRModel <- srr$SRRModel
  List$R0 <- List2Array(srr$R0, "Stock") |> aperm(c("Sim", "Stock", "TimeStep"))
  List$SPFrom <- srr$SPFrom
  List$RecDevInit <- srr$RecDevInit
  List$RecDevs <- srr$RecDevs |> List2Array("Stock") |> aperm(c("Sim", "Stock", "TimeStep"))
  List$SpawnTimeFrac <- srr$SpawnTimeFrac
  List$RelRecFun <- srr$RelRecFun
  
  spatial <- MakeSpatialList(OM, Period)
  List$UnfishedDist <- spatial$UnfishedDist
  List$ProbStaying <- spatial$ProbStaying
  List$RelativeSize <- List2Array(spatial$RelativeSize, "Stock") |> aperm(c('Sim', 'Stock', 'Area'))
  List$Movement <- spatial$Movement
  
  
  Depletion <- MakeDepletionList(OM, Period)
  List$DepletionInitial <- Depletion$Initial
  List$DepletionFinal <- Depletion$Final
  List$DepletionReference <- Depletion$Reference
  List$DepletionMisc <- Depletion$Misc
  
  # Arrays to be  filled 
  List$NumberAtAgeArea <- ListArraySimAgeTimeArea(OM, Period) 
  
  List$Biomass <- ListArraySimAgeTime(OM, Period) |>
    purrr::map(\(x)  DropDimension(x, 'Age', warn=FALSE)) 
  List$Biomass <- List2Array( List$Biomass , 'Stock') |> aperm(c('Sim','Stock', 'TimeStep'))
  
  List$SProduction <- List$Biomass
  List$SBiomass <-  List$SProduction
  
  List$CurrentYear <- purrr::map(OM@Stock, \(x) methods::slot(x, 'CurrentYear'))
  List$TimeUnits <- purrr::map(OM@Stock, \(x) methods::slot(x, 'TimeUnits'))
  List$TimeSteps <- TimeSteps(OM@Stock[[1]])
  List$TimeStepsHist <- TimeSteps(OM@Stock[[1]], "Historical")
  List$TimeStepsProj <- TimeSteps(OM@Stock[[1]], 'Projection')
  List$TimeStepsPerYear <- purrr::map(OM@Stock, \(x) methods::slot(x, 'TimeStepsPerYear'))
  List$Misc <- purrr::map(OM@Stock, \(x) methods::slot(x, 'Misc'))
  
  List
}

MakeStockSlotList <- function(OM, slot='Length', Period='Historical', TimeSteps=NULL) {
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
  
  sNames <- slotNames(methods::slot(OM@Stock[[1]], slot))
  
  meta <- GetMetaData(OM)
  
  List <- list()
  
  if ('Pars' %in% sNames) {
    List$Pars <- purrr::map(OM@Stock, \(x)
                            x |> 
                              slot(slot) |>
                              slot('Pars'))
    
    for (i in seq_along(List$Pars)) {
      stList <-  List[['Pars']][[i]]
      for (j in seq_along(stList)) {
        List$Pars[[i]][[j]] <-  List$Pars[[i]][[j]] |> ArrayExpand(OM@nSim, 
                                                                   nAge(OM@Stock[[j]]),
                                                                   TimeSteps)
      }
    }
  }
    
  if ('Model' %in% sNames) 
    List$Model <- purrr::map(OM@Stock, \(x)
                             x |> 
                               methods::slot(slot) |>
                               methods::slot('Model')
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
    List$CVAtAge <- GetCVAtAge(OM@Stock, TimeSteps, slot) |>
      purrr::imap(\(x, idx) {
        ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                    TimeSteps) 
      })
  }
  
  if ('Dist' %in% sNames)
    List$Dist <- purrr::map(OM@Stock, \(x) x |> 
                                   methods::slot(slot) |>
                                   methods::slot('Dist'))
  
  if ('TruncSD' %in% sNames)
    List$TruncSD <- purrr::map(OM@Stock, \(x) x |> 
                                      methods::slot(slot) |>
                                      methods::slot('TruncSD'))
  
  if ('Timing' %in% sNames)
    List$Timing <- purrr::map(OM@Stock, \(x) x |> 
                                     methods::slot(slot) |>
                                     methods::slot('Timing'))
  
  # TODO
  if ('Random' %in% sNames)
    List$Random <- purrr::map(OM@Stock, \(x) x |> 
                                     methods::slot(slot) |>
                                     methods::slot('Random'))
  
  if ('ASK' %in% sNames)
    List$ASK <- purrr::map(OM@Stock, \(x) x |> 
                             methods::slot(slot) |>
                             methods::slot('ASK')) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps) 
    }) 
  
  List$ASK$Female |> dimnames()
  t = ArrayExpand(List$ASK[[1]], nSim=OM@nSim, nAges=meta$nAges[[1]],
                  TimeSteps)
  
  
  
  
  if ('Classes' %in% sNames)
    List$Classes <- purrr::map(OM@Stock, \(x) x |> 
                                      methods::slot(slot) |>
                                      methods::slot('Classes'))
  
  if ('Semelparous' %in% sNames) {
    List$Semelparous <- GetSemelparous(OM@Stock, TimeSteps) |>
      purrr::imap(\(x, idx) {
        ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                    TimeSteps) 
      })
  }
  
  if ('Misc' %in% sNames)
    List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                              methods::slot(slot) |>
                              methods::slot('Misc'))
  
  List
}


MakeSRRList <- function(OM, Period='Historical', TimeSteps=NULL) {
  meta <- GetMetaData(OM)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)
  
  List <- list()
  
  List$SRRPars <- purrr::map(OM@Stock, \(x)
                          x |> 
                            methods::slot('SRR') |>
                            methods::slot('Pars')) 
  
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
                            methods::slot('SRR') |>
                            methods::slot('Model') |>
                            get()) 
 
  List$R0 <- purrr::map(OM@Stock, \(x) 
                        x |> slot('SRR') |> slot('R0')
                        ) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps)
    })
  
  
  List$SPFrom <- purrr::map(OM@Stock, \(x) 
    x |> methods::slot('SRR') |> methods::slot('SPFrom')) 
  
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
                                x |> methods::slot('SRR') |> slot('RecDevInit')
                                ) |>
    purrr::map(ExpandSims, OM@nSim)
  
  
  RecDevHist <- purrr::map(OM@Stock, \(x) 
                                x |> methods::slot('SRR') |> slot('RecDevHist')
  ) |>
    purrr::map(ExpandSims, OM@nSim)
    
 
  RecDevProj <- purrr::map(OM@Stock, \(x) 
                                x |> methods::slot('SRR') |> slot('RecDevProj')
  ) |>
    purrr::map(ExpandSims, OM@nSim)
  

  List$RecDevs <- purrr::map2(RecDevHist,RecDevProj,  \(x,y) {
    r <- cbind(x,y)
    names(dimnames(r)) <- c('Sim', 'TimeStep')
    r
  }) 
  

  List$SpawnTimeFrac <- purrr::map(OM@Stock, \(x) 
                                   x |> methods::slot('SRR') |> methods::slot('SpawnTimeFrac')
                                   ) |> unlist()
  
  List$RelRecFun <- purrr::map(OM@Stock, \(x)
                               x |> methods::slot('SRR') |> methods::slot('RelRecFun')
                               )
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            methods::slot('SRR') |>
                            methods::slot('Misc'))
  
  List
}

MakeSpatialList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, Period)

  List <- list()
  
  List$UnfishedDist <- GetUnfishedDist(OM, TimeSteps) |>
    purrr::map(\(x) aperm(x, c('Sim', 'Age', 'TimeStep', 'Area'))) |>
    purrr::imap(\(x, idx) {
      ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
                  TimeSteps) 
      }) 
  

  List$ProbStaying <- GetProbStaying(OM, TimeSteps) |>
    purrr::map(\(x) aperm(x, c('Sim', 'Age', 'TimeStep', 'Area'))) |>
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
    purrr::map(\(x) aperm(x, c('Sim', 'Age', 'TimeStep', 'FromArea', 'ToArea'))) |>
    purrr::map(\(x) Array2List(x, "TimeStep"))

  # List$FracOther <- purrr::map(OM@Stock, \(x)
  #                              x@Spatial@FracOther
  #                              ) |>
  #   purrr::imap(\(x, idx) {
  #     ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
  #                 TimeSteps)
  #   }) 
  
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
  List$Initial <- List2Array(List$Initial, "Stock")
  
  List$Final <- purrr::map(OM@Stock, \(x)
                             x@Depletion@Final) 
  
  
  if (length(List$Final)>0) {
    List$Final <- List$Final  |>
      purrr::map(ExpandSims,OM@nSim)
  } else {
    List$Final <- MakeNamedList(1:OM@nSim)
  }
  List$Final <- List2Array(List$Final, "Stock")
  
  List$Reference <- purrr::map(OM@Stock, \(x)
                                        x@Depletion@Reference)
  List$Reference <- unlist(List$Reference)
  
  List$Misc <- purrr::map(OM@Stock, \(x) x |> 
                            slot('Depletion') |>
                            slot('Misc'))
 
  List
}
