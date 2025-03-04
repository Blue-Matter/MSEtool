MakeStockParsList <- function(OM, Period='Historical') {
  # most list elements are a list of length `nsim`
  nSim <- nSim(OM)
  nAges <- purrr::map(OM@Stock, nAge) |> unlist() |> max()
  TimeSteps <- TimeSteps(OM, Period)
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM@Stock[[1]])
  
 
  StockParsList <- list()
  
  StockParsList$Ages <- purrr::map(OM@Stock, \(x) slot(x, 'Ages'))

  StockParsList <- c(StockParsList, 
                     MakeStockSlotList(OM, Period, 'Length', nSim, nAges, TimeSteps),
                     MakeStockSlotList(OM, Period, 'Weight', nSim, nAges, TimeSteps),
                     MakeStockSlotList(OM, Period, 'NaturalMortality', nSim, nAges, TimeSteps),
                     MakeStockSlotList(OM, Period, 'Maturity', nSim, nAges, TimeSteps),
                     MakeStockSlotList(OM, Period, 'Fecundity', nSim, nAges, TimeSteps),
                     MakeSRRList(OM, Period, nSim, nAges, TimeSteps),
                     MakeSpatialList(OM, Period, nSim, nAges, TimeSteps),
                     MakeDepletionList(OM, Period, nSim)
  )
  
  # Arrays to be populated

  StockParsList$NumberAtAgeArea <- ArraySimStockAgeTimeArea(OM, Period) 
  StockParsList$BiomassAtAgeArea <- StockParsList$NumberAtAgeArea
  StockParsList$SBiomass <- StockParsList$NumberAtAgeArea
  StockParsList$SProduction <- ArraySimStockAgeTimeArea(OM, Period)  |> 
    DropDimension('Age', warn=FALSE) |>
    DropDimension('Area', warn=FALSE)
  
  StockParsList$CurrentYear <- purrr::map(OM@Stock, \(x) slot(x, 'CurrentYear'))
  StockParsList$TimeUnits <- purrr::map(OM@Stock, \(x) slot(x, 'TimeUnits'))
  StockParsList$TimeSteps <- purrr::map(OM@Stock, \(x) slot(x, 'TimeSteps'))
  StockParsList$TimeStepsHist <- purrr::map(OM@Stock, \(x) TimeSteps(x, 'Historical'))
  StockParsList$TimeStepsProj <- purrr::map(OM@Stock, \(x) TimeSteps(x, 'Projection'))
  StockParsList$TimeStepsPerYear <- purrr::map(OM@Stock, \(x) slot(x, 'TimeStepsPerYear'))
  StockParsList$Misc <- purrr::map(OM@Stock, \(x) slot(x, 'Misc'))
  
  StockParsList$Sim <- as.list(1:nSim)
  
  class(StockParsList) <- c('list', 'StockParsList')
  StockParsList
}


MakeStockSlotList <- function(OM, Period='Historical', slot='Length',
                              nSim, nAges, TimeSteps) {

  sNames <- slotNames(slot(OM@Stock[[1]], slot))
  
  List <- list()
  
  if ('Pars' %in% sNames) 
    List[[paste0(slot, 'Pars')]] <- purrr::map(OM@Stock, \(x)
                                               x |> 
                                                 slot(slot) |>
                                                 slot('Pars'))
  
  if ('Model' %in% sNames) 
    List[[paste0(slot, 'Model')]] <- purrr::map(OM@Stock, \(x)
                                               x |> 
                                                 slot(slot) |>
                                                 slot('Model'))
  
  if ('MeanAtAge' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtAge'))
    List[[paste0(slot, 'MeanAtAge')]] <- fun(OM@Stock, TimeSteps) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
   
  if ('MeanAtLength' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtLength'))
    List[[paste0(slot, 'MeanAtLength')]] <- fun(OM@Stock, TimeSteps) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Class', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  
  if ('CVatAge' %in% sNames) {
    fun <- get('GetCVAtAge')
    List[[paste0(slot, 'CVAtAge')]] <- fun(OM@Stock, TimeSteps, slot) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  if ('Dist' %in% sNames)
    List[[paste0(slot, 'Dist')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                 slot(slot) |>
                                                 slot('Dist'))
  
  if ('TruncSD' %in% sNames)
    List[[paste0(slot, 'TruncSD')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                 slot(slot) |>
                                                 slot('TruncSD'))
  
  if ('Timing' %in% sNames)
    List[[paste0(slot, 'Timing')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                    slot(slot) |>
                                                    slot('Timing'))
  
  # TODO
  if ('Random' %in% sNames)
    List[[paste0(slot, 'Random')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                   slot(slot) |>
                                                   slot('Random'))
  
 # TODO
  if ('ASK' %in% sNames)
    List[[paste0(slot, 'ASK')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                   slot(slot) |>
                                                   slot('ASK'))
  
  if ('Classes' %in% sNames)
    List[[paste0(slot, 'Classes')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                slot(slot) |>
                                                slot('Classes'))
  
  if ('Semelparous' %in% sNames) {
    List[[paste0('Semelparous')]] <- GetSemelparous(OM@Stock, TimeSteps) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  
  if ('Misc' %in% sNames)
    List[[paste0(slot, 'Misc')]] <- purrr::map(OM@Stock, \(x) x |> 
                                                    slot(slot) |>
                                                    slot('Misc'))
  List
}


MakeSRRList <- function(OM, Period, nSim, nAges, TimeSteps) {
  List <- list()
  
  List$SRRPars <- purrr::map(OM@Stock, \(x)
                          x |> 
                            slot('SRR') |>
                            slot('Pars')) 
                              
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

MakeSpatialList <- function(OM, Period, nSim, nAges, TimeSteps) {
  List <- list()
  
  List$UnfishedDist <- GetUnfishedDist(OM, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area', 'Age', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps)
  
  List$ProbStaying <- GetProbStaying(OM, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area', 'Age', 'Time Step')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 
  
  List$RelativeSize <- GetRelativeSize(OM) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area')) |>
    ExpandSims(nSim) 
  
  List$Movement <- GetMovementAtAge(OM) |>
    List2Array('Stock') |>
    aperm(c(1, 6, 2, 3, 4, 5)) |>
    ArrayExpand(nSim, nAges, TimeSteps)
  
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

MakeDepletionList <- function(OM, Period, nSim) {
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