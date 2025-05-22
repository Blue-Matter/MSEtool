ReplaceNULL <- function(object) {
  if (is.null(object))
    return(NA)
  object
}

CopySlots <- function(ObjectOut,
                      ObjectIn, 
                      Concate=c("nYear", "pYear", "nSim", 
                                "CurrentYear", "TimeUnits",
                                "TimeStepsPerYear"),
                      List=c('Misc', 'Log')) {
  Slots <- slotNames(ObjectOut)
  for (sl in Concate) {
    if (sl %in% Slots) {
      slot(ObjectOut, sl) <- List2Array(lapply(ObjectIn, slot, sl))[1,]
    }
  }
  
  for (sl in List) {
    if (sl %in% Slots) {
      slot(ObjectOut, sl) <- lapply(ObjectIn, slot, sl)
    }
  }
  
  ObjectOut
}

OM2Hist <- function(OM, silent=FALSE) {

  if (!silent) 
    id <- cli::cli_progress_bar("Initializing `Hist` Object")
  
  Hist <- new('hist')
  nms <- slotNames(OM)
  nms2 <- c('Stock', 'Fleet', 'Obs', 'Imp', 'CatchFrac')
  nms <- nms[!nms %in% nms2]
  for (nm in nms) {
    slot(Hist@OM, nm) <- slot(OM, nm)
  }
  
  if (!silent)
    cli::cli_progress_update(id=id)

  Hist@OM@Stock <- Stock2Hist(OM@Stock, nSim(OM), TimeSteps(OM), silent, id) 
  nAgesList <- lapply(Hist@OM@Stock@Ages@Classes, length) 
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Hist@OM@Fleet <- vector('list', nStock(OM))
  names(Hist@OM@Fleet) <- StockNames(OM)
  Hist@OM@Fleet <- purrr::map2(OM@Fleet, nAgesList, Fleet2Hist, nSim(OM), TimeSteps(OM),
                               silent, id)
  
  if (!silent) 
    cli::cli_progress_done()

  
  Hist
}
  
Stock2Hist <- function(StockList, nSim, TimeSteps, silent=FALSE, id=NULL) {
  Stock <- StockList
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  histstock <- new('stock')
  histstock@Name <- unlist(lapply(Stock, slot, 'Name'))
  histstock@CommonName <- unlist(lapply(Stock, slot, 'CommonName'))
  histstock@Species <- unlist(lapply(Stock, slot, 'Species'))
  
  ages <- lapply(Stock, slot, 'Ages')
  histstock@Ages@MaxAge <- List2Array(lapply(ages, slot, 'MaxAge'))[1,]
  histstock@Ages@Units <- List2Array(lapply(ages, slot, 'Units'))[1,]
  histstock@Ages@PlusGroup <- List2Array(lapply(ages, slot, 'PlusGroup'))[1,]
  histstock@Ages@Classes <- lapply(ages, slot, 'Classes')
  ind <-lapply(histstock@Ages@Classes , length) |> unlist() |> which.max()
  classes <- histstock@Ages@Classes[[ind]]
  
  nAges <- length(classes)

  if (!silent)
    cli::cli_progress_update(id=id)
  histstock@Length <- CombineStockObjects(lapply(Stock, slot, 'Length'), 
                                          nSim, nAges, TimeSteps)
  
  histstock@Weight <- CombineStockObjects(lapply(Stock, slot, 'Weight'), 
                                          nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  histstock@NaturalMortality <- CombineStockObjects(lapply(Stock, slot, 'NaturalMortality'), 
                                          nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  histstock@Maturity <- CombineStockObjects(lapply(Stock, slot, 'Maturity'), 
                                                    nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  histstock@Fecundity <- CombineStockObjects(lapply(Stock, slot, 'Fecundity'), 
                                            nSim, nAges, TimeSteps)
  
  histstock@SRR <- CombineSRR(lapply(Stock, slot, 'SRR'), nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  histstock@Spatial <- CombineSpatial(List=lapply(Stock, slot, 'Spatial'),
                                      nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)

  histstock@Depletion <- CombineDepletion(List=lapply(Stock, slot, 'Depletion'),
                                        nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  histstock <- CopySlots(histstock, Stock)

  histstock@TimeSteps <- TimeSteps
  histstock@Misc <- lapply(Stock, slot, 'Misc')
  histstock@Log <- lapply(Stock, slot, 'Log')
  histstock
}
    
Fleet2Hist <- function(FleetList, nAges, nSim, TimeSteps, silent=FALSE, id=NULL) {
  Fleet <- new('fleet')
  Fleet@Name <- unlist(names(FleetList))
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@FishingMortality <- CombineFishingMortality(lapply(FleetList, slot, 
                                                           'FishingMortality'),
                                                    nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@DiscardMortality  <- CombineStockObjects(lapply(FleetList, slot, 
                                                        'DiscardMortality'),
                                                 nSim, nAges, TimeSteps) 
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Effort <- CombineEffort(lapply(FleetList, slot, 'Effort'),
                                nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Selectivity <- CombineStockObjects(lapply(FleetList, slot, 
                                                  'Selectivity'),
                                           nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Retention <- CombineStockObjects(lapply(FleetList, slot, 
                                                'Retention'),
                                         nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Distribution <- CombineDistribution(lapply(FleetList, slot, 
                                                   'Distribution'),
                                            nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@WeightFleet <- lapply(FleetList, slot, 'WeightFleet') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Age', 'TimeStep', 'Fleet')) 
  Fleet@BioEconomic <- lapply(FleetList, slot, 'BioEconomic')
  Fleet <- CopySlots(Fleet, FleetList)
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@TimeSteps <- TimeSteps
  Fleet
}


    
    
  

  
  
  
CombineStockObjects <- function(object, nSim, nAges, TimeSteps) {
  class <- class(object[[1]])
  out <- new(class)
  nms <- slotNames(out)
  if ('Pars' %in% nms) 
    out@Pars <- lapply(object, slot, 'Pars')
  
  if ('Model' %in% nms) 
    out@Model <- lapply(object, slot, 'Model')
  
  if ('Units' %in% nms) 
    out@Units <- List2Array(lapply(object, slot, 'Units'))[1,]
  
  if ('MeanAtAge' %in% nms) 
    out@MeanAtAge <- lapply(object, slot, 'MeanAtAge') |> 
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep'))
  
  if ('MeanAtLength' %in% nms) 
    out@MeanAtLength <- lapply(object, slot, 'MeanAtAge') |> 
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) 
  
  if ('Semelparous' %in% nms) 
    out@Semelparous <- lapply(object, slot, 'Semelparous') |> 
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep'))
  
  if ('CVatAge' %in% nms) 
    out@CVatAge <- lapply(object, slot, 'CVatAge') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep'))
  
  if ('Dist' %in% nms) 
    out@Dist <- List2Array(lapply(object, slot, 'Dist'))[1,]
  
  if ('TruncSD' %in% nms) 
    out@TruncSD <- List2Array(lapply(object, slot, 'TruncSD'))[1,]
  
  if ('Random' %in% nms) 
    out@Random <- lapply(object, slot, 'Random') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep'))
  
  if ('ASK' %in% nms) 
    out@ASK <- lapply(object, slot, 'ASK') 
  
  if ('Classes' %in% nms) 
    out@Classes <- lapply(object, slot, 'Classes') 
  
  if ('Misc' %in% nms) 
    out@Misc <- lapply(object, slot, 'Misc')
  
  out
}
 

CombineSRR <- function(srr, nSim, nAges, TimeSteps) {
  SRR <- new('srr')
  
  SRR@Pars <- lapply(srr, slot, 'Pars')
  SRR@Model <- lapply(srr, slot, 'Model')
  SRR@R0 <- lapply(srr, slot, 'R0') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |> 
    aperm(c("Sim", "Stock", "TimeStep"))
  
  SRR@SPFrom <- lapply(srr, slot, 'SPFrom') |>
    lapply(ReplaceNULL) |> unlist()
    
  nms <- names(SRR@SPFrom)
  for (i in seq_along(SRR@SPFrom)) {
    if (is.na(SRR@SPFrom[i])) {
      SRR@SPFrom[i] <- i
    } else {
      if (is.character(SRR@SPFrom[i])) 
        SRR@SPFrom[i] <- match(SRR@SPFrom[i],nms)
    }
  }

  SRR@SPFrom <- as.numeric(SRR@SPFrom)
  names(SRR@SPFrom) <- nms
  SRR@TruncSD <- List2Array(lapply(srr, slot, 'TruncSD'))[1,]
  SRR@RecDevInit <- lapply(srr, slot, 'RecDevInit')
  SRR@RecDevHist <- lapply(srr, slot, 'RecDevHist') |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  SRR@RecDevProj <- lapply(srr, slot, 'RecDevProj') |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  SRR@SpawnTimeFrac <- List2Array(lapply(srr, slot, 'SpawnTimeFrac'))[1,]
  SRR@RelRecFun <- lapply(srr, slot, 'RelRecFun')
  SRR@Misc <- lapply(srr, slot, 'RelRecFun')
  
  SRR
}

CombineSpatial <- function(List, nSim, nAges, TimeSteps) {
  Spatial <- new('spatial')
  Spatial@UnfishedDist <- lapply(List, slot, 'UnfishedDist') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep', 'Area'))
  Spatial@ProbStaying <- lapply(List, slot, 'ProbStaying') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep', 'Area'))
  Spatial@RelativeSize <- lapply(List, slot, 'RelativeSize') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Area'))
  Spatial@Movement <- lapply(List, slot, 'Movement') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'TimeStep', 'FromArea', 'ToArea'))
  Spatial@FracOther  <- lapply(List, slot, 'FracOther') 
  Spatial@Arrangement <- lapply(List, slot, 'Arrangement') 
  Spatial@CVDist  <- List2Array(lapply(List, slot, 'CVDist'))[1,]
  Spatial@CVStay <- List2Array(lapply(List, slot, 'CVStay'))[1,]
  Spatial@Misc <- lapply(List, slot, 'Misc') 
  Spatial
}


CombineDepletion <- function(List, nSim, nAges, TimeSteps) {
  Depletion <- new('depletion')
  Depletion@Initial <- lapply(List, slot, 'Initial') |>
    List2Array('Stock') 
  Depletion@Final <- lapply(List, slot, 'Final') |>
    List2Array('Stock') 
  Depletion@Reference <- List2Array(lapply(List, slot, 'Reference'))[1,]
  Depletion@Misc <- lapply(List, slot, 'Misc')
  Depletion
}






CombineFishingMortality <- function(List, nSim, nAges, TimeSteps) {
  FishingMortality <- FishingMortality()
  
  FishingMortality@ApicalF <- lapply(List, slot, 'ApicalF') |> 
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'TimeStep', 'Fleet'))
  FishingMortality@DeadAtAge <- lapply(List, slot, 'DeadAtAge') |> 
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Age', 'TimeStep', 'Fleet')) 
  FishingMortality@RetainAtAge <- lapply(List, slot, 'RetainAtAge') |> 
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Age', 'TimeStep', 'Fleet')) 
  FishingMortality@Misc <- lapply(List, slot, 'Misc')
  FishingMortality
}

CombineEffort <- function(List, nSim, nAges, TimeSteps) {
  Effort <- Effort()
  Effort@Effort <- lapply(List, slot, 'Effort') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'TimeStep', 'Fleet')) 
  
  Effort@Catchability <- lapply(List, slot, 'Catchability') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'TimeStep', 'Fleet')) 
  
  Effort@qCV <- lapply(List, slot, 'qCV') 
  Effort@qInc <- lapply(List, slot, 'qInc') 
  Effort@Vessels <- lapply(List, slot, 'Vessels') 
  Effort@Trips <- lapply(List, slot, 'Trips') 
  Effort@MaxVessels <- List2Array(lapply(List, slot, 'MaxVessels'))[1,]
  Effort@MaxTrips <- List2Array(lapply(List, slot, 'MaxTrips'))[1,]
  Effort@Units <- List2Array(lapply(List, slot, 'Units'))[1,]
  Effort@Misc <- lapply(List, slot, 'Misc')
  
  Effort
}

CombineDistribution <- function(List, nSim, nAges, TimeSteps) {
  Distribution <- Distribution()
  Distribution@Closure <- lapply(List, slot, 'Closure') |>
    List2Array() |>
    aperm(c('Sim', 'TimeStep', 'Fleet', 'Area'))
  
  Distribution@Cost <- lapply(List, slot, 'Cost')
  Distribution@Effort <- lapply(List, slot, 'Effort')
  Distribution@Misc <- lapply(List, slot, 'Misc')
  Distribution
}