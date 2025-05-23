
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
  
  OM <- Populate(OM, silent=silent)
  
  Hist <- new('hist')
  Hist@OM <- OM
  TimeSteps <- TimeSteps(OM)
  nArea <- nArea(OM)
  
  Hist@OM@Stock <- purrr::map(Hist@OM@Stock, \(Stock) 
                              Stock2Hist(Stock, nSim=nSim(OM), TimeSteps=TimeSteps(OM), silent, id)
  )
  
  nAgesList <- purrr::map(Hist@OM@Stock, \(Stock) 
                      length(Stock@Ages@Classes))
  
  Hist@OM@Fleet <- purrr::map2(Hist@OM@Fleet, nAgesList, \(FleetList,nAges)
                               Fleet2Hist(FleetList, nAges, nSim=nSim(OM), TimeSteps=TimeSteps(OM), nArea, silent, id)
  )
  

  Hist@TimeSeries@Number <- ListArraySimAgeTimeArea(OM, 'Historical')
  
  Hist@TimeSeries@Biomass <- ListArraySimAgeTime(OM, 'Historical') |> lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  Hist@TimeSeries@SBiomass <- Hist@TimeSeries@Biomass 
  Hist@TimeSeries@SProduction <- Hist@TimeSeries@Biomass 
  
  Hist@TimeSeries@Removals <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist@TimeSeries@Landings <- Hist@TimeSeries@Removals
  
  Hist@TimeSeries@Effort <- ListArraySimAgeTimeFleet(OM, 'Historical') |> lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'TimeStep', 'Fleet'))
  
  for (st in 1:nStock(OM)) {
    for (fl in 1:nFleet(OM)) {
      Hist@TimeSeries@Effort[,st,,fl] <- OM@Fleet[[st]][[fl]]@Effort@Effort    
    }
  }
  
  Hist@TimeSeries@FDeadAtAge <- ListArraySimAgeTimeFleet(OM, 'Historical') 
  Hist@TimeSeries@FRetainAtAge <- Hist@TimeSeries@FDeadAtAge
  
  Hist@TimeSeries@EffortArea <- ListArraySimAgeTimeFleetArea(OM, 'Historical')|>  lapply(DropDimension, 'Age', FALSE)
  
  Hist@TimeSeries@FDeadAtAgeArea <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist@TimeSeries@FRetainAtAgeArea <- Hist@TimeSeries@FDeadAtAgeArea
  
  
  if (!silent) 
    cli::cli_progress_done()

  Hist
}
  
Stock2Hist <- function(Stock, nSim, TimeSteps, silent=FALSE, id=NULL) {

  if (!silent)
    cli::cli_progress_update(id=id)
  
  nAges <- length(Stock@Ages@Classes)

  Stock@Length@Pars <- lapply(Stock@Length@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  Stock@Length@MeanAtAge <- ArrayExpand(Stock@Length@MeanAtAge, nSim, nAges, TimeSteps)
  Stock@Length@CVatAge <- ArrayExpand(Stock@Length@CVatAge, nSim, nAges, TimeSteps)
  Stock@Length@Random <- ArrayExpand(Stock@Length@Random, nSim, nAges, TimeSteps)
  Stock@Length@ASK <- ArrayExpand(Stock@Length@ASK, nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Weight@Pars <- lapply(Stock@Weight@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  Stock@Weight@MeanAtAge <- ArrayExpand(Stock@Weight@MeanAtAge, nSim, nAges, TimeSteps)
  Stock@Weight@MeanAtLength <- ArrayExpand(Stock@Weight@MeanAtLength, nSim, nAges, TimeSteps)
  Stock@Weight@CVatAge <- ArrayExpand(Stock@Weight@CVatAge, nSim, nAges, TimeSteps)
  Stock@Weight@Random <- ArrayExpand(Stock@Weight@Random, nSim, nAges, TimeSteps)
  Stock@Weight@ASK <- ArrayExpand(Stock@Weight@ASK, nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@NaturalMortality@Pars <- lapply(Stock@NaturalMortality@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  Stock@NaturalMortality@MeanAtAge <- ArrayExpand(Stock@NaturalMortality@MeanAtAge, nSim, nAges, TimeSteps)
  Stock@NaturalMortality@MeanAtLength <- ArrayExpand(Stock@NaturalMortality@MeanAtLength, nSim, nAges, TimeSteps)
  Stock@NaturalMortality@Random <- ArrayExpand(Stock@NaturalMortality@Random, nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Maturity@Pars <- lapply(Stock@Maturity@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  Stock@Maturity@MeanAtAge <- ArrayExpand(Stock@Maturity@MeanAtAge, nSim, nAges, TimeSteps)
  Stock@Maturity@MeanAtLength <- ArrayExpand(Stock@Maturity@MeanAtLength, nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Fecundity@Pars <- lapply(Stock@Fecundity@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  Stock@Fecundity@MeanAtAge <- ArrayExpand(Stock@Fecundity@MeanAtAge, nSim, nAges, TimeSteps)
  Stock@Fecundity@MeanAtLength <- ArrayExpand(Stock@Fecundity@MeanAtLength, nSim, nAges, TimeSteps)

  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@SRR@Pars <- lapply(Stock@SRR@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  Stock@SRR@R0 <- ArrayExpand(Stock@SRR@R0, nSim, nAges, TimeSteps)
  Stock@SRR@SD <- ArrayExpand(Stock@SRR@SD, nSim, nAges, TimeSteps)
  Stock@SRR@AC <- ArrayExpand(Stock@SRR@AC, nSim, nAges, TimeSteps)
  # Stock@SRR@RecDevInit <- ArrayExpand(Stock@SRR@RecDevInit, nSim, nAges-1, TimeSteps)
  Stock@SRR@RecDevHist <- ArrayExpand(Stock@SRR@RecDevHist, nSim, nAges, TimeSteps=NULL)
  Stock@SRR@RecDevProj <- ArrayExpand(Stock@SRR@RecDevProj, nSim, nAges, TimeSteps=NULL)
  Stock@SRR@SpawnTimeFrac <- ArrayExpand(Stock@SRR@SpawnTimeFrac, nSim, nAges, TimeSteps=NULL)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Spatial@UnfishedDist <- ArrayExpand(Stock@Spatial@UnfishedDist, nSim, nAges, TimeSteps)
  Stock@Spatial@ProbStaying <- ArrayExpand(Stock@Spatial@ProbStaying, nSim, nAges, TimeSteps)
  Stock@Spatial@RelativeSize <- ArrayExpand(Stock@Spatial@RelativeSize, nSim, nAges, TimeSteps)
  Stock@Spatial@Movement <- ArrayExpand(Stock@Spatial@Movement, nSim, nAges, TimeSteps)
  Stock@Spatial@FracOther <- ArrayExpand(Stock@Spatial@FracOther, nSim, nAges, TimeSteps)
  Stock@Spatial@Arrangement <- ArrayExpand(Stock@Spatial@Arrangement, nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)

  Stock@Depletion@Initial <- ArrayExpand(Stock@Depletion@Initial, nSim, nAges, TimeSteps)
  Stock@Depletion@Final <- ArrayExpand(Stock@Depletion@Final, nSim, nAges, TimeSteps)
  
  Stock
}
    
Fleet2Hist <- function(FleetList, nAges, nSim, TimeSteps, nArea, silent=FALSE, id=NULL) {
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet <- new('fleet')
  
  
  Fleet@FishingMortality <- CombineFishingMortality(lapply(FleetList, slot, 
                                                           'FishingMortality'),
                                                    nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  

 
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Effort <- CombineEffort(lapply(FleetList, slot, 'Effort'),
                                nSim, nAges, TimeSteps)
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet@Selectivity <- CombineFleetObject(lapply(FleetList, slot, "Selectivity"), nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet@Retention <- CombineFleetObject(lapply(FleetList, slot, "Retention"), nSim, nAges, TimeSteps)
  
  if (is.null(Fleet@Retention@MeanAtAge)) {
    Fleet@Retention@MeanAtAge <- Fleet@Selectivity@MeanAtAge
    Fleet@Retention@MeanAtAge[] <- 1
  }
  
  
  Fleet@DiscardMortality <- CombineFleetObject(lapply(FleetList, slot, "DiscardMortality"), nSim, nAges, TimeSteps)
  if (is.null(Fleet@DiscardMortality@MeanAtAge)) {
    Fleet@DiscardMortality@MeanAtAge <- Fleet@Selectivity@MeanAtAge
    Fleet@DiscardMortality@MeanAtAge[] <- 0
  }
  
    
    
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Distribution <- CombineDistribution(lapply(FleetList, slot, 
                                                   'Distribution'),
                                            nSim, nAges, TimeSteps, nArea)
  
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



CombineFleetObject <- function(List, nSim, nAges, TimeSteps) {
  out <- new(class(List[[1]]))
  nms <- slotNames(out)
  
  if ('MeanAtAge' %in% nms) 
    out@MeanAtAge <- lapply(List, slot, 'MeanAtAge') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet')

  if ('MeanAtLength' %in% nms) 
    out@MeanAtLength <- lapply(List, slot, 'MeanAtLength') |>
    purrr::map(ArrayExpand, nSim, nAges, TimeSteps) |>
    List2Array('Fleet')
  
  if ('Classes' %in% nms) 
    out@Classes <- lapply(List, slot, 'Classes') 
  
  if ('Misc' %in% nms) 
    out@Misc <- lapply(List, slot, 'Misc')
  
  out
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

CombineDistribution <- function(List, nSim, nAges, TimeSteps, nArea) {
  Distribution <- Distribution()
  Distribution@Closure <- lapply(List, slot, 'Closure') |>
    List2Array() |>
    aperm(c('Sim', 'TimeStep', 'Fleet', 'Area'))
  
  Distribution@Cost <- lapply(List, slot, 'Cost')
  
  EffortArea <- lapply(List, slot, 'EffortArea')
  for (i in seq_along(EffortArea)) {
    if (is.null(EffortArea[[i]])) {
      EffortArea[[i]] <- array(tiny/2, c(nSim, length(TimeSteps), nArea),
                               dimnames = list(Sim=1:nSim,
                                               TimeStep=TimeSteps,
                                               Area=1:nArea))
    }
    if (any((dim(EffortArea[[i]]) != c(nSim, length(TimeSteps), nArea)))) {
      cli::cli_abort("Dimensions not correct for EffortArea")
    }
  }
  
  Distribution@EffortArea <- EffortArea |>
    List2Array() |>
    aperm(c('Sim', 'TimeStep', 'Fleet', 'Area'))
  
  Distribution@Misc <- lapply(List, slot, 'Misc')
  Distribution
}

    
    
  

  
  