
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
  
  Hist@OM@Stock <- purrr::map(Hist@OM@Stock, \(x)  {
    x@SRR@SPFrom <- match(x@SRR@SPFrom,StockNames(OM))
    x
  })
  

  nAgesList <- purrr::map(Hist@OM@Stock, \(Stock) 
                      length(Stock@Ages@Classes))
  
  Hist@OM@Fleet <- purrr::map2(Hist@OM@Fleet, nAgesList, \(FleetList,nAges)
                               Fleet2Hist(FleetList, nAges, nSim=nSim(OM), TimeSteps=TimeSteps(OM), nArea, silent, id)
  )
  
  Hist@Number <- ListArraySimAgeTimeArea(OM, 'Historical') 
  
  Hist@Biomass <- ListArraySimAgeTime(OM, 'Historical') |> lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'TimeStep'))
  Hist@SBiomass <- Hist@Biomass 
  Hist@SProduction <- Hist@Biomass 
  
  Hist@Removals <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist@Landings <- Hist@Removals
  
  Hist@Effort <- ListArraySimAgeTimeFleet(OM, 'Historical') |> lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'TimeStep', 'Fleet'))
  
  for (st in 1:nStock(OM)) {
    for (fl in 1:nFleet(OM)) {
      Hist@Effort[,st,,fl] <- OM@Fleet[[st]][[fl]]@Effort@Effort    
    }
  }
  
  Hist@FDeadAtAge <- ListArraySimAgeTimeFleet(OM, 'Historical') 
  Hist@FRetainAtAge <- Hist@FDeadAtAge
  
  Hist@EffortArea <- ListArraySimAgeTimeFleetArea(OM, 'Historical')|>  lapply(DropDimension, 'Age', FALSE)
  # TODO add from Fleet@Distribution if available 
  
  Hist@FDeadAtAgeArea <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist@FRetainAtAgeArea <- Hist@FDeadAtAgeArea
  
  
  if (!silent) 
    cli::cli_progress_done()

  Hist
}
  

StockObject <- function(object, nSim, nAges, TimeSteps) {
  nms <- slotNames(object)
  
  if ("Pars" %in% nms)
    object@Pars <- lapply(object@Pars, ArrayExpand, nSim, nAges, TimeSteps)
  
  if ("Model" %in% nms) {
    if (!is.null(object@Model)) {
      if (inherits(object@Model, 'character')) {
        mod <- get(object@Model)
        class(mod) <- 'function'
        object@Model <- mod
      }
    }
  }
  
  if ("MeanAtAge" %in% nms)
    object@MeanAtAge <- ArrayExpand(object@MeanAtAge, nSim, nAges, TimeSteps)
  
  if ("CVatAge" %in% nms)
    object@CVatAge <- ArrayExpand(object@CVatAge, nSim, nAges, TimeSteps)
  
  if ("MeanAtLength" %in% nms)
    object@MeanAtLength <- ArrayExpand(object@MeanAtLength, nSim, nAges, TimeSteps)
  
  if ("Semelparous" %in% nms)
    object@Semelparous  <- ArrayExpand(object@Semelparous , nSim, nAges, TimeSteps)
  
  if ("Random" %in% nms)
    object@Random <- ArrayExpand(object@Random, nSim, nAges, TimeSteps)
  
  if ("ASK" %in% nms)
    object@ASK <- ArrayExpand(object@ASK, nSim, nAges, TimeSteps)
  
  
  object
  
}

Stock2Hist <- function(Stock, nSim, TimeSteps, silent=FALSE, id=NULL) {

  if (!silent)
    cli::cli_progress_update(id=id)
  
  nAges <- length(Stock@Ages@Classes)
  Stock@Length <- StockObject(Stock@Length, nSim, nAges, TimeSteps)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Weight <- StockObject(Stock@Weight, nSim, nAges, TimeSteps)
  

  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@NaturalMortality <- StockObject(Stock@NaturalMortality, nSim, nAges, TimeSteps)

  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Maturity <- StockObject(Stock@Maturity, nSim, nAges, TimeSteps)


  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Fecundity <- StockObject(Stock@Fecundity, nSim, nAges, TimeSteps)

  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@SRR <- StockObject(Stock@SRR, nSim, nAges, TimeSteps)

  Stock@SRR@R0 <- ArrayExpand(Stock@SRR@R0, nSim, nAges, TimeSteps)
  Stock@SRR@SD <- ArrayExpand(Stock@SRR@SD, nSim, nAges, TimeSteps)
  Stock@SRR@AC <- ArrayExpand(Stock@SRR@AC, nSim, nAges, TimeSteps)
  # Stock@SRR@RecDevInit <- ArrayExpand(Stock@SRR@RecDevInit, nSim, nAges-1, TimeSteps)
  Stock@SRR@RecDevHist <- Stock@SRR@RecDevHist |> ExpandSims(nSim)
  Stock@SRR@RecDevProj <- Stock@SRR@RecDevProj |> ExpandSims(nSim)
  Stock@SRR@SpawnTimeFrac <- ArrayExpand(Stock@SRR@SpawnTimeFrac, nSim, nAges, TimeSteps=NULL)
  
  if (is.null(Stock@SRR@SPFrom)) {
    Stock@SRR@SPFrom <- Stock@Name
  }
  
  
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
  
  Fleet@Name <- unlist(lapply(FleetList, slot, "Name"))
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

    





Hist2HistSimList <- function(Hist) {
  HistSimList <- purrr::map(1:nSim(Hist@OM), \(x)  
                            SubsetSim(Hist, Sim=x, drop=TRUE)
                            , .progress = 'Building internal object `HistSimList`')
  names(HistSimList) <- 1:nSim(Hist@OM)
  
  nstock <- nStock(HistSimList[[1]]@OM)
  
  for (i in 1:nSim(Hist@OM)) {
    HistSimList[[i]]@FDeadAtAgeArea <- lapply(HistSimList[[i]]@FDeadAtAgeArea, Array2List, 2)
    HistSimList[[i]]@FRetainAtAgeArea <- lapply(HistSimList[[i]]@FRetainAtAgeArea, Array2List, 2) 
    HistSimList[[i]]@Removals <- lapply(HistSimList[[i]]@Removals, Array2List, 2)
    HistSimList[[i]]@Landings <- lapply(HistSimList[[i]]@Landings, Array2List, 2) 
    
    for (st in 1:nstock) {
      movement <- HistSimList[[i]]@OM@Stock[[st]]@Spatial@Movement
      HistSimList[[i]]@OM@Stock[[st]]@Spatial@Movement <- Array2List(movement, 4)  
    }
  }
  
  
  class(HistSimList) <- 'histsimlist'
  HistSimList
}
    
  
# Convert HistSimList back to Hist
HistSimList2Hist <- function(Hist, HistSimList, TimeSteps=NULL) {
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist@OM, "Historical")
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  # Stock
  Hist <-  HistSimListStock(Hist, HistSimList)

  # Fleet 
  Hist <- HistSimListFleet(Hist, HistSimList)
  
  # Time Series
  Hist <- HistSimListTimeSeries(Hist, HistSimList, TimeSteps)

  Hist
}
  

HistSimListStock <- function(Hist, HistSimList) {
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    Stock <- Hist@OM@Stock[[st]] 
    StockList <- purrr::map(HistSimList, \(x) x@OM@Stock[[st]])
    Stock <- StockList2SimArray(Stock, StockList, "Length")
    Stock <- StockList2SimArray(Stock, StockList, "Weight")
    Stock <- StockList2SimArray(Stock, StockList, "NaturalMortality")
    Stock <- StockList2SimArray(Stock, StockList, "Maturity")
    Stock <- StockList2SimArray(Stock, StockList, "Fecundity")
    Stock <- StockList2SimArray(Stock, StockList, "SRR")
    Hist@OM@Stock[[st]] <- Stock
  }
  Hist
}

HistSimListFleet <- function(Hist, HistSimList) {
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    Fleet <- Hist@OM@Fleet[[st]]
    FleetList <- purrr::map(HistSimList, \(x) x@OM@Fleet[[st]])
    
    Fleet@Effort@Catchability <- purrr::map(FleetList, \(x) x@Effort@Catchability) |> 
      List2Array('Sim') |>
      aperm(c('Sim','TimeStep', 'Fleet'))
    
    # Fleet@Selectivity@MeanAtAge <- purrr::map(FleetList, \(x) x@Selectivity@MeanAtAge) |> 
    #   List2Array('Sim') |>
    #   aperm(c('Sim', 'Age', 'TimeStep', 'Fleet'))
    # 
    # Fleet@Retention@MeanAtAge <- purrr::map(FleetList, \(x) x@Retention@MeanAtAge) |> 
    #   List2Array('Sim') |>
    #   aperm(c('Sim', 'Age', 'TimeStep', 'Fleet'))
    # 
    # Fleet@DiscardMortality@MeanAtAge <- purrr::map(FleetList, \(x) x@DiscardMortality@MeanAtAge) |> 
    #   List2Array('Sim') |>
    #   aperm(c('Sim', 'Age', 'TimeStep', 'Fleet'))

    Hist@OM@Fleet[[st]] <- Fleet
  }
  Hist
}

StockList2SimArray <- function(Stock, StockList, slot="Length") {
  nms <- slotNames(slot(Stock, slot))
  
  if ("Pars" %in% nms) {
    Pars <- slot(Stock, slot)@Pars
    if (!any(lapply(Pars, is.na) |> unlist())) {
      slot(Stock, slot)@Pars <- purrr::map(StockList, \(x) slot(x, slot)@Pars) |> 
        ReverseList() |>
        purrr::map(List2Array, 'Sim') |>
        purrr::map(aperm, c('Sim', 'TimeStep'))
    }
    
  }
  
  
  if ("MeanAtAge" %in% nms)
    slot(Stock, slot)@MeanAtAge <- purrr::map(StockList, \(x) slot(x, slot)@MeanAtAge) |> 
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
  
  if ("CVatAge" %in% nms)
    slot(Stock, slot)@CVatAge <- purrr::map(StockList, \(x) slot(x, slot)@CVatAge) |> 
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
  
  if ("MeanAtLength" %in% nms)
    slot(Stock, slot)@MeanAtLength <- purrr::map(StockList, \(x) slot(x, slot)@MeanAtLength) |> 
      List2Array('Sim') |>
      aperm(c('Sim', 'Class', 'TimeStep'))
  
  
  Stock
}


  
HistSimListTimeSeries <- function(Hist, HistSimList, TimeSteps= NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist@OM, "Historical")
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    Hist@Number[[st]] <- purrr::map(HistSimList, \(HistSim) {
      AddDimNames(HistSim@Number[[st]],  c("Age", "TimeStep", "Area"), TimeSteps=TimeSteps)
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Area'))
  }
  
  Hist@Biomass <- purrr::map(HistSimList, \(HistSim)
                             AddDimNames(HistSim@Biomass, c('Stock', 'TimeStep'), TimeSteps=TimeSteps,
                                         values=list(StockNames))
  ) |> List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "TimeStep"))
  
  Hist@SBiomass <- purrr::map(HistSimList, \(HistSim)
                              AddDimNames(HistSim@SBiomass, c('Stock', 'TimeStep'), TimeSteps=TimeSteps,
                                          values=list(StockNames))
  ) |> List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "TimeStep"))
  
  Hist@SProduction <- purrr::map(HistSimList, \(HistSim)
                                 AddDimNames(HistSim@SProduction, c('Stock', 'TimeStep'), TimeSteps=TimeSteps,
                                             values=list(StockNames))
  ) |> List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "TimeStep"))
  
  
  for (st in 1:nStock) {
    Hist@Removals[[st]] <- purrr::map(HistSimList, \(HistSim) {
      List2Array(HistSim@Removals[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"), 
                    TimeSteps=TimeSteps,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Fleet', 'Area'))
    
    Hist@Landings[[st]] <- purrr::map(HistSimList, \(HistSim) {
      List2Array(HistSim@Landings[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"), 
                    TimeSteps=TimeSteps,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Fleet', 'Area'))
  }
  
  Hist@Effort <- purrr::map(HistSimList, \(HistSim)
                            AddDimNames(HistSim@Effort, c('Stock', 'TimeStep', 'Fleet'), TimeSteps=TimeSteps,
                                        values=c(list(StockNames), list(NA), list(FleetNames)))) |> 
    List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "TimeStep", "Fleet"))
  
  
  
  for (st in 1:nStock) {
    Hist@FDeadAtAgeArea[[st]] <- purrr::map(HistSimList, \(HistSim) {
      List2Array(HistSim@FDeadAtAgeArea[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"), 
                    TimeSteps=TimeSteps,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Fleet', 'Area'))
    
    Hist@FDeadAtAge[[st]] <- purrr::map(HistSimList, \(HistSim) {
      HistSim@FDeadAtAge[[st]] |>
        AddDimNames(c("Age", "TimeStep", "Fleet"), 
                    TimeSteps=TimeSteps,
                    values=c(list(NA), list(NA), list(FleetNames)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Fleet'))
    
    
    Hist@FRetainAtAgeArea[[st]] <- purrr::map(HistSimList, \(HistSim) {
      List2Array(HistSim@FRetainAtAgeArea[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"), 
                    TimeSteps=TimeSteps,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Fleet', 'Area'))
    
    Hist@FRetainAtAge[[st]] <- purrr::map(HistSimList, \(HistSim) {
      HistSim@FRetainAtAge[[st]] |>
        AddDimNames(c("Age", "TimeStep", "Fleet"), 
                    TimeSteps=TimeSteps,
                    values=c(list(NA), list(NA), list(FleetNames)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'TimeStep', 'Fleet'))
    
    
    Hist@EffortArea[[st]] <- purrr::map(HistSimList, \(HistSim) 
                                        AddDimNames(HistSim@EffortArea[[st]], c('TimeStep', 'Fleet', "Area"), TimeSteps=TimeSteps,
                                                    values=c(list(NA), list(FleetNames), list(NA)))) |> 
      List2Array("Sim") |>
      aperm(c("Sim", "TimeStep", "Fleet", 'Area'))
    
    
  }
  
  Hist@Effort <- purrr::map(HistSimList, \(HistSim)
                            AddDimNames(HistSim@Effort, c('Stock', 'TimeStep', 'Fleet'), TimeSteps=TimeSteps,
                                        values=c(list(StockNames), list(NA), list(FleetNames)))) |> 
    List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "TimeStep", "Fleet"))
  Hist
}
