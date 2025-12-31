ExtendFleet <- function(FleetList, AgeClasses, nSim, Years, nArea, silent=FALSE, id=NULL) {
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet <- new('fleet')
  
  Fleet@Name <- unlist(lapply(FleetList, slot, "Name"))
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet@Effort@Value <- purrr::map(FleetList, \(fleet) 
                                   ArrayExpand(fleet@Effort@Value, nSim, AgeClasses, Years)
  ) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Year', 'Fleet'))
  
  Fleet@Effort@Distribution <- purrr::map(FleetList, \(fleet) 
                                   ArrayExpand(fleet@Effort@Distribution, nSim, AgeClasses, Years)
  ) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Year', 'Fleet', 'Area'))
  
  
  if (!silent)
    cli::cli_progress_update(id=id)
  

  Fleet@Catchability@Value <- purrr::map(FleetList, \(fleet) 
                                   ArrayExpand(fleet@Catchability@Value, nSim, AgeClasses, Years)
  ) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Year', 'Fleet'))
  
  Fleet@Catchability@qArea <- purrr::map(FleetList, \(fleet) 
                                         ArrayExpand(fleet@Catchability@qArea, nSim, AgeClasses, Years)
  ) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Year', 'Fleet', 'Area'))

  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet@Selectivity <- CombineFleetObject(lapply(FleetList, slot, "Selectivity"), 
                                          nSim, AgeClasses, Years)
  
  if (!is.null(Fleet@Selectivity@MeanAtAge)) 
    dimnames(Fleet@Selectivity@MeanAtAge)$Age <- AgeClasses
  

  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet@Retention <- CombineFleetObject(lapply(FleetList, slot, "Retention"),
                                        nSim, AgeClasses, Years)
  
  if (is.null(Fleet@Retention@MeanAtAge)) {
    Fleet@Retention@MeanAtAge <- Fleet@Selectivity@MeanAtAge
    Fleet@Retention@MeanAtAge[] <- 1
  }
  
  if (!is.null(Fleet@Retention@MeanAtAge)) 
    dimnames(Fleet@Retention@MeanAtAge)$Age <- AgeClasses

  Fleet@DiscardMortality <- CombineFleetObject(lapply(FleetList, slot, "DiscardMortality"),
                                               nSim, AgeClasses, Years)
  if (is.null(Fleet@DiscardMortality@MeanAtAge)) {
    Fleet@DiscardMortality@MeanAtAge <- Fleet@Selectivity@MeanAtAge
    Fleet@DiscardMortality@MeanAtAge[] <- 0
  }
  
  if (!is.null(Fleet@DiscardMortality@MeanAtAge)) 
    dimnames(Fleet@DiscardMortality@MeanAtAge)$Age <- AgeClasses

  Fleet@Closure <- lapply(FleetList, slot, 'Closure') |> 
    purrr::map(ArrayExpand, nSim, AgeClasses, Years) |> 
    List2Array('Fleet') |>
    aperm(c('Sim', 'Year', 'Fleet', 'Area'))
  
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Fleet@WeightFleet <- lapply(FleetList, slot, 'WeightFleet') |>
    purrr::map(ArrayExpand, nSim, AgeClasses, Years) |>
    List2Array('Fleet') |>
    aperm(c('Sim', 'Age', 'Year', 'Fleet')) 
  
  dimnames(Fleet@WeightFleet)$Age <- AgeClasses
  
  Fleet@BioEconomic <- lapply(FleetList, slot, 'BioEconomic')
  Fleet <- CopySlots(Fleet, FleetList)
  
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Fleet@Years <- Years
  Fleet
}

CombineFleetObject <- function(List, nSim, AgeClasses, Years) {
  out <- new(class(List[[1]]))
  nms <- slotNames(out)
  
  if ('MeanAtAge' %in% nms) {
    out@MeanAtAge <- lapply(List, slot, 'MeanAtAge') |>
      purrr::map(ExtendYears, Years) |>
      List2Array('Fleet') |>
      ExtendSims(nSim) |>
      ExtendAges(AgeClasses)
  }
  
  if ('MeanAtLength' %in% nms) {
    out@MeanAtLength <- lapply(List, slot, 'MeanAtLength') |>
      purrr::map(ExtendYears, Years) |>
      List2Array('Fleet') |>
      ExtendSims(nSim) 
  } 
  
  if ('MeanAtWeight' %in% nms) {
    out@MeanAtWeight <- lapply(List, slot, 'MeanAtWeight') |>
      purrr::map(ExtendYears, Years) |>
      List2Array('Fleet') |>
      ExtendSims(nSim) 
  }
  
  if ('Classes' %in% nms) 
    out@Classes <- lapply(List, slot, 'Classes') 
  
  if ('Misc' %in% nms) 
    out@Misc <- lapply(List, slot, 'Misc')
  
  out
}

CopySlots <- function(ObjectOut,
                      ObjectIn, 
                      Concate=c("nYear", "pYear", "nSim", 
                                "CurrentYear", "TimeUnits",
                                "Seasons"),
                      List=c('Misc', 'Log')) {
  Slots <- slotNames(ObjectOut)
  for (sl in Concate) {
    if (sl %in% Slots) {
      slot(ObjectOut, sl) <- List2Array(lapply(ObjectIn, slot, sl))[1,] |> unique()
    }
  }
  
  for (sl in List) {
    if (sl %in% Slots) {
      slot(ObjectOut, sl) <- lapply(ObjectIn, slot, sl)
    }
  }
  
  ObjectOut
}
