ExtendFleet <- function(FleetList, AgeClasses, nSim, Years, silent=FALSE, id=NULL) {
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }
    
  
  # Effort
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@Effort@Effort <- Extend(fleet@Effort@Effort, nSim, AgeClasses=NULL, Years)
    fleet@Effort@Distribution <- Extend(fleet@Effort@Distribution, nSim, AgeClasses=NULL, Years)
    fleet@Effort@Targeting <- Extend(fleet@Effort@Targeting, nSim, AgeClasses=NULL, Years)
    fleet
  }) 
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }
  
  # Catchability 
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@Catchability@Efficiency <- Extend(fleet@Catchability@Efficiency, nSim, AgeClasses=NULL, Years)
    fleet
  })
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }
 
  # Selectivity 
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@Selectivity@MeanAtAge <- Extend(fleet@Selectivity@MeanAtAge, nSim, AgeClasses, Years)
    fleet
  })
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }
  
  # Retention 
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@Retention@MeanAtAge <- Extend(fleet@Retention@MeanAtAge, nSim, AgeClasses, Years)
    fleet
  })
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }
  
  # DiscardMortality 
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@DiscardMortality@MeanAtAge <- Extend(fleet@DiscardMortality@MeanAtAge, nSim, AgeClasses, Years)
    fleet
  })
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }

  # Closure
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@Closure <- Extend(fleet@Closure, nSim, AgeClasses=NULL, Years)
    fleet
  })
  
  # WeightFleet
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@WeightFleet <- Extend(fleet@WeightFleet, nSim, AgeClasses, Years)
    fleet
  })
 
  # BioEconomic
  FleetList <- purrr::map(FleetList, \(fleet) {
    fleet@BioEconomic <- Extend(fleet@BioEconomic, nSim, AgeClasses, Years)
    fleet
  })
  
  
  if (!silent) {
    cli::cli_progress_update(id=id)
  }
  
  FleetList
}
# 
# CombineFleetObject <- function(List, nSim, AgeClasses, Years) {
#   out <- new(class(List[[1]]))
#   nms <- slotNames(out)
#   
#   if ('MeanAtAge' %in% nms) {
#     out@MeanAtAge <- lapply(List, slot, 'MeanAtAge') |>
#       purrr::map(ExtendYears, Years) |>
#       List2Array('Fleet') |>
#       ExtendSims(nSim) |>
#       ExtendAges(AgeClasses)
#   }
#   
#   if ('MeanAtLength' %in% nms) {
#     out@MeanAtLength <- lapply(List, slot, 'MeanAtLength') |>
#       purrr::map(ExtendYears, Years) |>
#       List2Array('Fleet') |>
#       ExtendSims(nSim) 
#   } 
#   
#   if ('MeanAtWeight' %in% nms) {
#     out@MeanAtWeight <- lapply(List, slot, 'MeanAtWeight') |>
#       purrr::map(ExtendYears, Years) |>
#       List2Array('Fleet') |>
#       ExtendSims(nSim) 
#   }
#   
#   if ('Classes' %in% nms) 
#     out@Classes <- lapply(List, slot, 'Classes') 
#   
#   if ('Misc' %in% nms) 
#     out@Misc <- lapply(List, slot, 'Misc')
#   
#   out
# }
# 
# CopySlots <- function(ObjectOut,
#                       ObjectIn, 
#                       Concate=c("nYear", "pYear", "nSim", 
#                                 "CurrentYear", "TimeUnits",
#                                 "Seasons"),
#                       List=c('Misc', 'Log')) {
#   Slots <- slotNames(ObjectOut)
#   for (sl in Concate) {
#     if (sl %in% Slots) {
#       slot(ObjectOut, sl) <- List2Array(lapply(ObjectIn, slot, sl))[1,] |> unique()
#     }
#   }
#   
#   for (sl in List) {
#     if (sl %in% Slots) {
#       slot(ObjectOut, sl) <- lapply(ObjectIn, slot, sl)
#     }
#   }
#   
#   ObjectOut
# }
