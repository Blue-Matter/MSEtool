
UpdateMSEObject <- function(MSE, SimList_MP, mp, TimeStepsHist, TimeStepsProj, MP) {
  
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj)
  
  nStock <- nStock(SimList_MP[[1]]@OM)
  
  FleetNames <- as.vector(SimList_MP[[1]]@OM@Fleet[[1]]@Name)
  StockNames <- StockNames(SimList_MP[[1]]@OM)
  for (st in 1:nStock) {
    MSE@Number[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@Number[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "TimeStep", "Area", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "Age", "TimeStep", "Area"))
  }
  
  MSE@Biomass[,,,mp] <- purrr::map(SimList_MP, \(x) x@Biomass) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "TimeStep", "Sim"), TimeSteps=TimeStepsAll) |> 
    ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
    aperm(c("Sim", "Stock", "TimeStep"))
  
  MSE@SBiomass[,,,mp] <- purrr::map(SimList_MP, \(x) x@SBiomass) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "TimeStep", "Sim"), TimeSteps=TimeStepsAll) |> 
    ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
    aperm(c("Sim", "Stock", "TimeStep"))
  
  MSE@SProduction[,,,mp] <- purrr::map(SimList_MP, \(x) x@SProduction) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "TimeStep", "Sim"), TimeSteps=TimeStepsAll) |> 
    ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
    aperm(c("Sim", "Stock", "TimeStep"))
  
  
  LandingsList <- purrr::map(SimList_MP, \(ProjSim) {
    purrr::map(ProjSim@Landings, \(Landings) {
      List2Array(Landings, "TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
        ArrayReduceDims(IncTimeStep=FALSE)
    })
  }) |> 
    ReverseList() |>
    purrr::map(List2Array,"Sim") |>
    purrr::map(aperm, c('Sim', 'Age', 'TimeStep',  'Fleet', 'Area'))
  
  MSE@Landings <- purrr::map2(MSE@Landings, LandingsList, \(MSELanding, Landings) {
    MSELanding[,,,,,mp] <- Landings
    MSELanding
  })
  
  DiscardsList <- purrr::map(SimList_MP, \(ProjSim) {
    purrr::map(ProjSim@Discards, \(Discards) {
      List2Array(Discards, "TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
        ArrayReduceDims(IncTimeStep=FALSE)
    })
  }) |> 
    ReverseList() |>
    purrr::map(List2Array,"Sim") |>
    purrr::map(aperm, c('Sim', 'Age', 'TimeStep',  'Fleet', 'Area'))
  
  MSE@Discards <- purrr::map2(MSE@Discards, DiscardsList, \(MSEDiscards, Discards) {
    MSEDiscards[,,,,,mp] <- Discards
    MSEDiscards
  })
  
  
  MSE@Effort[,,,,mp] <- purrr::map(SimList_MP, \(x) 
                                   x@Effort |> 
                                     AddDimNames(c("Stock", "TimeStep", "Fleet"),
                                                 values=c(list(StockNames),
                                                          list(NA),
                                                          list(FleetNames)),
                                                 TimeSteps=TimeStepsAll) |>
                                     ArraySubsetTimeStep(TimeSteps=TimeStepsProj)
  ) |>
    List2Array("Sim") |>
    aperm(c("Sim", "Stock", "TimeStep", "Fleet"))
  
  for (st in 1:nStock) {
    MSE@FDead[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@FDead[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "TimeStep", "Fleet", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet"))
    
    MSE@FRetain[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@FRetain[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "TimeStep", "Fleet", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet"))
    
    
    MSE@Distribution[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@Distribution[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("TimeStep", "Fleet", "Area", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "TimeStep", "Fleet", "Area"))
    
    MSE@FDeadArea[[st]][,,,,,mp] <- purrr::map(SimList_MP, \(x) {
      x@FDeadArea[[st]] |> 
        List2Array("TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) 
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet", "Area"))
    
    MSE@FRetainArea[[st]][,,,,,mp] <- purrr::map(SimList_MP, \(x) {
      x@FRetainArea[[st]] |> 
        List2Array("TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) 
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet", "Area"))
  }
  
  # Misc 
  # keep MPAdvice 
  if (is.null(MSE@Misc$Advice)) {
    MSE@Misc$Advice <- list()
  }
  MPName <- names(MSE@MPs)[mp]
  MSE@Misc$Advice[[MPName]] <- lapply(SimList_MP, slot, 'Misc') |> lapply("[[", "MPAdvice")
  MSE@Misc$Failed[[MPName]] <- lapply(SimList_MP, slot, 'Misc') |> 
    lapply("[[", "Failed") |> 
    unlist() |> 
    as.numeric()
  
  MSE <- MSE |> 
    KeepRetention(SimList_MP, mp) |>
    KeepSelectivity(SimList_MP, mp) |> 
    KeepDiscardMortality(SimList_MP, mp) |>
    AddPPD(SimList_MP, mp) |>
    ProcessLogMSE(SimList_MP, mp, MP)
  
  MSE
}



ProcessLogMSE <- function(MSE, SimList_MP, mp, MP) {
  
  LogList <- purrr::map(SimList_MP, \(ProjSim) {
    Log <- ProjSim@Log
    Log$OptDepletionRatio <- NULL
    Log
  })
  
  if (is.null(MSE@Log[["MP"]]))
    MSE@Log[["MP"]] <- list()
  
  MSE@Log[["MP"]][[MP]] <- LogList
  MSE
}


KeepSelectRetenDisc <- function(MSE, SimList_MP, mp, Slot='Retention') {
  
  ProjTimeStep <- TimeSteps(MSE@OM, 'Projection')
  AtAge <- purrr::map(SimList_MP, \(ProjSim) {
    purrr::map(ProjSim@OM@Fleet, \(fleet) {
      slot(fleet, Slot)@MeanAtAge |> ArraySubsetTimeStep(ProjTimeStep) 
    }) 
  }) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    purrr::map(aperm, c('Sim', 'Age', 'TimeStep', 'Fleet')) 
  
  
  # AtLength <- purrr::map(SimList_MP, \(ProjSim) {
  #   purrr::map(ProjSim@OM@Fleet, \(fleet) {
  #     slot(fleet, Slot)@MeanAtLength
  #   }) 
  # }) |> 
  #   ReverseList() |>
  #   purrr::map(List2Array, 'Sim') |>
  #   purrr::map(aperm, c('Sim', 'Class', 'TimeStep', 'Fleet'))
  
  stocks <- StockNames(MSE@OM)
  MPName <- names(MSE@MPs)[mp]
  for (st in seq_along(stocks)) {
    
    dd <- dimnames(AtAge[[st]])
    
    omvals <- slot(MSE@OM@Fleet[[st]],Slot)@MeanAtAge |> ArraySubsetTimeStep(ProjTimeStep) |>
      ArrayExpand(nSim=length(dd$Sim), nAges=length(dd$Age), TimeSteps = dd$TimeStep)
    
    if (!prod(AtAge[[st]] == omvals)) {
      if (is.null(MSE@Misc[[Slot]])) {
        MSE@Misc[[Slot]] <- list()
      }
      if (is.null(MSE@Misc[[Slot]][[MPName]])) {
        MSE@Misc[[Slot]][[MPName]] <- list()
      }
      MSE@Misc[[Slot]][[MPName]][[stocks[st]]] <- AtAge[[st]]
    }
  }
  # MSE@Misc[[Slot]][[MPName]] <- MSE@Misc[[Slot]][[MPName]][stocks]
  MSE
  
}

KeepRetention <- function(MSE, SimList_MP, mp) {
  KeepSelectRetenDisc(MSE, SimList_MP, mp)
}

KeepSelectivity <- function(MSE, SimList_MP, mp) {
  KeepSelectRetenDisc(MSE, SimList_MP, mp, 'Selectivity')
}

KeepDiscardMortality <- function(MSE, SimList_MP, mp) {
  KeepSelectRetenDisc(MSE, SimList_MP, mp, 'DiscardMortality')
}

AddPPD <- function(MSE, SimList_MP, mp) {
  PPD <- purrr::map(SimList_MP, slot, 'Data')
  MPName <- names(MSE@MPs)[mp]
  MSE@PPD[[MPName]] <- PPD
  MSE
}

