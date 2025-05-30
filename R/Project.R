
ExtendHist <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps=TimeSteps(Hist@OM)
  
  slots <- slotNames('timeseries')
  slots <- slots[!slots=='Misc']
  for (sl in slots) {
    object <- slot(Hist, sl) 
    if (is.list(object)) {
      object <- purrr::map(object, \(x) ExpandTimeSteps(x, TimeSteps, default = tiny/2))
    } else {
      object <- ExpandTimeSteps(object, TimeSteps, default = tiny/2)
    }
    slot(Hist, sl) <- object
  }
  Hist
  # 
  # if (isS4(object)) {
  #   nms <- slotNames(object)
  #   for (i in seq_along(nms)) {
  #     slot(object, nms[i]) <- Recall(slot(object, nms[i]), TimeSteps)
  #   }  
  # }
  # 
  # if (is.list(object)) {
  #   for (i in seq_along(object)) {
  #     object[[i]] <- Recall(object[[i]], TimeSteps)
  #   }  
  # }
  # if (inherits(object,'array'))  {
  #   dnames <- names(dimnames(object))
  #   if ("TimeStep" %in% dnames)
  #     object <- ExpandTimeSteps(object, TimeSteps, default=tiny/2)
  # }
  # object

}

ProjectDEV <- function(Hist=NULL, MPs=NA, silent=FALSE) {
  
  # Extend Arrays for Projection TimeSteps 
  Proj <- ExtendHist(Hist)
  
  # List of `Hist` objects, each with one simulation
  ProjSimList <- Hist2HistSimList(Proj)
  
  # Populate Number-at-Age at Beginning of Projection TimeStep
  LastHistTS <- tail(TimeSteps(Hist@OM,"Historical"),1)
  ProjSimList <- purrr::map(ProjSimList, \(ProjSim) PopulateNumberNext_(ProjSim, LastHistTS))
  # TODO add Recruitment for first projection time step if Age-Recruitment = 1 (i.e use SP from last historical)
  
  # Calculate Reference Catch 
  ProjSimList <- OptimRefYield(ProjSimList) 
  
  
  # Process MPs
  # Hist@MPs - need to store in Misc ?
  
  MSE <- Hist2MSE(Hist, MPs)

  TimeStepsAll <- TimeSteps(Hist@OM)
  TimeStepsProj <- TimeSteps(Hist@OM, "Projection")
  nStock <- nStock(Hist@OM)
  
  nMPs <- length(MPs)
  # Projection MP loop
  cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')
  
  for (mp in seq_along(MPs)) {
    
    
    # *********************************** # 
    ProjSim <- ProjSimList[[1]]
    MP <- MPs[1]
    # ProjectMP
    ProjSim@Data <- list(Data)
    
    # TODO Simulate historical data 
    # *********************************** # 
    
    
    # TODO keep this messages
    ProjSimListMP <- purrr::map(ProjSimList, \(ProjSim) 
                                ProjectMP(ProjSim, MPs[mp]),
                                .progress = list(
                                  type = "tasks", 
                                  format = "Running {.val {MPs[mp]}} {cli::pb_bar} {cli::pb_percent}",
                                  clear = TRUE))
    cli::cli_alert_success('{.val {MPs[mp]}}')
    
    MSE <- UpdateMSEObject(MSE, ProjSimListMP, mp)
    
  }
    
  
  matplot(MSE@Removals[1,1,,1,], type='b')
  
  MSE@Effort[1,1,,1,]
  # TO DO
  MSE@EffortArea$`Day octopus` 
  
  MPs
  
 b <- apply(MSE@Biomass, c("TimeStep", "MP"), quantile, 0.025)
  matplot(b, type='b')
  
  
  
  
  
}


UpdateMSEObject <- function(MSE, ProjSimListMP, mp) {
  nStock <- nStock(ProjSimListMP[[1]]@OM)
  
  TimeStepsAll <- TimeSteps(ProjSimListMP[[1]]@OM)
  TimeStepsProj <- TimeSteps(ProjSimListMP[[1]]@OM, "Projection")
  FleetNames <- as.vector(ProjSimListMP[[1]]@OM@Fleet[[1]]@Name)
  StockNames <- StockNames(ProjSimListMP[[1]]@OM)
  for (st in 1:nStock) {
    MSE@Number[[st]][,,,,mp] <- purrr::map(ProjSimListMP, \(x) x@Number[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "TimeStep", "Area", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "Age", "TimeStep", "Area"))
  }
  
  MSE@Biomass[,,,mp] <- purrr::map(ProjSimListMP, \(x) x@Biomass) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "TimeStep", "Sim"), TimeSteps=TimeStepsAll) |> 
    ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
    aperm(c("Sim", "Stock", "TimeStep"))
  
  MSE@SBiomass[,,,mp] <- purrr::map(ProjSimListMP, \(x) x@SBiomass) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "TimeStep", "Sim"), TimeSteps=TimeStepsAll) |> 
    ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
    aperm(c("Sim", "Stock", "TimeStep"))
  
  MSE@SProduction[,,,mp] <- purrr::map(ProjSimListMP, \(x) x@SProduction) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "TimeStep", "Sim"), TimeSteps=TimeStepsAll) |> 
    ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
    aperm(c("Sim", "Stock", "TimeStep"))
  
  
  MSE@Removals[,,,,mp] <- purrr::map(ProjSimListMP, \(x) {
    purrr::map(x@Removals, \(y) {
      List2Array(y, "TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
        apply(c("TimeStep", "Fleet"), sum)
    }) 
  } ) |> 
    ReverseList() |>
    purrr::map(List2Array,"Sim") |>
    List2Array("Stock") |>
    aperm(c("Sim", "Stock", "TimeStep", "Fleet"))
  
  MSE@Landings[,,,,mp] <- purrr::map(ProjSimListMP, \(x) {
    purrr::map(x@Landings, \(y) {
      List2Array(y, "TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
        apply(c("TimeStep", "Fleet"), sum)
    }) 
  } ) |> 
    ReverseList() |>
    purrr::map(List2Array,"Sim") |>
    List2Array("Stock") |>
    aperm(c("Sim", "Stock", "TimeStep", "Fleet"))
  
  
  MSE@Effort[,,,,mp] <- purrr::map(ProjSimListMP, \(x) 
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
    
    
  MSE
}