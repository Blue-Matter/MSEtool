
ExtendHist <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist@OM)
  
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

MSE2Hist <- function(MSE) {
  Hist <- Hist()
  Hist@OM <- MSE@OM
  Hist@Unfished <- MSE@Unfished
  Hist@RefPoints <- MSE@RefPoints
  Hist@Data <- list(MSE@OM@Data)
  
  slots <- slotNames(MSE@Hist)
  for (sl in slots)  {
    slot(Hist, sl) <- slot(MSE@Hist, sl) 
  }
  Hist
}

#' @export
ProjectDEV <- function(Hist=NULL, MPs=NA, silent=FALSE, parallel=FALSE) {
  
  on.exit(cli::stop_app())
  
  # Set up parallel processing 
  if (parallel & !snowfall::sfIsRunning())
    setup()
  ncpus <- set_parallel(any(unlist(parallel)))
  
  # Set pbapply functions 
  .lapply <- define.lapply(silent) 
  .sapply <- define.sapply(silent)

  # Extend Arrays for Projection TimeSteps

  MSEobj <- FALSE
  if (inherits(Hist,'mse')) {
    MSE <- Hist
    Hist <- MSE2Hist(Hist) 
  }
  
  Proj <- ExtendHist(Hist)
  
  
  # List of `Hist` objects, each with one simulation
  ProjSimList <- Hist2HistSimList(Proj)
  
  # Calculate Reference Catch 
  # TODO calculate externally with option in Simulate or OM@Control
   
  # Populate Number-at-Age at Beginning of Projection TimeStep
  LastHistTS <- tail(TimeSteps(Hist@OM,"Historical"),1)
  ProjSimList <- purrr::map(ProjSimList, \(ProjSim) PopulateNumberNext_(ProjSim, LastHistTS))
  # TODO add Recruitment for first projection time step if Age-Recruitment = 1 (i.e use SP from last historical)
  
  if(!MSEobj) {
    MSE <- Hist2MSE(Hist, MPs)  
  } else {
    newMPs <- which(!MPs %in% names(MSE@MPs))
    if (length(newMPs)>0) {
      newMPList <- lapply(MPs[newMPs], get)
      names(newMPList) <- MPs[newMPs]
      attributes(newMPList)$complete <- rep(FALSE, length(newMPList))
      MSE@MPs <- c(MSE@MPs, newMPList)
      
      
      # TODO - add dimensions for new MPs 
      # check if MP has changed
      # check if MSE object has changed
      # run new MPs and changed MP - or all MPs if MSE object has changed
      stop()
      MSE@Number[[1]] 
      MSE@Number[[1]] |> dimnames()
      
    }
  }
  
  TimeStepsHist <- TimeSteps(ProjSimList[[1]]@OM, "Historical")
  TimeStepsProj <- TimeSteps(ProjSimList[[1]]@OM, "Projection")
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj)
  nStock <- nStock(Hist@OM)
  
  nMPs <- length(MPs)
  # Projection MP loop
  mp <- 1 # for debugging 
  cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')
  
  for (mp in seq_along(MPs)) {
    
    # *********************************** # 
 
    # TODO Simulate historical data 
    # *********************************** # 
  
    MP <- MPs[mp]
    
    # TODO add option to specify Interval by MP
    ind <- seq(1, by=ProjSimList[[1]]@OM@Interval, to=length(TimeStepsProj)) 
    ManagementTimeSteps <- TimeStepsProj[ind] # time steps where management will be implemented
    
    st <- Sys.time()
    ProjSimListMP <- purrr::map(ProjSimList, \(ProjSim) 
                                try(
                                  ProjectMP(ProjSim, MP, TimeStepsHist, TimeStepsProj, ManagementTimeSteps),
                                  silent=TRUE
                                ),
                                .progress = list(
                                  type = "tasks", 
                                  caller = environment(),
                                  format = "Projecting {.val {MP}} {cli::pb_bar} {cli::pb_percent}",
                                  clear = TRUE))
    end <- Sys.time()
    
    elapse_secs <- round(difftime(time1 = end, time2 = st, units = "secs"),2) |> as.numeric()
    elapse_auto <- round(difftime(time1 = end, time2 = st, units = "auto"),2) |> format()
    
    incElapse <- FALSE
    if (elapse_secs > 5) {
      incElapse <- TRUE
    }  
    
    check <- CheckMSERun(ProjSimListMP, MP)
    
    if (check) {
      if (incElapse) {
        cli::cli_alert_success('{.val {MP}} ({elapse_auto})')
      } else {
        cli::cli_alert_success('{.val {MP}}')  
      }
      
      attributes(MSE@MPs)$complete[mp] <- TRUE
      MSE <- UpdateMSEObject(MSE, ProjSimListMP, mp, TimeStepsAll, TimeStepsProj)
    }
  }

  MSE
}

CheckMSERun <- function(ProjSimListMP, MP) {
  ErrorCheck <- unlist(lapply(ProjSimListMP, class))
  if (any(ErrorCheck=='try-error')) {
    if (all(ErrorCheck=='try-error')) {
      cli::cli_alert_danger(c("x"="Error: {.val {MP}} failed for all simulations. Skipping this MP"))
    } else {
      ind <- which(ErrorCheck=='try-error')
      cli::cli_alert_danger(c("x"="Error: {.val {MP}} failed for simulations {.val {ind}}. Skipping this MP"))
    }
    time <- format(Sys.time(), "%Y%m%d%H%M")
    logFile <- paste0(time, "_", MP, '.log')
    logFile <- file.path(getwd(), logFile)
    file.create(logFile)
    for (i in seq_along(ProjSimListMP)) {
      if (inherits(ProjSimListMP[[i]],'try-error')) {
        cat(paste0("\nSimulation ", i , '\n'), file=logFile, append=TRUE)
        cat(ProjSimListMP[[i]], file=logFile, append=TRUE)
      }
    }
    cli::cli_alert_info("Writing error log to {.file {logFile}}")
    return(FALSE)
  }
  TRUE
}


UpdateMSEObject <- function(MSE, ProjSimListMP, mp, TimeStepsAll, TimeStepsProj) {

  nStock <- nStock(ProjSimListMP[[1]]@OM)
  
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
    
  for (st in 1:nStock) {
    MSE@FDeadAtAge[[st]][,,,,mp] <- purrr::map(ProjSimListMP, \(x) x@FDeadAtAge[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "TimeStep", "Fleet", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet"))
    
    MSE@FRetainAtAge[[st]][,,,,mp] <- purrr::map(ProjSimListMP, \(x) x@FRetainAtAge[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "TimeStep", "Fleet", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet"))
    
    
    MSE@EffortArea[[st]][,,,,mp] <- purrr::map(ProjSimListMP, \(x) x@EffortArea[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("TimeStep", "Fleet", "Area", "Sim"), TimeSteps=TimeStepsAll) |> 
      ArraySubsetTimeStep(TimeSteps=TimeStepsProj) |>
      aperm(c("Sim", "TimeStep", "Fleet", "Area"))
    
    MSE@FDeadAtAgeArea[[st]][,,,,,mp] <- purrr::map(ProjSimListMP, \(x) {
      x@FDeadAtAgeArea[[st]] |> 
        List2Array("TimeStep") |>
        AddDimNames(c("Age", "Fleet", "Area", "TimeStep"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    TimeSteps=TimeStepsAll) |>
        ArraySubsetTimeStep(TimeSteps=TimeStepsProj) 
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", "Age", "TimeStep", "Fleet", "Area"))
    
    
    MSE@FRetainAtAgeArea[[st]][,,,,,mp] <- purrr::map(ProjSimListMP, \(x) {
      x@FRetainAtAgeArea[[st]] |> 
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
  MSE@Misc$Advice[[MPName]] <- lapply(ProjSimListMP, slot, 'Misc') |> lapply("[[", "MPAdvice")
  
  # keep Retention if changed 
  MSE <- KeepRetention(MSE, ProjSimListMP, mp)
  
  MSE
}

#' @export
ReProject <- function(MSE, MPs, mp) {
  
  Hist <- Hist()
  Hist@OM <- MSE@OM
  Hist@Unfished <- MSE@Unfished
  
  slots <- slotNames(MSE@Hist)
  for (sl in slots) {
    slot(Hist, sl) <- slot(MSE@Hist,sl)
  }
  
  Proj <- ExtendHist(Hist)
  ProjSimList <- Hist2HistSimList(Proj)
  
  TimeStepsAll <- TimeSteps(ProjSimList[[1]]@OM)
  TimeStepsHist <- TimeSteps(ProjSimList[[1]]@OM, "Historical")
  TimeStepsProj <- TimeSteps(ProjSimList[[1]]@OM, "Projection")
  
  MP <- MPs[mp]
  
  # TODO add option to specify Interval by MP
  ind <- seq(1, by=ProjSimList[[1]]@OM@Interval, to=length(TimeStepsProj)) 
  ManagementTimeSteps <- TimeStepsProj[ind] # time steps where management will be implemented
  
  st <- Sys.time()
  ProjSimListMP <- purrr::map(ProjSimList, \(ProjSim) 
                              try(
                                ProjectMP(ProjSim, MP, TimeStepsHist, TimeStepsProj, ManagementTimeSteps),
                                silent=TRUE
                              ),
                              .progress = list(
                                type = "tasks", 
                                caller = environment(),
                                format = "Projecting {.val {MP}} {cli::pb_bar} {cli::pb_percent}",
                                clear = TRUE))
  end <- Sys.time()
  elapse <- paste0(round(as.numeric(difftime(time1 = end, time2 = st, units = "secs")), 0), " Seconds")
  
  check <- CheckMSERun(ProjSimListMP, MP)

  
  if (check) {
    cli::cli_alert_success('{.val {MP}} ({elapse})')
    MSE <- UpdateMSEObject(MSE, ProjSimListMP, mp, TimeStepsAll, TimeStepsProj)
  }
  MSE
  
}


KeepRetention <- function(MSE, ProjSimListMP, mp) {
  MPretention <- purrr::map(ProjSimListMP, \(ProjSim) {
    purrr::map(ProjSim@OM@Fleet, \(fleet) {
      fleet@Retention@MeanAtAge
    }) 
  }) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    purrr::map(aperm, c('Sim', 'Age', 'TimeStep', 'Fleet'))
  
  stocks <- StockNames(MSE@OM)
  for (st in seq_along(stocks)) {
    if (!prod(MPretention[[st]] == MSE@OM@Fleet[[st]]@Retention@MeanAtAge)) {
      if (is.null(MSE@Misc$Retention)) {
        MSE@Misc$Retention <- list()
      }
      
      MPName <- names(MSE@MPs)[mp]
      MSE@Misc$Retention[[MPName]]  <- list()
      MSE@Misc$Retention[[MPName]][[stocks[st]]] <- MPretention[[st]]
      
    }
    
  }
  MSE
}