GetPreviousMPAdvice <- function(ProjSim) {
  if (!is.null(ProjSim@Misc$MPAdvice) & length(ProjSim@Misc$MPAdvice)>0) {
    # Has MPAdvice Changed from last time 
    PreviousMPAdvice <- ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]] 
  } else {
    PreviousMPAdvice <- NULL
  }
  PreviousMPAdvice
}

GetMPData <- function(ProjSim, TimeStep, TimeStepsAll, ManagementTimeSteps) {
  TSIndex <- match(TimeStep, TimeStepsAll)
  DataTimeStep <- TimeStepsAll[TSIndex - (ProjSim@OM@DataLag+1)]
  
  MPData <- purrr::map(ProjSim@Data, \(Data) 
                       DataTrim(Data, TimeStep=DataTimeStep)
  )
  # Don't think applies anymore ...                                      
  # if (TimeStep>ManagementTimeSteps[1]) {
  #   # don't lag data in first projection time-step
  #   MPData <- purrr::map(ProjSim@Data, \(Data) 
  #                        DataTrim(Data, TimeStep=TimeStep-(ProjSim@OM@DataLag+1))
  #   )
  # } else {
  #   MPData <- ProjSim@Data
  # }
  MPData
}

ApplyMPAdvice <- function(ProjSim, MP, TimeStep, TimeStepsHist, TimeStepsProj, ManagementTimeSteps) {
  
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj) 
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  PreviousMPAdvice <- GetPreviousMPAdvice(ProjSim)
 
  if (!TimeStep %in% ManagementTimeSteps) {
    MPAdvice <- PreviousMPAdvice
  } else {
    MPFunction <- get(MP)
    MPData <- GetMPData(ProjSim, TimeStep, TimeStepsAll, ManagementTimeSteps)
   
    if (inherits(MPFunction, 'mmp')) {
      # TODO
      cli::cli_abort("MP class `mmp` currently not supported")
      MPAdvice <- try(MPFunction(Data=MPData), silent=TRUE)
      
    } else if (inherits(MPFunction, 'mp')) {
      for (i in seq_along(MPData)) { # loop over stock complexes
        MPAdvice <- try(MPFunction(Data=MPData[[i]]), silent=TRUE)
        if (!inherits(MPAdvice, 'advice'))
          cli::cli_abort("MP {.val {MP}} failed in TimeStep {.val {TimeStep}}")
        
        # TODO - clean this up
        ProjSim@Data[[i]]@Misc <- MPAdvice@Misc
        if (length(MPAdvice@Log)) 
          ProjSim@Log[[as.character(TimeStep)]] <- MPAdvice@Log
        
        if (length(ProjSim@Data[[i]]@TAC)<1) {
          ProjSim@Data[[i]]@TAC <- array(NA, length(TimeStepsProj), dimnames = list(TimeStep=TimeStepsProj))
        }
        ProjSim@Data[[i]]@TAC[match(TimeStep, TimeStepsProj)] <- ifelse(is.null(MPAdvice@TAC), NA, MPAdvice@TAC)
        
        if (is.null(ProjSim@Misc$MPAdvice))
          ProjSim@Misc$MPAdvice <- list()
        ProjSim@Misc$MPAdvice[[as.character(TimeStep)]] <- MPAdvice
      }
      
    }
    
  }
  
  # TODO update for complexes and `mmp`
  ProjSim <- ProjSim |> 
    UpdateSpatial(MPAdvice, PreviousMPAdvice, TSIndex) |>
    UpdateSelectivity(MPAdvice, PreviousMPAdvice, TimeStepsAll, TSIndex) |>
    UpdateRetention(MPAdvice, PreviousMPAdvice, TimeStepsAll, TSIndex) |>
    UpdateDiscardMortality(MPAdvice, PreviousMPAdvice, TimeStepsAll, TSIndex) |>
    UpdateTAC(MPAdvice, TSIndex) |>
    UpdateApicalF(MPAdvice, TimeStep, TSIndex) |>
    UpdateEffort(MPAdvice, PreviousMPAdvice, TimeStepsAll, TimeStepsHist, TSIndex) 
  
  # TODO
  # - apply BioEconomic to Effort
  
  
  ProjSim
}