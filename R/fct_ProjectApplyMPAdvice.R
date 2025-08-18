
ApplyMPAdvice <- function(ProjSim, MP, TimeStep, TimeStepsHist, TimeStepsProj, ManagementTimeSteps) {
  
  ###############################
  st <- 1 
  ##############################
  
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj) 
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  if (!is.null(ProjSim@Misc$MPAdvice) & length(ProjSim@Misc$MPAdvice)>0) {
    # Has MPAdvice Changed from last time 
    MPAdvicePrevious <- ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]] 
  } else {
    MPAdvicePrevious <- NULL
  }
  
  if (TimeStep %in% ManagementTimeSteps) {
    # Apply MP to Data
    MPFunction <- get(MP)
    
    # TODO add tryCatch
    # TODO - by stock?
    
    if (TimeStep>ManagementTimeSteps[1]) {
      # don't lag data in first projection time-step
      MPData <- purrr::map(ProjSim@Data, \(Data) 
                           DataTrim(Data, TimeStep=TimeStep-(ProjSim@OM@DataLag+1))
      )
    } else {
      MPData <- ProjSim@Data
    }
    
    MPAdvice <- MPFunction(Data=MPData[[st]])
    ProjSim@Data[[st]]@Misc <- MPAdvice@Misc
    
    if (length(MPAdvice@Log)) 
      ProjSim@Log[[as.character(TimeStep)]] <- MPAdvice@Log
    
    if (length(ProjSim@Data[[st]]@TAC)<1) {
      ProjSim@Data[[st]]@TAC <- array(NA, length(TimeStepsProj), dimnames = list(TimeStep=TimeStepsProj))
    }
    ProjSim@Data[[st]]@TAC[match(TimeStep, TimeStepsProj)] <- ifelse(is.null(MPAdvice@TAC), NA, MPAdvice@TAC)
    
    if (is.null(ProjSim@Misc$MPAdvice))
      ProjSim@Misc$MPAdvice <- list()
    ProjSim@Misc$MPAdvice[[as.character(TimeStep)]] <- MPAdvice
    
    
  } else {
    MPAdvice <- MPAdvicePrevious
  }
  
  ProjSim <- ProjSim |> 
    UpdateSpatial(MPAdvice, MPAdvicePrevious, TSIndex) |>
    UpdateSelectivity(MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) |>
    UpdateRetention(MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) |>
    UpdateDiscardMortality(MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) |>
    UpdateTAC(MPAdvice, TSIndex) |>
    UpdateApicalF(MPAdvice, TimeStep, TSIndex) |>
    UpdateEffort(MPAdvice, MPAdvicePrevious, TimeStepsAll, TimeStepsHist, TSIndex) 
  
  
  
  # TODO
  # - apply BioEconomic to Effort
  
  
  ProjSim
}