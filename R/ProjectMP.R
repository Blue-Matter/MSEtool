
ProjectionTimeStep <- function(Data) {
  length(Data@TimeSteps[Data@TimeSteps>Data@LastHistTS])+1
}


ApplyMPInternal <- function(ProjSim, MP, TimeStep) {

  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TimeStepsHist <- TimeSteps(ProjSim@OM, "Historical")
  TimeStepsProj <- TimeSteps(ProjSim@OM, "Projection")
  
  ind <- seq(1, by=ProjSim@OM@Interval, to=length(TimeStepsProj))
 
  ManagementTimeSteps <- TimeStepsProj[ind] # time steps where management will be implemented
  
  if (!TimeStep %in% ManagementTimeSteps) {
    MPAdvice <- ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]] # get previous management advice
  } else {
    # Apply MP to Data
    Data <- ProjSim@Data[[1]]
    MPFunction <- get(MP)

    MPAdvice <- MPFunction(Data)
    
    if (is.null(ProjSim@Misc$MPAdvice))
      ProjSim@Misc$MPAdvice <- list()
    ProjSim@Misc$MPAdvice[[as.character(TimeStep)]] <- MPAdvice
  }
  
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  if (length(MPAdvice@TAC)>0) {
    
  }
  
  if (length(MPAdvice@Effort)>0) {
    ProjSim@Effort[1,TSIndex,1] <- MPAdvice@Effort
  } else {
    ProjSim@Effort[1,TSIndex,1] <- ProjSim@Effort[1,TSIndex-1,1]
  }
  
  if (length(MPAdvice@Spatial)>0) {
    ProjSim@OM@Fleet[[1]]@Distribution@Closure[TSIndex,1,] <- MPAdvice@Spatial
  } else {
    ProjSim@OM@Fleet[[1]]@Distribution@Closure[TSIndex,1,] <- ProjSim@OM@Fleet[[1]]@Distribution@Closure[TSIndex-1,1,]
  }
  

  # - update selectivity, vulnerability, discard mortality from MP
  # - update closed area from MP
  
  # - calculate Effort from TAC (if applicable)
  # - apply BioEconomic to Effort
  # - return Effort, Selectivity, Vuln, Discard, Closure
  
  ProjSim
}

# tictoc::tic()
# tt <- purrr::map(ProjSimList, ProjectMP, MP)
# tictoc::toc()

#' Projects a single MP from the output of `Simulate`
ProjectMP <- function(ProjSim, MP) {
  
  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TimeStepsHist <- TimeSteps(ProjSim@OM, "Historical")
  TimeStepsProj <- TimeSteps(ProjSim@OM, "Projection")
  
  Data <- new('data')
  
  for (TimeStep in TimeStepsProj) {
    
    # Generate Data up to TimeStep - 1
    # TODO - add option for Data Lag 
    Data@LastHistTS <- max(TimeStepsHist)
    Data@TimeSteps <- TimeStepsAll[1:(match(TimeStep, TimeStepsAll)-1)]
    ProjSim@Data <- list(Data)
    
    # --- Update `ProjSim` with MP Advice ----
    ProjSim <- ApplyMPInternal(ProjSim, MP, TimeStep)
    
    # --- Simulate Pop Dynamics for this Time Step ----
    ProjSim <- SimulateDynamics_(ProjSim, TimeStep)
  
  } 
  
  ProjSim
  
}

