
ProjectionTimeStep <- function(Data) {
  length(Data@TimeSteps[Data@TimeSteps>Data@LastHistTS])+1
}


UpdateTAC <- function(ProjSim, MPAdvice, TimeStep) {
  if (length(MPAdvice@TAC)<1)
    return(ProjSim)
  
  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  stop("TAC not done yet")
  
  ProjSim
}

UpdateEffort <- function(ProjSim, MPAdvice, TSIndex) {

  
  if (length(MPAdvice@Effort)>0) {
    ProjSim@Effort[1,TSIndex,1] <- MPAdvice@Effort
  } else {
    ProjSim@Effort[1,TSIndex,1] <- ProjSim@Effort[1,TSIndex-1,1]
  }
  ProjSim
}

UpdateSpatial <- function(ProjSim, MPAdvice, TSIndex) {
  if (length(MPAdvice@Spatial)>0) {
    ProjSim@OM@Fleet[[st]]@Distribution@Closure[TSIndex,1,] <- MPAdvice@Spatial
  } else {
    ProjSim@OM@Fleet[[st]]@Distribution@Closure[TSIndex,1,] <- ProjSim@OM@Fleet[[st]]@Distribution@Closure[TSIndex-1,1,]
  }
  ProjSim
}


UpdateRetention <- function(ProjSim, MPAdvice, TSIndex) {
  stop("TODO")
}


UpdateDiscardMortality <- function(ProjSim, MPAdvice, TSIndex) {
  stop("TODO")
}
  
UpdateSelectivity <- function(ProjSim, MPAdvice, TimeStep) {
  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  Selectivity <- MPAdvice@Selectivity
  if (EmptyObject(Selectivity))
    return(ProjSim)
  
  # TODO checks
  Selectivity@Model <- FindModel(Selectivity)
  
  ModelClass <- getModelClass(Selectivity@Model)
  
  Length <- ProjSim@OM@Stock[[st]]@Length
  Length@ASK <- Length@ASK[,,TSIndex]
  Selectivity@Classes <- Length@Classes
  Ages <- ProjSim@OM@Stock[[st]]@Ages
  if (grepl('at-Length',ModelClass)) {
    LengthClasses <- ProjSim@OM@Stock[[st]]@Length@Classes
    Selectivity@MeanAtLength <- GenerateMeanatLength(Model=Selectivity@Model,
                                                     Pars=Selectivity@Pars,
                                                     Length=Length@Classes)
    
    dimnames(Selectivity@MeanAtLength) <- list(Sim=1,
                                               Class=LengthClasses,
                                               TimeStep=TimeStep)
  } else {
    Selectivity <- PopulateMeanAtAge(Selectivity, Ages, TimeSteps=TimeStep)
  }
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, Ages, nsim=1,
                                        TimeSteps=TimeStep)
  
  
  # *************************** # 
  fl <- 1
  # *************************** # 

  ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtLength[,TSIndex,fl] <- Selectivity@MeanAtLength[1,,1]
  ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,fl] <- Selectivity@MeanAtAge[1,,1]
  
  
  ProjSim
}


ApplyMPInternal <- function(ProjSim, MP, TimeStep) {

  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TimeStepsHist <- TimeSteps(ProjSim@OM, "Historical")
  TimeStepsProj <- TimeSteps(ProjSim@OM, "Projection")
  
  ind <- seq(1, by=ProjSim@OM@Interval, to=length(TimeStepsProj))
 
  ManagementTimeSteps <- TimeStepsProj[ind] # time steps where management will be implemented
  
  ###############################
  st <- 1 
  ##############################
  
  if (!TimeStep %in% ManagementTimeSteps) {
    MPAdvice <- ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]] # get previous management advice
  } else {
    # Apply MP to Data
    Data <- ProjSim@Data[[st]] # by stock?
    MPFunction <- get(MP)

    MPAdvice <- MPFunction(Data)
    
    if (is.null(ProjSim@Misc$MPAdvice))
      ProjSim@Misc$MPAdvice <- list()
    ProjSim@Misc$MPAdvice[[as.character(TimeStep)]] <- MPAdvice
  }
  
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  
  

  
  # NOTE: MPs must return advice in the slot every iteration; ie 
  # if they return Selectivity once, it must return populated Selectivity every time
  # otherwise selectivity in projection years will revert back to original OM
  
  ProjSim <- ProjSim |> 
    UpdateSpatial(MPAdvice, TSIndex) |>
    UpdateSelectivity(MPAdvice, TimeStep) |>
    UpdateRetention(MPAdvice, TimeStep) |>
    UpdateDiscardMortality(MPAdvice, TimeStep) |>
    UpdateTAC(MPAdvice, TimeStep) |>
    UpdateEffort(MPAdvice, TSIndex) 

  # apply bioeconomic ...
  
  EmptyObject(MPAdvice@Retention)

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
  
  # *********************************** # 
  Data <- new('data')
  TimeStep <- TimeStepsProj[1]
  # *********************************** # 
  
  
  for (TimeStep in TimeStepsProj) {
    
    # Generate Data up to TimeStep - 1
    # TODO - add option for Data Lag 
    # Data@LastHistTS <- max(TimeStepsHist)
    # Data@TimeSteps <- TimeStepsAll[1:(match(TimeStep, TimeStepsAll)-1)]
    
    # ProjSim@Data <- list(Data)
    
    # --- Update `ProjSim` with MP Advice ----
    ProjSim <- ApplyMPInternal(ProjSim, MP, TimeStep)
    
    # --- Simulate Pop Dynamics for this Time Step ----
    ProjSim <- SimulateDynamics_(ProjSim, TimeStep)
  
  } 
  
  ProjSim
  
}

