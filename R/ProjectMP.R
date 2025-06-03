
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

UpdateEffort <- function(ProjSim, MPAdvice, TimeStep) {

  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TimeStepsHist <- TimeSteps(ProjSim@OM, "Historical")
  LastHistIndex <-  match(max(TimeStepsHist), TimeStepsAll)
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  if (length(MPAdvice@Effort)>0) {
    ProjSim@Effort[st,TSIndex,fl] <- ProjSim@Effort[st,LastHistIndex,fl] * MPAdvice@Effort
  } else {
    ProjSim@Effort[st,TSIndex,fl] <- ProjSim@Effort[st,TSIndex-1,fl]
  }
  ProjSim
}

UpdateSpatial <- function(ProjSim, MPAdvice, TimeStep) {
  
  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  if (length(MPAdvice@Spatial)>0) {
    ProjSim@OM@Fleet[[st]]@Distribution@Closure[TSIndex,fl,] <- MPAdvice@Spatial
  } else {
    ProjSim@OM@Fleet[[st]]@Distribution@Closure[TSIndex,fl,] <- ProjSim@OM@Fleet[[st]]@Distribution@Closure[TSIndex-1,fl,]
  }
  ProjSim
}


UpdateRetention <- function(ProjSim, MPAdvice, TSIndex) {
  UpdateSelectivity(ProjSim, MPAdvice, TimeStep, 'Retention')
}


UpdateDiscardMortality <- function(ProjSim, MPAdvice, TSIndex) {
  if (EmptyObject(MPAdvice@DiscardMortality))
    return(ProjSim)
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  if (length(MPAdvice@DiscardMortality@MeanAtAge)>0) {
    stop("Discard mortality TODO")
    
    # check its length age@Classes
    # calc DiscMatLength
    ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtAge[,TSIndex,fl]
  }
  # repeat for MeanAtLength
  ProjSim
  
  
}
  
UpdateSelectivity <- function(ProjSim, MPAdvice, TimeStep, type='Selectivity') {
  TimeStepsAll <- TimeSteps(ProjSim@OM)
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  Selectivity <- slot(MPAdvice,type)
  if (EmptyObject(Selectivity))
    return(ProjSim)
  
  # TODO checks
  
  if (!is.null(Selectivity@Misc$Type) && Selectivity@Misc$Type=="Weight") {
    # Convert weight to length
    len <- ProjSim@OM@Stock[[st]]@Length@MeanAtAge[,TSIndex]
    wght <- ProjSim@OM@Stock[[st]]@Weight@MeanAtAge[,TSIndex]
    SL95 <- LinInterp_cpp(wght, len, Selectivity@Pars$SL50+Selectivity@Pars$SL50_95) 
    Selectivity@Pars$SL50 <- LinInterp_cpp(wght, len, Selectivity@Pars$SL50)
    Selectivity@Pars$SL50_95 <- SL95 -Selectivity@Pars$SL50
  }
  
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
  if (type=="Selectivity") {
    ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtLength[,TSIndex,fl] <- Selectivity@MeanAtLength[1,,1]
    ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,fl] <- Selectivity@MeanAtAge[1,,1]
  } else if (type=="Retention") {
    ProjSim@OM@Fleet[[st]]@Retention@MeanAtLength[,TSIndex,fl] <- Selectivity@MeanAtLength[1,,1]
    ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,fl] <- Selectivity@MeanAtAge[1,,1]
  } else{
    stop("type must be `Selectivity` or `Retention`")
  }
 
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
    UpdateSpatial(MPAdvice, TimeStep) |>
    UpdateSelectivity(MPAdvice, TimeStep) |>
    UpdateRetention(MPAdvice, TimeStep) |>
    UpdateDiscardMortality(MPAdvice, TimeStep) |>
    UpdateTAC(MPAdvice, TimeStep) |>
    UpdateEffort(MPAdvice, TimeStep) 

  # apply bioeconomic ...
  
  # - calculate Effort from TAC (if applicable)
  # - apply BioEconomic to Effort
  
  
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

