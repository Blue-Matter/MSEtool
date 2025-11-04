
ProjectMP <- function(SimList, MSE, MPs, TimeStepsHist, TimeStepsProj, mp=1) {
  
  MP <- MPs[mp]
  
  # Management Interval
  # TODO add option to specify Interval by MP
  ind <- seq(1, by=SimList[[1]]@OM@Interval, to=length(TimeStepsProj)) 
  ManagementTimeSteps <- TimeStepsProj[ind] # time steps where management will be implemented
  
  StartTime <- Sys.time()
  SimList_MP <- purrr::map(SimList, \(ProjSim) 
                           try(
                             ProjectMP_Sim(ProjSim, 
                                       MP, 
                                       TimeStepsHist, 
                                       TimeStepsProj, 
                                       ManagementTimeSteps),
                             silent=TRUE
                           ),
                           .progress = list(
                             type = "tasks", 
                             caller = environment(),
                             format = "Projecting {.val {MP}} {cli::pb_bar} {cli::pb_percent}",
                             clear = TRUE))
  EndTime <- Sys.time()
  
  SimList_MP <- CheckMSERun(SimList_MP, SimList, MP, StartTime, EndTime)
  MSE <- UpdateMSEObject(MSE, SimList_MP, mp, TimeStepsHist, TimeStepsProj, MP)
  MSE
}


#' Projects a single MP from the output of `Simulate`
ProjectMP_Sim <- function(ProjSim, MP, TimeStepsHist, TimeStepsProj, ManagementTimeSteps) {
  # for debugging
  TimeStep <- TimeStepsProj[1]; ts =1
  
  for (ts in seq_along(TimeStepsProj)) {
    TimeStep <- TimeStepsProj[ts]
    
    # Generate Data up to TimeStep - 1 - Data Lag done in ApplyMPAdvice
    ProjSim <- GenerateProjectionData(ProjSim, TimeStep, TimeStepsHist, TimeStepsProj)
    
    ProjSim <- ApplyMPAdvice(ProjSim, 
                             MP, 
                             TimeStep, 
                             TimeStepsHist,
                             TimeStepsProj,
                             ManagementTimeSteps)

    #  Simulate Pop Dynamics for this Time Step
    ProjSim <- SimulateDynamics_(ProjSim, TimeStep) 
    if (!is.na(TimeStepsProj[ts+1])) {
      # calc recruits before fishing mortality 
      # - updated again after fishing mortality for SpawnTimeFrac > 0
      ProjSim <- SimulateDynamics_(ProjSim, TimeStepsProj[ts+1], CalcCatch = 0)
    }
  } 
  ProjSim
}


