


#' Projects a single MP from the output of `Simulate`
ProjectMP <- function(ProjSim, MP, TimeStepsHist, TimeStepsProj, ManagementTimeSteps) {
  # tictoc::tic("Project TimeSteps")

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
    # tictoc::tic("Update Dynamics")
    ProjSim <- SimulateDynamics_(ProjSim, TimeStep) 
    if (!is.na(TimeStepsProj[ts+1])) {
      # calc recruits before fishing mortality 
      # - updated again after fishing mortality for SpawnTimeFrac > 0
      ProjSim <- SimulateDynamics_(ProjSim, TimeStepsProj[ts+1], CalcCatch = 0)
    }
    # tictoc::toc()
    
  } 
  # tictoc::toc()
  
  ProjSim
}


