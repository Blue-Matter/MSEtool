CalcManagementInterval <- function(SimList, YearsProj) {
  # TODO add option to specify Interval by MP
  ind <- seq(1, by=SimList[[1]]@OM@Interval, to=length(YearsProj)) 
  YearsProj[ind]
}


ProjectMP <- function(SimList, MSE, MP, mp=1, YearsHist,YearsProj) {

  ManagementYears <- CalcManagementInterval(SimList, YearsProj)
  
  StartTime <- Sys.time()
  SimList_MP <- purrr::imap(SimList, \(ProjSim, Sim) 
                           try(
                             ProjectMP_Sim(ProjSim, 
                                           MP, 
                                           YearsHist, 
                                           YearsProj, 
                                           ManagementYears,
                                           Sim=Sim),
                             silent=TRUE
                           ),
                           .progress = list(
                             type = "tasks", 
                             caller = environment(),
                             format = "Projecting {.val {MP}} {cli::pb_bar} {cli::pb_percent}",
                             clear = TRUE))
  EndTime <- Sys.time()
  
  SimList_MP <- CheckMSERun(SimList_MP, SimList, MP, StartTime, EndTime)
  MSE <- UpdateMSEObject(MSE, SimList_MP, MP, mp,YearsHist, YearsProj)
  MSE
}


#' Projects a single MP from the output of `Simulate`
ProjectMP_Sim <- function(ProjSim, MP,YearsHist,YearsProj, ManagementYears, Sim) {
  # for debugging
 Year <- YearsProj[1]; ts =1; Sim=1
  
  for (ts in seq_along(YearsProj)) {
   Year <-YearsProj[ts]
    
    # Generate Data up toYear - 1 - Data Lag done in ApplyMPAdvice
    ProjSim <- GenerateProjectionData(ProjSim, Year, YearsHist, YearsProj)
    
    ProjSim <- ApplyMPAdvice(ProjSim, 
                             MP, 
                             Year, 
                             YearsHist,
                             YearsProj,
                             ManagementYears,
                             Sim)

    #  Simulate Pop Dynamics for this Time Step
    ProjSim <- SimulateDynamics_(ProjSim,Year) 
    if (!is.na(YearsProj[ts+1])) {
      # calc recruits before fishing mortality 
      # - updated again after fishing mortality for SpawnTimeFrac > 0
      ProjSim <- SimulateDynamics_(ProjSim,YearsProj[ts+1], CalcCatch = 0)
    }
  } 
  ProjSim
}


