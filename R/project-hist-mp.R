CalcManagementInterval <- function(Hist, YearsProj) {
  # TODO add option to specify Interval by MP
  ind <- seq(1, by=Hist@OM@Interval, to=length(YearsProj)) 
  YearsProj[ind]
}


# Project Hist object for a single MP 
Project_MP <- function(Proj, MSE, MP, mp = 1, 
                       YearsHist, 
                       YearsProj) {
  
  ManagementYears <- CalcManagementInterval(Proj, YearsProj)
  
  StartTime <- Sys.time()
  
  # for debugging
  Year <- YearsProj[1]; ts =1;
  
  for (ts in seq_along(YearsProj)) {
    
    # TODO add progress 
    Year <- YearsProj[ts]
    
    ProjSim <- GenerateProjectionData(ProjSim, Year, YearsHist, YearsProj)
    
    
    
    
    
  }
  
  
  # SimList_MP <- purrr::map(SimList, \(ProjSim) 
  #                          try(
  #                            ProjectMP_Sim(ProjSim, 
  #                                          MP, 
  #                                          YearsHist, 
  #                                          YearsProj, 
  #                                          ManagementYears),
  #                            silent=TRUE
  #                          ),
  #                          .progress = list(
  #                            type = "tasks", 
  #                            caller = environment(),
  #                            format = "Projecting {.val {MP}} {cli::pb_bar} {cli::pb_percent}",
  #                            clear = TRUE))
  
  EndTime <- Sys.time()
  
  
}