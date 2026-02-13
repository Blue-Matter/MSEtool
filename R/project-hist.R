
Project_hist <- function(Hist,
                         MPs = NULL, 
                         parallel=FALSE, 
                         silent=FALSE, 
                         nSim=NULL, 
                         Reduce=TRUE) {
  
  
  # ---- Initial Checks and Setup ----
  OnExit()
  CheckClass(Hist, 'hist', 'Hist')
  CheckMPClass(MPs)
  
  YearsHist <- Years(Hist@OM, "Historical")
  YearsProj <- Years(Hist@OM, "Projection")
  nMPs <- length(MPs)
  
  # ---- Reduce nSim if provided ----
  Proj <- Hist |> ReduceNSim(nSim)
  
  # ---- Add temporary lists and arrays to Hist@Misc ----
  Proj <- PrepHistMisc(Proj) 
  
  # ---- Extend Arrays with Projection Years ----
  Proj <- ExtendHist(Proj, 
                     Years = c(YearsHist, YearsProj))
  
  
  # TODO
  # - initialize projection values as NA
  # - if Effort is NA, 
  # current issue: TAC needs to be constrained by Effort if it has been set by an MP
  # otherwise is should not be constrained
  # if both TAC and Effort are NA, use effort from previous (seasonal) timestep
  
  stop()
  
  # ---- Populate Number-at-Age at Beginning of Projection Year ----
  Proj <- CalcFisheryDynamics(Proj, 
                              Years=c(tail(YearsHist,1), head(YearsProj,1)))
  
  # ---- Create MSE Object ----
  MSE <- Hist2MSE(Proj, MPNames = MPs) 
  
  # ---- Project MPs ----
  mp <- 1 # for debugging
  
  if (!silent) 
    cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')
  
  for (mp in seq_along(MPs)) {
    MPName <- MPs[mp]
    MPfunction <- MSE@MPs[[MPName]]
    
    MSE <- Project_MP(Proj, 
                      MSE,
                      MPName,
                      MPfunction,
                      mp = mp,
                      YearsHist,
                      YearsProj)
    
  }

  
  
  
  
  # UP THE HERE!!
  
}