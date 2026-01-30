
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
  
  

  # ---- Populate Number-at-Age at Beginning of Projection Year ----
  Proj <- CalcFisheryDynamics(Proj, 
                              Years=c(tail(YearsHist,1), head(YearsProj,1)))
  

  
  # ---- Create MSE Object ----
  MSE <- Hist2MSE(Hist, MPs) 
  
  # UP THE HERE!!
  
}