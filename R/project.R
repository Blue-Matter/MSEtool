






UpdateAllocation <- function(SimList) {
  chk <- purrr::map(SimList, \(ProjSim) ProjSim@OM@Allocation) |> unlist()
  if (is.null(chk)) {
    # cli::cli_alert_warning("`OM@Allocation` has not been specified. Using OM@CatchFrac. This should be fixed in the MSEtool code!")
    SimList <- purrr::map(SimList, \(ProjSim) {
      ProjSim@OM@Allocation <- ProjSim@OM@CatchFrac
      ProjSim
    })
  }
  class(SimList) <- "simlist"
  SimList
}

# ProcessDotsList <- function(...) {
#   dotslist <- list(...)  
#   
# }


Project_hist_OLD <- function(Hist,
                         MPs, 
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
  
  # ---- Extend Arrays with Projection Years ----
  Proj <- Hist |> ReduceNSim(nSim) |> ExtendHist()
  
  # ---- Build SimList ----
  SimList <- Hist2SimList(Proj) |> UpdateAllocation()
  
  # ---- Populate Number-at-Age at Beginning of Projection Year ----
  SimList <- InitialProjectionYear(SimList, YearsHist, YearsProj)
  
  # ---- Create MSE Object ----
  MSE <- Hist2MSE(Hist, MPs) 

  # ---- Project MPs ----
  mp <- 1 # for debugging
  
  if (!silent) 
    cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')

  for (mp in seq_along(MPs)) {
    MP <- MPs[mp]
    MSE <- ProjectMP(SimList, MSE, MP, mp, YearsHist, YearsProj) 
  }
  
  MSE <- ReduceMSE(MSE, Reduce)
  MSE@Log <- c(Hist@Log, MSE@Log)
  MSE 
}



MSE2Hist <- function(MSE) {
  Hist <- Hist()
  Hist@OM <- MSE@OM
  Hist@Unfished <- MSE@Unfished
  Hist@RefPoints <- MSE@RefPoints
  Hist@Data <- list(MSE@OM@Data)
  
  slots <- slotNames(MSE@Hist)
  for (sl in slots)  {
    slot(Hist, sl) <- slot(MSE@Hist, sl) 
  }
  Hist
}



  