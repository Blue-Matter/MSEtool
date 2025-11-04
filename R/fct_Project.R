CheckMPClass <- function(MPs) {
  MPFunctions <- purrr::map(MPs, get)
  names(MPFunctions) <- MPs
  MPClass <- purrr::map(MPFunctions, class) |> unlist()
  if (any(MPClass != 'mp')) 
    cli::cli_abort("Currently only MPs of class `mp` are supported")
  
}


InitialProjectionTimeStep <- function(SimList, TimeStepsHist, TimeStepsProj) {
  TS <- c(tail(TimeStepsHist,1), head(TimeStepsProj,1))
  SimList <- purrr::map(SimList, \(ProjSim) 
                        SimulateDynamics_(ProjSim, TS))
  class(SimList) <- "simlist"
  SimList
}

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


Project_hist <- function(Hist=NULL, 
                         MPs=NA, 
                         parallel=FALSE, 
                         silent=FALSE, 
                         nSim=NULL, 
                         Reduce=TRUE) {
  
  # ---- Initial Checks and Setup ----
  OnExit()
  CheckClass(Hist, 'hist', 'Hist')
  CheckMPClass(MPs)
  
  TimeStepsHist <- TimeSteps(Hist@OM, "Historical")
  TimeStepsProj <- TimeSteps(Hist@OM, "Projection")
  
  nMPs <- length(MPs)
  
  # ---- Extend Arrays with Projection TimeSteps ----
  Proj <- Hist |> ReduceNSim(nSim) |> ExtendHist()
  
  # ---- Build SimList ----
  SimList <- Hist2SimList(Proj) |> UpdateAllocation()
  
  # ---- Populate Number-at-Age at Beginning of Projection TimeStep ----
  SimList <- InitialProjectionTimeStep(SimList, TimeStepsHist, TimeStepsProj)
  
  # ---- Create MSE Object ----
  MSE <- Hist2MSE(Hist, MPs) 
  
  # ---- Project MPs ----
  cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')
  for (mp in seq_along(MPs)) {
    MSE <- ProjectMP(SimList, MSE, MPs, TimeStepsHist, TimeStepsProj, mp) 
  }
  
  MSE@Log <- c(Hist@Log, MSE@Log)
  if (Reduce) {
    MSE@OM <- ArrayReduceDims(MSE@OM)
    MSE@Hist <- ArrayReduceDims(MSE@Hist)
    
    # TODO 
    # - historical data is repeated in each MP - reduce
  }
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



  