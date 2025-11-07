#' @export
print.simlist <- function(x, ...) {
  cli::cli_text('Internal `simlist` object. List of length `nSim`')
}

GetRefPointTimeSteps <- function(OM) {
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  RefPointTimeSteps <- OM@Control$RefPointTimeSteps
  if (is.null(RefPointTimeSteps))
    RefPointTimeSteps <- tail(HistTimeSteps, OM@TSperYear)
  RefPointTimeSteps
}



# Simulate for new `om` class objects
Simulate_om <- function(OM=NULL, 
                        parallel=FALSE,
                        silent=FALSE,
                        nSim=NULL,
                        RefPointsMSY=TRUE,
                        Reduce=FALSE,
                        ...) {
 
  
  # ---- Initial Checks and Setup ----
  OnExit()
  OM <- StartUp(OM, nSim) 
  
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  ProjTimeSteps <- TimeSteps(OM, 'Projection')
  RefPointTimeSteps <- GetRefPointTimeSteps(OM) # historical time steps to calculate ref points
  
  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM, RefPointsMSY, silent)
  
  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  # ---- Calculate Number-at-Age for Initial TimeStep ----
  Hist <- CalcInitialTimeStep(Hist)

  # ---- Build SimList ----
  SimList <- Hist2SimList(Hist)  # List of `Hist` objects, each with one simulation

  # ---- Calculate Reference Points ----
  SimList <- CalcSPR0(SimList)  # unfished spawning per recruit (i.e. fecundity) 
  SimList <- CalcMSYRefPoints(SimList, RefPointTimeSteps, RefPointsMSY)
  
  # TODO
  # - Per-Recruit Curves 
  # - FCrash, etc 
 
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  SimList <- CalcDynamicUnfished(SimList)
  
  # ---- Optimize for Final Depletion ----
  SimList <- OptFinalDepletion(SimList)

  # ---- Historical Population Dynamics ----
  SimList <- SimulateDynamics(SimList, HistTimeSteps) 

  # ---- Condition Observation Object on Real Fishery Data ----
  SimList <- ConditionObs(SimList, HistTimeSteps, ProjTimeSteps)
  
  # ---- Historical Fishery Data ----
  SimList <- GenerateHistoricalData(SimList, HistTimeSteps)
 
  # ---- Return `hist` Object ----
  Hist <- SimList2Hist(Hist, SimList, HistTimeSteps, Reduce) 
  Hist
}










