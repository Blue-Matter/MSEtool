#' @export
print.simlist <- function(x, ...) {
  cli::cli_text('Internal `simlist` object. List of length `nSim`')
}

GetRefPointYears <- function(OM) {
  HistYears <- Years(OM, 'Historical')
  RefPointYears <- OM@Control$RefPointYears
  if (is.null(RefPointYears))
    RefPointYears <- tail(HistYears, OM@TSperYear)
  RefPointYears
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
  
  HistYears <- Years(OM, 'Historical')
  ProjYears <- Years(OM, 'Projection')
  RefPointYears <- GetRefPointYears(OM) # historical time steps to calculate ref points
  
  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM, RefPointsMSY, silent)
  
  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  # ---- Calculate Number-at-Age for Initial Year ----
  Hist <- CalcInitialYear(Hist)

  # ---- Build SimList ----
  SimList <- Hist2SimList(Hist)  # List of `Hist` objects, each with one simulation

  # ---- Calculate Reference Points ----
  SimList <- CalcSPR0(SimList)  # unfished spawning per recruit (i.e. fecundity) 
  SimList <- CalcMSYRefPoints(SimList, RefPointYears, RefPointsMSY)
  
  # TODO
  # - Per-Recruit Curves 
  # - FCrash, etc 
 
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  SimList <- CalcDynamicUnfished(SimList)
  
  # ---- Optimize for Final Depletion ----
  SimList <- OptFinalDepletion(SimList)

  # ---- Historical Population Dynamics ----
  SimList <- SimulateDynamics(SimList, HistYears) 

  # ---- Condition Observation Object on Real Fishery Data ----
  SimList <- ConditionObs(SimList, HistYears, ProjYears)
  
  # ---- Historical Fishery Data ----
  SimList <- GenerateHistoricalData(SimList, HistYears)
 
  # ---- Return `hist` Object ----
  Hist <- SimList2Hist(Hist, SimList, HistYears, Reduce) 
  Hist
}










