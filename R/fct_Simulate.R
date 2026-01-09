#' @export
print.simlist <- function(x, ...) {
  cli::cli_text("Internal `simlist` object. List of length `nSim`")
}

GetRefPointYears <- function(OM, HistYears) {
  return(tail(HistYears, 1))

  # TODO - calculate ref points for seasonal time steps

  HistYears <- Years(OM, "Historical")
  RefPointYears <- OM@Control$RefPointYears
  if (is.null(RefPointYears)) {
    RefPointYears <- tail(HistYears, OM@Seasons)
  }
  RefPointYears
}


# Simulate for new `om` class objects
Simulate_om <- function(OM = NULL,
                        parallel = FALSE,
                        silent = FALSE,
                        nSim = NULL,
                        Reference = list(
                          MSY = TRUE,
                          Landings = TRUE,
                          Removals = FALSE
                        ),
                        DynamicUnfished=TRUE,
                        ConditionObs = TRUE,
                        GenerateData = TRUE,
                        Reduce = TRUE,
                        ...) {
  # ---- Initial Checks and Setup ----
  OnExit() # Terminate `cli` on exit
  
  # Populate OM, reduce nSim if applicable, checks and warning messages 
  OM <- StartUp(OM, nSim) 
  
  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")

  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM, silent)
  
  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  # ---- Dynamic Number-at-Age for Initial Time Step ----
  # - initial age structure
  # - distribute over areas
  # - account for Initial Depletion 
  Hist <- CalcDynamicInitial(Hist)
  
  
  # ---- Build SimList ----
  SimList <- Hist2SimList(Hist) # List of `Hist` objects, each with one simulation
  
  
  # ---- Calculate Reference Points ----
  SimList <- CalcSPR0(SimList) # unfished spawning per recruit (i.e. fecundity)
  
  if (inherits(Reference$MSY , "refpointsMSY")) {
    Hist@Reference@MSY <- Reference$MSY
  } else {
    RefPointYears <- GetRefPointYears(OM, HistYears) # historical time steps to calculate ref points
    if (!inherits(Reference, "logical")) {
      SimList <- CalcMSYRefPoints(SimList, RefPointYears, Reference$MSY)
    }
  }
  
 
  # ---------------------- DEBUG ----------------------
  
  
  
  # up to here ...
  # need to fix CalcMSY ref points for new structure Hist@OM@Fleet
  
  stop()
  # -------------------- END DEBUG --------------------
  
  
  # ---- Add Reference Points if they exist ----
  # won't be re-calculated
 
 

  # ---- Calculate Reference Points ----
 

  
  # TODO
  # - Per-Recruit Curves
  # - FCrash, etc
  # - update for seasonal model

  # ---- Calculate Unfished Equilibrium and Dynamic ----
  if (DynamicUnfished) {
    SimList <- CalcDynamicUnfished(SimList)  
  }
  
  # ---- Optimize for Final Depletion ----
  SimList <- OptFinalDepletion(SimList)

  # ---- Historical Population Dynamics ----
  SimList <- SimulateDynamics(SimList, HistYears)

  # ---- Calculate Reference Yield ----
  if (!inherits(Reference, "logical")) {
    SimList <- CalcRefLandings(SimList, HistYears, ProjYears, "Landings", Calc = Reference$Landings)
    SimList <- CalcRefLandings(SimList, HistYears, ProjYears, "Removals", Calc = Reference$Removals)
  }

  # ---- Condition Observation Object on Real Fishery Data ----
  if (ConditionObs) {
    SimList <- ConditionObs(SimList, HistYears, ProjYears)
  }

  # ---- Historical Fishery Data ----
  if (GenerateData) {
    SimList <- GenerateHistoricalData(SimList, HistYears)
  }

  # ---- Aggregate SimList into Hist object ----
  Hist <- SimList2Hist(Hist, SimList, HistYears)

  # ---- Reduce Dimension Size ----
  Hist <- ReduceHist(Hist, Reduce)

  SetDigest(Hist)
}
