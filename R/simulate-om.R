

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
                        DynamicUnfished = TRUE,
                        ConditionObs = TRUE,
                        GenerateData = TRUE,
                        Reduce = TRUE,
                        ...) {
  
  # ---- Initial Checks and Setup ----
  OnExit()
  CheckClass(OM) # confirm that OM is class `om`

  # Populate OM, reduce nSim if applicable, checks and warning messages
  OM <- StartUp(OM, nSim)

  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")
  IdenticalHist <- IdenticalSims(OM, ignore='RecDevProj')
  
  # ---- Make Hist Object ----
  Hist <- Hist(OM, silent)
  
  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(OM, silent)

  # ---- Dynamic Number-at-Age for Initial Time Step ----
  # - initial age structure
  # - distribute over areas
  # - account for Initial Depletion
  Hist <- CalcDynamicInitial(Hist)

  # ---- Add temporary lists and arrays to Hist@Misc ----
  # use for easy acces in C++  - removed later
  Hist <- PrepHistMisc(Hist) 
  

  # ---- Calculate Unfished Equilibrium and Dynamic ----
  if (DynamicUnfished) {
    Hist@Unfished@Dynamic <- CalcUnfished_Dynamic(Hist, IdenticalHist, silent=silent)
  }
  
  # ---- Optimize for Final Depletion ----
  Hist <- OptFinalDepletion(Hist)
  
  
  # ---- Add Reference Points if they exist ----
  # won't be re-calculated
  
  
  # ---- Calculate Reference Points ----
  # TODO
  
  # SimList <- CalcSPR0(SimList) # unfished spawning per recruit (i.e. fecundity)
  # 
  # if (inherits(Reference$MSY, "refpointsMSY")) {
  #   Hist@Reference@MSY <- Reference$MSY
  # } else {
  #   RefPointYears <- GetRefPointYears(OM, HistYears) # historical time steps to calculate ref points
  #   if (!inherits(Reference, "logical")) {
  #     SimList <- CalcMSYRefPoints(SimList, RefPointYears, Reference$MSY)
  #   }
  # }
  
  # TODO
  # - Per-Recruit Curves
  # - FCrash, etc
  # - update for seasonal model
  
  
  # ---- Historical Population Dynamics ----
  # Hist <- CalcFisheryDynamics(Hist, IdenticalSim=IdenticalHist)
  Hist <- CalcFisheryDynamics(Hist, IdenticalSim=FALSE)
  
  if (!silent) cli::cli_alert_success("Simulated Historical Fishery")

 
  
  # ---- Calculate Reference Yield ----
  
  # TODO 
  # if (!inherits(Reference, "logical")) {
  #   if (Reference$Landings) {
  #     
  #   }
  #   
  #   if (Reference$Removals) {
  #     
  #   }
  #   
  #   
  #   SimList <- CalcRefLandings(SimList, HistYears, ProjYears, "Landings", Calc = Reference$Landings)
  #   SimList <- CalcRefLandings(SimList, HistYears, ProjYears, "Removals", Calc = Reference$Removals)
  # }

  
  # ---- Remove temporary lists and arrays from Hist@Misc ----
  # see PrepHistMisc above
  Hist <- RestoreHistMisc(Hist)
  
  
  # ---- Condition Observation Object on Real Fishery Data ----
  if (ConditionObs) Hist <- ConditionObs(Hist, silent)
  

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