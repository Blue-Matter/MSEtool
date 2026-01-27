

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
                        DynamicUnfished = TRUE,
                        ConditionObs = TRUE,
                        GenerateData = TRUE,
                        Reduce = TRUE,
                        ...) {
  
  # ---- Initial Checks and Setup ----
  OnExit()

  # Populate OM, reduce nSim if applicable, checks and warning messages
  OM <- StartUp(OM, nSim)

  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")

  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM, silent)
  
  
  # ---- Check if Hist is Identical over Sims
  Identical_Hist <- Identical_Sim(OM, ignore='RecDevProj')
  
 
  
  
  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(OM)
  
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
    Hist@Unfished@Dynamic <- CalcUnfished_Dynamic(Hist)
  }
  
  # ---- Optimize for Final Depletion ----
  Hist <- OptFinalDepletion(Hist)
  
  
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
  

  # ---- Historical Population Dynamics ----
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM)
  HistYears <- Years(OM,'H')
  AllYears <- Years(OM)
  
  t <- Identical_Sim(Hist, ignore='RecDevProj', TRUE)
  
  object <- Hist@OM@Fleet$Female$F4_IATTC@Selectivity@MeanAtLength
  Identical_Sim(object)
  
  ExtendYears
  array <- fleet@Selectivity@MeanAtLength
  t <- ExtendYears(fleet@Selectivity@MeanAtLength, Years) 
  t[1,1:2,,]
  
  array <- OM@Fleet$Female$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Selectivity@MeanAtLength 
  t <- ExtendYears(array, Years) 
  
  range(t)
  
  t[1,,1:2,1]
  object[20,,,]
  
  
  tictoc::tic()
  Hist <- CalcFisheryDynamics_(Hist, 
                               HistYears,
                               AllYears=AllYears,
                               nSim=Hist@OM@nSim,
                               nStock,
                               nFleet,
                               nArea,
                               DoCalcCatch=1,
                               DoCalcaggF=1)
  
  
  tictoc::toc()

  
  
  

  # ---- Remove temporary lists and arrays from Hist@Misc ----
  # see PrepHistMisc above
  Hist <- RestoreHistMisc(Hist)
  
  
  
  # ---------------------- DEBUG ----------------------
  

  
  
  # - check is ASK doesn't exist in C++ 
  
  HistYears <- Years(OM, "H")
  AllYears <- Years(OM)
  nSim <- OM@nSim
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM)

  tictoc::tic()
  Hist <- CalcFisheryDynamics_(Hist,
    Years = HistYears,
    AllYears,
    nSim,
    nStock,
    nFleet,
    nArea
  )
  tictoc::toc()

  fl <- 1
  Hist@FDeadArea$Female[1,,188,fl,] 
  Hist@FDead$Female[1,,188,fl]
  
  
  # TODO - add CAL calcs to C++ - update ALK internally
  # continue with rest of Hist development
  
  
  # TEST With TESTOM first !!!!
  # check with NPSWO - does my ASK match the SS one??
  return(Hist)
  


  ##############################################################################







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
