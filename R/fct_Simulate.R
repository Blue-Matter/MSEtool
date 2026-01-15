

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

  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)

  # ---- Dynamic Number-at-Age for Initial Time Step ----
  # - initial age structure
  # - distribute over areas
  # - account for Initial Depletion
  Hist <- CalcDynamicInitial(Hist)
  
  
  HistYears <- Years(OM, "H")
  nSim <- OM@nSim
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM)

  
  tictoc::tic()
  HistOUT <- CalcFisheryDynamics_(Hist,
    Years = HistYears,
    nSim,
    nStock,
    nFleet,
    nArea
  )
  tictoc::toc() # 0.47 seconds laptop
  
  # TODO
  # - prevent clone within time loop
  # - don't update values in Hist slots if they are already populated
  # - check speed of CalcFisheryDynamics_ for NPSWO
  
  
  HistOUT@FDeadArea[[1]][1, 1, 1, ,1] # need to check if these Fs are right!!
  RepList$`1`$timeseries |> dplyr::filter(Yr==1975)
  
  range(  HistOUT@FDead[[1]])
  
  
  Hist@Misc$Targeting |> dim()
  
  range(Hist@Distribution)
  range(HistOUT@Distribution)
  


  # 1. make all the arrays and add to Misc
  # 2. Drop OM from Hist for now - re add later?save for space
  # 3.


  stop()
  # calculate VB in c++ etc
  TSind <- 0 # first year

  NumStock <- Hist@Number

  HistOUT <- CalcFisheryDynamics_(
    NumStock,
    nSim
  )


  NumStock <- purrr::map(Hist@Number, \(stockN) {
    abind::asub(stockN, TSind, 3, drop = FALSE) |> abind::adrop(3)
  })

  ##############################################################################

  # ---- Build SimList ----
  SimList <- Hist2SimList(Hist) # List of `Hist` objects, each with one simulation


  # ---- Calculate Reference Points ----
  SimList <- CalcSPR0(SimList) # unfished spawning per recruit (i.e. fecundity)

  if (inherits(Reference$MSY, "refpointsMSY")) {
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
