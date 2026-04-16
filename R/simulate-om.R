
Simulate_om <- function(OM = NULL,
                        parallel = FALSE,
                        silent = FALSE,
                        nSim = NULL,
                        DoDynamicUnfished = TRUE,
                        DoRefMSY = TRUE,
                        DoRefLandings = TRUE,
                        DoRefRemovals = FALSE,
                        DoConditionObs = TRUE,
                        DoGenerateData = TRUE,
                        DoMSYRefs = TRUE,
                        Reduce = TRUE,
                        ...) {
  
  # ---- Initial Checks and Setup ----
  StartTime <- Sys.time()
  OnExit()
  CheckClass(OM)
  OM <- UpdateObject(OM)

  OM <- StartUp(OM, nSim, silent=silent)
  
  if (is.null(OM@Name) || nchar(OM@Name) < 2)
    OM@Name <- 'Unnamed OM'
  
  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")
  IdenticalHist <- IdenticalSims(OM, ignore='RecDevProj')
  
  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting  {.val Simulate} for OM {.val {OM@Name}}')
  }
    
  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM=OM, silent=silent)

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
  if (DoDynamicUnfished) 
    Hist@Unfished@Dynamic <- CalcUnfished_Dynamic(Hist = Hist, 
                                                  IdenticalHist = IdenticalHist, 
                                                  silent = silent)
  
  # ---- Optimize for Final Depletion ----
  Hist <- OptFinalDepletion(Hist, silent = silent)
  
  # TODO - check that depletion converged on specified values
  
  # ---- Add Reference Points if they exist ----
  # won't be re-calculated
  
  Hist@Reference@SPR0 <- CalcSPR0(Hist)
  
  # ---- Calculate Reference Points ----
  # if (DoMSYRefs) {
  #   # do try Catch
  #   
  #   
  #   Hist@Reference@MSY@FMSY
  #  
  #   
  #   
  #   
  #   
  # }
  
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
  Hist <- CalcFisheryDynamics(Hist, IdenticalSim=IdenticalHist, clone = 1)
  
  if (!silent)
    cli::cli_alert_success("Simulated Historical Fishery")

  # ---- Reference Yield ----
  ref_types <- c(
    if (DoRefLandings) 'Landings',
    if (DoRefRemovals) 'Removals'
  )
  if (length(ref_types) > 0)
    Hist <- CalcRefYield(Hist, type=ref_types, Units='Biomass', silent=silent)
  
  # ---- Restore Hist@Misc ----
  Hist <- RestoreHistMisc(Hist)

  # ---- Condition Observation Object on Real Fishery Data ----
  if (DoConditionObs) 
    Hist <- ConditionObs(Hist, silent)
  
  # ---- Historical Fishery Data ----
  if (DoGenerateData)
    Hist <- GenerateHistoricalData(Hist, silent=silent)

  # Add Simulation Number to Data@Misc 
  Hist <- AddSimNumber(Hist)
  
  # ---- Reduce Dimension Size ----
  Hist <- ReduceHist(Hist, Reduce)
  
  # ---- Report Run Time ----
  EndTime <- Sys.time()
  
  elapsed <- round(difftime(Sys.time(), StartTime, units='auto'), 2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed {.val Simulate} for OM {.val {OM@Name}} ({elapsed})')
  
  SetDigest(Hist)
}







# GetRefPointYears <- function(OM, HistYears) {
#   return(utils::tail(HistYears, 1))
#   
#   # TODO - calculate ref points for seasonal time steps
#   
#   HistYears <- Years(OM, "Historical")
#   RefPointYears <- OM@Control$RefPointYears
#   if (is.null(RefPointYears)) {
#     RefPointYears <- utils::tail(HistYears, OM@Seasons)
#   }
#   RefPointYears
# }