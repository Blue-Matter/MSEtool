
Simulate_om <- function(OM = NULL,
                        parallel = FALSE,
                        silent = FALSE,
                        nSim = NULL,
                        DoDynamicUnfished = TRUE,
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

  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting  {.val Simulate} for OM {.val {OM@Name}}')
  }
  
  OM <- StartUp(OM, nSim, silent=silent)
  
  if (is.null(OM@Name) || nchar(OM@Name) < 2)
    OM@Name <- 'Unnamed OM'
  
  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")
  IdenticalHist <- IdenticalSims(OM, ignore='RecDevProj')

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
  
  # ---- Calculate Reference Points ----
  # TODO - add options for MSY type and years
  
  # TODO Add Reference Points if they ae passed in via OM 
  Hist@Reference@SPR0 <- CalcSPR0(Hist)
  
  if (DoMSYRefs) 
    Hist <- CalcRefMSY(Hist)
  
  # TODO
  # - Per-Recruit Curves
  # - FCrash, etc
  # - update for seasonal model
  
  # ---- Historical Population Dynamics ----
  Hist <- CalcFisheryDynamics(Hist, IdenticalSim=IdenticalHist, clone = 1)
  
  if (!silent)
    cli::cli_alert_success("Simulated Historical Fishery")

  # ---- Compute Catch & Discards at Size ----
  Hist <- CalcCatchAtSize(Hist, Years = HistYears)
  
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
  
  # ---- Check Allocation ----
  Hist <- CheckAllocation(Hist)
  
  # ---- Reduce Dimension Size ----
  Hist <- ReduceHist(Hist, Reduce)
  
  # ---- Report Run Time ----
  EndTime <- Sys.time()
  
  elapsed <- round(difftime(Sys.time(), StartTime, units='auto'), 2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed {.val Simulate} for OM {.val {OM@Name}} ({elapsed})')
  
  Hist@Log <- JoinLog(OM@Log, Hist@Log)
  
  # TODO CheckLog(Hist)
  # - change from Warnings only to Notes/Assumptions 
  
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