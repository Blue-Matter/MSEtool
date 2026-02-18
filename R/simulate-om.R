# TODO  add year broadcast in C++



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
                        Reduce = TRUE,
                        ...) {
  
  # ---- Initial Checks and Setup ----
  StartTime <- Sys.time()
  OnExit()
  CheckClass(OM) # confirm that OM is class `om`

  # Populate OM, reduce nSim if applicable, checks and warning messages
  OM <- StartUp(OM, nSim)

  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")
  IdenticalHist <- IdenticalSims(OM, ignore='RecDevProj')
  
  
  if (is.null(OM@Name) || nchar(OM@Name)<2)
    OM@Name <- 'Unnamed OM'
  
  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting  {.val Simulate} for OM {.val {OM@Name}}')
    
  }
    
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
  if (DoDynamicUnfished) 
    Hist@Unfished@Dynamic <- CalcUnfished_Dynamic(Hist = Hist, 
                                                  IdenticalHist = IdenticalHist, 
                                                  silent = silent)
  
  # ---- Optimize for Final Depletion ----
  Hist <- OptFinalDepletion(Hist, silent = silent)
  
  # TODO - check that depletion converged on specified values
  
  
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
  Hist <- CalcFisheryDynamics(Hist, IdenticalSim=IdenticalHist)
  

  if (!silent)
    cli::cli_alert_success("Simulated Historical Fishery")

  # ---- Calculate Reference Yield ----
  type <- c()
  if (DoRefLandings) type <- c(type, "Landings")
  if (DoRefRemovals) type <- c(type, "Removals")
  
  if (length(type) > 0) {
    Hist <- CalcRefYield(Hist,
                         type=type,
                         Units = "Biomass",
                         silent = silent)
    
  }
  
  # ---- Remove temporary lists and arrays from Hist@Misc ----
  # see PrepHistMisc above
  Hist <- RestoreHistMisc(Hist)

  # ---- Condition Observation Object on Real Fishery Data ----
  if (DoConditionObs) 
    Hist <- ConditionObs(Hist, silent)
  
  
  # ---- Historical Fishery Data ----
  
  if (DoGenerateData) 
    Hist <- GenerateHistoricalData(Hist)
  

  # ---- Reduce Dimension Size ----
  Hist <- ReduceHist(Hist, Reduce)
  
  
  # ---- Report Run Time ----
  EndTime <- Sys.time()
  
  elapse_secs <- round(difftime(time1 = EndTime, time2 = StartTime, units = "secs"),2) |> as.numeric()
  elapse_auto <- round(difftime(time1 = EndTime, time2 = StartTime, units = "auto"),2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed {.val Simulate} for OM {.val {OM@Name}} ({elapse_auto})') 
  
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