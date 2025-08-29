




#' @describeIn runMSE Run the Historical Simulations from an object of class `OM` or class `om`
#' @export
#
Simulate <- function(OM=MSEtool::testOM, 
                     parallel=FALSE, 
                     silent=FALSE, 
                     nsim=NULL, 
                     nSim=NULL, ...) {
  
  if (!is.null(nsim))
    nSim <- nsim
  
  if (inherits(OM, 'om'))
    return(Simulate_om(OM, parallel, silent, nSim))
  
  SimulateOM(OM, parallel, silent, nSim)
}

# Simulate for new `om` class objects
Simulate_om <- function(OM=NULL, 
                        parallel=FALSE,
                        silent=FALSE,
                        nSim=NULL,
                        ...) {
  OnExit()
  
  # ---- Initial Checks and Setup ----
  CheckClass(OM)
  OM <- OM |> StartUp(nSim) 
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  
  # Set up parallel processing 
  # if (parallel & !snowfall::sfIsRunning())
  #   setup()
  # ncpus <- set_parallel(any(unlist(parallel)))

  # Set pbapply functions
  # .lapply <- define.lapply(silent)
  # .sapply <- define.sapply(silent)
  
  # NOTE: future appears a lot slower 
  # if (parallel) {
  #   ncores <- parallelly::availableCores(logical=FALSE)
  #   future::plan('multisession', workers=ncores)
  # } else {
  #   future::plan()
  # }
  
  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM, silent)
  
  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  # ---- Calculate Number-at-Age for Initial TimeStep ----
  Hist <- CalcInitialTimeStep(Hist)
  
  # ---- Build HistSimList ----
  # List of `Hist` objects, each with one simulation
  HistSimList <- Hist2HistSimList(Hist)
  
  # ---- Calculate Reference Points ----
  # unfished spawning per recruit (egg production; i.e. fecundity)
  HistSimList <- purrr::map(HistSimList, \(HistSim) {
    HistSim@RefPoints@SPR0 <- CalcSPR0(HistSim)
    HistSim
    }) 
  
  # MSY Ref Points 
  # HistSim=HistSimList$`1`
  # StockList=HistSim@OM@Stock
  # FleetList=HistSim@OM@Fleet
  # 
  # Complexes=HistSim@OM@Complexes
  # TimeSteps = tail(HistTimeSteps,1)
  # maxF=OM@maxF
  
  # TODO - check if varies over simulations
  
  HistSimList <- purrr::map(HistSimList, \(HistSim) {
    HistSim@RefPoints@MSYRefPoints <- CalculateMSYSim(StockList=HistSim@OM@Stock,
                                             FleetList=HistSim@OM@Fleet,                                  
                                             Complexes=HistSim@OM@Complexes,
                                             TimeSteps = tail(HistTimeSteps,1),
                                             maxF=OM@maxF)
    HistSim
  }, .progress = list(
    type = "iterator",
    format = "Calculating MSY Reference Points {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))

  # Per-Recruit Curves TODO
  
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  HistSimList <- CalcDynamicUnfished(HistSimList)
  
  # ---- Optimize for Final Depletion ----
  # # check if catchability values exist
  Catchability <- purrr::map(HistSimList, \(HistSim) {
    purrr::map(HistSim@OM@Fleet, \(StockFleet)
               apply(StockFleet@Effort@Catchability, 2, max)
    ) |>
      List2Array('Stock')
  })|> List2Array()
  
  # TODO - check if CatchFrac exists for multiple fleets
  if (!(all(Catchability>1E-5))) {
    if (parallel) {
      cli::cli_progress_message("Optimizing catchability (q) for Final Depletion")
      HistSimList <- .lapply(HistSimList, OptimizeCatchability)
      cli::cli_progress_done()
    } else {
      HistSimList <- purrr::map(HistSimList, \(HistSim)
                                OptimizeCatchability(HistSim),
                                .progress = list(
                                  type = "iterator",
                                  format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
                                  clear = TRUE))
    }
  }

  # ---- Historical Population Dynamics ----
  
  # if (IdenticalAcrossSims) {
  #   # TODO - only run SimulateDynamics_ once and copy across HistSimList
  #   # need to make sure to update all historical dynamics - eg Stock@Length for each sim
  #   # if MICE is used
  # } 
  # 
  HistSimList <- purrr::map(HistSimList, \(HistSim) 
                            SimulateDynamics_(HistSim, HistTimeSteps),
                            .progress = list(
                              type = "iterator", 
                              format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))
  

  # update CatchFrac 
  HistSimList <- purrr::map(HistSimList, \(HistSim) {
    HistSim@OM@CatchFrac <- purrr::map(HistSim@Removals, \(stock) {
      fleetCatch <- apply(stock[[length(stock)]],2, sum)
      fleetCatch/sum(fleetCatch)
    })
    HistSim
  })
  
  # tictoc::toc()
 
  # ---- Check for Depletion Optimization ----
  # OM@Stock$Albacore@Depletion@Final
  # OM@Stock$Albacore@Depletion@Reference
  # 
  # HistSimList$`1`@Biomass[1,50]/HistSimList$`1`@Unfished@Equilibrium@Biomass[1,50]
  
  # TODO 
  # B0 <- Hist@Unfished@Equilibrium@Biomass[,1,1:360]
  # B <- Hist@Biomass[,1,]
  # B <- B/B0
  # matplot(t(B), type='l', ylim=c(0,1.2))
  # 
  # which(B[,360] > 0.4)
  

  # ---- Condition Observation Object on Real Fishery Data ----
  ProjectionTimeSteps <- TimeSteps(OM, 'Projection')
  HistSimList <- purrr::map(HistSimList, \(HistSim)
                            ConditionObs(HistSim, HistTimeSteps, ProjectionTimeSteps))
  
  # TODO - check for identical sims 
  # TODO - multiple stocks
  # TODO - pass Obs in OM and update 
  
  
  # ---- Historical Fishery Data ----
  # TODO - check for identical sims
  HistSimList <- purrr::map(HistSimList, \(HistSim)
                            GenerateHistoricalData(HistSim, HistTimeSteps),
                            .progress = list(
                              type = "iterator",
                              format = "Generating Historical Data {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))

  # Data:
  # - list of length `nSim` then
  # - list of length `nStock`
  
  # MPs: 
  # - `MMP`
  # - 'complex' 
  
  
  # ---- Return `hist` Object ----
  Hist <- HistSimList2Hist(Hist, HistSimList)
  
  Hist <- SetDigest(Hist)
  Hist
}












