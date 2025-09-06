




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
                        Reduce=TRUE,
                        ...) {
 
  
  # ---- Initial Checks and Setup ----
  OnExit()
  
  CheckClass(OM)
  OM <- OM |> StartUp(nSim) 
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  
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
    HistSim@OM@CatchFrac <- purrr::map2(HistSim@Landings, HistSim@Discards, \(landings, discards) {
      removals <- landings[[length(landings)]] + discards[[length(discards)]]
      fleetCatch <- apply(removals,2, sum)
      fleetCatch/sum(fleetCatch)
    })
    HistSim
  })
  
  
 
  # ---- Check for Depletion Optimization ----
  OptDepletionRatio <- CheckDepletionOpt(HistSimList, HistTimeSteps) # TODO - warning message or re-sample 
  Hist@Log$OptDepletionRatio <- OptDepletionRatio

  # ---- Condition Observation Object on Real Fishery Data ----
  # TODO - check for identical sims - but need to generate independent obs error by sim
  ProjectionTimeSteps <- TimeSteps(OM, 'Projection')
  HistSimList <- purrr::map(HistSimList, \(HistSim)
                            ConditionObs(HistSim, HistTimeSteps, ProjectionTimeSteps),
                            .progress = list(
                              type = "iterator",
                              format = "Conditioning Observation Error on Provided Fishery Data {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))
  


  
  # # # ---- Historical Fishery Data ----
  # TODO - check for identical sims 
  HistSimList <- purrr::map(HistSimList, \(HistSim)
                            GenerateHistoricalData(HistSim, HistTimeSteps),
                            .progress = list(
                              type = "iterator",
                              format = "Generating Historical Data {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))
  
  
  # Hist@Data:
  # - list of length `nSim` (or length 1) then
  # - list of length `nComplex`
  
  # ---- Return `hist` Object ----
  Hist <- Hist |> 
    HistSimList2Hist(HistSimList) |> 
    SetDigest()
  
  if (Reduce)
    Hist <- ArrayReduceDims(Hist) 
  Hist
}












