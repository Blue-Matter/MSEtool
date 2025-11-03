

GetRefPointTimeSteps <- function(OM) {
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  RefPointTimeSteps <- OM@Control$RefPointTimeSteps
  if (is.null(RefPointTimeSteps))
    RefPointTimeSteps <- tail(HistTimeSteps, OM@TimeStepsPerYear)
  RefPointTimeSteps
}


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
    return(Simulate_om(OM, parallel, silent, nSim, ...))
  
  SimulateOM(OM, parallel, silent, nSim, ...)
}

# Simulate for new `om` class objects
Simulate_om <- function(OM=NULL, 
                        parallel=FALSE,
                        silent=FALSE,
                        nSim=NULL,
                        RefPointsMSY=TRUE,
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
  SimList <- Hist2SimList(Hist)

  # ---- Calculate Reference Points ----
  # unfished spawning per recruit (egg production; i.e. fecundity)
  SimList <- purrr::map(SimList, \(HistSim) {
    HistSim@RefPointsPR@SPR0 <- CalcSPR0(HistSim)
    HistSim
    }) 
  
  # MSY Ref Points 
  # HistSim=SimList$`1`
  # StockList=HistSim@OM@Stock
  # FleetList=HistSim@OM@Fleet
  # 
  # Complexes=HistSim@OM@Complexes
  # TimeSteps =  tail(HistTimeSteps,OM@TimeStepsPerYear)
  # maxF=OM@maxF
  # 
  # TODO - check if varies over simulations
  
  RefPointTimeSteps <- GetRefPointTimeSteps(OM) # historical time steps to calculate ref points

  if (inherits(RefPointsMSY, 'logical') && RefPointsMSY) {
    
    if (CheckIdenticalSims(SimList, Equilibrium=TRUE)) {
      SimOne <- SimList[[1]]
      SimOne@RefPointsMSY <- CalculateMSYSim(StockList=SimOne@OM@Stock,
                                              FleetList=SimOne@OM@Fleet,                                  
                                              Complexes=SimOne@OM@Complexes,
                                              TimeSteps = RefPointTimeSteps,
                                              maxF=OM@maxF)
      
      SimList <- purrr::map(SimList, \(HistSim) {
        HistSim@RefPointsMSY <- SimOne@RefPointsMSY
        HistSim
      })
    } else {
      SimList <- purrr::map(SimList, \(HistSim) {
        HistSim@RefPointsMSY <- CalculateMSYSim(StockList=HistSim@OM@Stock,
                                                FleetList=HistSim@OM@Fleet,                                  
                                                Complexes=HistSim@OM@Complexes,
                                                TimeSteps = RefPointTimeSteps,
                                                maxF=OM@maxF)
      HistSim
    }, .progress = list(
      type = "iterator",
      format = "Calculating MSY Reference Points {cli::pb_bar} {cli::pb_percent}",
      clear = TRUE))
    }
  } else if (inherits(RefPointsMSY, 'refpointsMSY')) {
    Hist@RefPointsMSY <- RefPointsMSY
  }
  
  # Per-Recruit Curves 
  # TODO
  
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  SimList <- CalcDynamicUnfished(SimList)
  
  # ---- Optimize for Final Depletion ----
  SimList <- purrr::map(SimList, \(HistSim)
                        OptimizeCatchability(HistSim),
                        .progress = list(
                          type = "iterator",
                          format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))


  # ---- Historical Population Dynamics ----
  
  # if (IdenticalAcrossSims) {
  #   # TODO - only run SimulateDynamics_ once and copy across SimList
  #   # need to make sure to update all historical dynamics - eg Stock@Length for each sim
  #   # if MICE is used
  # } 
  
  SimList <- purrr::map(SimList, \(HistSim) 
                        SimulateDynamics_(HistSim, HistTimeSteps),
                        .progress = list(
                          type = "iterator", 
                          format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))
  
  # update CatchFrac 
  SimList <- purrr::map(SimList, \(HistSim) {
    HistSim@OM@CatchFrac <- purrr::map2(HistSim@Landings, HistSim@Discards, \(landings, discards) {
      removals <- landings[[length(landings)]] + discards[[length(discards)]]
      fleetCatch <- apply(removals,2, sum)
      fleetCatch/sum(fleetCatch)
    })
    HistSim
  })
  
  # ---- Check for Depletion Optimization ----
  OptDepletionRatio <- CheckDepletionOpt(SimList, HistTimeSteps) # TODO - warning message or re-sample 
  Hist@Log$OptDepletionRatio <- OptDepletionRatio

  # ---- Condition Observation Object on Real Fishery Data ----
  # TODO - check for identical sims - but need to generate independent obs error by sim
  # TODO - add conditioning obs error for Effort
  ProjectionTimeSteps <- TimeSteps(OM, 'Projection')
  SimList <- purrr::map(SimList, \(HistSim)
                            ConditionObs(HistSim, HistTimeSteps, ProjectionTimeSteps),
                            .progress = list(
                              type = "iterator",
                              format = "Conditioning Observation Error on Provided Fishery Data {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))
  

  # # # ---- Historical Fishery Data ----
  HistSim <- SimList$`1` # for debugging
  # TODO - check for identical sims 
  SimList <- purrr::map(SimList, \(HistSim)
                        GenerateHistoricalData(HistSim, HistTimeSteps),
                        .progress = list(
                          type = "iterator",
                          format = "Generating Historical Data {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))
  
  
  # Hist@Data:
  # - list of length `nSim` (or length 1) then
  # - list of length `nComplex`
  
  # ---- Return `hist` Object ----
  Hist <- SimList2Hist(Hist, SimList) 
    
  if (Reduce)
    Hist <- ArrayReduceDims(Hist)
  SetDigest(Hist)
}










