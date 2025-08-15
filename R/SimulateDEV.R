# 
# SimulateDEV <- function(OM=NULL, 
#                         silent=FALSE,
#                         parallel=FALSE, 
#                         ...) {
#   
#   CheckClass(OM)
#   
#   on.exit(cli::stop_app())
#   
#   # if (isTRUE(silent)) 
#   #   messages <- FALSE
# 
#   # ---- Initial Checks and Setup ----
#   # chk <- Check(OM) # TODO OM checks
#   
#   OM <- StartUp(OM) 
#   
#   # Set up parallel processing 
#   if (parallel & !snowfall::sfIsRunning())
#     setup()
#   ncpus <- set_parallel(any(unlist(parallel)))
# 
#   # Set pbapply functions
#   # .lapply <- define.lapply(silent)
#   # .sapply <- define.sapply(silent)
#   
#   # NOTE: future appears a lot slower 
#   # if (parallel) {
#   #   ncores <- parallelly::availableCores(logical=FALSE)
#   #   future::plan('multisession', workers=ncores)
#   # } else {
#   #   future::plan()
#   # }
#   
#   
#   # ---- Make Hist Object ----
#   Hist <- OM2Hist(OM, silent)
#   
#   # ---- Calculate Equilibrium Unfished ----
#   Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
#   
#   # ---- Calculate Number-at-Age for Initial TimeStep ----
#   Hist <- CalcInitialTimeStep(Hist)
# 
#   # ---- Build HistSimList ----
#   # List of `Hist` objects, each with one simulation
#   HistSimList <- Hist2HistSimList(Hist)
# 
#   # ---- Calculate Unfished Equilibrium and Dynamic ----
#   HistSimList <- CalcDynamicUnfished(HistSimList)
#   
#   # ---- Optimize for Final Depletion ----
#   
#   # TODO - check for identical across sims
#   if (parallel) {
#     cli::cli_progress_message("Optimizing catchability (q) for Final Depletion")
#     HistSimList <- .lapply(HistSimList, OptimizeCatchability)
#     cli::cli_progress_done()
#   } else {
#     HistSimList <- purrr::map(HistSimList, \(HistSim)
#                               OptimizeCatchability(HistSim),
#                               .progress = list(
#                                 type = "iterator",
#                                 format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
#                                 clear = TRUE))
#   }
# 
#  
#   # ---- Calculate Reference Points ----
#   HistTimeSteps <- TimeSteps(OM, 'Historical')
#   
#   HistSimList <- CalcRefPoints(HistSimList, TimeSteps=tail(HistTimeSteps,1)) 
#   
#   # ---- Historical Population Dynamics ----
#   # tictoc::tic()
# 
# 
#   # if (IdenticalAcrossSims) {
#   #   # TODO - only run SimulateDynamics_ once and copy across HistSimList
#   #   # need to make sure to update all historical dynamics - eg Stock@Length for each sim
#   #   # if MICE is used
#   # } 
#   # 
#   HistSimList <- purrr::map(HistSimList, \(HistSim) 
#                             SimulateDynamics_(HistSim, HistTimeSteps),
#                             .progress = list(
#                               type = "iterator", 
#                               format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
#                               clear = TRUE))
#   
#   
# 
#   
#   # tictoc::toc()
#  
#   # ---- Check for Depletion Optimization ----
#  
#   # OM@Stock$Albacore@Depletion@Final
#   # OM@Stock$Albacore@Depletion@Reference
#   # 
#   # HistSimList$`3`@Biomass[1,50]/HistSimList$`3`@Unfished@Equilibrium@Biomass[1,50]
#   
#   # TODO 
#   # B0 <- Hist@Unfished@Equilibrium@Biomass[,1,1:360]
#   # B <- Hist@Biomass[,1,]
#   # B <- B/B0
#   # matplot(t(B), type='l', ylim=c(0,1.2))
#   # 
#   # which(B[,360] > 0.4)
#   
# 
#   # ---- Condition Observation Object on Real Fishery Data ----
#   ProjectionTimeSteps <- TimeSteps(OM, 'Projection')
#   HistSimList <- purrr::map(HistSimList, \(HistSim)
#                             ConditionObs(HistSim, HistTimeSteps, ProjectionTimeSteps))
#   
#   # TODO - check for identical sims 
#   # TODO - multiple stocks
#   # TODO - pass Obs in OM and update 
#   
#   
#   # ---- Historical Fishery Data ----
#   # TODO - check for identical sims
#   HistSimList <- purrr::map(HistSimList, \(HistSim)
#                             GenerateHistoricalData(HistSim, HistTimeSteps),
#                             .progress = list(
#                               type = "iterator",
#                               format = "Generating Historical Data {cli::pb_bar} {cli::pb_percent}",
#                               clear = TRUE))
#   
#   
#   
#   # Data:
#   # - list of length `nSim` then
#   # - list of length `nStock`
#   
#   # MPs: 
#   # - `MMP`
#   # - 'complex' 
#   
# 
# 
#   
#   
#   # Simulate Historical Fishery Data
#   
#  
#   
#   # ---- Simulate Fishery Data ----
#   
#   # ---- Return `hist` Object ----
#   
#   # Convert Back to Hist
# 
#   
#   # make Hist object
#   Hist <- HistSimList2Hist(Hist, HistSimList)
# 
#   
#   Hist <- SetDigest(Hist)
#   Hist
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
