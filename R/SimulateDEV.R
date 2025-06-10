
# TODO check where

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  CheckClass(OM)
  
  # if (isTRUE(silent)) 
  #   messages <- FALSE

  # ---- Initial Checks and Setup ----
  # chk <- Check(OM) # TODO OM checks
  
  OM <- StartUp(OM) 
  
  # ---- Make Hist Object ----
  Hist <- OM2Hist(OM, silent)

  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  # ---- Calculate Number-at-Age for Initial TimeStep ----
  Hist <- CalcInitialTimeStep(Hist)
  
  # ---- Build HistSimList ----
  # List of `Hist` objects, each with one simulation
  HistSimList <- Hist2HistSimList(Hist)
  
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  HistSimList <- CalcDynamicUnfished(HistSimList)

  # ---- Optimize for Final Depletion ----
  
  # EXTREMELY SLOW
  # future::plan(future::multisession, workers = 6)
  # tictoc::tic()
  # HistSimList <- furrr::future_map(HistSimList, \(HistSim) 
  #                                   OptimizeCatchability(HistSim))
  # tictoc::toc()
  
 
  # for (i in 1:length(HistSimList))
       # HistSimList[[i]]@OM@Fleet[[1]]@Effort@Catchability[] <- tiny
       
  # HistSim <- HistSimList[[5]]
  # tictoc::tic()
  # t <- OptimizeCatchability(HistSim)
  # tictoc::toc()
  
  # tictoc::tic()
  HistSimList <- purrr::map(HistSimList, \(HistSim) 
                            OptimizeCatchability(HistSim),
                            .progress = list(
                              type = "iterator", 
                              format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))
  # tictoc::toc()
  
 
  
  # ---- Calculate Reference Points ----
  # TODO speed up - CalcRefPoints.R
  # RefPoints <- CalcRefPoints(OM, Unfished)
  
  # ---- Historical Population Dynamics ----
  HistSimList <- purrr::map(HistSimList, \(x) 
                       SimulateDynamics_(x, TimeSteps(OM, 'Historical')),
                       .progress = list(
                         type = "iterator", 
                         format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                         clear = TRUE))
  
  # ---- Check for Depletion Optimization ----
 
  # TODO 
  # B0 <- Hist@Unfished@Equilibrium@Biomass[,1,1:360]
  # B <- Hist@Biomass[,1,]
  # B <- B/B0
  # matplot(t(B), type='l', ylim=c(0,1.2))
  # 
  # which(B[,360] > 0.4)
   

  # ---- Historical Fishery Data ----
  # HistSimList <- purrr::map(HistSimList, \(x) 
  #                           GenerateHistoricalData(x),
  #                           .progress = list(
  #                             type = "iterator", 
  #                             format = "Generating Historical Data {cli::pb_bar} {cli::pb_percent}",
  #                             clear = TRUE))
  
  
  # Data:
  # - list of length `nSim` then
  # - list of length `nStock`
  
  # MPs: 
  # - `MMP`
  # - 'complex' 
  


  
  
  # Simulate Historical Fishery Data
  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  # Convert Back to Hist

  
  # make Hist object
  Hist <- HistSimList2Hist(Hist, HistSimList)
  Hist
  
}












