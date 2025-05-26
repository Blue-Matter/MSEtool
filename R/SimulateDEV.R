
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
  chk <- Check(OM) # TODO OM checks
  
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
  HistSimList <- purrr::map(HistSimList, \(HistSim) 
                            OptimizeCatchability(HistSim),
                            .progress = list(
                              type = "iterator", 
                              format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
                              clear = TRUE))

  # ---- Calculate Reference Points ----
  # TODO speed up - CalcRefPoints.R
  # RefPoints <- CalcRefPoints(OM, Unfished)
  
  # ---- Historical Population Dynamics ----
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  
  # Calculate Population Dynamics (Fishing Mortality & Number by Area)
  HistSimList <- purrr::map(HistSimList, \(x) 
                       SimulateDynamics_(x, HistTimeSteps, MP=NULL),
                       .progress = list(
                         type = "iterator", 
                         format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                         clear = TRUE))
  
  
 
  
  # Condition Observation Model observed Historical Fishery Data
  
  # Simulate Historical Fishery Data
  
 
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  # Convert Back to Hist

  
  # make Hist object
  UpdateHist(Hist, HistSimList)
}












