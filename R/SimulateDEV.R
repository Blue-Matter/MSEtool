
#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        messages='default',
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  MSEtool:::CheckClass(OM)
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- Check(OM) # TODO OM checks
  
  OM <- MSEtool:::StartUp(OM, messages, nSim) 
  
  Hist <- MSEtool:::Hist(OM)

  # ---- Calculate Unfished Dynamics ----
  Hist@Unfished <- MSEtool:::CalcUnfishedDynamics(OM)

  # ---- Calculate Reference Points ----
  Hist@RefPoints <- MSEtool:::CalcRefPoints(Hist)

  # ---- Optimize for Initial Depletion ----
  Hist <- MSEtool:::OptimInitDepletion(Hist)
  
  # ---- Number and Biomass in Initial Time Step ----
  Hist <- MSEtool:::CalcInitialTimeStep(Hist)
  
  # ---- Optimize Catchability for Terminal Depletion ----
  Hist <- OptimCatchability(Hist)
  
  
  # ---- Historical Population Dynamics ----
  
  profvis::profvis(
    postHist <- MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist,'Historical'), silent=T)
  )
  
  
  st <- Sys.time()
  postHist <- MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist,'Historical'), silent=T)
  Sys.time() -st 
  
  Hist <-  MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist, 'Historical'))
  
  

  
 
  
  
  
 
 

  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  
  
  # # --- Sample Obs Parameters ----
  # # TODO - updated Obs object
  # 
  # ObsPars[[p]] <- lapply(1:nf, function(f) {
  #   SampleObsPars(MOM@Obs[[p]][[f]], nsim,
  #                 cpars = SampCpars[[p]][[f]],
  #                 Stock = StockPars[[p]],
  #                 nyears, proyears)
  # })
  # 
  # # --- Sample Imp Parameters ----
  # # TODO - updated Imp object
  # 
  # ImpPars[[p]] <- lapply(1:nf, function(f) {
  #   SampleImpPars(MOM@Imps[[p]][[f]], nsim,
  #                 cpars = SampCpars[[p]][[f]],
  #                 nyears, proyears)
  # })
  
  

  
  
  

    
  
  
  
                        
  
  
  
  
}


