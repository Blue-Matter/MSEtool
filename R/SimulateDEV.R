# 2025/01/20
# - NSWO.R - keep working on SPR Curve calcs. Added StockList, StockFleetList, and FleetList S3 classes

# 2025/01/17 
# - NSWO.R - generalize SPR calcs for multiple stocks and fleets 

# 2025/01/16 
# - testOM.R - code RefPoint calcs and compare with Hist - working test after NSWO working 


# TODO
# - add check in Populate for Selectivity to have max value 1. 




#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        messages='default',
                        nSim=NULL,
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  CheckClass(OM)
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- Check(OM) # TODO OM checks
  
  OM <- StartUp(OM, messages, nSim) 
  
  Hist <- new('hist') 
  for (nm in slotNames(OM)) {
    slot(Hist, nm) <- slot(OM, nm)
  }


  Hist@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  # TODO Dynamic Unfished
  
  Hist@RefPoints <- CalcRefPoints(OM, Hist@Unfished)
  # TODO - SPRcrash, MGT, RefYield 

  
  # ---- Non-Equilibrium Initial Year ----
  Hist@Number <- purrr::map2(OM@Stock, Hist@Unfished@Equilibrium@Number, InitNumber)
  
  ####### UP TO HERE ################
  # check distribution .... 
  
  # loop over historical timesteps 
  TimeStepsHist <- TimeSteps(OM, 'Historical')
  
  for (ts in seq_along(TimeStepsHist)[-1]) {
    # beginning of time step
    
    # calculate fishing spatial distribution last time step
    
    # calculate FDead and FRetain for last time step
    
    # calculate Spawning Output last time step
    
    # calculate recruitment last time step
    
    # do MICE stuff if applicable
    
    # Mortality and Aging
    
    # Move Stock for beginning of this time step
    
    
    
    
  }
  
  # PopDynamicsHist <- function(Hist) {
    
  
  # }
  
  
 
  
  
  
  # ---- Optimize Rec Devs for Initial Depletion ----
  
  
  HistRel <- SetHistRel(OM) 
  
  
  
  # ---- Optimize catchability (q) to fit depletion ----
  # (if needed)
  
  # ---- Run Historical Simulations ----
  
  # ---- Calculate Historical Catch ----
  
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


