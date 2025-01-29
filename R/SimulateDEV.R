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
    
  # TODO - re-populates if Stock/Fleet converted to list - fix hash
  
  HistOut <- new('hist') # new Hist object to return 
  
  # ---- Calculate Unfished Dynamics ----
  # TODO Dynamic Unfished - elsewhere by simulating dynamics
  HistOut@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  
  
  # ---- Calculate Curves -----
  
  #New Classes (temp) 
  
  # Equilibrium values for a given F 
  setClass("curves",
           slots=c(
                   SPR='list', 
                   YPR='list',
                   Recruit='list',
                   Yield='list',
                   Biomass='list',
                   SBiomass='list',
                   SP='list',
                   Misc='list'
           ),
           contains='Created_ModifiedClass'
  )
  Curves <- new('curves')
  Curves@SPR <- CalcSPRCurve(OM)
  
  # YPR
  # RelRecruit
  # Recruit
  
  
  fs <- as.numeric(names(Curves@SPR$Albacore[1,1,]))
  plot(fs, Curves@SPR$Albacore[1,1,], ylim=c(0,1), type='l')
  # SPR
  

  



  
  # CalculateEqRecruitment 
  
  # SPR
  
  # - yield curve
  # - SPR v F
  
  
  # ---- Calculate Reference Points ----
  HistOut@RefPoints <- CalcReferencePoints(OM, parallel, messages,
                                           Unfished = HistOut@Unfished)

  

  
    
    
  

  
  
  
  ## ---- Unfished Equilibrium ----
  
  ## ---- Per-Recruit Reference Points ----
  
  ## ---- Unfished Reference Points ----
  
  ## ---- Dynamic Unfished Reference Points ---- 
  
  ## ---- MSY Reference Points ----
  
  ## ---- Calculate Spawning Potential Ratio ----
  
  ## ---- Mean Generation Time ----
  
  ## ---- Reference Yield ----
  
  
  
  # ---- Optimize Rec Devs for Initial Depletion ----
  
  
  # ---- Non-Equilibrium Initial Year ----
  
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
  
  

  
  
  
  HistRel <- SetHistRel(OM) 
    
  
  
  
                        
  
  
  
  
}


