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
  
  Hist <- new('hist') # new Hist object to return 
  
  # ---- Calculate Unfished Dynamics ----
  # TODO Dynamic Unfished
  Hist@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  

  # ---- Calculate Curves -----
  # NOTE: Curves do not account for spatial closures or MICE interactions
  
  #New Classes (temp) 
  
  # Equilibrium values for a given F 
  setClass("curves",
           slots=c(
                   SPR='list', 
                   YPR='list',
                   RPR='list',
                   RelRec='list',
                   Recruit='list',
                   Yield='list',
                   Removal='list',
                   Biomass='list',
                   SBiomass='list',
                   SP='list',
                   Misc='list'
           ),
           contains='Created_ModifiedClass'
  )
  
  SPR0 <- CalcSPR0(OM) 
  
  Curves <- new('curves')
  Curves@SPR <- CalcSPR(OM, SPR0, FSearch=OM@Control$Curves$FSearch)
  Curves@RelRec <- CalcRelRec(OM, SPR=Curves@SPR)
  ypr <-  CalcYPR(OM)
  Curves@RPR <- lapply(ypr, '[[', 1)
  Curves@YPR <- lapply(ypr, '[[', 2)
  
  R0 <- purrr::map(GetR0(OM), AddDimension, name='apicalF')
  Curves@Recruit <- purrr::map2(R0, Curves@RelRec, ArrayMultiply)
  Curves@Yield <- purrr::map2(Curves@YPR, Curves@RelRec, ArrayMultiply)
  Curves@Removal <- purrr::map2(Curves@RPR, Curves@RelRec, ArrayMultiply)
  
  # add number-per-recruit
  # calculate B, SB, and SP
  
  # Recruit
  # Yield 
  
  # MSY - use removals or retain?
  
  
 
  
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


