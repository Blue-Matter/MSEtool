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
  
  Hist@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  # TODO Dynamic Unfished
  
  Hist@RefPoints <- CalcRefPoints(OM, Hist@Unfished)
  # TODO - SPRcrash, MGT, RefYield 

  
  # ---- Non-Equilibrium Initial Year ----
  
  InitialTimeStepN <- purrr::map2(Hist@Unfished@Equilibrium@Number,
                                  purrr::map(OM@Stock,GetRecDevInitAgeClasses),
                                  AddInitialRecDev
    
  )
  
  ## UP TO HERE --- 
  # make arrays for historical and populate
  # make arrays nsim dimension, then reduce later if not needed
  
  
  CreateArraySATR <- function(Stock) {
    if (inherits(Stock, 'om')) {
      return(purrr::map(Stock@Stock, CreateArraySATR))
    }
    # sim, age, time step, region (area)
    array <- array(NA, dim=c(nSim(Stock),nAge(Stock), nTS(Stock), nArea(Stock)))
    AddDimNames(array, names=c("Sim", "Age", "Time Step", 'Area'),
                TimeSteps = TimeSteps(Stock))
  }


  for (i in 1:nStock(OM)) {
    # Numbers for initial age class
    InitialTimeStepN <- AddInitialRecDev(Hist@Unfished@Equilibrium@Number[[i]],
                                           GetRecDevInitAgeClasses(OM@Stock[[i]]))
   
  }
  
  
  
  Number <- Hist@Unfished@Equilibrium@Number$Female
  InitAgeClassRecDevs <- GetRecDevInitAgeClasses(OM@Stock$Female)
  
  GetRecDevInitAgeClasses <- function(Stock) {
    init1plus <- GetRecDevInit(Stock)
    init0 <- abind::adrop(GetRecDevHist(Stock)[,1, drop=FALSE],2)
    out <- cbind(init0, init1plus)
    dimnames(out) <- list(Sim=1:nrow(out),
                          Age=0:(ncol(out)-1))
    out
  }
  
  
  AddInitialRecDev <- function(Number, InitAgeClassRecDevs) {
    InitAgeClassRecDevs <- AddDimension(InitAgeClassRecDevs, 'Time Step')
    InitAgeClassRecDevs <- AddDimension(InitAgeClassRecDevs, 'Area')
    ArrayMultiply(Number[, ,1,, drop=FALSE], InitAgeClassRecDevs)
    
  }
  
  Hist@Unfished@Equilibrium@Number$Female 
  
  
  R0 <- purrr::map(OM@Stock, GetR0)
  
  
  
  RecDevInit <- purrr::map(OM@Stock, GetRecDevInit)
  
  
  abind::adrop(Hist@Unfished@Equilibrium@Number$Female[,,1,, drop=FALSE],3)
  
  ArrayMultiply(abind::adrop(Hist@Unfished@Equilibrium@Number$Female[,,1,, drop=FALSE],3),
                AddDimension(OM@Stock$Female@SRR@RecDevInit, 'Area'))
  
  Hist@Unfished@Equilibrium@Number$Female[,,1,]
  
  OM@Stock$Female@SRR@RecDevHist
  
  
  GetRecDevInit(OM)
  GetRecDevHist(OM)$Female |> dimnames()
 
  
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


