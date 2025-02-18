
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
  
  
  Hist <- Hist(OM)

  # ---- Calculate Unfished Dynamics ----
  Hist@Unfished <- CalcUnfishedDynamics(OM)

  # ---- Calculate Reference Points ----
  Hist@RefPoints <- CalcRefPoints(Hist)

  # ---- Optimize for Initial Depletion ----
  Hist <- OptimInitDepletion(Hist)
  
  # ---- Number and Biomass in Initial Time Step ----
  Hist <- CalcInitialTimeStep(Hist)
  
  # ---- Optimize Catchability for Terminal Depletion ----
  Hist <- OptimCatchability(Hist)
  
  
  # ---- Historical Population Dynamics ----

  # loop over historical timesteps 
  TimeStepsHist <- TimeSteps(OM, 'Historical')
  nHistTS <- length(TimeStepsHist)
  ts <- 1
  nHistTS + 1
  for (ts in cli::cli_progress_along(1:nHistTS,
                                     'Calculating Population Dyamics')) {
    
    
    
    thisTimeStep <- TimeStepsHist[ts]
    nextTimeStep <- TimeStepsHist[ts+1]
    
    # ---- Do MICE stuff during this Time Step (if applicable) -----
    # TODO
    Hist <- CalcMICE(Hist, TimeStep=thisTimeStep)
    
    # for MPs - Calculate Effort, Selectivity, etc
    
    # ---- Calculate Catch by Area this Time Step ----
    Hist <- CatchByArea(Hist, TimeSteps=thisTimeStep)
  
    # ---- Calculate and Update overall F by Fleet this Time Step ----
    Hist <- CalcFleetFMortality(Hist, TimeSteps=thisTimeStep)
    
    # ---- Calculate Recruitment  Time Step ----
    Hist <- CalcRecruitment(Hist, TimeStep=thisTimeStep)
    
    # ---- Number, Biomass at beginning of Next Time Step and Move ----
    Hist <- CalcNumberNext(Hist, thisTimeStep)

  }
  
  # TODO - fix and match BAM
  
  bam <- rdata$N.age |> apply(1, sum)
  m1 <- apply(multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,2:21,,],2, sum)
  m2 <- Hist@Number$`SA Red Snapper`[1,2:21,,] |> apply(c('Time Step'), sum)
  
  plot(bam)
  lines(m1)
  lines(m2, col='blue')
 
  
  
  

  
  

  
  
  
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


