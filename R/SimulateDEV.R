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
  
  # TODO - this should be done in the call to Hist() creating `hist` object
  # arrays: sim, age, time-step, region (area) fleet # list by stock
  Hist@Removal <- purrr::map(OM@Stock, 
                             CreateArraySATRF, 
                             nSim(OM),
                             TimeSteps(OM, 'Historical'),
                             fleetnames=FleetNames(OM))
  
  Hist@Retain <- purrr::map(OM@Stock, 
                             CreateArraySATRF, 
                             nSim(OM),
                             TimeSteps(OM, 'Historical'),
                             fleetnames=FleetNames(OM))
  
  Hist@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  
  # TODO Dynamic Unfished
  
  # TODO add check for changes to all functions 
  Hist@RefPoints <- CalcRefPoints(OM, Hist@Unfished)
  
  # TODO - SPRcrash, MGT, RefYield 
  # GetFSPR(SPRarray=Hist@RefPoints@Curves@SPR[[1]], SPRvalue=0.3)
  
  # ---- Optimize Rec Devs for Initial Depletion ----
  # TODO
  if (length(OM@Stock[[1]]@Depletion@Initial)>0)
    cli::cli_abort('Initial depletion not done', .internal=TRUE)
  
  # ---- Non-Equilibrium Initial Time Step ----
  Hist@Number <- purrr::map2(OM@Stock,
                             Hist@Unfished@Equilibrium@Number, 
                             InitNumber)


  
  ###################

  
  # loop over historical timesteps 
  TimeStepsHist <- TimeSteps(OM, 'Historical')
  nHistTS <- length(TimeStepsHist)
  ts <- 2 
  nHistTS + 1
  for (ts in 2:nHistTS) {
    
    thisTimeStep <- TimeStepsHist[ts]
    lastTimeStep <- TimeStepsHist[ts-1]
  
    # number at the beginning of last time step
    NumberLastTimeStepByArea <- purrr::map(Hist@Number, 
                                     ArraySubsetTimeStep, 
                                     TimeSteps=lastTimeStep)
    
    # sum over areas 
    NumberLastTimeStep <- purrr::map(NumberLastTimeStepByArea, \(x) {
      d <- dim(x)
      if (d[4]==1) {
        return(abind::adrop(x[,,,1, drop=FALSE],4))
      } 
      apply(x, 1:3, sum)
    })
    
    # ---- Do MICE stuff during Last Time Step (if applicable) -----
    # TODO
    Hist <- CalcMICE(Hist, TimeStep=lastTimeStep)
    
    CalcEffortDist <- function(Hist, TimeSteps=NULL) {
      # Distributes Effort over Areas 
      # Proportional to Relative Density of Vulnerable Biomass
      # Note: calculated independently by stock to deal with 
      # multi-stock OMs imported from single-stock, non-spatial assessments
      
      if (is.null(TimeSteps))
        TimeSteps <- TimeSteps(Hist@Stock[[1]], 'Historical')
      
      RelativeDensity <- purrr::pmap(list(Hist@Stock,
                                          Hist@Fleet,
                                          NumberLastTimeStepByArea  
      ), 
      TimeSteps=lastTimeStep, CalcDensity)
      
      Effort <- purrr::map(Hist@Fleet, GetEffort, TimeSteps=lastTimeStep)
      Effort <- purrr::map(Effort, AddDimension, 'Area')
      
      purrr::map2(Effort, RelativeDensity, ArrayMultiply)
    }
    
    EffortDist <- CalcEffortDist(Hist, lastTimeStep)
    Catchability <- purrr::map(Hist@Fleet, GetCatchability,
                               TimeSteps=lastTimeStep) |>
      purrr::map(AddDimension, 'Area')
    
    ApicalF <- purrr::map2(Catchability, EffortDist, ArrayMultiply) |>
      purrr::map(AddDimension, 'Age') |>
      purrr::map(aperm, c(1,5,2,3,4))
    
    selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=lastTimeStep) |>
      purrr::map(AddDimension, 'Area')
    retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=lastTimeStep) |>
      purrr::map(AddDimension, 'Area')
    discardmortality <- purrr::map(Hist@Fleet,
                                   GetDiscardMortalityAtAge, TimeSteps=lastTimeStep) |>
      purrr::map(AddDimension, 'Area')
    
    FInteract <- purrr::map2(ApicalF, selectivity, ArrayMultiply)
    FRetain <- purrr::map2(FInteract, retention, ArrayMultiply)
    FDiscardTotal <- purrr::map2(FInteract, FRetain, ArraySubtract)
    FDiscardDead <-  purrr::map2(FDiscardTotal, discardmortality, ArrayMultiply)
    FDead <- purrr::map2(FRetain, FDiscardDead, ArrayAdd)
    
    NMortalityAtAge <- purrr::map(Hist@Stock,  GetNMortalityAtAge, TimeSteps=lastTimeStep) |>
      purrr::map(AddDimension, 'Area')

    FDeadTotal <- purrr::map(FDead, \(x)
                             apply(x, c(1,2,3,5), sum))
    
    ZatAge <- purrr::map2(FDeadTotal, NMortalityAtAge, ArrayAdd)
    
    BatAge <- purrr::pmap(list(Hist@Stock,
                     Hist@Fleet,
                     NumberLastTimeStepByArea  
    ), 
    TimeSteps=lastTimeStep, GetBiomassAtAge)
    
    # TODO - replace other CalcCatch function with this one
    FatAge <- FDead$`SA Red Snapper`
    ZatAge <- ZatAge$`SA Red Snapper`
    BatAge <- BatAge$`SA Red Snapper`
    NatAge <- NumberLastTimeStepByArea$`SA Red Snapper`
    
    # TODO calculate number first then multiply by weight
    CalcCatch2 <- function(FatAge, ZatAge, NatAge) {
      
      NatAge <- AddDimension(NatAge, 'Fleet') |> 
        aperm(c(1,2,3,5,4))
      ZatAge <- AddDimension(ZatAge, 'Fleet') |> 
        aperm(c(1,2,3,5,4)) 
      
      RemovalNumber <- ArrayMultiply(NatAge, (1-exp(-ZatAge))) |>
        ArrayMultiply(ArrayDivide(FatAge,ZatAge))
      
      RemovalBiomass <- ArrayMultiply(RemovalNumber, 
                                      AddDimension(FleetWeightatAge, 'Area'))
      
      # checking F
      RemovalBiomass[1,,1,1,] |> sum()
      CatchAtAge <- apply(RemovalNumber[1,,1,1,], 1, sum)
      PopoAtAge <- apply(NatAge[1,,1,1,], 1, sum)
      M <- NMortalityAtAge$`SA Red Snapper`[1,,1,1]
      selectivity$`SA Red Snapper` |> dim()
      
      # TODO check if the Fs are equal
      # update CalcFfromCatch for discards etc?
      t <- CalcFfromCatch()
      # 
      
    }
    
    
    # sim, time step, fleet 
    
    
    # ---- Calculate F Overall - Last TS ----
    Hist@Fleet <- CalcFatAge(Hist@Fleet, TimeSteps=lastTimeStep) # TODO this should update the array rather than replace it
    Hist@Fleet <- CalcApicalF(Hist@Fleet, TimeStep=lastTimeStep)
    
    Catches <- purrr::pmap(
      list(
        Hist@Stock, 
        Hist@Fleet, 
        NumberLastTimeStep
      ), 
      CalcCatch, TimeSteps=lastTimeStep)

    # Dead overall: Catches$`Day octopus`$RemovalNumber
    
    CalcEffortFromFDead <- function(Fleet) {
      
    }
    
    # Set effort by area - overall effort divide by relative size
    
    
    
    # TODO: expand for multi fleet
    SolveFInteractArea <- function(logFArea, ...) {
      Farea <- exp(logFArea)
      nArea <- length(Farea)
      
      FInteractAgeArea <- Farea * replicate(nArea,select)
        
      ZArea <- FArea+ replicate(nArea,NaturalMortality)
      
    }
    
    apicalF <- GetApicalF(Hist@Fleet$`Day octopus`$`Octopus Fleet`, lastTimeStep)
    M <- GetNMortalityAtAge(Hist@Stock$`Day octopus`, lastTimeStep)
    NaturalMortality <- M[1,,1]
    select <- GetSelectivityAtAge(Hist@Fleet$`Day octopus`$`Octopus Fleet`, lastTimeStep)
    select <- select[1,,1]
    logFArea <- rep(log(apicalF[1,1]),2)
    
    dopt <- optim(logFArea, SolveFInteractArea,  method = "BFGS")
    
    # ----  F, Effort Catch, Removals, Discards by Area - Last TS ----
    
    # Calc F-at-Age Array Overall
  
    
    

    Fleet@FishingMortality

    
    Fleet <- Hist@Fleet$`Day octopus`$`Octopus Fleet`
    
    
    # F-at-Age by Area 
    
    # Update F-at-Age Overall if needed
    
    
    
    
    
    
    
    
    Stock <- Hist@Stock$`Day octopus`
    Fleet <- Hist@Fleet$`Day octopus`
    NatAge <- NumberLastTimeStep$`Day octopus`
    
    # CalcApicalF <- function(Fleet, TimeStep) {
    #   # ApicalF should be calculated from max FDead
    #   if (inherits(Fleet, 'fleet')) {
    #     effort <- ArraySubsetTimeStep(Fleet@Effort@Effort, TimeStep)
    #     q <- ArraySubsetTimeStep(Fleet@Effort@Catchability, TimeStep)
    #     if (!is.null(ArraySubsetTimeStep(Fleet@Effort@qCV)))
    #       cli::cli_abort('`qCV` not done yet', .internal=TRUE)
    #     if (!is.null(ArraySubsetTimeStep(Fleet@Effort@qInc)))
    #       cli::cli_abort('`qInc` not done yet', .internal=TRUE)
    #     
    #     if (is.null(Fleet@FishingMortality@ApicalF)) {
    #       Fleet@FishingMortality@ApicalF <- ArrayMultiply(q, effort)
    #     } else {
    #       Fleet@FishingMortality@ApicalF <- cbind(Fleet@FishingMortality@ApicalF, ArrayMultiply(q, effort))
    #     }
    #     return(Fleet)
    #   }
    #   purrr::map(Fleet, CalcApicalF, TimeStep=TimeStep)
    # } 
    
    
    
    
    Hist@Fleet <- purrr::map(Hist@Fleet, CalcApicalF, TimeStep=lastTimeStep)
    

    
    # sim, time-step, area 
    Hist@Fleet$`Day octopus`$`Octopus Fleet`@Distribution@ApicalF
    GetApicalF(Hist@Fleet$`Day octopus`$`Octopus Fleet`, TimeSteps = lastTimeStep)
    
    
    Hist@Fleet$`Day octopus`$`Octopus Fleet`@FishingMortality@ApicalF
    
    
    
   
    
    # TODO - this needs to fill the array each time 
    Hist@Removal$`Day octopus` |> dim()
    object <- Hist@Removal$`Day octopus`
    value <- t
    `Removal<-` <- function(object, value) {
      dimnames(object)$`Time Step` %in% dimnames(value)$`Time Step`  

      length(dim(value)) < length(dim(object))
      
            length(dim(object))
      
      abind::afill(object) <- value
      
    }
    
    
    t <- lapply(Catches, '[[', 'RemovalBiomass')
    
    t = t$`Day octopus`
    
    
    Hist@Removal <- lapply(Catches, '[[', 'RemovalBiomass')
    Catches$`Day octopus`$RemovalNumber
    Catches$`Day octopus`$RemovalBiomass
                
    Hist@Removal                                                                                               
    Hist@Retain
    
    Stock <- Hist@Stock$`Day octopus`
    Fleet <- Hist@Fleet$`Day octopus`
    NatAge <- NumberLastTimeStep$`Day octopus`
    
    
   
 
    
    t = Hist@Number$`Day octopus`
    
   
    ListArray <- 
    
    
    
    Hist@Number$`Day octopus`[1,,ts-1,]
    
    Hist@Fleet$`Day octopus`$`Octopus Fleet`@Effort@Effort[,ts-1]
    

    Hist@Fleet$`Day octopus`$`Octopus Fleet`@Distribution@ApicalF
    
    Hist@Fleet$`Day octopus`$`Octopus Fleet`@Distribution@Effort
    
    
    # Total catch last time step
    Hist@Fleet <- CalcFatAge(Hist@Fleet, TimeSteps=lastTimeStep)
  
    # Distribute Effort over Areas (if applicable)  
    
    
    # Fishing Mortality by Area for last time step
    
    # Catch by Area for Last
    
    # Calculate Spawning Output  and Recruitment for last time step
    

    # Mortality and Aging into this time step
    
    # Move Stock at beginning of this time step
    
    
    
    
  }
  
  
  
 
  
  
  

  
  
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


