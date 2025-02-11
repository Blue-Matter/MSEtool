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
  
  # TODO add check for changes to all functions 
  Hist@RefPoints <- CalcRefPoints(OM, Hist@Unfished)
  # TODO - SPRcrash, MGT, RefYield 
  # GetFSPR(SPRarray=Hist@RefPoints@Curves@SPR[[1]], SPRvalue=0.3)
  
  # ---- Optimize Rec Devs for Initial Depletion ----
  # TODO
  if (length(OM@Stock[[1]]@Depletion@Initial)>0)
    cli::cli_abort('Initial depletion not done', .internal=TRUE)
  
  # ---- Non-Equilibrium Initial Time Step ----
  Hist@Number <- purrr::map2(OM@Stock, Hist@Unfished@Equilibrium@Number, InitNumber)

  ##############################################################################
  CheckBAMN <- function(ts=1) {
    # Match  numbers from BAM to OM 
    ts <- 1
    BAMdata <- BAMGetObject()
    df <- data.frame(BAM=c(0,BAMdata$N.age[ts,]), 
                     OM= rowSums(Hist@Number$`SA Red Snapper`[1,,ts,]))
    plot(df$BAM)
    lines(df$OM)
    invisible(df)
  }
  ##############################################################################
  
  # loop over historical timesteps 
  TimeStepsHist <- TimeSteps(OM, 'Historical')
  nHistTS <- length(TimeStepsHist)
  ts <- 2 
  for (ts in 2:nHistTS) {
    
    thisTimeStep <- TimeStepsHist[ts]
    lastTimeStep <- TimeStepsHist[ts-1]
  
    # Do MICE stuff during last time step (if applicable)
    if (length(OM@Relations)>0) {
      cli::cli_abort('MICE not done', .interal=TRUE)
    }
    
    # Distribute F over areas (if applicable)
    NatAgeArea <- purrr::map(Hist@Number, ArraySubsetTimeStep, TimeSteps=lastTimeStep)
    FleetWeightAgeArea <- purrr::map2(Hist@Stock, Hist@Fleet, GetFleetWeightAtAge,
                                      TimeSteps=lastTimeStep) |>
      purrr::map(AddDimension, name='Area') |>
      purrr::map(aperm, perm=c(1,2,3,5,4))
    
    if (!is.null(Hist@Fleet[[1]][[1]]@Distribution@Closure))
      cli::cli_abort('Spatial closures not done yet', .internal=TRUE)
    
    # TODO
    # account for Fleet@Distribution@Closure is N/B by area/fleet
    
    NatAgeAreaFleet <- purrr::map(NatAgeArea, AddDimension, name='Fleet')
    BatAgeAreaFleet <- purrr::map2(NatAgeAreaFleet, FleetWeightAgeArea, ArrayMultiply)
    
    # selectivity at age by fleet (list by stock)
    selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=lastTimeStep) |>
      purrr::map(function(.x) {
        array <- array(unlist(.x), dim=c(dim(.x[[1]]), length(.x)))
        dimnames(array) <- c(dimnames(.x[[1]]), Fleet=list(names(.x)))
        AddDimension(array, 'Area') |>
          aperm(c(1,2,3,5,4))
      })
    
    retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=lastTimeStep) 
    if (any(is.null(unlist(retention)))) {
      SelectivityRetention <- selectivity
    } else {
      retention <- purrr::map(retention, function(.x) {
        array <- array(unlist(.x), dim=c(dim(.x[[1]]), length(.x)))
        dimnames(array) <- c(dimnames(.x[[1]]), Fleet=list(names(.x)))
        AddDimension(array, 'Area') |>
          aperm(c(1,2,3,5,4))
      })
      SelectivityRetention <- purrr::map2(selectivity, retention, ArrayMultiply)
    }
    
    
    VBatAgeAreaFleet <- purrr::map2(BatAgeAreaFleet, 
                               SelectivityRetention, 
                               ArrayMultiply)
 
    
    
    # Calculate F-at-Age by Fleet from Effort and q - last time step 
    OM@Fleet$`Day octopus`$`Octopus Fleet`@Effort@Effort[,1] <- 0.3
    
    
    
    
    
    
    
    
    
    
    OM@Fleet <- CalcFatAge(OM@Fleet, TimeSteps=lastTimeStep)
    
    # Calc overall catch
    FatAgeLastTimeStep <- GetFatAgeArray(OM@Fleet, lastTimeStep)
    
    
    
    VBStockFleetArea <- CalcVulnBiomassByStockFleetArea(Hist, lastTimeStep)
    NatAge <- purrr::map(Hist@Number, function(x) {
      ArraySubsetTimeStep(x, TimeSteps=lastTimeStep)
    })
    
    # Calculate overall Vulnerable N at Age 
    
    GetNatAge <- function(Hist, TimeStep) {
      
      Hist@Number
    }
    
    
    
    

    
    
    # Calculate Total Catch last time step given an overall F 
    stock <- Hist@Stock$`Day octopus`
    fleet <- Hist@Fleet$`Day octopus`
    N <- Hist@Number$`Day octopus`[1,,1,]
    
    GetFatAgeArray(fleet, TimeSteps = lastTimeStep)
    
    
    
    CalcFfromCatch(Catch, PopatAge, MatAge, SelectAtAge)
    
    
    # TODO skip if no spatial areas
    # Calculate Utility by Area 
    UtilityByArea <- CalcUtilityByStockFleetArea(Hist, TimeStep=lastTimeStep)
    
    
    fl <- 1
    
    EffortLastTimeStep <- fleet@Effort@Effort[1,ts-1]
    
    EffortLastTimeStep <- 1
    fleet@Distribution@Effort <- EffortLastTimeStep *UtilityByArea$`Day octopus`[1,1,,fl]
    
    NbyArea <- Hist@Number$`Day octopus`[1,,ts-1,]
    BbyArea <- NbyArea^3
    select <- GetSelectivityAtAge(fleet, TimeSteps=lastTimeStep)
    select <- select[1,,1]
    
    
    effortbyarea <-  fleet@Distribution@Effort
    
    effortbyarea <- c(0.05,0.95)
    
    UtilityByArea$`Day octopus`[1,1,,1]
    effortbyarea <- apply(NbyArea, 2, sum)/sum(NbyArea)
      
    optfun <- function(logq) {
      
      Q <- exp(logq)
     
       # Calc F by area
      FbyArea <- cbind(effortbyarea[1] * select,
                       effortbyarea[2] * select) *
        Q
      
      
      MbyArea <- GetNMortalityAtAge(Hist@Stock$`Day octopus`, TimeSteps=lastTimeStep)[1,,1]
      MbyArea <- cbind(MbyArea,MbyArea)
      
      ZbyArea <- MbyArea+FbyArea
      
      
      # Calc catch by area 
      CatchbyArea <- FbyArea/ZbyArea * (1-exp(-ZbyArea)) * BbyArea
      
      relUtility <- colSums(CatchbyArea)/sum(CatchbyArea)
      relUtility <- relUtility/effortbyarea
      relUtility
      
      
      
      # Apical F by Area 
      Farea <- rep(0, 2)
      for (area in 1:2) {
        Farea[area] <- CalcFfromCatch(sum(CatchbyArea[,area]), BbyArea[,area], MatAge, select)  
      }
      
      Farea/sum(Farea)
      
      
      overallCatch <- rowSums(CatchbyArea)
      overallPop <- rowSums(NbyArea)
      overallF <- CalcFfromCatch(sum(overallCatch), overallPop, MatAge, select)  
      (overallF-1)^2
    }
  
    
    doopt <- optimize(optfun, log(c(0.001, 5)))
    Q <- exp(doopt$minimum)
     
    overallF
    
    exp(mean(log(Farea)))
    
    
    fleet@Effort@Effort[1,1] <- 1
    
    
    
    # TODO expand to multi-fleet
    

    
  
   
    
    
 
    
    
    # Distribute F/Effort over Areas according to Utility and overall F constraint (if applicable)
    
    
    
    t <- CalcCatch(Stock=Hist@Stock$`Day octopus`,
                   Fleet=Hist@Fleet$`Day octopus`, 
                   NatAge=apply(Hist@Number$`Day octopus`[,,ts-1,, drop=FALSE], 1:3, sum)
    )
    
                   
    
    # Distribute Effort across areas according to Utility
    
    #  
    
    GetEffort(OM@Fleet$`Day octopus`, lastTimeStep)
    FatAgeLastTimeStep$`Day octopus` |> dim()
    
    UtilityByArea$`Day octopus`[1,1,,]
    
    OM@Fleet$`Day octopus`$`Octopus Fleet`@Distribution@Effort
    
    
    
    
    
    DistributeFishing <- function(Hist, TimeStep) {
      # each fleet should distribute according to overall utility across stocks
      
      
      # TODO: account for price/value per stock and Cost per fleet/area
      # TODO: account for Targeting parameter
      
      
      
      
      VBatAgeStockAreaFleetList <- purrr::map2(BatAgeAreaFleet, 
                                               SelectivityRetention, 
                                               ArrayMultiply)
      dd <- dim(VBatAgeStockAreaFleetList[[1]]) 
      VBatAgeStockAreaFleet <- array(unlist(VBatAgeStockAreaFleetList), 
                                     dim=c(dd,
                                           length(VBatAgeStockAreaFleetList)))
      
      dimnames(VBatAgeStockAreaFleet) <- c(dimnames(VBatAgeStockAreaFleetList[[1]]),
                                           list(Stock=StockNames(Hist)))
      
      # Fleet@Distribution@Cost
      # Calculate relative value by area, stock, fleet (cost of area & value of age class by stock/fleet)
      
      ValueAreaFleet <-  apply(VBatAgeStockAreaFleet, c(1,4,5), sum)
      nAreas <- dim(ValueAreaFleet)[2]
      TotalValueAreaFleet <- replicate(nAreas, apply(ValueAreaFleet, c(1,3), sum)) |> aperm(c(1,3,2))
      
      RelativeValueAreaFleet <- ValueAreaFleet/TotalValueAreaFleet
      RelativeValueAreaFleet[!is.finite(RelativeValueAreaFleet)] <- 0
      
      # distribute effort by area
      Fleet@Effort@Effort[1,1] * RelativeValueAreaFleet[1,,1]
      
      ########## UPTO HERE #####
      
      # calculate F by area given overall F 
      
      
      
      
      
      
      
      
    }
    
    
    
    
  
    


    # Hist@Fleet <- CalcFatAge(Hist@Fleet, TimeSteps=lastTimeStep)
    # FTotal <- CalcFTotal(Hist@Fleet, TimeSteps = lastTimeStep)
    


    
    
    
    # Calculate fishing spatial distribution for last time step
    # Distribute fishing effort while maintaining overall F
  
   
    
    
    # Calculate fishing mortality for last time step
    
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


