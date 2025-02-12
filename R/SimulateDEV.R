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
  Hist@Number <- purrr::map2(OM@Stock,
                             Hist@Unfished@Equilibrium@Number, 
                             InitNumber)


  
  # loop over historical timesteps 
  TimeStepsHist <- TimeSteps(OM, 'Historical')
  nHistTS <- length(TimeStepsHist)
  ts <- 2 
  for (ts in 2:nHistTS) {
    
    # up to here - do spatial F distribution
    
    thisTimeStep <- TimeStepsHist[ts]
    lastTimeStep <- TimeStepsHist[ts-1]
  
    # Do MICE stuff during last time step (if applicable)
    Hist <- CalcMICE(Hist, TimeStep=lastTimeStep)
    
    Hist@Fleet[[1]][[1]]@Effort@Effort[,1]  <- 0.5
    
    # Total catch last time step
    Hist@Fleet <- CalcFatAge(Hist@Fleet, TimeSteps=lastTimeStep)
  
    NatAge <- purrr::map(Hist@Number, \(x) apply(x[,,ts-1,, drop=FALSE], 1:3, sum))
    
    CatchFleet <- CalcCatch(Stock=Hist@Stock[[1]],
                            Fleet=Hist@Fleet[[1]], 
                            NatAge=NatAge[[1]], 
                            TimeSteps=lastTimeStep)
    
    CatchFleet$RetainBiomass |> sum()
    
    
    # Distribute F over areas (if applicable)
    Hist <- CalcFleetDistribution(Hist, TimeStep=lastTimeStep)
    
    # Update total catch given effort constraints by area 
    TimeStep=lastTimeStep
    
    CalcFleetDistribution <- function(Hist, TimeStep) {
      narea <- nArea(Hist@Stock[[1]])
      if (narea < 2)
        return(Hist)
      
      if (!is.null(Hist@Fleet[[1]][[1]]@Distribution@Closure))
        cli::cli_abort('Spatial closures not done yet', .internal=TRUE)
      
      NatAgeArea <- purrr::map(Hist@Number, ArraySubsetTimeStep, TimeSteps=TimeStep)
      FleetWeightatAge <- purrr::map2(Hist@Stock, Hist@Fleet, \(x,y) 
                                      GetFleetWeightAtAge(x,y, TimeSteps=TimeStep))
                                      
      # Calculate relative utility (current VB) by area
      RelativeSize <- Hist@Stock[[1]]@Spatial@RelativeSize # should be the same across stocks
      
      UtilityByArea <- CalcUtilityByStockFleetArea(Hist, TimeStep=TimeStep)
      UtilityDensity <- purrr::map(UtilityByArea, CalcUtilityDensity, RelativeSize)
      
      # TODO - note relative utility is calculated by stock 
      # ie Stock-Fleet is independent 
      # technically fleet distribution should be proportional to overall utility
      # ie summed across stocks
     
      # optimize fleet effort distribution
      for (st in 1:nStock(Hist)) {
        selectivity <- GetSelectivityAtAge(Hist@Fleet[[st]], TimeStep) |>
          List2Array()
        
        # TODO get these functions to return default arrays with correct dimensions
        retention <- GetRetentionAtAge(object=Hist@Fleet[[st]], TimeStep) |>
          List2Array()
        

        discardmortality <- GetDiscardMortalityAtAge(Hist@Fleet[[st]], TimeStep) |>
          List2Array()
        
        retention <- discardmortality <- selectivity
        retention[]<- 1
        discardmortality[] <- 0
       
        fleetweight <- FleetWeightatAge[[st]]
        
        # this should probably be vulnerable biomass instead of utility
        RelDensity <- UtilityDensity[[st]]
        
        CurrentEffort <- GetEffort(Hist@Fleet[[st]], TimeStep) |> List2Array() 
        Catchability <- GetCatchability(Hist@Fleet[[st]], TimeSteps=TimeStep) |> List2Array() 
        
        naturalmortality <- GetNMortalityAtAge(Hist@Stock[[st]], TimeStep)
        numberatage <- NatAgeArea[[st]]
        
        nFleet <- nFleet(Hist)
        nAge <- nAge(Hist@Stock[[st]])
        nArea <- nArea(Hist@Stock[[st]])
        
        
        # for (s in 1:nSim(Hist)) {
          s <- 1
          SpatTarg <- rep(1, nFleet)
          
          
          OptSpatTarg <- function(SpatTarg, ...) {
           
            reldensity <- abind::adrop(RelDensity[s,,, drop=FALSE],1)
          
            SpatTarg <- matrix(SpatTarg, nArea, nFleet, byrow=TRUE)
            EffortDist <- reldensity^SpatTarg
            EffortDist[!is.finite(EffortDist)] <- 0
            RelativeQ <- Catchability[s,1,]/replicate(nFleet, RelativeSize[s,])
            
            ApicalFInteractArea <- t(CurrentEffort[s,1,] * t(EffortDist)) * RelativeQ
            FInteractAgeAreaFleet <- array(NA, dim=c(nAge, nArea, nFleet))
            FRetainAgeAreaFleet <- FInteractAgeAreaFleet
            FDiscardAgeAreaFleet <- FInteractAgeAreaFleet
            FDiscardDeadAgeAreaFleet <- FInteractAgeAreaFleet
            FDeadAgeAreaFleet <- FInteractAgeAreaFleet
            for (fl in 1:nFleet) {
              FInteractAgeAreaFleet[,,fl] <- replicate(nArea,selectivity[s,,1,fl]) * 
                t(replicate(nAge, ApicalFInteractArea[,fl]))
              FRetainAgeAreaFleet[,,fl] <- replicate(nArea,retention[s,,1,fl]) *
                FInteractAgeAreaFleet[,,fl] 
              FDiscardAgeAreaFleet[,,fl] <- FInteractAgeAreaFleet[,,fl] -  FRetainAgeAreaFleet[,,fl]
              FDiscardDeadAgeAreaFleet[,,fl] <-  FDiscardAgeAreaFleet[,,fl] *
                replicate(nArea,discardmortality[s,,1,fl])
              FDeadAgeAreaFleet[,,fl] <- FRetainAgeAreaFleet[,,fl] + FDiscardDeadAgeAreaFleet[,,fl]
            }
            
            ZatAgeArea <- replicate(nArea, naturalmortality[s,,1]) + apply(FDeadAgeAreaFleet, 1:2, sum)
            ZatAgeArea <- replicate(nFleet, ZatAgeArea) 
            
            InteractNumber <- FInteractAgeAreaFleet/ZatAgeArea * (1-exp(-ZatAgeArea)) * 
              replicate(nFleet, numberatage[s,,1,])
            
            CatchNumber <- FRetainAgeAreaFleet/ZatAgeArea * (1-exp(-ZatAgeArea)) * 
              replicate(nFleet, numberatage[s,,1,])
            
            fleetweightarea <- replicate(nArea, abind::adrop(fleetweight[s,,1,, drop=FALSE], c(1,3))) |> 
              aperm(c(1,3,2))
            CatchBiomass <- CatchNumber * fleetweightarea
            
            TotalCatchByFleet <- apply(CatchBiomass, 3, sum)
            
            # Calculate overall F by Area
            # population wide F by area
            m <- replicate(nArea, naturalmortality[s,,1])
            m <- replicate(nFleet, m)
            sel <- abind::adrop(selectivity[s,,1,, drop=FALSE], c(1,3))
            sel <- replicate(nArea, sel) |> aperm(c(1,3,2))
            
            # This calculate F Interact (proxy of Effort) by Fleet and Area
            # i.e population wide 'F'
            PopFInteractbyArea <- CalcFfromCatchArea(Catch=InteractNumber,
                                                     PopatAge=replicate(nFleet, numberatage[s,,1,]),
                                                     MatAge=m,
                                                     SelectAtAge=sel)
            
            EffortArea <- PopFInteractbyArea/matrix(colSums(PopFInteractbyArea), 
                                                    nArea, nFleet, byrow=TRUE) * 
              matrix(CurrentEffort[s,1,], nArea, nFleet, byrow=TRUE)
            
            # Utility per Unit Effort by Area & Fleet
            # currently catch biomass but can convert to utility later
            # i.e take into account targeting, cost of area, value of stocks, etc
            # should be approximately equal across areas
            CPUEArea <- apply(CatchBiomass, 2:3, sum)/EffortArea
            CPUEArea[!is.finite(CPUEArea)] <- 0
            
            # SSQs
            CPUEDistSSQ <- (CPUEArea -  matrix(apply(CPUEArea, 2, mean),nArea, nFleet, byrow=TRUE))^2
            
            CatchSSQ <- (TotalCatchByFleet - apply(abind::adrop(CatchFleet$RetainBiomass[s,,1,, drop=FALSE], c(1,3)), 2, sum))^2
            
            out <-  sum(CatchSSQ, CPUEDistSSQ)
            # print(list(SpatTarg, out, CatchSSQ, CPUEDistSSQ))
            out
          }
          
        
          SpatTarg <- rep(1, nFleet)
          dopt <- optim(SpatTarg, OptSpatTarg)
          
          SpatTarg <- dopt$par
          
          
        # }
        
      }
    

     
      # by Stock and Sim 
      RelDensity <- abind::adrop(UtilityDensity$`SA Red Snapper`[1,,, drop=FALSE],1)
      CurrentEffort <- GetEffort(Hist@Fleet$`SA Red Snapper`, TimeStep) 
     
      
      
     
      
      CurrentEffort <- CurrentEffort[1,,]
      Catchability <- Catchability[1,,]
      RelativeSize <- RelativeSize[1,]
     
      
  
      
  
      
      
      
      
    
      
      
    
      
      # Calculate retained catch by area in numbers at age
      
      
      # Calculate overall F by area
      
      # Calculate relative utility by area
      
      
      # Calculate overall catch
      
      # SSQs
      
      
      
      
      
      
      CalcFatAge(Hist@Fleet$`SA Red Snapper`, ApicalFArea)
      
      ZArea <- FArea + M 
      CatchArea <- FArea/ZArea * (1-exp(-ZArea)) * BiomassByArea
     
      
      
      
      
      
      
      
      
      
      
      
  
      
      FleetWeightAgeArea <- purrr::map2(Hist@Stock, Hist@Fleet, GetFleetWeightAtAge,
                                        TimeSteps=lastTimeStep) |>
        purrr::map(AddDimension, name='Area') |>
        purrr::map(aperm, perm=c(1,2,3,5,4))
      
      # TODO
      # account for Fleet@Distribution@Closure in N/B by area/fleet
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
      
      Hist@Fleet$`SA Red Snapper`$cHL@Distribution@Effort
      
    }
    
  
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


