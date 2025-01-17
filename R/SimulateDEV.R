# 2025/01/17 
# - NSWO.R - generalize SPR calcs for multiple stocks and fleets 

# 2025/01/16 
# - testOM.R - code RefPoint calcs and compare with Hist - working test after NSWO working 

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        messages='default',
                        nSim=NULL,
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- OM |> CheckClass() |> Check() # TODO OM checks
  
  OM <- StartUp(OM, messages, nSim) 
    
  # TODO - re-populates if Stock/Fleet converted to list - fix hash
  
  HistOut <- new('hist') # new Hist object to return 
  
  # ---- Calculate Unfished Dynamics ----
  HistOut@Unfished <- CalcUnfishedDynamics(OM, messages, parallel=parallel)
  
  
  # ---- Calculate Curves -----
  
  Stock <- OM@Stock[[1]]
  Fleet <- OM@Fleet[[1]]
  
  
  
  CalcFishedSurvival <- function(Stock, Fleet, apicalF=NULL) {
    
    # TODO - include spatial closures
    
    if (!is.null(apicalF)) {
      
      ApicalFbyFleet <- purrr::map(Fleet, slot, 'FishingMortality') |> 
        purrr::map(slot, 'ApicalF') 
      
      dd <- dim(ApicalFbyFleet[[1]])
      
      ApicalFbyFleetArray <- array(unlist(ApicalFbyFleet), dim=c(dd[1], dd[2], length(ApicalFbyFleet))) |>
        AddDimNames(names=c('Sim', 'Time Step', 'Fleet'), TimeSteps=TimeSteps(Stock, 'Historical'))
      
      # normalize and set to apicalF by fleet fraction
      Ftotal <- apply(ApicalFbyFleetArray, c(1,2), sum)
      ApicalFbyFleetArray <- ApicalFbyFleetArray/replicate(length(ApicalFbyFleet), Ftotal) * apicalF
      
      ApicalF <- lapply(seq(dim(ApicalFbyFleetArray)[3]), function(x) 
        abind::adrop(ApicalFbyFleetArray[ , , x, drop=FALSE],3))
       
      for (fl in seq_along(ApicalFbyFleet)) {
        ApicalF(Fleet[[fl]]@FishingMortality) <- ApicalF[[fl]]
      }
    }
    
    FDead <- purrr::map(Fleet, CalcFatAge, return='FDead')
    
    FDead[[1]][1,,1] |> max()
    
    
    FDead <- array(unlist(FDead), dim=c(dim(FDead[[1]])[1], 
                                        dim(FDead[[1]])[2], 
                                        dim(FDead[[1]])[3], 
                                        length(FDead))) |>
      AddDimNames(names=c('Sim', 'Age', 'Time Step', 'Fleet'), TimeSteps=TimeSteps(Stock, 'Historical'))
    
    apply(FDead, c(1,2,3), sum) |> max()
    
    ### ----- UP TO HERE -----
    # - need to account for discard mortality for apicalF
    # - resulting max F is lower than apicalF is discardmort < 1
    # - depends how apicalF is defined!
    
    
    
    M_at_Age <- Stock@NaturalMortality@MeanAtAge
    PlusGroup <- Stock@Ages@PlusGroup
    
    if (SP) {
      SpawnTimeFrac <- Stock@SRR@SpawnTimeFrac  
    } else {
      SpawnTimeFrac <- NULL
    }
    
    # TODO sum F over fleets
    CalcSurvival(M_at_Age, PlusGroup=PlusGroup, SpawnTimeFrac=SpawnTimeFrac, F_at_Age=FDead[[1]])
  }
  
  

  CalculateSPRCurve <- function(OM, 
                           messages='default',
                           nSim=NULL,
                           parallel=FALSE) {
    
    # TODO add FVector to OM Control
    boundsF <- c(1E-3, 3)
    F_search <- exp(seq(log(min(boundsF)), log(max(boundsF)), length.out = 50))
    
    OM@Control$FVector <- F_search
    
    FVector <- OM@Control$FVector
    
    SPR <- vector('list', nStock(OM))
    names(SPR) <- StockNames(OM)
    
    # TODO pass as input to function if already calculated outside
    # TODO update function to take `hist` object as well 

    UnfishedSurvival <- purrr::map(OM@Stock, CalcUnfishedSurvival, SP=TRUE)
    
    # unfished egg production per recruit
    EP0 <- purrr::map2(UnfishedSurvival, purrr::map(OM@Stock, GetFecundityAtAge),
                       MultiplyArrays)
    
    # TODO Calculate EP0 as a reference point and pass into function
    # TODO account for SexPars
    
    
    
    
    
    
    OM@Fleet[[1]][[1]]@FishingMortality@ApicalF
    OM@Fleet[[1]][[2]]@FishingMortality@ApicalF
    
    
    CalculateSPR_ <- function(apicalF, Stock, Fleet) {
      FishedSurvival <- CalcFishedSurvival(Stock, Fleet, apicalF)
      EPF <- FishedSurvival * Stock@Fecundity@MeanAtAge # fished egg production per recruit
    }
    
    
    for (i in seq_along(OM@Control$FVector)) {
      apicalF <- OM@Control$FVector[i]
      
      
      

     
      
      sum(EPF)/sum(EP0)
    }
    
    OM@Fleet$Female$SPN_1@FishingMortality@ApicalF
    OM@Fleet$Female$US_2@FishingMortality@ApicalF
    OM@Fleet$Female$US_2@FishingMortality
    
    
  
    # SP0  # Unfished Spawning Production per Recruit
    
    
    # SPR <- SPF/SP0 
    
 
    
    list(FVector=F_search,
         SPR=SPR)
    
  }
  
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


