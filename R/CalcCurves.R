
# devtools::load_all()




CalcSPR0 <- function(OM) {
  SPR0 <- purrr::map2(CalcUnfishedSurvival(OM@Stock, SP=TRUE), 
                      GetFecundityAtAge(OM@Stock),
                      MultiplyArrays)
  
  SPR0 <- purrr::map(SPR0, function(x)
    apply(x, c(1,3), sum) |> process_cpars()
  )
  
  # Map to Stock number
  if (!is.null(OM@SexPars@SPFrom)) {
    for (i in 1:nrow(OM@SexPars@SPFrom)) {
      ind <- which(OM@SexPars@SPFrom[i,]==1)
      if (i !=ind) {
        SPR0[[i]] <- ind
      }
    }
  }
  SPR0
}


CalcSPRCurve <- function(OM, 
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
  
  # TODO Calculate SPR0 as a reference point and pass into function

  SPR0 <- CalcSPR0(OM) # unfished spawning production per recruit

  
  Stock <- OM@Stock[[1]]
  FleetList <- OM@Fleet[[1]]
  
  apicalF <- 0.05
  
  CalcFishedSurvival <- function(Stock, FleetList, apicalF=NULL, SP=FALSE) {
    
    # TODO - include spatial closure and distribution for MSY and other ref points?
    # TODO does this need to be for every year?
    
   

    FDead <- purrr::map(FleetList, CalcFatAge, return='FDead')
    
    
    # Apical F is by fleet, not over all fleets
    
    FDead <- array(unlist(FDead), dim=c(dim(FDead[[1]])[1], 
                                        dim(FDead[[1]])[2], 
                                        dim(FDead[[1]])[3], 
                                        length(FDead))) |>
      AddDimNames(names=c('Sim', 'Age', 'Time Step', 'Fleet'), TimeSteps=TimeSteps(Stock, 'Historical'))
    
    FDeadOverTotal <- apply(FDead, c(1,2,3), sum) 
    
    if (!is.null(apicalF)) 
      FDeadOverTotal <- UpdateApicalF(FDeadOverTotal, apicalF)
    
    
    M_at_Age <- Stock@NaturalMortality@MeanAtAge
    PlusGroup <- Stock@Ages@PlusGroup
    
    if (SP) {
      SpawnTimeFrac <- Stock@SRR@SpawnTimeFrac  
    } else {
      SpawnTimeFrac <- NULL
    }
    
    ### UP TO HERE ####
    # Convert this to the generic in CalcSurvival.R
    CalcSurvival(M_at_Age, PlusGroup=PlusGroup, SpawnTimeFrac=SpawnTimeFrac, F_at_Age=FDeadOverTotal)
  }
  
  
  
  
  CalculateSPR_ <- function(apicalF, Stock, Fleet) {
    FishedSurvival <- CalcFishedSurvival(Stock, FleetList, apicalF)
    
    # fished egg production per recruit
    SPRF <- MultiplyArrays(array1=FishedSurvival, array2=Stock@Fecundity@MeanAtAge) |>
      apply(c(1,3), sum) |> process_cpars()
    
    ## over stocks --
    DivideArrays(SPR0, SPRF)
    
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