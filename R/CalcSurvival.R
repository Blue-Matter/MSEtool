CalcSurvival <- function(M_at_Age, PlusGroup=TRUE, SpawnTimeFrac=NULL, F_at_Age=NULL) {
  Z_at_Age <- M_at_Age
  if (!is.null(F_at_Age))
    Z_at_Age <- M_at_Age + F_at_Age
  
  dd <- dim(M_at_Age)
  nSim <- dd[1]
  nAge <- dd[2]
  nTS <- dd[3]
  
  surv <- array(1, dim=c(nSim, nAge, nTS))
  if (is.null(SpawnTimeFrac)) {
    SpawnTimeFrac <- 0
  }
  SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[1:nSim]
  
  surv[,1,] <- exp(-M_at_Age[,1,]*SpawnTimeFrac)
  for (a in 2:nAge) {
    surv[,a,] <- surv[,a-1,]*exp(-(Z_at_Age[,a-1,]*(1-SpawnTimeFrac)+Z_at_Age[,a,]*SpawnTimeFrac))
  }
  if (PlusGroup)
    surv[,nAge,] <- surv[,nAge,]/(1-exp(-Z_at_Age[,nAge,]))
  
  surv |> AddDimNames(TimeSteps=attributes(M_at_Age)$TimeSteps)
}


# ---- CalcUnfishedSurvival -----

setGeneric('CalcUnfishedSurvival', function(x, SP=FALSE)
  standardGeneric('CalcUnfishedSurvival')
)

setMethod('CalcUnfishedSurvival', c('stock', 'ANY'), function(x, SP=FALSE) {
  if (SP) {
    SpawnTimeFrac <- x@SRR@SpawnTimeFrac  
  } else {
    SpawnTimeFrac <- NULL
  }
  
  CalcSurvival(M_at_Age=x@NaturalMortality@MeanAtAge,
               PlusGroup=x@Ages@PlusGroup,
               SpawnTimeFrac)
})

setMethod('CalcUnfishedSurvival', c('StockList', 'ANY'), function(x, SP=FALSE) {
  purrr::map(x, CalcUnfishedSurvival, SP=SP)
})


setMethod('CalcUnfishedSurvival', c('om', 'ANY'), function(x, SP=FALSE) {
  CalcUnfishedSurvival(x@Stock, SP)
})


# ---- CalcFishedSurvival -----

# normalize and set to apicalF by fleet fraction

UpdateApicalF <- function(object, apicalF=0.1) {
  
  if (methods::is(object, 'om')) 
    object <- object@Fleet
  
  ApicalFbyFleet <- GetApicalF(object) 
  if (methods::is(ApicalFbyFleet, 'array')) {
    # single fleet
    ApicalF(object@FishingMortality)[] <- apicalF
            
    curr <- process_cpars(ApicalF(object@FishingMortality))
    ApicalF(object@FishingMortality) <- curr
    return(object)
  }
  
  if (all(purrr::map_lgl(ApicalFbyFleet, methods::is, 'array'))) {
    
    dd <- dim(ApicalFbyFleet[[1]])
    
    ApicalFbyFleetArray <- array(unlist(ApicalFbyFleet),
                                 dim=c(dd[1], dd[2], length(ApicalFbyFleet))) |>
      AddDimNames(names=c('Sim', 'Time Step', 'Fleet'))
    
    Ftotal <- apply(ApicalFbyFleetArray, c(1,2), sum)
    ApicalFbyFleetArray <- ApicalFbyFleetArray/replicate(length(ApicalFbyFleet), Ftotal) * apicalF
    
    ApicalF <- lapply(seq(dim(ApicalFbyFleetArray)[3]), function(x) 
      abind::adrop(ApicalFbyFleetArray[ , , x, drop=FALSE],3))
    
    for (fl in seq_along(ApicalFbyFleet)) {
      ApicalF(object[[fl]]@FishingMortality) <- ApicalF[[fl]]
    }
    return(object)
  }
  purrr::map(object, UpdateApicalF, apicalF=apicalF)
}


setGeneric('CalcFishedSurvival', function(x, Fleet=NULL, apicalF=NULL)
  standardGeneric('CalcFishedSurvival')
)


setMethod('CalcFishedSurvival', c('stock', 'FleetList',  'ANY'), function(x, Fleet, apicalF) {
  
  # TODO - include spatial closure and distribution?
  
  if (!is.null(apicalF)) 
    Fleet <- UpdateApicalF(Fleet, apicalF)
  
  FDead <- purrr::map(Fleet, CalcFatAge, return='FDead')
  
})


setMethod('CalcFishedSurvival', c('StockList', 'StockFleetList',  'ANY'), function(x, Fleet, apicalF) {
  
  # TODO - include spatial closure and distribution?
  
  if (!is.null(apicalF)) 
    Fleet <- UpdateApicalF(Fleet, apicalF)
  
  FDead <- purrr::map(Fleet, CalcFatAge, return='FDead')
  
})



CalcFishedSurvival <- function(Stock, Fleet, apicalF=NULL) {
  



  
  
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
