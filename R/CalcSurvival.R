CalcSurvival <- function(M_at_Age, PlusGroup=TRUE, SpawnTimeFrac=NULL, F_at_Age=NULL) {
  Z_at_Age <- M_at_Age
  if (!is.null(F_at_Age))
    Z_at_Age <- AddArrays(M_at_Age, F_at_Age)
  
  dd <- dim(Z_at_Age)
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
  
  dimnames(surv) <- dimnames(Z_at_Age)
  surv 
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


# ---- UpdateApicalF -----

setGeneric('UpdateApicalF', function(x, apicalF)
  standardGeneric('UpdateApicalF')
)

setMethod('UpdateApicalF', c('array',  'ANY'), function(x, apicalF) {
  dd <- dim(x)
  MaxF <- apply(x, c(1,3), max)
  MaxF <- replicate(dd[2], MaxF) |> aperm(c(1,3,2))
  dimnames(MaxF) <- dimnames(x)
  DivideArrays(x, MaxF) * apicalF
  
})

setMethod('UpdateApicalF', c('FleetList',  'ANY'), function(x, apicalF) {

# normalize and set to apicalF by fleet fraction
  ApicalFbyFleet <- GetApicalF(FleetList) 
  
  if (methods::is(ApicalFbyFleet, 'array')) {
    stop()
    # single fleet
    ApicalF(object@FishingMortality)[] <- apicalF
    
    curr <- process_cpars(ApicalF(object@FishingMortality))
    ApicalF(object@FishingMortality) <- curr
    return(object)
  }
  
  if (all(purrr::map_lgl(ApicalFbyFleet, methods::is, 'array'))) {
    
    dd <- dim(ApicalFbyFleet[[1]])
    TimeSteps <- dimnames(ApicalFbyFleet[[1]])$`Time Step`
    
    ApicalFbyFleetArray <- array(unlist(ApicalFbyFleet),
                                 dim=c(dd[1], dd[2], length(ApicalFbyFleet))) |>
      AddDimNames(names=c('Sim', 'Time Step', 'Fleet'), TimeSteps=TimeSteps)
    
    Ftotal <- apply(ApicalFbyFleetArray, c(1,2), sum)
    ApicalFbyFleetArray <- ApicalFbyFleetArray/replicate(length(ApicalFbyFleet), Ftotal) * apicalF
    
    ApicalF <- lapply(seq(dim(ApicalFbyFleetArray)[3]), function(x) 
      abind::adrop(ApicalFbyFleetArray[ , , x, drop=FALSE],3))
    
    for (fl in seq_along(ApicalFbyFleet)) {
      ApicalF(FleetList[[fl]]@FishingMortality) <- ApicalF[[fl]]
    }
    return(FleetList)
  }
  
  purrr::map(FleetList, UpdateApicalF, apicalF=apicalF)
})



# ---- CalcFishedSurvival -----

setGeneric('CalcFishedSurvival', function(x, Fleet=NULL, apicalF=NULL)
  standardGeneric('CalcFishedSurvival')
)


setMethod('CalcFishedSurvival', c('stock', 'FleetList',  'ANY'), function(x, Fleet, apicalF) {
  

  
  FDead <- purrr::map(Fleet, CalcFatAge, return='FDead')
  
})


setMethod('CalcFishedSurvival', c('StockList', 'StockFleetList',  'ANY'), function(x, Fleet, apicalF) {
  


  
})




