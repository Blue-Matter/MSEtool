CalcSurvival <- function(M_at_Age, PlusGroup=TRUE, SpawnTimeFrac=NULL, F_at_Age=NULL) {
  Z_at_Age <- M_at_Age
  if (!is.null(F_at_Age))
    Z_at_Age <- ArrayAdd(M_at_Age, F_at_Age)
  
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

setGeneric('UpdateApicalF', function(x, apicalF, TimeSteps=NULL)
  standardGeneric('UpdateApicalF')
)

setMethod('UpdateApicalF', c('StockFleetList',  'ANY', 'ANY'),
          function(x, apicalF, TimeSteps=NULL) {
            purrr::map(x, UpdateApicalF, apicalF=apicalF, TimeSteps=TimeSteps)
          })

setMethod('UpdateApicalF', c('array',  'ANY', 'ANY'), 
          function(x, apicalF, TimeSteps=NULL) {
            dd <- dim(x)
            MaxF <- apply(x, c(1,3), max)
            MaxF <- replicate(dd[2], MaxF) |> aperm(c(1,3,2))
            dimnames(MaxF) <- dimnames(x)
            ArrayDivide(x, MaxF) * apicalF
          })

setMethod('UpdateApicalF', c('FleetList', 'ANY', 'ANY'), function(x, apicalF,  TimeSteps=NULL) {
  if (length(x)==1) {
    x[[1]]@FishingMortality@ApicalF <- array(apicalF, dim=c(1,1))
    dimnames(x[[1]]@FishingMortality@ApicalF) <- list(Sim=1,
                                                      `Time Step`=1) # TODO 
    return(x)
  }
  
  x <- CalcFatAge(x, TimeSteps)
  FDeadbyFleet <- GetFatAgeArray(x, TimeSteps)

  FDeadTotal <- apply(FDeadbyFleet, 1:3, sum)
  apicalFCurr <- apply(FDeadTotal, c(1,3), max)
  adjust <- apicalF/apicalFCurr

  dd <- dim(FDeadbyFleet)
  adjust <- replicate(dd[2], adjust)
  adjust <- replicate(dd[4], adjust) 
  adjust <- aperm(adjust, c(1,3,2,4))
  FDeadbyFleet <- FDeadbyFleet*adjust
  newApicalF <- apply(FDeadbyFleet, c(1,3,4), max)

  for (fl in seq_along(x)) {
    x[[fl]]@FishingMortality@ApicalF <- abind::adrop(newApicalF[,,fl, drop=FALSE],3)
  }
  x
})



# ---- CalcFishedSurvival -----

setGeneric('CalcFishedSurvival', function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL)
  standardGeneric('CalcFishedSurvival')
)


setMethod('CalcFishedSurvival', c('stock', 'FleetList',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL) {
            
            Fleet <- CalcFatAge(Fleet, TimeSteps)
            FDead <- GetFatAgeArray(Fleet, TimeSteps)
            FDeadOverTotal <- apply(FDead, c(1,2,3), sum) 

            if (SP) {
              SpawnTimeFrac <- x@SRR@SpawnTimeFrac  
            } else {
              SpawnTimeFrac <- NULL
            }
            
            CalcSurvival(GetNMortalityAtAge(x, TimeSteps),
                         x@Ages@PlusGroup,
                         SpawnTimeFrac, 
                         FDeadOverTotal)
          })


# setMethod('CalcFishedSurvival', c('StockList', 'StockFleetList',  'ANY'),
#           function(x, Fleet=NULL, SP=FALSE) {
#             purrr::map2(x, Fleet, CalcFishedSurvival, SP=SP)
# })

setMethod('CalcFishedSurvival', c('om', 'ANY',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL) {
            purrr::map2(x@Stock, x@Fleet, CalcFishedSurvival, SP=SP, TimeSteps=TimeSteps)
          })


