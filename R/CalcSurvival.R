CalcSurvival <- function(M_at_Age, F_at_Age=NULL,
                         PlusGroup=TRUE, SpawnTimeFrac=NULL, 
                         Semelparous=FALSE) {
  Z_at_Age <- M_at_Age
  if (!is.null(F_at_Age))
    Z_at_Age <- ArrayAdd(M_at_Age, F_at_Age)
  
  dd <- dim(Z_at_Age)
  nSim <- dd[1]
  nAge <- dd[2]
  nTS <- dd[3]
  
  surv <- array(NA, dim=c(nSim, nAge, nTS))
  dimnames(surv) <- dimnames(Z_at_Age)
    
  if (is.null(SpawnTimeFrac)) {
    SpawnTimeFrac <- 0
  }
  SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[1:nSim]
  
  if (inherits(Semelparous, 'logical')) {
    Semelparous <- M_at_Age[,,1]
    Semelparous[] <- 0
  }
  
  for (a in 1:nAge) {
    ZthisAge <- abind::adrop(Z_at_Age[,a,, drop=FALSE], 2)
    if (a==1) {
      surv[,a,] <- exp(-ZthisAge*SpawnTimeFrac)
    } else {
      ZlastAge <- abind::adrop(Z_at_Age[,a-1,, drop=FALSE], 2)
      PostSpawnMortalityLastAge <- abind::adrop(Semelparous[,a-1,, drop=FALSE], 2)

      # surv[,a,] <- surv[,a-1,]*exp(-(Z_at_Age[,a-1,]*(1-SpawnTimeFrac)+Z_at_Age[,a,]*SpawnTimeFrac))
      
      surv[,a,] <- ArrayMultiply(abind::adrop(surv[,a-1,, drop=FALSE],2),
                                 exp(-(ZlastAge*(1-SpawnTimeFrac)+ZthisAge*SpawnTimeFrac)))|>
        ArrayMultiply(1-PostSpawnMortalityLastAge)
    }
  }
  if (PlusGroup)
    surv[,nAge,] <- surv[,nAge,]/(1-exp(-Z_at_Age[,nAge,]))
  
  dimnames(surv) <- dimnames(Z_at_Age)
  surv 
}


# ---- CalcUnfishedSurvival -----

setGeneric('CalcUnfishedSurvival', function(x, SP=FALSE, TimeSteps=NULL)
  standardGeneric('CalcUnfishedSurvival')
)

setMethod('CalcUnfishedSurvival', c('stock', 'ANY', 'ANY'), function(x, SP=FALSE, TimeSteps=NULL) {
  if (SP) {
    SpawnTimeFrac <- x@SRR@SpawnTimeFrac  
  } else {
    SpawnTimeFrac <- NULL
  }
 
  CalcSurvival(M_at_Age=GetNaturalMortalityAtAge(x, TimeSteps),
               PlusGroup=GetPlusGroup(x),
               SpawnTimeFrac=SpawnTimeFrac,
               Semelparous=GetSemelparous(x, TimeSteps))
})

setMethod('CalcUnfishedSurvival', c('StockList', 'ANY'), function(x, SP=FALSE, TimeSteps=NULL) {
  purrr::map(x, CalcUnfishedSurvival, SP=SP, TimeSteps=TimeSteps)
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
  # updates the F-at-age for each fleet such that overall apical F = `apicalF`
  if (length(x)==1) {
    x[[1]]@FishingMortality@ApicalF <- array(apicalF, dim=c(1,1))
    dimnames(x[[1]]@FishingMortality@ApicalF) <- list(Sim=1,
                                                      `Time Step`=TimeSteps[1]) 
    return(x)
  }
  
  x <- CalcFatAge(x, TimeSteps)
  FDeadbyFleet <- GetFatAgeArray(x, TimeSteps)

  FDeadTotal <- apply(FDeadbyFleet, 1:3, sum)
  apicalFCurr <- apply(FDeadTotal, c(1,3), max)
  
  adjust <- apicalF/apicalFCurr
  adjust <- adjust |> 
    AddDimension('Age') |> 
    AddDimension('Fleet') |>
    aperm(c(1,3,2,4))
  
  FDeadbyFleet <- ArrayMultiply(FDeadbyFleet,adjust)
  
  newApicalF <- apply(FDeadbyFleet, c(1,3,4), max)

  for (fl in seq_along(x)) {
    ArrayFill(x[[fl]]@FishingMortality@ApicalF) <- abind::adrop(newApicalF[,,fl, drop=FALSE],3)
  }
  x
})



# ---- CalcFishedSurvival -----

setGeneric('CalcFishedSurvival', function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL)
  standardGeneric('CalcFishedSurvival')
)


setMethod('CalcFishedSurvival', c('stock', 'FleetList',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL) {
            FDead <- GetFatAgeArray(Fleet, TimeSteps)
            dd <- dim(FDead)
            if (dd[4]==1) {
              FDeadOverTotal <- abind::adrop(FDead[,,,1, drop=FALSE], 4)
            } else {
              # sum over fleets
              FDeadOverTotal <- apply(FDead, c(1,2,3), sum)   
            }
            
            if (SP) {
              SpawnTimeFrac <- x@SRR@SpawnTimeFrac  
            } else {
              SpawnTimeFrac <- NULL
            }
            
            CalcSurvival(M_at_Age=GetNaturalMortalityAtAge(x, TimeSteps),
                         F_at_Age=FDeadOverTotal,
                         PlusGroup=GetPlusGroup(x),
                         SpawnTimeFrac=SpawnTimeFrac,
                         Semelparous=GetSemelparous(x, TimeSteps))
          })


# setMethod('CalcFishedSurvival', c('StockList', 'StockFleetList',  'ANY'),
#           function(x, Fleet=NULL, SP=FALSE) {
#             purrr::map2(x, Fleet, CalcFishedSurvival, SP=SP)
# })

setMethod('CalcFishedSurvival', c('om', 'ANY',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL) {
            purrr::map2(x@Stock, x@Fleet, CalcFishedSurvival, SP=SP, TimeSteps=TimeSteps)
          })


