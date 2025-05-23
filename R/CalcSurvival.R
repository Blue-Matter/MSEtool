CalcSurvival <- function(NaturalMortalityAtAge, # nAge, nTS
                         FishingMortalityAtAge=NULL, # nAge, nTS
                         PlusGroup=TRUE, # logical
                         SpawnTimeFrac=NULL, # double  
                         Semelparous=FALSE) { # FALSE or array nAge, nTS
  
  
  TotalMortalityAtAge <- NaturalMortalityAtAge
  if (!is.null(FishingMortalityAtAge))
    TotalMortalityAtAge <- ArrayAdd(NaturalMortalityAtAge, FishingMortalityAtAge)
  
  nAge <- nrow(TotalMortalityAtAge)
  nTS <- ncol(TotalMortalityAtAge)
  
  survival <- array(NA, dim=c(nAge, nTS))
  dimnames(survival) <- dimnames(TotalMortalityAtAge)
    
  if (is.null(SpawnTimeFrac)) {
    SpawnTimeFrac <- 0
  }
  
  if (inherits(Semelparous, 'logical')) {
    Semelparous <- NaturalMortalityAtAge[,1]
    Semelparous[] <- 0
  }
  
  for (a in 1:nAge) {
    ZthisAge <- TotalMortalityAtAge[a,]
    if (a==1) {
      survival[a,] <- exp(-ZthisAge*SpawnTimeFrac)
    } else {
      ZlastAge <- TotalMortalityAtAge[a-1,]
      PostSpawnMortalityLastAge <- Semelparous[a-1,]
      survival[a,] <- survival[a-1,]*
        exp(-(ZlastAge*(1-SpawnTimeFrac)+ZthisAge*SpawnTimeFrac)) *
        (1-PostSpawnMortalityLastAge)
    }
  }
  if (PlusGroup)
    survival[nAge,] <- survival[nAge,]/(1-exp(-TotalMortalityAtAge[nAge,]))
  
  dimnames(survival) <- dimnames(TotalMortalityAtAge)
  survival 
}


# # ---- CalcUnfishedSurvival -----

setGeneric('CalcUnfishedSurvival', function(x, SP=FALSE, TimeSteps=NULL)
  standardGeneric('CalcUnfishedSurvival')
)

setMethod('CalcUnfishedSurvival', c('stock', 'ANY', 'ANY'), function(x, SP=FALSE, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- x@TimeSteps
  
  nAges <- x@Ages@Classes |> length()
  NaturalMortalityAtAge <- x@NaturalMortality@MeanAtAge |>
    ArrayExpand(x@nSim, nAges, TimeSteps)
  
  PlusGroup <- x@Ages@PlusGroup
  if (SP) {
    SpawnTimeFrac <- x@SRR@SpawnTimeFrac
  } else {
    SpawnTimeFrac <- NULL
  }
  
  Semelparous <- x@Maturity@Semelparous |>
    ArrayExpand(x@nSim, nAges, TimeSteps)
  
  IsIdenticalTime <- IdenticalTime(NaturalMortalityAtAge) & IdenticalTime(Semelparous)
  IsIdenticalSim <- IdenticalSim(NaturalMortalityAtAge) & IdenticalSim(Semelparous)
  
  if (IsIdenticalSim & IsIdenticalTime) {
    NaturalMortalityAtAge <- abind::adrop(NaturalMortalityAtAge[1,,1, drop=FALSE], 1)
    Semelparous <- abind::adrop(Semelparous[1,,1, drop=FALSE], 1)
    Survival <- CalcSurvival(NaturalMortalityAtAge, 
                             PlusGroup=PlusGroup, 
                             SpawnTimeFrac=SpawnTimeFrac, 
                             Semelparous=Semelparous
    )
    Survival <- replicate(1, Survival) |> 
      AddDimNames(c('Age', 'TimeStep', 'Sim'), TimeSteps) |> 
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  } else if (IsIdenticalSim & !IsIdenticalTime) {
    NaturalMortalityAtAge <- abind::adrop(NaturalMortalityAtAge[1,,, drop=FALSE], 1)
    Semelparous <- abind::adrop(Semelparous[1,,, drop=FALSE], 1)
    Survival <- CalcSurvival(NaturalMortalityAtAge, 
                             PlusGroup=PlusGroup, 
                             SpawnTimeFrac=SpawnTimeFrac, 
                             Semelparous=Semelparous
    )
    Survival <- replicate(1, Survival) |> 
      AddDimNames(c('Age', 'TimeStep', 'Sim'), TimeSteps) |> 
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  } else if (!IsIdenticalSim & IsIdenticalTime) {
    NaturalMortalityAtAgeList <- Array2List(NaturalMortalityAtAge[,,1, drop=FALSE], 1)
    SemelparousList <- Array2List(Semelparous[1,,1, drop=FALSE], 1)
    Survival <- purrr::pmap(list(
      NaturalMortalityAtAge=NaturalMortalityAtAgeList,
      PlusGroup=PlusGroup, 
      SpawnTimeFrac=SpawnTimeFrac, 
      Semelparous=SemelparousList),
      CalcSurvival
    ) |> List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  } else {
    NaturalMortalityAtAgeList <- Array2List(NaturalMortalityAtAge, 1)
    SemelparousList <- Array2List(Semelparous, 1)
    Survival <- purrr::pmap(list(
      NaturalMortalityAtAge=NaturalMortalityAtAgeList,
      PlusGroup=PlusGroup, 
      Semelparous=SemelparousList),
      CalcSurvival, SpawnTimeFrac=SpawnTimeFrac) |> 
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  }
  Survival
})

setMethod('CalcUnfishedSurvival', c('StockList', 'ANY'), function(x, SP=FALSE, TimeSteps=NULL) {
  purrr::map(x, CalcUnfishedSurvival, SP=SP, TimeSteps=TimeSteps)
})


setMethod('CalcUnfishedSurvival', c('om', 'ANY'), function(x, SP=FALSE, TimeSteps=NULL) {
  x <- Populate(x)
  if (is.null(TimeSteps))
    TimeSteps <- x@TimeSteps
  CalcUnfishedSurvival(x@Stock, SP, TimeSteps)
})
# 
# 
# 
# # ---- UpdateApicalF -----
# 
# setGeneric('UpdateApicalF', function(x, apicalF, TimeSteps=NULL)
#   standardGeneric('UpdateApicalF')
# )
# 
# setMethod('UpdateApicalF', c('StockFleetList',  'ANY', 'ANY'),
#           function(x, apicalF, TimeSteps=NULL) {
#             purrr::map(x, UpdateApicalF, apicalF=apicalF, TimeSteps=TimeSteps)
#           })
# 
# setMethod('UpdateApicalF', c('array',  'ANY', 'ANY'), 
#           function(x, apicalF, TimeSteps=NULL) {
#             dd <- dim(x)
#             MaxF <- apply(x, c(1,3), max)
#             MaxF <- replicate(dd[2], MaxF) |> aperm(c(1,3,2))
#             dimnames(MaxF) <- dimnames(x)
#             ArrayDivide(x, MaxF) * apicalF
#           })
# 
# setMethod('UpdateApicalF', c('FleetList', 'ANY', 'ANY'), function(x, apicalF,  TimeSteps=NULL) {
#   # updates the F-at-age for each fleet such that overall apical F = `apicalF`
#   if (length(x)==1) {
#     x[[1]]@FishingMortality@ApicalF <- array(apicalF, dim=c(1,1))
#     dimnames(x[[1]]@FishingMortality@ApicalF) <- list(Sim=1,
#                                                       TimeStep=TimeSteps[1]) 
#     return(x)
#   }
#   
#   x <- CalcFatAge(x, TimeSteps)
#   FDeadbyFleet <- GetFatAgeArray(x, TimeSteps)
# 
#   FDeadTotal <- apply(FDeadbyFleet, 1:3, sum)
#   apicalFCurr <- apply(FDeadTotal, c(1,3), max)
#   
#   adjust <- apicalF/apicalFCurr
#   adjust <- adjust |> 
#     AddDimension('Age') |> 
#     AddDimension('Fleet') |>
#     aperm(c(1,3,2,4))
#   
#   FDeadbyFleet <- ArrayMultiply(FDeadbyFleet,adjust)
#   
#   newApicalF <- apply(FDeadbyFleet, c(1,3,4), max)
# 
#   for (fl in seq_along(x)) {
#     ArrayFill(x[[fl]]@FishingMortality@ApicalF) <- abind::adrop(newApicalF[,,fl, drop=FALSE],3)
#   }
#   x
# })
# 
# 
# 
# # ---- CalcFishedSurvival -----
# 
# setGeneric('CalcFishedSurvival', function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL)
#   standardGeneric('CalcFishedSurvival')
# )
# 
# 
# setMethod('CalcFishedSurvival', c('stock', 'FleetList',  'ANY', 'ANY'), 
#           function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL) {
#             FDead <- GetFatAgeArray(Fleet, TimeSteps)
#             dd <- dim(FDead)
#             if (dd[4]==1) {
#               FDeadOverTotal <- abind::adrop(FDead[,,,1, drop=FALSE], 4)
#             } else {
#               # sum over fleets
#               FDeadOverTotal <- apply(FDead, c(1,2,3), sum)   
#             }
#             
#             if (SP) {
#               SpawnTimeFrac <- x@SRR@SpawnTimeFrac  
#             } else {
#               SpawnTimeFrac <- NULL
#             }
#             
#             CalcSurvival(M_at_Age=GetNaturalMortalityAtAge(x, TimeSteps),
#                          F_at_Age=FDeadOverTotal,
#                          PlusGroup=GetPlusGroup(x),
#                          SpawnTimeFrac=SpawnTimeFrac,
#                          Semelparous=GetSemelparous(x, TimeSteps))
#           })
# 
# 
# # setMethod('CalcFishedSurvival', c('StockList', 'StockFleetList',  'ANY'),
# #           function(x, Fleet=NULL, SP=FALSE) {
# #             purrr::map2(x, Fleet, CalcFishedSurvival, SP=SP)
# # })
# 
# setMethod('CalcFishedSurvival', c('om', 'ANY',  'ANY', 'ANY'), 
#           function(x, Fleet=NULL, SP=FALSE, TimeSteps=NULL) {
#             purrr::map2(x@Stock, x@Fleet, CalcFishedSurvival, SP=SP, TimeSteps=TimeSteps)
#           })
# 
# 
