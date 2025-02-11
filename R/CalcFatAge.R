# ---- CalcFatAge ----
setGeneric('CalcFatAge', function(x, TimeSteps=NULL, apicalF=NULL)
  standardGeneric('CalcFatAge')
)

setMethod('CalcFatAge', c('om', 'ANY'), function(x, TimeSteps=NULL, apicalF=NULL) {
  purrr::map(x, CalcFatAge, TimeSteps, apicalF)
})

setMethod('CalcFatAge', c('StockFleetList', 'ANY'),
          function(x, TimeSteps=NULL, apicalF=NULL) {
  out <- purrr::map(x, CalcFatAge, TimeSteps, apicalF)
  class(out) <- 'StockFleetList'
  out
})


setMethod('CalcFatAge', c('FleetList', 'ANY'), 
          function(x, TimeSteps=NULL, apicalF=NULL) {
  out <- purrr::map(x, CalcFatAge, TimeSteps, apicalF)
  class(out) <- 'FleetList'
  out
})

setMethod('CalcFatAge', c('fleet', 'ANY'),
          function(x, TimeSteps=NULL, apicalF=NULL) {
  
  # x = OM@Fleet[[1]][[1]]
  # TimeSteps <- TimeSteps(OM, 'Historical')
  
  if (is.null(apicalF)) { 
    Effort <- GetEffort(x, TimeSteps)
    q <- GetCatchability(x, TimeSteps)
    apicalF <- ArrayMultiply(Effort, q)
    # TODO SetApicalF(x, TimeSteps) <- apicalF
  }
  
  selectivity <-  GetSelectivityAtAge(x, TimeSteps)
  retention <- GetRetentionAtAge(x, TimeSteps)
  discardmortality <- GetDiscardMortalityAtAge(x, TimeSteps)
  DimSelectivity <- dim(selectivity)
  DimapicalF <- dim(apicalF)
  
  if (all(DimapicalF==1)) {
    # add sim, age, and time-step dimensions
    apicalF <- array(apicalF, dim=c(1,DimSelectivity[2], DimSelectivity[3])) |>
      AddDimNames(names=names(dimnames(selectivity)), TimeSteps =  dimnames(selectivity)$`Time Step`)
  } else {
    # add age dimension
    apicalF <- replicate(DimSelectivity[2], apicalF, simplify = 'array') |> 
      aperm(c(1,3,2))
    names(dimnames(apicalF))[2] <- 'Age'
    dimnames(apicalF)[[2]] <- 1:DimSelectivity[2]
  }
  
  FInteract <- ArrayMultiply(apicalF, selectivity)
  if (is.null(retention)) {
    FRetain <- retention <- FInteract
    retention[] <- 1
  } else {
    FRetain <- ArrayMultiply(FInteract, retention)   
  }
  
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  
  if (is.null(discardmortality)) {
    discardmortality <- FInteract
    discardmortality[] <- 0
    FDiscardDead <- discardmortality
  } else {
    FDiscardDead <- ArrayMultiply(FDiscardTotal, discardmortality)
  }
  
  FDead <- FRetain + FDiscardDead  
 
  DeadApicalF <- AddDimension(apply(FDead, c(1,3), max), 'Age') |> aperm(c(1,3,2)) 
  InteractDeadRatio <- ArrayDivide(apicalF, DeadApicalF)
  InteractDeadRatio[!is.finite(InteractDeadRatio)] <- 1
  if (!all(InteractDeadRatio==1)) {
    # Inflate ApicalF to account for discard mortality
    # FishingMortality@apicalF is the apicalF of Dead Fish;
    # Inside this function 'apicalF' is for 'Caught' or 'Interacted' fish,
    # which will be higher than 'apicalF' for dead fish if discard mortality is < 1.

    apicalF <- ArrayMultiply(apicalF, InteractDeadRatio)
    FInteract <- ArrayMultiply(apicalF, selectivity)
    FRetain <- ArrayMultiply(FInteract, retention)
    FDiscardTotal <- ArraySubtract(FInteract, FRetain)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, discardmortality)
    FDead <- FRetain + FDiscardDead 
  }
  
  x@FishingMortality@ApicalF <- apply(FDead, c(1,3), max)
  x@FishingMortality@DeadAtAge <- FDead
  x@FishingMortality@RetainAtAge <- FRetain
  x
  
})
# ---- CalcFTotal ----
setGeneric('CalcFTotal', function(x, TimeSteps=NULL)
  standardGeneric('CalcFTotal')
)

setMethod('CalcFTotal', c('om', 'ANY'), function(x, TimeSteps=NULL) {
  purrr::map(x, CalcFTotal, TimeSteps)
})

setMethod('CalcFTotal', c('StockFleetList', 'ANY'),
          function(x, TimeSteps=NULL) {
            out <- purrr::map(x, CalcFTotal, TimeSteps=TimeSteps)
            class(out) <- 'StockFleetList'
            out
          })


setMethod('CalcFTotal', c('FleetList', 'ANY'), 
          function(x, TimeSteps=NULL) {
            FFleet <- purrr::map(x, GetFatAgeArray, TimeSteps=TimeSteps)
            array <- array(unlist(FFleet), dim=c(dim(FFleet[[1]]), length(FFleet)))
            array <- apply(array, 1:3, sum)
            dd <- dim(array)
            dimnames(array) <- dimnames(FFleet[[1]])
            class(array) <- 'FTotal'
            array
          })

