# ---- CalcFatAge ----
setGeneric('CalcFatAge', function(x, TimeSteps=NULL)
  standardGeneric('CalcFatAge')
)

setMethod('CalcFatAge', c('om', 'ANY'), function(x, TimeSteps=NULL) {
  purrr::map(x, CalcFatAge, TimeSteps)
})

setMethod('CalcFatAge', c('StockFleetList', 'ANY'), function(x, TimeSteps=NULL) {
  purrr::map(x, CalcFatAge, TimeSteps)
})


setMethod('CalcFatAge', c('FleetList', 'ANY'), function(x, TimeSteps=NULL) {
  purrr::map(x, CalcFatAge, TimeSteps)
})

setMethod('CalcFatAge', c('fleet', 'ANY'), function(x, TimeSteps=NULL) {
  
  # x = OM@Fleet[[1]][[1]]
  # TimeSteps <- TimeSteps(OM, 'Historical')
  
  if (is.null(x@FishingMortality@ApicalF)) { 
    stop('need to calculate apical F from Effort and q')
  }
  
  apicalF <- GetApicalF(x, TimeSteps)
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
  
  SelectivityRetention <- ArrayMultiply(selectivity, retention)
  
  isDiscards <- FALSE
  if (!is.null(discardmortality)) {
    isDiscards <- TRUE
    # Inflate ApicalF to account for discard mortality
    # FishingMortality@apicalF is the apicalF of Dead Fish;
    # Inside this function 'apicalF' is for 'Caught' or 'Interacted' fish,
    # which will be higher than 'apicalF' for dead fish if discard mortality is < 1.
    
    SelectivityDiscardMort <- ArrayMultiply(selectivity, discardmortality)
    
    SelectivityDiscardMortRetention <- ArrayMultiply(SelectivityDiscardMort, 
                                                     retention)
    
    InflateApicalF <- ArraySubtract(
      ArrayAdd(SelectivityRetention, SelectivityDiscardMort),
      SelectivityDiscardMortRetention
    )
    InflateApicalF <- apply(InflateApicalF, c(1,3), max) 
    InflateApicalF <- AddDimension(InflateApicalF, 'Age') |> aperm(c(1,3,2))
    apicalF <- ArrayDivide(apicalF, InflateApicalF)
  }
  
  FCaught <- ArrayMultiply(apicalF, selectivity)
  
  FRetain <- ArrayMultiply(retention, FCaught)
  if (isDiscards) {
    FDiscard <- ArraySubtract(FCaught, FRetain)
  } else {
    FDiscard <- array(0, dim=dim(FCaught))
    dimnames(FDiscard) <- dimnames(FCaught)
  }
  
  if (!isDiscards) {
    FDead <- FRetain   
  } else {
    FDead <- ArrayAdd(array1=FRetain, 
                       array2=ArrayMultiply(FDiscard, SelectivityDiscardMort))
  }
  
  x@FishingMortality@DeadAtAge <- FDead
  x@FishingMortality@RetainAtAge <- FRetain
  x
  
})


