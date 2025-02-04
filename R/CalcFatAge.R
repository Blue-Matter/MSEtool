# ---- CalcFatAge ----
setGeneric('CalcFatAge', function(x)
  standardGeneric('CalcFatAge')
)

setMethod('CalcFatAge', 'om', function(x) {
  purrr::map(x, CalcFatAge)
})

setMethod('CalcFatAge', 'StockFleetList', function(x) {
  purrr::map(x, CalcFatAge)
})


setMethod('CalcFatAge', 'FleetList', function(x) {
  purrr::map(x, CalcFatAge)
})

setMethod('CalcFatAge', 'fleet', function(x) {

  if (is.null(x@FishingMortality@ApicalF)) { 
    stop('need to calculate apical F from Effort and q')
  }
  
  apicalF <- x@FishingMortality@ApicalF
  selectivity <-  x@Selectivity@MeanAtAge
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
  
  SelectivityRetention <- ArrayMultiply(x@Selectivity@MeanAtAge, 
                                         x@Retention@MeanAtAge)
  
  isDiscards <- FALSE
  if (!is.null(x@DiscardMortality@MeanAtAge)) {
    isDiscards <- TRUE
    # Inflate ApicalF to account for discard mortality
    # FishingMortality@apicalF is the apicalF of Dead Fish;
    # Inside this function 'apicalF' is for 'Caught' or 'Interacted' fish,
    # which will be higher than 'apicalF' for dead fish if discard mortality is < 1.
    
    SelectivityDiscardMort <- ArrayMultiply(x@Selectivity@MeanAtAge, 
                                             x@DiscardMortality@MeanAtAge)
    
    SelectivityDiscardMortRetention <- ArrayMultiply(SelectivityDiscardMort, 
                                                      x@Retention@MeanAtAge)
    
    InflateApicalF <- ArraySubtract(
      ArrayAdd(SelectivityRetention, SelectivityDiscardMort),
      SelectivityDiscardMortRetention
    )
    InflateApicalF <- apply(InflateApicalF, c(1,3), max) 
    InflateApicalF <- AddDimension(InflateApicalF, 'Age') |> aperm(c(1,3,2))
    apicalF <- ArrayDivide(apicalF, InflateApicalF)
  }
  
  FCaught <- ArrayMultiply(apicalF, x@Selectivity@MeanAtAge)
  
  FRetain <- ArrayMultiply(x@Retention@MeanAtAge, FCaught)
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
  
  
  # apicalF[1,1,71]
  # x@FishingMortality@ApicalF[1,71]
  # max(FDead[1,,71])
  
  
  x@FishingMortality@DeadAtAge <- FDead
  x@FishingMortality@RetainAtAge <- FRetain
  x
  
})


# ---- GetFatAgeArray ----
setGeneric('GetFatAgeArray', function(x, type='Dead')
  standardGeneric('GetFatAgeArray')
)

setMethod('GetFatAgeArray', c('FleetList', 'ANY'), function(x, type=c('Dead', 'Retain')) {
  type <- match.arg(type)
  List <- purrr::map(x, GetFatAgeArray, type=type)

  checkdims <- purrr::map_df(List, dim)

  Array <- array(as.numeric(unlist(List)), dim=c(dim(List[[1]]), length(List)))
  l <- dimnames(List[[1]])
  l$Fleet <- names(List)
  dimnames(Array) <- l
  Array

})

setMethod('GetFatAgeArray', c('fleet', 'ANY'), function(x, type=c('Dead', 'Retain')) {
  type <- match.arg(type)
  if (type=='Dead')
    return(x@FishingMortality@DeadAtAge)
  x@FishingMortality@RetainAtAge
})




