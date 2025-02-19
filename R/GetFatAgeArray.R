setGeneric('GetFatAgeArray', function(x, TimeSteps=NULL, type='Dead')
  standardGeneric('GetFatAgeArray')
)

setMethod('GetFatAgeArray', c('StockFleetList', 'ANY', 'ANY'), 
          function(x, TimeSteps=NULL, type=c('Dead', 'Retain')) {
            type <- match.arg(type)
            purrr::map(x, GetFatAgeArray, type=type, TimeSteps=TimeSteps)
          })

setMethod('GetFatAgeArray', c('FleetList', 'ANY', 'ANY'), 
          function(x, TimeSteps=NULL, type=c('Dead', 'Retain')) {
  type <- match.arg(type)
  List <- purrr::map(x, GetFatAgeArray, type=type, TimeSteps=TimeSteps)
  
  checkdims <- purrr::map_df(List, dim)
  
  Array <- array(as.numeric(unlist(List)), dim=c(dim(List[[1]]), length(List)))
  l <- dimnames(List[[1]])
  l$Fleet <- names(List)
  dimnames(Array) <- l
  Array
  
})

setMethod('GetFatAgeArray', c('fleet', 'ANY', 'ANY'), 
          function(x, TimeSteps=NULL, type=c('Dead', 'Retain')) {
  type <- match.arg(type)
  if (type=='Dead') {
    array <- x@FishingMortality@DeadAtAge
  } else {
    array <- x@FishingMortality@RetainAtAge  
  }
  if (is.null(array)) {
    ZeroArray <- array(tiny, dim=c(1,1,1), 
                       dimnames=list(Sim=1,
                                     Age=0,
                                     `Time Step`=TimeSteps))
    return(ZeroArray)
  }
    
  
  ArraySubsetTimeStep(array, TimeSteps)
})
