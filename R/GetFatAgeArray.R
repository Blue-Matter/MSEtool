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
