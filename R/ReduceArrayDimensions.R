ReduceArraysTS_ <- function(Array, TimeSteps) {
  dd <- dim(Array)
  
  # reduces the time-step dimension to unique values
  # adds dimnames and attributes 

  # for 3D arrays: Sim, Age, Time Step
  if (length(dd)==3) {
    List <- lapply(seq(dim(Array)[3]), function(x) Array[ , , x, drop=FALSE])
    KeepInd <- duplicated(List) |> not() |> which()
    List <- List[KeepInd]
    
    Array <- do.call(abind::abind, List)
    attributes(Array)$TimeSteps <- TimeSteps[KeepInd]
    Array <- AddDimNames(Array, TimeSteps=TimeSteps[KeepInd])
  }
  
  Array
}


# reduce arrays 
setGeneric('ReduceArraysTS', function(x, TimeSteps=NULL)
  standardGeneric('ReduceArraysTS')
)

setMethod('ReduceArraysTS', c('om', 'ANY'), function(x, TimeSteps=NULL) {
  timesteps <- TimeSteps(x)
  # stock
  x@Stock <- x@Stock |> 
    purrr::map(\(x) ReduceArraysTS(x, TimeSteps=timesteps))
  
  # fleet
  x@Fleet <- x@Fleet |> 
    purrr::map(\(x) ReduceArraysTS(x, TimeSteps=timesteps))
  
  # obs  & imp
  x
})

setMethod('ReduceArraysTS', c('StockList'), function(x, TimeSteps) {
  x <- x |> 
    purrr::map(\(x) ReduceArraysTS(x, TimeSteps=TimeSteps))
  x
})

setMethod('ReduceArraysTS', c('stock'), function(x, TimeSteps) {
  SetLengthAtAge(x) <- ReduceArraysTS_(GetLengthAtAge(x), TimeSteps)
  SetWeightAtAge(x) <- ReduceArraysTS_(GetWeightAtAge(x), TimeSteps)
  SetMaturityAtAge(x) <- ReduceArraysTS_(GetMaturityAtAge(x), TimeSteps)
  SetFecundityAtAge(x) <- ReduceArraysTS_(GetFecundityAtAge(x), TimeSteps)
  x
})

setMethod('ReduceArraysTS', c('StockFleetList'), function(x, TimeSteps) {
  x <- x |> 
    purrr::map(\(x) ReduceArraysTS(x, TimeSteps=TimeSteps))
  x
})

setMethod('ReduceArraysTS', c('FleetList'), function(x, TimeSteps) {
  x <- x |> 
    purrr::map(\(x) ReduceArraysTS(x, TimeSteps=TimeSteps))
  x
})

setMethod('ReduceArraysTS', c('fleet'), function(x, TimeSteps) {
  SetDiscardMortalityAtAge(x) <- ReduceArraysTS_(GetDiscardMortalityAtAge(x), TimeSteps)
  SetSelectivityAtAge(x) <- ReduceArraysTS_(GetSelectivityAtAge(x), TimeSteps)
  SetRetentionAtAge(x) <- ReduceArraysTS_(GetRetentionAtAge(x), TimeSteps)
  SetFleetWeightAtAge(x) <- ReduceArraysTS_(GetFleetWeightAtAge(x), TimeSteps)
  x
})


