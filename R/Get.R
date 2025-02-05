ArraySubsetTimeStep <- function(object, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    return(object)
  
  DN <- dimnames(object)
  TSind <- which(names(DN) == 'Time Step')
  if (length(TSind)==0)
    cli::cli_abort("`Time Step` dimension not found in this array", .internal=TRUE)
  abind::asub(object, (DN[[TSind]] %in% TimeSteps), TSind, drop=FALSE)
}

Get <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  
  cl <- class(object)
  for (i in seq_along(slots)) {
    object <- slot(object, slots[i])
  }
  if (is.null(object))
    return(object)
  object <- ArraySubsetTimeStep(object, TimeSteps)
  
  if (df) 
    object <- as.data.frame.table(object, stringsAsFactors =FALSE,
                                  responseName =slots[length(slots)]) |>
    dplyr::mutate_if(is.character, as.numeric)
  
  
  object
}

# Stock -----
GetStockAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Stock
  
  if (isS4(object))
    return(Get(object, slots, TimeSteps, df))
  
  if (methods::is(object, 'StockList')) 
    return(
      purrr::map(object, Get, slots=slots, TimeSteps=TimeSteps, df=df)
    )
  Get(object, slots, TimeSteps, df)
}


GetLengthAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  slots=c('Length', 'MeanAtAge'), TimeSteps, df)
}


`SetLengthAtAge<-` <- function(x, value) {
  x@Length@MeanAtAge <- value
  x
}


GetWeightAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Weight', 'MeanAtAge'), TimeSteps, df)
}

`SetWeightAtAge<-` <- function(x, value) {
  x@Weight@MeanAtAge <- value
  x
}

GetNMortalityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('NaturalMortality', 'MeanAtAge'), TimeSteps, df)
}

`SetGetNMortalityAtAge<-` <- function(x, value) {
  x@NaturalMortality@MeanAtAge <- value
  x
}

GetMaturityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Maturity', 'MeanAtAge'), TimeSteps, df)
}

`SetMaturityAtAge<-` <- function(x, value) {
  x@Maturity@MeanAtAge <- value
  x
}


GetFecundityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object, c('Fecundity', 'MeanAtAge'), TimeSteps, df)
}

`SetFecundityAtAge<-` <- function(x, value) {
  x@Fecundity@MeanAtAge <- value
  x
}


GetR0 <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'R0'), TimeSteps, df)
}

GetSPFrom <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'SPFrom'), TimeSteps, df)
}


GetSpawnTimeFrac <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'SpawnTimeFrac'), TimeSteps, df)
}

GetRecDevInit <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevInit'), TimeSteps, df)
}

GetRecDevHist <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevHist'), TimeSteps, df)
}


GetRecDevProj <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevProj'), TimeSteps, df)
}


# Fleet -----

GetFleetAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Fleet
  
  if (isS4(object))
    return(Get(object, slots, TimeSteps, df))
  
  if (methods::is(object, 'FleetList')) {
      object <- purrr::map(object, GetFleetAtAge, slots=slots, TimeSteps=TimeSteps, df=df)  
  }
  object
}


GetApicalF <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object, slots=c('FishingMortality', 'ApicalF'), TimeSteps, df)
}


GetDiscardMortalityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object,  c('DiscardMortality', 'MeanAtAge'), TimeSteps, df)
}

`SetDiscardMortalityAtAge<-` <- function(x, value) {
  x@DiscardMortality@MeanAtAge <- value
  x
}

GetSelectivityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object,  c('Selectivity', 'MeanAtAge'), TimeSteps, df)
}

`SetSelectivityAtAge<-` <- function(x, value) {
  x@Selectivity@MeanAtAge <- value
  x
}

GetRetentionAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object,  c('Retention', 'MeanAtAge'),TimeSteps, df)
}

`SetRetentionAtAge<-` <- function(x, value) {
  x@Retention@MeanAtAge <- value
  x
}

GetFleetWeightAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object,  c('Weight'),TimeSteps, df)
}

`SetFleetWeightAtAge<-` <- function(x, value) {
  x@Weight <- value
  x
}





