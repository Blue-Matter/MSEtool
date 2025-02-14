ArraySubsetTimeStep <- function(object, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    return(object)
  
  TimeSteps <- TimeSteps |> as.numeric() |> round(2)
  
  DN <- dimnames(object)
  TSind <- which(names(DN) == 'Time Step')
  if (length(TSind)==0)
    cli::cli_abort("`Time Step` dimension not found in this array", .internal=TRUE)
  
  if (any(TimeSteps > max(DN$`Time Step`))) {
    
    TSexist <- TimeSteps[TimeSteps %in% DN$`Time Step`]
    TSimpute <- TimeSteps[!TimeSteps %in% DN$`Time Step`]
    if (length(TSimpute)) {
      matchTS <- rep(NA, length(TSimpute))
      for (i in seq_along(TSimpute)) {
        matchTS[i] <- DN$`Time Step`[DN$`Time Step` < TSimpute[i]] |> max()
      }
    }
    TimeStepsMod <- c(TSexist, matchTS)
    array <- abind::asub(object, TimeStepsMod, TSind, drop=FALSE)
    dimnames(array)$`Time Step` <- TimeSteps
    return(array)
  } 
  abind::asub(object, (DN[[TSind]] %in% TimeSteps), TSind, drop=FALSE)
}

Get <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  
  cl <- class(object)
  for (i in seq_along(slots)) {
    object <- slot(object, slots[i])
  }
  if (is.null(object) | all(is.na(object)))
    return(NULL)
  
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

GetSemelparous <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Maturity', 'Semelparous'), TimeSteps, df)
}



GetFecundityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object, c('Fecundity', 'MeanAtAge'), TimeSteps, df)
}

`SetFecundityAtAge<-` <- function(x, value) {
  x@Fecundity@MeanAtAge <- value
  x
}


GetPlusGroup <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Ages', 'PlusGroup'), TimeSteps, df)
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

`SetRecDevInit<-` <- function(x, value) {
  x@SRR@RecDevInit <- value
  x
}

GetRecDevHist <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevHist'), TimeSteps, df)
}

`SetRecDevHist<-` <- function(x, value) {
  x@SRR@RecDevHist <- value
  x
}


GetRecDevProj <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevProj'), TimeSteps, df)
}

`SetRecDevProj<-` <- function(x, value) {
  x@SRR@RecDevProj <- value
  x
}

GetRelativeSize <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Spatial', 'RelativeSize'), TimeSteps, df)
}


# Fleet -----

GetFleetAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Fleet
  
  if (isS4(object))
    return(Get(object, slots, TimeSteps, df))
  
  if (methods::is(object, 'FleetList')) {
      object <- purrr::map(object, GetFleetAtAge, slots=slots, TimeSteps=TimeSteps, df=df) 
      return(object)
  }
  object
}


GetApicalF <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object, slots=c('FishingMortality', 'ApicalF'), TimeSteps, df)
}

GetEffort <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object, slots=c('Effort', 'Effort'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}

GetCatchability <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object, slots=c('Effort', 'Catchability'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}



GetDiscardMortalityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object,  c('DiscardMortality', 'MeanAtAge'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}

`SetDiscardMortalityAtAge<-` <- function(x, value) {
  x@DiscardMortality@MeanAtAge <- value
  x
}

GetSelectivityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object,  c('Selectivity', 'MeanAtAge'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}

`SetSelectivityAtAge<-` <- function(x, value) {
  x@Selectivity@MeanAtAge <- value
  x
}

GetRetentionAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object,  c('Retention', 'MeanAtAge'),TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}

`SetRetentionAtAge<-` <- function(x, value) {
  x@Retention@MeanAtAge <- value
  x
}

GetEmpiricalWeightAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object,  slots=c('Weight'),TimeSteps, df)
}

`SetFleetWeightAtAge<-` <- function(x, value) {
  x@Weight <- value
  x
}

GetFleetWeightAtAge <- function(Stock, FleetList, TimeSteps=NULL) {
  nFleet <- length(FleetList)
  FleetEmpiricalWeightatAge <- GetEmpiricalWeightAtAge(FleetList, TimeSteps=TimeSteps) 
  StockWeightatAge <- GetWeightAtAge(Stock, TimeSteps)
  FleetWeightatAge <- replicate(nFleet, StockWeightatAge)
  l <- dimnames(StockWeightatAge)
  l$Fleet <- names(FleetList)
  dimnames(FleetWeightatAge) <- l
  
  for (fl in seq_along(FleetEmpiricalWeightatAge)) {
    if (is.null(FleetEmpiricalWeightatAge[[fl]])) {
      array <- AddDimension(StockWeightatAge, 'Fleet')
      dimnames(array)$Fleet <- l$Fleet[fl]
      ArrayFill(FleetWeightatAge) <- array
    } else {
      array <- AddDimension(FleetEmpiricalWeightatAge[[fl]], 'Fleet')
      dimnames(array)$Fleet <- l$Fleet[fl]
      ArrayFill(FleetWeightatAge) <- array
    }
  }
  FleetWeightatAge
}

# ---- Hist ----

GetHistAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {

  if (isS4(object))
    return(Get(object, slots, TimeSteps, df))
  
  if (methods::is(object, 'StockList')) 
    return(
      purrr::map(object, Get, slots=slots, TimeSteps=TimeSteps, df=df)
    )
  Get(object, slots, TimeSteps, df)
}


GetNumber <- function(object, TimeSteps=NULL, df=FALSE) {
  t = GetHistAtAge(object, 'Number', TimeSteps, df)
}



