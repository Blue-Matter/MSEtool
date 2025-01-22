
Get <- function(object, slots, df=FALSE) {

  cl <- class(object)
  for (i in seq_along(slots)) {
    object <- slot(object, slots[i])
  }
  if (df) 
    object <- as.data.frame.table(object, stringsAsFactors =FALSE,
                            responseName =slots[length(slots)]) |>
     dplyr::mutate_if(is.character, as.numeric)
  
  object

}

# Stock -----
GetStockAtAge <- function(object, slots, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Stock
  
  if (isS4(object))
    return(Get(object, slots, df))
  
  if (methods::is(object, 'StockList')) 
    return(
      purrr::map(object, Get, slots=slots, df=df)
    )
  Get(object, slots, df)
}


GetLengthAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  slots=c('Length', 'MeanAtAge'),df)
}


`SetLengthAtAge<-` <- function(x, value) {
  x@Length@MeanAtAge <- value
  x
}


GetWeightAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Weight', 'MeanAtAge'),df)
}

`SetWeightAtAge<-` <- function(x, value) {
  x@Weight@MeanAtAge <- value
  x
}


GetMaturityAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Maturity', 'MeanAtAge'),df)
}

`SetMaturityAtAge<-` <- function(x, value) {
  x@Maturity@MeanAtAge <- value
  x
}


GetFecundityAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Fecundity', 'MeanAtAge'),df)
}

`SetFecundityAtAge<-` <- function(x, value) {
  x@Fecundity@MeanAtAge <- value
  x
}


GetR0 <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'R0'),df)
}



GetRecDevInit <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevInit'),df)
}

GetRecDevHist <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevHist'),df)
}


GetRecDevProj <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevProj'),df)
}


# Fleet -----

GetFleetAtAge <- function(object, slots, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Fleet
  
  if (isS4(object))
    return(Get(object, slots, df))
  
  if (methods::is(object, 'FleetList')) {
      object <- purrr::map(object, GetFleetAtAge, slots=slots, df=df)  
  }
  object
}


GetApicalF <- function(object, df=FALSE) {
  GetFleetAtAge(object, slots=c('FishingMortality', 'ApicalF'), df)
}


GetDiscardMortalityAtAge <- function(object, df=FALSE) {
  GetFleetAtAge(object,  c('DiscardMortality', 'MeanAtAge'),df)
}

`SetDiscardMortalityAtAge<-` <- function(x, value) {
  x@DiscardMortality@MeanAtAge <- value
  x
}

GetSelectivityAtAge <- function(object, df=FALSE) {
  GetFleetAtAge(object,  c('Selectivity', 'MeanAtAge'),df)
}

`SetSelectivityAtAge<-` <- function(x, value) {
  x@Selectivity@MeanAtAge <- value
  x
}

GetRetentionAtAge <- function(object, df=FALSE) {
  GetFleetAtAge(object,  c('Retention', 'MeanAtAge'),df)
}

`SetRetentionAtAge<-` <- function(x, value) {
  x@Retention@MeanAtAge <- value
  x
}

GetFleetWeightAtAge <- function(object, df=FALSE) {
  GetFleetAtAge(object,  c('Weight'),df)
}

`SetFleetWeightAtAge<-` <- function(x, value) {
  x@Weight <- value
  x
}





