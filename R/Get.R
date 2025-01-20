
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
  
  if (methods::is(object, 'StockList')) 
    return(
      purrr::map(object, Get, slots=slots, df=df)
    )
  Get(object, slots, df)
}



GetLengthAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Length', 'MeanAtAge'),df)
}

GetWeightAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Weight', 'MeanAtAge'),df)
}

GetMaturityAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Maturity', 'MeanAtAge'),df)
}

GetFecundityAtAge <- function(object, df=FALSE) {
  GetStockAtAge(object,  c('Fecundity', 'MeanAtAge'),df)
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
  
  if (methods::is(object, 'list')) {
      object <- purrr::map(object, GetFleetAtAge, slots=slots, df=df)  
  }
  if (!isS4(object))
    return(object)
  Get(object, slots, df)
}


GetApicalF <- function(object, df=FALSE) {
  GetFleetAtAge(object, slots=c('FishingMortality', 'ApicalF'), df)
}
