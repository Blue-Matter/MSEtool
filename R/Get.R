Get <- function(slot, x, y=NULL) {
  cl <- class(x)
  slots <- slotNames(cl)
  ind <- match(slot, slots)
  if (!is.na(ind))
    return(slot(x, slot))
  
  if (!is.null(y)) {
    obj <- slot(x,y)
  } else {
    # find object with unique `slot`
    for (i in seq_along(slots)) {
      obj <- slot(x, slots[i])
      slots_i <- slotNames(obj)
      ind <- match(slot, slots_i)
      if (is.na(ind)) {
        obj <- NULL
      } else {
        break()
      }
    }
  }
  if (is.null(obj))
    return(NULL)
  
  slot(obj, slot)
}

GetR0 <- function(Stock) {
  Get('R0', Stock)
}

GetWeightAtAge <- function(Stock) {
  Get('MeanAtAge', Stock, 'Weight')
}

GetMaturityAtAge <- function(Stock) {
  Get('MeanAtAge', Stock, 'Maturity')
}

GetFecundityAtAge <- function(Stock) {
  Get('MeanAtAge', Stock, 'Fecundity')
}

GetRecDevInit <- function(Stock) {
  Get('RecDevInit', Stock, 'SRR')
}

GetRecDevHist <- function(Stock) {
  Get('RecDevHist', Stock, 'SRR')
}

GetRecDevProj <- function(Stock) {
  Get('RecDevProj', Stock, 'SRR')
}
