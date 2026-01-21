#' General Helper Functions
#' 
#' @name helpers
#' @export
nSim <- function(x) {
  if (isS4(x)) {
    slots <- slotNames(x)
    if ('nSim' %in% slots)
      return(x@nSim)
    return(x@OM@nSim)
  }

  if (is.list(x)) {
    return(purrr::map(x, nSim) |> unlist())
  }

  dnames <- dimnames(x)
  if (!is.null(dnames))
    return(length(dnames[['Sim']]))
}


#' @rdname helpers
#' @export
nArea <- function(x, st=1) {
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      stock <- stock[[st]]
    }
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  } else {
    stock <- x
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  }

  if (length(dd)<1) {
    if (length(d1)>0)
      return(d1)
    return(1)
  }

  nms <- names(dimnames(stock@Spatial@UnfishedDist))
  dd[which(nms=='Area')]

}

#' @rdname helpers
#' @export
nStock <- function(object) {
  
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    return(length(object@Stock))
  }
  
  if (inherits(object,'hist')) {
    return(length(object@OM@Stock))
  }
  
  if (inherits(object,'mse')) {
    return(length(object@OM@Stock))
  }
  
}

#' @rdname helpers
#' @export
nFleet <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    fleet <- object@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if(isS4(fleet[[1]])) {
      dd <- dim(object@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  return(dim(object@Landings)[4])
  
}

StockNames
