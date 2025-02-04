
# ---- CalcRelRec -----
setGeneric('CalcRelRec', function(x, SPR=seq(0.01,1, by=0.01))
  standardGeneric('CalcRelRec')
)


setMethod('CalcRelRec', c('stock', 'numeric'), function(x, SPR=seq(0.01,1, by=0.01)) {
  if (inherits(x@SRR@RelRecFun, 'function')) {
    stop('Custom `RelRecFun` not working')
  }
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom != x@Name) 
    return(NULL)
  
  fun <- get(paste0(x@SRR@Model, 'RelRec'))
  fun(x@SRR@Pars, SPR)
})

setMethod('CalcRelRec', c('stock', 'array'), function(x, SPR=array()) {
  if (inherits(x@SRR@RelRecFun, 'function')) {
    stop('Custom `RelRecFun` not working')
  }
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom != x@Name) 
    return(NULL)
  
  fun <- get(paste0(x@SRR@Model, 'RelRec'))
  fun(x@SRR@Pars, SPR)
})

setMethod('CalcRelRec', c('StockList', 'numeric'), function(x, SPR=seq(0.01,1, by=0.01)) {
  purrr::map(x, CalcRelRec, SPR)
})

setMethod('CalcRelRec', c('StockList', 'list'), function(x, SPR=list()) {
  purrr::map2(x, SPR, CalcRelRec)
})

setMethod('CalcRelRec', c('om', 'ANY'), function(x, SPR=seq(0.01,1, by=0.01)) {
  RelRec <- CalcRelRec(x@Stock, SPR)
  for (st in 1:nStock(x)) {
    stock <- x@Stock[[st]]
    if (!is.null(stock@SRR@SPFrom) && stock@SRR@SPFrom != stock@Name) {
      RelRec[[st]] <- RelRec[[stock@SRR@SPFrom]]
    }
  }
  RelRec
})


BevertonHoltRelRec <- function(Pars, SPR) {
  # TODO doesn't do time-varying h - make other SRR funs
  # TODO RelRec functions for Ricker and HockeyStick
  h <- Pars$h
  
  CR <- (4*h)/(1-h) # Goodyear Compensation Ratio
  
  if (inherits(SPR, 'numeric')) {
    l <- dimnames(h)
    l$SPR <- SPR
    SPR <- matrix(SPR, nrow(h), length(SPR), byrow = TRUE)
    SPR <- replicate(ncol(h), SPR) |> aperm(c(1,3,2))
    dimnames(SPR) <- l
    CR <- AddDimension(CR, 'SPR')
  } else {
    CR <- AddDimension(CR, 'apicalF')
  }
  
  out <- ArrayDivide(ArrayMultiply(CR, SPR)-1, ArrayMultiply((CR-1), SPR))
  out[out<0] <- 0
  out 
}