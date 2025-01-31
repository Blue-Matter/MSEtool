
# ---- CalcRelRec -----
setGeneric('CalcRelRec', function(x, SPR=seq(0,1, by=0.01))
  standardGeneric('CalcRelRec')
)

setMethod('CalcRelRec', c('stock', 'ANY'), function(x, SPR=seq(0,1, by=0.01)) {
  if (inherits(x@SRR@RelRecFun, 'function')) {
    stop('Custom `RelRecFun` not working')
  }
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom != x@Name) 
    return(NULL)
  
  fun <- get(paste0(x@SRR@Model, 'RelRec'))
  fun(x@SRR@Pars, SPR)
})

setMethod('CalcRelRec', c('StockList', 'ANY'), function(x, SPR=seq(0,1, by=0.01)) {
  purrr::map(x, CalcRelRec, SPR)
})


setMethod('CalcRelRec', c('om', 'ANY'), function(x, SPR=seq(0,1, by=0.01)) {
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
  
  out <- array(NA, dim=c(length(h), length(SPR)))
  l <- list()
  l$Sim <- 1:length(h)
  l$SPR <- SPR
  dimnames(out) <- l
  ind <- expand.grid(1:length(h), 1:length(SPR)) |> as.matrix()
  out[ind] <- (CR[ind[,1]]*SPR[ind[,2]]-1)/((CR[ind[,1]]-1)*SPR[ind[,2]])
  out[out<0] <- 0
  out
}