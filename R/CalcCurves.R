
# devtools::load_all()

# NOTE: Curves do not account for spatial closures or MICE interactions

SPRCurve <- function(OM, 
                     messages='default',
                     nSim=NULL,
                     parallel=FALSE,
                     FSearch=NULL,
                     SPR0=NULL) {
  
  # TODO add option to specify Time Steps to calculate
  # currently does all
  
  # TODO  modify for herm species
  
  OM <- StartUp(OM, messages, nSim) 
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  CalcSPR(OM, FSearch, SPR0)
}

RelRecruitCurve <- function(OM, SPR=seq(0,1, by=0.01)) {

  RelRec <- CalcRelRec(OM, SPR)
  for (st in 1:nStock(OM)) {
    stock <- OM@Stock[[st]]
    if (!is.null(stock@SRR@SPFrom) && stock@SRR@SPFrom != stock@Name) {
      RelRec[[st]] <- RelRec[[x@SRR@SPFrom]]
    }
  }
  RelRec
}

YPRCurve <- function(OM, FSearch=NULL) {
  
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  
  # output - yield per recruit for each sim & time-step
  OM@Stock[[1]]
  N <- CalcFishedSurvival(OM, apicalF=FSearch[1])
  
  NatAge <- N$Female
  WeightatAge <- OM@Stock[[1]]@Weight@MeanAtAge
  MatAge <- OM@Stock[[1]]@NaturalMortality@MeanAtAge
  
  # TODO - empirical OM@Fleet[[1]][[1]]@Weight |> dim()
  FatAge <- UpdateApicalF(OM@Fleet[[1]], FSearch[1])
  CalcBaranov <- function(NatAge, WeightatAge, MatAge, FatAge) {
    
  }
  
}

YieldCurve <- function(OM, RelRec=NULL, SPR=NULL, FSearch=NULL) {
  if (is.null(RelRec))
    RelRec <- RelRecruitCurve(OM)
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  if (is.null(SPR))
    SPR <- SPRCurve(OM, FSearch=FSearch)
    
  # output - yield per recruit for each 
}

# ---- CalcRelRec -----
setGeneric('CalcRelRec', function(x, SPR)
  standardGeneric('CalcRelRec')
)

setMethod('CalcRelRec', c('stock', 'numeric'), function(x, SPR) {
  if (inherits(x@SRR@RelRecFun, 'function')) {
    stop('Custom `RelRecFun` not working')
  }
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom != x@Name) 
    return(NULL)
    
  fun <- get(paste0(x@SRR@Model, 'RelRec'))
  fun(x@SRR@Pars, SPR)
})

setMethod('CalcRelRec', c('StockList', 'numeric'), function(x, SPR) {
  purrr::map(x, CalcRelRec, SPR)
})

setMethod('CalcRelRec', c('om', 'numeric'), function(x, SPR) {
  CalcRelRec(x@Stock, SPR)
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

