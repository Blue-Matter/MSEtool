
# TODO add check for changes to all functions 
# TODO - SPRcrash, MGT, RefYield 
# GetFSPR(SPRarray=Hist@RefPoints@Curves@SPR[[1]], SPRvalue=0.3)

#' Calculate Biological Reference Points
#'
#' @param OM 
#' @param Unfished 
#'
#' @returns
#' @export
CalcRefPoints <- function(OM, Unfished=NULL, TimeSteps=NULL) {
  
  if (inherits(OM, 'hist'))
    Unfished <- OM@Unfished
  
  if (is.null(Unfished))
    Unfished <- CalcUnfishedDynamics(OM)
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(OM, 'Historical') |> tail(1)
  
  RefPoints <- new('refpoints')
  
  # At F Curves ----
  RefPoints@SPR0 <- CalcSPR0(OM, TimeSteps)
  RefPoints@Curves <- CalcCurves(OM, TimeSteps=TimeSteps)
  
  # MSY Ref Points ----
  Yield <- CalcYieldComplex(RefPoints@Curves@Yield, OM)
  MaxYieldIndList <- purrr::map(Yield, \(x) apply(x, 1:2, which.max))
  
  FValuesList <- vector('list', nStock(OM))
  FValues <- array(RefPoints@Curves@FValues, 
                   c(1,1, length(RefPoints@Curves@FValues)))
  dimnames(FValues) <- list(Sim=1,
                            `Time Step`=tail(TimeSteps),
                            ApicalF=RefPoints@Curves@FValues)
  
  
  names(FValuesList) <- names(Yield)
  for (i in seq_along(FValuesList)) {
    ArrayFill(FValuesList[[i]]) <- FValues
  }
  
  RefPoints@FMSY <- purrr::map2(FValuesList,
                                MaxYieldIndList, 
                                GetMSYValue)
  
  RefPoints@MSY <- purrr::map2(RefPoints@Curves@Yield,
                               MaxYieldIndList, 
                               GetMSYValue)
  
  RefPoints@BMSY <- purrr::map2(RefPoints@Curves@Biomass,
                                MaxYieldIndList, 
                                GetMSYValue)
  
  RefPoints@SBMSY <- purrr::map2(RefPoints@Curves@SBiomass,
                                 MaxYieldIndList, 
                                 GetMSYValue)
  
  RefPoints@SPMSY <- purrr::map2(RefPoints@Curves@SP,
                                 MaxYieldIndList, 
                                 GetMSYValue)
  
  RefPoints@SPRMSY <- purrr::map2(RefPoints@Curves@SPR,
                                  MaxYieldIndList, 
                                  GetMSYValue)
  
  # Fx% Ref Points ----
  # TODO - add option for reference points for Fx% SPR
  # Fx <- GetFSPR(RefPoints@Curves@SPR$`SA Red Snapper`, 0.3)
  

  # Fx
  # Bx
  # SBx
  # SPx
  # 
  
  
  # F-based Ref Points ----
  # yield per recruit accounting for complexes in yield calculation
  YPR <- CalcYieldComplex(RefPoints@Curves@YPR, OM) 
  MaxYPRIndList <- purrr::map(YPR, \(x) apply(x, 1:2, which.max))
  
  RefPoints@FMax <- purrr::map2(FValuesList,
                                MaxYPRIndList, 
                                GetMSYValue)
  
  RefPoints@F01 <- purrr::map2(YPR, FValuesList, CalcF01)
  RefPoints@FCrash <- purrr::map(RefPoints@Curves@RelRec, \(x)
                                 CalcFCrash(x, RefPoints@Curves@FValues))
  
  
  # SPR Crash ----
  # TODO - can calculate directly from SRR pars - see CalcRelRec
  
  # MGT  ----
  # TODO RefPoints@MGT 
  
  # Reference Yield ----
  # TODO
  # RefPoints@RefYield
  
  # BLow  ----
  # TODO
  # RefPoints@BLow
  
  
  # Equilibrium Unfished ----
  RefPoints@Equilibrium@N0 <- purrr::map(Unfished@Equilibrium@Number,
                                         \(x) apply(x, c("Sim","Time Step"), sum))
  
  RefPoints@Equilibrium@B0 <- purrr::map(Unfished@Equilibrium@Biomass,
                                         \(x) apply(x, c("Sim","Time Step"), sum))
  
  RefPoints@Equilibrium@SB0 <- purrr::map(Unfished@Equilibrium@SBiomass,
                                          \(x) apply(x, c("Sim","Time Step"), sum))
  
  RefPoints@Equilibrium@SP0 <- Unfished@Equilibrium@SProduction
 
  # Dynamic Unfished ---- 
  # TODO
  
  RefPoints
  
  
}


CalcYieldComplex <- function(Yield, OM) {
  # Sums equilibrium yield at given F over stocks
  # so that the MSY calcs account for total yield for the
  # complex
  # Currently only does Female and Male stocks from `SPFrom`
  # but needs to be extended for Herm stocks and for generic
  # complexes 
  
  # TODO - add option to define complexes for MSY calcs
  YieldOut <- vector('list', nStock(OM))
  names(YieldOut) <- names(Yield)
  L <- vector('list', nStock(OM))
  SPFrom <- GetSPFrom(OM)
  for (i in seq_along(L)) {
    ind <- match(SPFrom[[i]], names(SPFrom))
    if (length(ind)<1) {
      L[[i]] <- i
    } else {
      L[[i]] <- c(L[[i]], i, ind) |> unique() |> sort()
      L[[ind]] <- L[[i]]
    }
  }
  for (i in seq_along(L)) {
    yieldList <- Yield[L[[i]]] 
    if (length(yieldList)>2) 
      cli::cli_abort('List must be length 2', .internal=TRUE)
    if (length(yieldList)==1) {
      YieldOut[[i]] <- yieldList[[i]]
    } else {
      YieldOut[[i]] <- ArrayAdd(yieldList)
    }
  }
  YieldOut
}


# returns the subset of array where the third dimension is selected
# from MaxYieldInd 
GetMSYValue <- function(Array, MaxYieldInd) {
  if (is.null(Array) || length(Array)<1)
    return(NULL)
  d1 <- dim(Array)
  d2 <- dim(MaxYieldInd)
  out <- abind::adrop(Array[,,1, drop=FALSE],3)
  out[] <- NA
  
  # TODO fix loop later
  for (i in 1:d1[1]) {
    for (j in 1:d1[2]) {
      out[i,j] <- Array[i,j,MaxYieldInd[i,j]]
    }
  }
  l <- list()
  l[[1]] <- dimnames(Array)[[1]]
  l[[2]] <- dimnames(Array)[[2]]
  dimnames(out) <- l
  out
}

CalcF01 <- function(ypr, fvalues) {
  out <- abind::adrop(ypr[,,1,drop=FALSE],3)
  out[] <- NA 
  dd <- dim(ypr)
  for (i in 1:dd[1]) {
    for (j in 1:dd[2]) {
      ypr_ <- ypr[i,j,]
      dYPR_dF <- (ypr_[-1]-ypr_[-length(ypr_)])/ (fvalues[-1]-fvalues[-length(fvalues)])
      out[i,j] <- LinInterp_cpp(dYPR_dF, fvalues[-length(ypr_)], xlev = 0.1 * dYPR_dF[1])
    }
  }
  out
}


FirstFZero <- function(vec, FValues) {
  val <- which(vec==0)
  if (length(val)<1)
    return(Inf)
  FValues[min(val)]
}

CalcFCrash <- function(RelRec, FValues) {
  apply(RelRec, 1:2, FirstFZero, FValues=FValues)
}


GetFSPR <- function(SPRarray, SPRvalue, TimeSteps=NULL) {
  if (inherits(SPRarray, 'curves')) {
    stop('need to make generic')
  }
  dimnames <- dimnames(SPRarray)
  nms <- names(dimnames)
  
  if (is.null(TimeSteps)) {
    TimeSteps <- dimnames[['Time Step']] |> as.numeric() |> max()
  }
  
  FValues <- dimnames$apicalF |> as.numeric()
  SPRarray2 <- abind::asub(SPRarray,
                           as.character(TimeSteps), 
                           which(nms=='Time Step'),
                           drop=FALSE)
  
  Diff <- abs(SPRarray2 - SPRvalue)
  FArray <- abind::adrop(Diff[,,1, drop=FALSE],3) 
  FArray[] <- 0
  for (i in 1:nrow(Diff)) {
    for (j in 1:ncol(Diff)) {
      ind <- which.min(Diff[i,j,])
      FArray[i,j] <- FValues[ind]
    }
  }
  FArray
}

GetValueAtF <- function(Array, FValue, TimeSteps=NULL) {

  dimnames1 <- dimnames(Array)
  nms1 <- names(dimnames1)
  
  dimnames2 <- dimnames(FValue)
  nms2 <- names(dimnames2)
  
  if (is.null(TimeSteps)) {
    TimeSteps <- dimnames1[['Time Step']] |> as.numeric() |> max()
  }
  
  FValue2 <- abind::asub(FValue,
                         as.character(TimeSteps), 
                         which(nms2=='Time Step'),
                         drop=FALSE)

  Array2 <- abind::asub(Array,
                           list(as.character(TimeSteps), 
                                as.character(FValue2)),
                           which(nms1%in%c('Time Step', 'ApicalF')),
                           drop=FALSE)
  
  Array2
 
}
