#' @rdname Convert
#' @export
ConvertMOM <- function(MOM, ...) {
  ConvertOM(MOM,...)
}


#' @rdname Convert
#' @export
ConvertOM <- function(OM, ...) {
  CheckClass(OM, c('OM', 'MOM'), 'OM')
  
  dotsList <- list()
  dotsList <- list(...)
  
  Author <- dotsList$Author
  CurrentYear <- dotsList$CurrentYear
  Populate <- ifelse(is.null(dotsList$Populate), TRUE, FALSE)
  
  isMOM <- inherits(OM, 'MOM')
  
  om <- OM()
  om@Name <- OM@Name
  om@Agency <-  OM@Agency
  om@Region <-  OM@Region
  om@Author <- Author
  om@Longitude <- OM@Longitude
  om@Latitude <- OM@Latitude
  om@Sponsor <- OM@Sponsor
  om@nSim <- OM@nsim
  om@nYear <- ifelse(isMOM,  OM@Fleets[[1]][[1]]@nyears, OM@nyears)
  om@pYear <- OM@proyears
  om@Interval <- OM@interval
  om@Seed <- OM@seed
  om@pStar <- OM@pstar
  om@maxF <- OM@maxF
  om@nReps <- OM@reps
  om@Source <- OM@Source
  
  if (is.null(CurrentYear)) {
    if (isMOM) {
      om@CurrentYear <- OM@Fleets[[1]][[1]]@CurrentYr
      if (om@CurrentYear < 1000)
        om@CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
    } else {
      om@CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
    }
  } else {
    om@CurrentYear <- CurrentYear
  }
  
  om@TimeUnits <- 'Year'
  om@TimeStepsPerYear <- 1
  om@TimeSteps <- CalcTimeSteps(nYear=om@nYear,
                                pYear=om@pYear,
                                CurrentYear=om@CurrentYear,
                                TimeUnits=om@TimeUnits)
  
  TimeSteps <- list(HistTS=TimeSteps(om, 'Historical'),
                    ProjTS=TimeSteps(om, 'Projection')
  )
  
  if (isMOM)
    stop("not done yet!")
  
  om@Stock <- OM2stock(OM, cpars=OM@cpars, TimeSteps, OM@nsim, OM@seed)
  om@Fleet <- OM2fleet(OM, OM@cpars, OM@Fdisc)
  
  om <- UpdateSelRet(OM, om)
  
  om@Obs <- OM2obs(OM, OM@cpars)
  om@Imp <- OM2imp(OM, OM@cpars)
  
  # update because Vmaxlen and Rmaxlen now correspond with maximum length class
  om <- SolveForVmaxlen(om) 
  om <- SolveForRmaxlen(om)
  if (Populate)
    om <- PopulateOM(om, silent=FALSE)
  
  om
}


UpdateSelRet <- function(OM, om) {
  if (!as.logical(OM@isRel))
    return(om)
  
  L50 <- GetLengthClass(om@Stock@Maturity, 0.5)
  
  om@Fleet@Selectivity@Pars <- StructurePars(Pars=om@Fleet@Selectivity@Pars,
                                             nsim=om@nSim, 
                                             TimeSteps=om@TimeSteps)
  
  om@Fleet@Selectivity@Pars$L5 <- ArrayMultiply(L50, om@Fleet@Selectivity@Pars$L5)
  om@Fleet@Selectivity@Pars$LFS <- ArrayMultiply(L50, om@Fleet@Selectivity@Pars$LFS)
  
  om@Fleet@Retention@Pars <- StructurePars(Pars=om@Fleet@Retention@Pars,
                                           nsim=om@nSim, 
                                           TimeSteps=om@TimeSteps)
  
  om@Fleet@Retention@Pars$LR5 <- ArrayMultiply(L50, om@Fleet@Retention@Pars$LR5)
  om@Fleet@Retention@Pars$LFR <- ArrayMultiply(L50, om@Fleet@Retention@Pars$LFR)
  om
}

GetLengthClass <- function(object, RefValue=0.5) {
  array <- object@MeanAtLength
  dd <- dim(array)
  
  out <- array(0, dim=dd[c(1,3)], dimnames=dimnames(array)[c(1,3)])
  
  for (s in 1:dd[1]) {
    for (ts in 1:dd[3]) {
      ind <- which.min(abs(array[s,,ts]-RefValue))
      out[s,ts] <- object@Classes[ind]
    }
  }
  out
}


SolveForVmaxlen <- function(om) {
  # calculates new value for Vmaxlen to correspond with maximum
  # length bin rather than Linf, as previously defined
  Linf <- om@Stock@Length@Pars$Linf 
  dd <- dim(Linf)
  L5 <- om@Fleet@Selectivity@Pars$L5
  LFS <- om@Fleet@Selectivity@Pars$LFS
  Vmaxlen <- om@Fleet@Selectivity@Pars$Vmaxlen
  
  df <- rbind(dim(Linf),
              dim(L5),
              dim(LFS),
              dim(Vmaxlen)
  )
  nsim <- max(df[,1])
  
  timestepsList <- list(dimnames(Linf)$TimeStep,
                        dimnames(L5)$TimeStep,
                        dimnames(LFS)$TimeStep,
                        dimnames(Vmaxlen)$TimeStep
  )
  timesteps <- timestepsList[[which.max(df[,2])]]                      
  
  Linf <- Linf |> ArrayExpand(nsim, TimeSteps=timesteps)
  L5 <- L5 |> ArrayExpand(nsim, TimeSteps=timesteps)
  LFS <- LFS |> ArrayExpand(nsim, TimeSteps=timesteps)
  Vmaxlen <- Vmaxlen |> ArrayExpand(nsim, TimeSteps=timesteps)
  
  VmaxlenOut <- array(0, dim=dd)
  dimnames(VmaxlenOut) <- dimnames(Linf)
  
  cli::cli_progress_bar('Calculating `Vmaxlen`', total=prod(dd))
  for (s in 1:nsim) {
    for (ts in seq_along(timesteps)) {
      VmaxlenOut[s,ts] <- VmaxLenOpt(L5[s,ts], 
                                     LFS[s,ts],
                                     Vmaxlen[s, ts],
                                     Linf[s,ts])
      cli::cli_progress_update()
    }
  }
  cli::cli_progress_done()
  om@Fleet@Selectivity@Pars$Vmaxlen <- VmaxlenOut
  om
}


VmaxLenOpt <- function(l5, lfs, vmaxlen, linf) {
  if (vmaxlen > 0.99)
    return(vmaxlen)
  opt <- optimize(optForVmaxLen,
                  interval=logit(c(0.001, 0.999)),
                  l5=l5,
                  lfs=lfs,
                  linf=linf,
                  vmaxlen=vmaxlen)
  return(ilogit(opt$minimum))
}

SolveForRmaxlen <- function(om) {
  # calculates new value for Rmaxlen  to correspond with maximum
  # length bin rather than Linf, as previously defined
  Linf <- om@Stock@Length@Pars$Linf
  dd <- dim(Linf)
  L5 <- om@Fleet@Retention@Pars$LR5
  LFS <- om@Fleet@Retention@Pars$LFR
  Rmaxlen <- om@Fleet@Retention@Pars$Rmaxlen
  
  RmaxlenOut <- array(0, dim=dd)
  dimnames(RmaxlenOut) <- dimnames(Linf)
  cli::cli_progress_bar('Calculating `Rmaxlen`', total=prod(dd))
  for (s in 1:dd[1]) {
    for (ts in 1:dd[2]) {
      l5 <- L5[GetIndex(s, nrow(L5)), GetIndex(ts, ncol(L5))]
      lfs <- LFS[GetIndex(s, nrow(LFS)), GetIndex(ts, ncol(LFS))]
      linf <- Linf[GetIndex(s, nrow(Linf)), GetIndex(ts, ncol(Linf))]
      rmaxlen <- Rmaxlen[GetIndex(s, nrow(Rmaxlen)), GetIndex(ts, ncol(Rmaxlen))]
      
      if (l5==0)
        next()
      
      
      if (rmaxlen==1)
        next()
      opt <- optimize(optForVmaxLen,
                      interval=logit(c(0.001, 0.999)),
                      l5=l5,
                      lfs=lfs,
                      linf=linf,
                      vmaxlen=rmaxlen)
      RmaxlenOut[s,ts] <- ilogit(opt$minimum)
      cli::cli_progress_update()
    }
  }
  cli::cli_progress_done()
  om@Fleet@Retention@Pars$Rmaxlen <- RmaxlenOut
  om
}

optForVmaxLen <- function(logitTrial, l5, lfs, linf, vmaxlen) {
  trial <- ilogit(logitTrial)
  lens <- seq(0, linf,length.out=100)
  sel <- DoubleNormal(lens,l5, lfs, trial)
  (sel[length(sel)] - vmaxlen)^2
}

