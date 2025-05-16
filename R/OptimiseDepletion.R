# Optimizes catchability to match specified terminal depletion and fleet catch fractions

OptimizeCatchability <- function(OMList) {
  
  # TODO add qinc, qvar etc for historical and projection
  
  bounds <- c(1e-03, 15)
  tol <- 1E-5
  silent <- TRUE
  
  nsims <- length(OMList)
  nStock <- length(OMList[[1]]$Ages)
  nFleet <- dim(OMList[[1]]$FishingMortality$DeadAtAge[[1]])[3]
  
  OMList <- purrr::map(OMList, \(OMListSim) {
    
    if (nStock > 1 || nFleet > 1) {
      # multi stock/fleet
      QMatrix <- OMListSim$Catchability[,,1]
      
      if (all(QMatrix>1E-5))
        return(OMListSim)
      
      # Catch divided by effort (q proxy)
      FDist <- OMListSim$CatchFrac/OMListSim$Effort[,length(OMListSim$TimeStepsHist[[1]]),]
      FDist[!is.finite(FDist)] <- tiny
      FDist <- FDist/apply(FDist[, , drop = FALSE], 1, sum)    # q ratio proxy (real space)
      
      if (nFleet == 1) {
        pars <- rep(-5, nStock)
      } else {
        # low initial F followed by logit guess at fraction based on Fdist
        # according to catch fraction in recent year
        pars <- c(rep(-5,nStock), logit(FDist[, 2:nFleet]))
      }
      
      doOpt <- optim(pars,
                     OptCatchability,
                     method = "L-BFGS-B",
                     lower = c(rep(log(bounds[1]), nStock), rep(-5, nStock * (nFleet-1))),
                     upper = c(rep(log(bounds[2]), nStock), rep(5, nStock*(nFleet-1))),
                     OMListSim=OMListSim,
                     control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps)
      )
      pars <- doOpt$par
    } else {
      # single stock/fleet
      q1 <- OMListSim$Catchability[1,1,1]
      if (q1>tiny)
        return(OMListSim)
      
      doOpt <- optimize(OptCatchability,
                        log(bounds), 
                        OMListSim=OMListSim,
                        tol=tol)
      pars <- doOpt$minimum
      
    }
    
    qStock <- exp(pars[1:nStock])
    qFleet <- matrix(1, nStock, nFleet)
    
    if (nFleet > 1) {
      qlogit <- matrix(0, nStock, nFleet)
      qlogit[, 2:nFleet] <- pars[(nStock+1):length(pars)]
      qFleet <- ilogitm(qlogit)
    }
    
    for (st in 1:nStock) {
      for (fl in 1:nFleet) {
        OMListSim$Catchability[st, , fl] <- qStock[st] * qFleet[st,fl]
      }
    }
    return(OMListSim)
    
  }, .progress = list(
    type = "iterator", 
    format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  
  OMList
}

OptCatchability <- function(pars, OMListSim) {
  
  nStock <- length(OMListSim$Ages)
  nFleet <- dim(OMListSim$FishingMortality$DeadAtAge[[1]])[3]
  
  DepletionTarget <- OMListSim$DepletionFinal
  DepletionReference <- OMListSim$DepletionReference
  
  if (length(DepletionTarget)!= nStock)
    cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Final`")
  
  if (length(DepletionReference)!= nStock)
    cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Reference`")
  
  qStock <- exp(pars[1:nStock])
  qFleet <- matrix(1, nStock, nFleet)
  
  if (nFleet > 1) {
    qlogit <- matrix(0, nStock, nFleet)
    qlogit[, 2:nFleet] <- pars[(nStock+1):length(pars)]
    qFleet <- ilogitm(qlogit)
  }
  
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      OMListSim$Catchability[st, , fl] <- qStock[st] * qFleet[st,fl]
    }
  }
  
  TimeStepsAll <- OMListSim$TimeSteps
  TimeStepsHist <- OMListSim$TimeStepsHist
  TermInd <- match(max(TimeStepsHist), TimeStepsAll)
  
  PopDynamicsHistorical <- SimulateFisheryDynamics_(OMListSim, 
                                                    TimeStepsHist, 
                                                    MP=NULL, 
                                                    CalcCatch = 0)

  # Depletion objective
  PredDep <- rep(NA, nStock)
  
  for (st in 1:nStock) {
    ref <- DepletionReference[st]
    if (!ref %in% c('B0', 'SB0'))
      cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
    RefVal <- OMListSim[[ref]][st,TermInd]
    var <- switch(ref, B0='Biomass', SB0="SBiomass")
    PredDep[st] <- PopDynamicsHistorical[[var]][st,TermInd]/RefVal
  }
  
  depOBJ <- sum(log(PredDep/DepletionTarget)^2)
  
  # TODO need to do SPFrom for Depletion sharing ---
  
  
  if (nFleet>1) {
    # Catch objective
    terminalLandings <- CalcCatch_(PopDynamicsHistorical, max(TimeStepsHist))
    predCatchFrac <- purrr::map(terminalLandings$RetainBiomassAtAge, \(x) 
                                apply(abind::adrop(x[,TermInd,, drop=FALSE],2), 2, sum)) |> 
      List2Array("Stock", "Fleet") |> 
      aperm(c('Stock', 'Fleet'))
    total <- matrix(apply(predCatchFrac, 1, sum), nrow=nStock, ncol=nFleet)
    total[total==0] <- tiny
    predCatchFrac <- predCatchFrac/total
    
    # Lazy - should be: sum(log(CFc[,2:nf]/Cpred[,2:nf])^2) but this doesn't work for single fleets and it makes no difference anyway
    cOBJ <- sum(log(OMListSim$CatchFrac/predCatchFrac)^2) 
    depOBJ <- depOBJ+cOBJ
  }
  depOBJ

}




