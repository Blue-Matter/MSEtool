
OptimizeCatchability <- function(OMList) {
  
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
      
      if (all(QMatrix>tiny))
        return(OMListSim)
      
      
      
      # Catch divided by effort (q proxy)
      FDist <- OMListSim$CatchFrac/OMListSim$Effort[,,length(OMListSim$TimeStepsHist[[1]])]
      FDist[!is.finite(FDist)] <- tiny
      FDist <- FDist/apply(FDist[, , drop = FALSE], 1, sum)    # q ratio proxy (real space)
      
  
      if (nFleet == 1) {
        pars <- rep(-5, nStock)
      } else {
        # low initial F followed by logit guess at fraction based on Fdist
        # according to catch fraction in recent year
        pars <- c(rep(-5,nStock), logit(FDist[, 2:nFleet]))
      }
      
      
      OptCatchabilityMulti <- function(pars, OMListSim) {
        
        nStock <- length(OMListSim$Ages)
        nFleet <- dim(OMListSim$FishingMortality$DeadAtAge[[1]])[3]
        
        DepletionTarget <- OMListSim$DepletionFinal
        DepletionReference <- OMListSim$DepletionReference
        
        if (length(DepletionTarget)!= nStock)
          cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Final`")
        
        if (length(DepletionReference)!= nStock)
          cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Reference`")
        
        qStock <- exp(pars[1:nStock])
        if (nFleet == 1) {
          qFleet <- matrix(1, nrow = qStock, 1)
        } else {
          qlogit <- matrix(0, nStock, nFleet)
          qlogit[, 2:nf] <- pars[(nStock+1):length(pars)]
          qFleet <- ilogitm(qlogit)
        }
        
        for (st in 1:nStock) {
          for (fl in 1:nFleet) {
            OMListSim$Catchability[st, fl, ] <- qStock[st] * qFleet[st,fl]
          }
        }
        
        
        
        
      
        
        
        TimeStepsAll <- OMListSim$TimeSteps[[1]]
        TimeStepsHist <- OMListSim$TimeStepsHist[[1]]
        TermInd <- match(max(TimeStepsHist), TimeStepsAll)
        
        PopDynamicsHistorical <- CalcPopDynamics_(OMListSim, TimeStepsHist)
        
        
        if (DepletionReference=='B0') {
          Bterminal <- PopDynamicsHistorical$Biomass[[1]][TermInd]
          depRef <- OMListSim$B0[[1]][TermInd]  
        } else if (DepletionReference=='SB0') {
          Bterminal <- PopDynamicsHistorical$SBiomass[[1]][TermInd]
          depRef <- OMListSim$SB0[[1]][TermInd] 
        } else {
          cli::cli_abort("Currently only accepts`Depletion@Reference = 'B0' or 'SB0'")
        }
        
        sum((Bterminal/depRef - DepletionTarget)^2)
      }
      
      
      
      doOpt <- optim(pars,
                     OptCatchabilityMulti,
                     method = "L-BFGS-B",
                     lower = c(rep(log(bounds[1]), nStock), rep(-5, nStock * (nFleet-1))),
                     upper = c(rep(log(bounds[2]), nStock), rep(5, nStock*(nFleet-1))),
                     OMListSim=OMListSim,
                     control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps)
                     )
      
      logQ <- doOpt$minimum
      qval <- exp(doOpt$minimum)
      
      # TODO add qinc, qvar etc
      OMListSim$Catchability[1,1,1] <- qval
      return(OMListSim)
      
      
    } else {
      # single stock/fleet
      q1 <- OMListSim$Catchability[1,1,1]
      if (q1>tiny)
        return(OMListSim)
      
      doOpt <- optimize(OptCatchabilitySingle,
                        log(bounds), 
                        OMListSim=OMListSim,
                        tol=tol)
      logQ <- doOpt$minimum
      qval <- exp(doOpt$minimum)
      
      # TODO add qinc, qvar etc
      OMListSim$Catchability[1,1,] <- qval
      return(OMListSim)
    }
  }, .progress = list(
    type = "iterator", 
    format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  
 
  OMList
}

OptCatchabilitySingle <- function(logQ, OMListSim) {
  
  stop()
  # fix 
 
  DepletionTarget <- OMListSim$DepletionFinal
  DepletionReference <- OMListSim$DepletionReference
  
  if (length(DepletionTarget)<1)
    cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Final`")

  OMListSim$Catchability[] <- exp(logQ)
  TimeStepsAll <- OMListSim$TimeSteps[[1]]
  TimeStepsHist <- OMListSim$TimeStepsHist[[1]]
  TermInd <- match(max(TimeStepsHist), TimeStepsAll)
  
  PopDynamicsHistorical <- CalcPopDynamics_(OMListSim, TimeStepsHist)


  if (DepletionReference=='B0') {
    Bterminal <- PopDynamicsHistorical$Biomass[[1]][TermInd]
    depRef <- OMListSim$B0[[1]][TermInd]  
  } else if (DepletionReference=='SB0') {
    Bterminal <- PopDynamicsHistorical$SBiomass[[1]][TermInd]
    depRef <- OMListSim$SB0[[1]][TermInd] 
  } else {
    cli::cli_abort("Currently only accepts`Depletion@Reference = 'B0' or 'SB0'")
  }
  
  sum((Bterminal/depRef - DepletionTarget)^2)
}



