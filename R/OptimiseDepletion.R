# Optimizes catchability to match specified terminal depletion and fleet catch fractions

OptimizeCatchability <- function(HistSim) {
  
  # TODO add qinc, qvar etc for historical and projection
  
  bounds <- c(1e-02, 3)
  tol <- 1E-5
  silent <- TRUE
  
  nStock <- nStock(HistSim@OM)
  nFleet <- nFleet(HistSim@OM)
  
  TimeStepsHist <-TimeSteps(HistSim@OM, 'Historical')
  
  if (nStock > 1 || nFleet > 1) {
    
    # multi stock/fleet
    QMatrix <- matrix(0, nStock, nFleet) 
    for (st in 1:nStock) {
      QMatrix[st,] <- apply(HistSim@OM@Fleet[[st]]@Effort@Catchability, 2, max)
    }

    if (all(QMatrix>1E-5))
      return(HistSim)
     
    # Catch divided by effort (q proxy)
    CatchFrac <- List2Array(HistSim@OM@CatchFrac, dimname = 'Stock') |> t()
    EffortFleet <- array(NA, dim=dim(CatchFrac))
    nTS <- length(TimeStepsHist)
    for (st in 1:nStock) {
      EffortFleet[st,] <- HistSim@OM@Fleet[[st]]@Effort@Effort[nTS]
    }
    
    FDist <- CatchFrac/EffortFleet
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
                   HistSim=HistSim,
                   TimeStepsHist=TimeStepsHist,
                   control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps)
    )
    pars <- doOpt$par
  } else {
      # single stock/fleet
      q1 <- HistSim@OM@Fleet[[1]]@Effort@Catchability[1,1]
      if (q1>tiny)
        return(HistSim)
      
      doOpt <- stats::optimize(OptCatchability,
                        log(bounds),
                        HistSim=HistSim,
                        TimeStepsHist=TimeStepsHist,
                        tol=tol)
      pars <- doOpt$minimum
      
    
      # qs <- seq(0.01, 0.5, length.out=100)
      # obj <- rep(NA, 100)
      # for (i in seq_along(qs)) {
      #   obj[i] = OptCatchability(log(qs[i]), HistSim)
      # }
      # 
      # plot(qs, obj)
      # qs[which.min(obj)]
      
      
      if (any(abs(exp(pars) - bounds) < 0.01)) {
        # more robust than optimize but slower
        doOpt <- stats::nlminb(mean(log(bounds)),
                               OptCatchability,
                               HistSim=HistSim,
                               TimeStepsHist=TimeStepsHist, 
                               lower=log(bounds[1]),
                               upper=log(bounds[2]))
        pars <- doOpt$par
      }
      
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
        HistSim@OM@Fleet[[st]]@Effort@Catchability[, fl] <- qStock[st] * qFleet[st,fl]
      }
    }
  
    HistSim
}

OptCatchability <- function(pars, HistSim, TimeStepsHist, debug=FALSE) {

  nStock <- nStock(HistSim@OM)
  nFleet <- nFleet(HistSim@OM) 

  qStock <- exp(pars[1:nStock])
  qFleet <- matrix(1, nStock, nFleet)
  
  DepletionTarget <- unlist(lapply(HistSim@OM@Stock, \(stock) stock@Depletion@Final))
  DepletionReference <- unlist(lapply(HistSim@OM@Stock, \(stock) stock@Depletion@Reference))
  
  if (length(DepletionTarget)!= nStock)
    cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Final`")
  
  if (length(DepletionReference)!= nStock)
    cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Reference`")
  
  if (nFleet > 1) {
    qlogit <- matrix(0, nStock, nFleet)
    qlogit[, 2:nFleet] <- pars[(nStock+1):length(pars)]
    qFleet <- ilogitm(qlogit)
  }
 
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      # TODO add qinc and qcv here
      HistSim@OM@Fleet[[st]]@Effort@Catchability[,fl] <- qStock[st] * qFleet[st,fl]
    }
  }
    
  TermInd <-  length(TimeStepsHist)
  PopDynamicsHistorical <- SimulateDynamics_(HistSim,
                                             TimeStepsHist,
                                             CalcCatch = 0)
 
  # Depletion objective
  PredDep <- rep(NA, nStock)
  for (st in 1:nStock) {
    ref <- DepletionReference[st]
    if (!ref %in% c('B0', 'SB0'))
      cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
    
    if (ref =='B0') {
      RefVal <- HistSim@Unfished@Equilibrium@Biomass[st,TermInd]
      PredDep[st] <- PopDynamicsHistorical@Biomass[st,TermInd]/RefVal
    } else {
      RefVal <- HistSim@Unfished@Equilibrium@SBiomass[st,TermInd]
      PredDep[st] <- PopDynamicsHistorical@SBiomass[st,TermInd]/RefVal
    }
  }
  
  depOBJ <- sum(log(PredDep/DepletionTarget)^2)
  
  if (debug) {
    print("*******************")
    print(paste("Pars = ", exp(pars)))
    print(paste("Depletion  = ", PredDep))
    print(paste("DepletionTarget  = ", DepletionTarget))
    print(paste("Objective = ", depOBJ))
    print("*******************")
    cat("\n")
    return(PopDynamicsHistorical)
  }
  
  # TODO need to do SPFrom for Depletion sharing ---
  if (nFleet>1) {
    # Catch objective
    terminalLandings <- CalcCatch_(PopDynamicsHistorical, max(TimeStepsHist))
    predCatchFrac <- purrr::map(terminalLandings@Landings, \(x) 
                                apply(x[[TermInd]],2, sum)) |> 
      List2Array("Stock", "Fleet") |> 
      aperm(c('Stock', 'Fleet'))
    total <- matrix(apply(predCatchFrac, 1, sum), nrow=nStock, ncol=nFleet)
    total[total==0] <- tiny
    predCatchFrac <- predCatchFrac/total
    
    # Lazy - should be: sum(log(CFc[,2:nf]/Cpred[,2:nf])^2) but this doesn't work for single fleets and it makes no difference anyway
    CatchFrac <- List2Array(HistSim@OM@CatchFrac, dimname = 'Stock') |> t()
    cOBJ <- sum(log(CatchFrac/predCatchFrac)^2) 
    depOBJ <- depOBJ+cOBJ
  }
  
  depOBJ

}






