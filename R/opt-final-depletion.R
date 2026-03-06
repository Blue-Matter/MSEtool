
#' Optimize Catchability to Match Final Depletion
#'
#' Internal function to adjust fleet-specific catchability (q) to match
#' the target final depletion for each stock in the historical operating model.
#' Works for single-stock and single-fleet models. Multi-stock/fleet
#' optimization is not yet supported.
#'
#' @param Hist A `Hist` object.
#' @param silent Logical; if `TRUE`, suppress progress output.
#'
#' The function loops over all simulations (`nSim`) in `Hist` and performs
#' numeric optimization to scale the initial catchability to achieve the
#' desired final depletion specified in `Stock@Depletion@Final`.
#' 
#' Uses `stats::optimize` with a fallback to `nlminb` if results are
#' near bounds. 
#' 
#' Currently, only single-stock and single-fleet scenarios
#' are supported.
#' 
#' @return A modified [Hist()] object with optimized `Catchability@Efficiency`
#'   in each fleet of the operating model.
#' 
#' @keywords internal
OptFinalDepletion <- function(Hist, silent=FALSE) {
  
  FinalDepletion <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Depletion@Final
  }) 
  
  if (!length(unlist(FinalDepletion)))  return(Hist)
  
  nStock <- nStock(Hist@OM)
  nFleet <- nFleet(Hist@OM)
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist)
  YearsHist <- Years(Hist@OM, 'Historical')
  
  # List length nSim, each with a Hist object with 1 sim
  HistSim_List <- lapply(seq_len(nSim), function(i) Subset(Hist, i))
  
  # HistSim <- HistSim_List[[1]] # for debugging
  
  opt_q <- if (silent) {
    lapply(HistSim_List, OptFinalDepletion_Sim, nStock, nFleet, nArea, YearsHist)
  } else {
    purrr::map(HistSim_List, \(HistSim) {
      OptFinalDepletion_Sim(HistSim, nStock, nFleet, nArea, YearsHist)
    }, .progress = list(
      type = "iterator",
      format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
      clear = TRUE))  
  }
  
  
  # Update Hist object
  for (s in seq_along(opt_q)) {
    for (st in 1:nStock) {
      for (fl in 1:nFleet) {
        Hist@OM@Fleet[[st]][[fl]]@Catchability@Efficiency[s,] <- opt_q[[s]][[st]][[fl]]@Catchability@Efficiency[1,]
        Hist@Misc$Catchability[s,st,, fl] <- opt_q[[s]][[st]][[fl]]@Catchability@Efficiency[1,]
      }
    }
  }
  if (!silent) cli::cli_alert_success("Optimized catchability (q) for Final Depletion")
  
  Hist
}


OptFinalDepletion_Sim <- function(HistSim, nStock, nFleet, nArea, YearsHist) {
  
  bounds <- c(1e-02, 3)
  tol <- 1E-5
  silent <- TRUE
  
  
  if (nStock > 1 || nFleet > 1) {
    pars <- OptimizeCatchability_Multi(HistSim, nStock, nFleet, nArea, YearsHist, bounds, tol, silent, debug)
    if (inherits(pars, 'hist'))
      return(pars)
  } else {
    pars <- OptimizeCatchability_Single(HistSim, nStock, nFleet, nArea, YearsHist, bounds, tol, silent, debug)
    if (inherits(pars, 'hist'))
      return(pars)
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
      StCatchability <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
      StCatchability <- StCatchability/StCatchability[1]
      HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency <- StCatchability * qStock[st] * qFleet[st,fl]
    }
  }
  
  HistSim@OM@Fleet
}

OptimizeCatchability_Single <- function(HistSim, nStock, nFleet, nArea, YearsHist, bounds, tol, silent, debug=FALSE) {
  
  FinalDepletion <- HistSim@OM@Stock[[1]]@Depletion@Final
  
  if (!length(FinalDepletion))
    return(HistSim)
  
  doOpt <- stats::optimize(OptCatchability,
                           log(bounds),
                           Hist=HistSim,
                           nStock=nStock,
                           nFleet=nFleet,
                           nArea=nArea,
                           YearsHist=YearsHist,
                           debug=debug,
                           tol=tol)
  pars <- doOpt$minimum
  
  if (any(abs(exp(pars) - bounds) < 0.01)) {
    # more robust than optimize but slower
    doOpt <- stats::nlminb(mean(log(bounds)),
                           OptCatchability,
                           Hist=HistSim,
                           nStock=nStock,
                           nFleet=nFleet,
                           nArea=nArea,
                           YearsHist=YearsHist,
                           debug=debug,
                           lower=log(bounds[1]),
                           upper=log(bounds[2]))
    pars <- doOpt$par
  }
  pars
  
}

OptCatchability <- function(pars, HistSim, nStock, nFleet, nArea, YearsHist, CatchFrac=NULL, debug=FALSE) {
  
  qStock <- exp(pars[1:nStock])
  qFleet <- matrix(1, nStock, nFleet)
  
  DepletionTarget <- purrr::map(HistSim@OM@Stock, \(stock) {
    stock@Depletion@Final
  }) |> unlist()
  
  DepletionReference <- purrr::map(HistSim@OM@Stock, \(stock) {
    stock@Depletion@Reference
  }) |> unlist()
  
  
  if (length(DepletionTarget)!= nStock)
    cli::cli_abort("`Depletion@Final` not set for all Stocks")
  
  if (length(DepletionReference)!= nStock)
    cli::cli_abort("`Depletion@Reference` not set for all Stocks")
  
  if (nFleet > 1) {
    qlogit <- matrix(0, nStock, nFleet)
    qlogit[, 2:nFleet] <- pars[(nStock+1):length(pars)]
    qFleet <- ilogitm(qlogit)
  }
  
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      StCatchability <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
      StCatchability <- StCatchability/StCatchability[1]
      # HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency <- StCatchability * qStock[st] * qFleet[st,fl]
      
      # update Misc used in C++
      HistSim@Misc$Catchability[1,st,,fl] <- StCatchability * qStock[st] * qFleet[st,fl]
    }
  }
  
  DoCalcCatch <- 0 
  if (nFleet>1) DoCalcCatch <- 1
  
  PopDynamicsHistorical <- CalcFisheryDynamics_(HistSim, 
                                                Years=YearsHist,
                                                AllYears=YearsHist,
                                                Sims=1,
                                                nSim=1,
                                                nStock,
                                                nFleet,
                                                nArea,
                                                DoCalcCatch=DoCalcCatch,
                                                clone=1)
  

  # Depletion objective
  TermInd <- length(YearsHist)
  
  PredDep <- rep(NA, nStock)
  for (st in 1:nStock) {
    ref <- DepletionReference[st]
    if (!ref %in% c('B0', 'SB0'))
      cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
    
    if (ref =='B0') {
      RefVal <- HistSim@Unfished@Equilibrium@Biomass[1, st,TermInd]
      PredDep[st] <- PopDynamicsHistorical@Biomass[1, st,TermInd]/RefVal
    } else {
      RefVal <- HistSim@Unfished@Equilibrium@SBiomass[1, st,TermInd]
      PredDep[st] <- PopDynamicsHistorical@SBiomass[1, st,TermInd]/RefVal
    }
  }
  
  depOBJ <- sum(log(PredDep/DepletionTarget)^2)
  
  if (nFleet==1) {
    return(depOBJ)
  }
  
  # TODO need to do SPFrom for Depletion sharing ---
  
  # Catch objective
  predCatchFrac <- PopDynamicsHistorical@Landings[1,,length(YearsHist), ,drop=FALSE] |>
    abind::adrop(c(1,3))
  
  total <- matrix(apply(predCatchFrac, 1, sum), nrow=nStock, ncol=nFleet)
  total[total==0] <- tiny
  predCatchFrac <- predCatchFrac/total
  
  # Lazy - should be: sum(log(CFc[,2:nf]/Cpred[,2:nf])^2) but this doesn't work for single fleets and it makes no difference anyway
  cOBJ <- sum(log(CatchFrac/predCatchFrac)^2) 
  depOBJ <- depOBJ+cOBJ
  
  depOBJ
  
}


OptimizeCatchability_Multi <- function(HistSim, nStock, nFleet, nArea, YearsHist, bounds, tol, silent, debug=FALSE) {
  
  FinalDepletion <- purrr::map(HistSim@OM@Stock, \(stock) stock@Depletion@Final) |>
    List2Array('Stock')
  
  if (!length(FinalDepletion))
    return(HistSim)
  
  CalcCatchFrac <- FALSE
  if (is.null(HistSim@OM@CatchFrac)) 
    CalcCatchFrac <- TRUE
    
  if (is.list(HistSim@OM@CatchFrac) && any(lapply(HistSim@OM@CatchFrac, is.null) |> unlist()))
    CalcCatchFrac <- TRUE
  
  # Catch divided by effort (q proxy)
  if (CalcCatchFrac) {
    HistSim@OM@CatchFrac <- MakeNamedList(StockNames(HistSim@OM))
    for (st in 1:nStock) {
      relF <- rep(NA, nFleet)
      for (fl in 1:nFleet) {
        effort <- HistSim@OM@Fleet[[st]][[fl]]@Effort@Effort
        q <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
        relF[fl] <- effort[1,ncol(effort)] * q[1,ncol(q)]
      }
      HistSim@OM@CatchFrac[[st]] <-   relF/sum(relF)
    }
  }
  
  CatchFrac <- List2Array(HistSim@OM@CatchFrac, name = 'Stock', pos=2) |>
    DropDimension('Sim', FALSE)
  EffortFleet <- array(NA, dim=dim(CatchFrac))
  nTS <- length(YearsHist)
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      EffortFleet[st,fl] <- HistSim@OM@Fleet[[st]][[fl]]@Effort@Effort[1,nTS]  
    }
    
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
                 HistSim = HistSim,
                 nStock = nStock,
                 nFleet = nFleet,
                 nArea = nArea,
                 YearsHist=YearsHist,
                 CatchFrac = CatchFrac,
                 debug=debug,
                 control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps)
  )
  pars <- doOpt$par
  pars
}



