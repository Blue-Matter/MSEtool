#' Optimize Fleet Effort to Match TAC
#'
#' Calculates fleet-specific effort that achieves target removals (TAC) per fleet.
#' Uses the derivative-based solver if multiple fleets, or direct optimization if a single fleet.
#' Falls back to `optim()` if convergence fails for multiple fleets.
#'
#' @param Proj `Hist` object containing projected effort and fishery dynamics
#' @param Year Numeric vector of projection years for this step
#' @param TSIndex Integer, time-step index corresponding to `Year`
#' @param sim Integer, simulation index
#' @param stocks Integer vector of stock indices to consider
#' @param TAC_by_Fleet Numeric vector of target TAC per fleet
#' @param TACType Character. Does the TAC refer to `"Removals"` (default) or `"Landings"`.
#' @param minEffort Small numeric to replace zero starting effort (default 1e-6)
#' @param tol Numeric, convergence tolerance for derivative solver (default 1e-6)
#' @param maxIter Integer, maximum iterations for derivative solver (default 20)
#' @return Numeric vector of optimized effort per fleet
#' @keywords internal
OptEffort <- function(Proj, Year, TSIndex, sim, stocks, TAC_by_Fleet, TACType,
                      minEffort = 1e-6, tol = 1e-2, maxIter = 50) {
  
  TACType <- match.arg(TACType, c('Removals', 'Landings'))
  nFleet <- length(TAC_by_Fleet)
  
  # Initial effort (replace zeros)
  LastEffort <- Proj@Effort[sim, TSIndex-1, ]
  Effort_init <- pmax(LastEffort, minEffort)
  
  # Pre-check: Check if TACs can be caught 
  MaxEffort <- CalcMaxEffort(Proj, sim, TSIndex, stocks, Year, TAC_by_Fleet)
  Proj@Effort[sim, TSIndex, ] <- MaxEffort
  Temp_max <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
  
  if (TACType == 'Removals') {
    MaxCatch <- Temp_max@Landings[sim, stocks, TSIndex, ] + Temp_max@Discards[sim, stocks, TSIndex, ]
  } else {
    MaxCatch <- Temp_max@Landings[sim, stocks, TSIndex, ]
  }
  MaxCatchByFleet <- if (is.null(ncol(MaxCatch))) MaxCatch else colSums(MaxCatch[, , drop=FALSE])
  
  # If max catch < TAC, return max effort immediately
  pos_idx <- which(TAC_by_Fleet > 0)
  zero_idx <- which(TAC_by_Fleet == 0)
  Effort_final <- Effort_init
  Effort_final[zero_idx] <- 0
  
  infeasible_idx <- pos_idx[MaxCatchByFleet[pos_idx] < TAC_by_Fleet[pos_idx]]
  if (length(infeasible_idx) > 0) {
    Effort_final[infeasible_idx] <- MaxEffort[infeasible_idx]
    pos_idx <- setdiff(pos_idx, infeasible_idx) # only optimize feasible fleets
  }
  
  # Skip if nothing feasible to optimize
  if (length(pos_idx) == 0) return(Effort_final)
  
  # Single fleet: optimize()
  if (length(pos_idx) == 1) {
    obj <- function(logEff) ObjEffort(logEff, Proj, sim, Year, TSIndex, stocks, TAC_by_Fleet, Effort_final, TACType)
    opt <- optimize(obj, interval = log(c(minEffort, MaxEffort[pos_idx])))
    Effort_final[pos_idx] <- Effort_final[pos_idx] * exp(opt$minimum)
    return(Effort_final)
  }
  
  # Multi-fleet: Newton-Raphson
  Effort <- Effort_final
  converged <- FALSE
  
  for (iter in seq_len(maxIter)) {
    Proj@Effort[sim, TSIndex, ] <- Effort
    Temp <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
    
    if (TACType=='Removals') {
      CatchByFleet <- Temp@Landings[sim, stocks, TSIndex, ] + Temp@Discards[sim, stocks, TSIndex, ]
    } else {
      CatchByFleet <- Temp@Landings[sim, stocks, TSIndex, ] 
    }
    CatchByFleet <- if (is.null(ncol(CatchByFleet))) CatchByFleet else colSums(CatchByFleet[, , drop=FALSE])
    
    diff <- TAC_by_Fleet[pos_idx] - CatchByFleet[pos_idx]
    
    # check convergence
    if (all(abs(diff) < tol)) {
      converged <- TRUE
      break
    }
    
    # approximate derivative
    deltaF <- pmax(Effort[pos_idx] * 1e-4, 1e-8)
    Effort_pert <- Effort
    Effort_pert[pos_idx] <- Effort[pos_idx] + deltaF
    Proj@Effort[sim, TSIndex, ] <- Effort_pert
    
    Temp_pert <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
    
    if (TACType=='Removals') {
      CatchByFleet_pert <- Temp@Landings[sim, stocks, TSIndex, ] + Temp@Discards[sim, stocks, TSIndex, ]
    } else {
      CatchByFleet_pert <- Temp@Landings[sim, stocks, TSIndex, ] 
    }
    CatchByFleet_pert <- if (is.null(ncol(CatchByFleet_pert))) CatchByFleet_pert else colSums(CatchByFleet_pert[, , drop=FALSE])
    
    dC_dF <- (CatchByFleet_pert[pos_idx] - CatchByFleet[pos_idx]) / deltaF
    dC_dF[dC_dF <= 0] <- 1e-8
    
    if (all(dC_dF < 1e-6)) {
      converged <- TRUE
      break
    }
    
    Effort[pos_idx] <- pmin(pmax(Effort[pos_idx] + diff / dC_dF, minEffort), MaxEffort[pos_idx])
  }
  

  # fallback to optim if derivative solver fails
  if (!converged) {
    objFun <- function(logEffortVec) {
      ObjEffort(logEffortVec, Proj, sim, Year, TSIndex, stocks, TAC_by_Fleet, Effort_final, TACType)
    }
    
    opt <- optim(
      par = rep(0, length(pos_idx)),
      fn = objFun,
      method = "BFGS",
      control = list(maxit = 100)
    )
    
    Effort[pos_idx] <- pmin(Effort[pos_idx] * exp(opt$par), MaxEffort[pos_idx])
  }
  Effort_final[pos_idx] <- Effort[pos_idx]
  Effort_final
}



