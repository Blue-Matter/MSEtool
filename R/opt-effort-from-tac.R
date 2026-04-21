#' Optimize Fleet Effort to Match TAC
#'
#' Calculates fleet-specific effort that achieves target removals (TAC) per
#' fleet. Fleets with `TAC == 0` are set to zero effort. Fleets with positive
#' TAC are solved using `optimize()` for a single fleet or Newton-Raphson with
#' a diagonal Jacobian approximation for multiple fleets, falling back to the
#' full Jacobian if the diagonal step fails to reduce the residual, and then to
#' `optim()` with BFGS if Newton-Raphson does not converge.
#'
#' @param Proj `Hist` object containing projected effort and fishery dynamics
#' @param Year Numeric vector of projection years for this step
#' @param TSIndex Integer, time-step index corresponding to `Year`
#' @param sim Integer, simulation index
#' @param stocks Integer vector of stock indices to consider
#' @param TAC_by_Fleet Numeric vector of target TAC per fleet
#' @param TACType Character. Does the TAC refer to `"Removals"` (default) or `"Landings"`.
#' @param minEffort Small numeric to replace zero starting effort (default 1e-6)
#' @param tol Numeric, convergence tolerance for derivative solver (default 1e-2)
#' @param maxIter Integer, maximum iterations for derivative solver (default 50)
#' @return Numeric vector of optimized effort per fleet
#' @keywords internal
OptEffort <- function(Proj, Year, TSIndex, sim, stocks, TAC_by_Fleet, TACType,
                      minEffort = 1e-15, tol = 1e-2, maxIter = 200) {
  
  TACType <- match.arg(TACType, c('Removals', 'Landings'))
  
  pos_idx  <- which(TAC_by_Fleet > 0)
  zero_idx <- which(TAC_by_Fleet == 0)
  
  Effort <- pmax(Proj@Effort[sim, TSIndex - 1, ], minEffort)
  Effort[zero_idx] <- 0
  Effort_final <- Effort  # snapshot before any NR modification
  
  if (length(pos_idx) == 0) return(Effort)
  
  # Single fleet: optimize()
  if (length(pos_idx) == 1) {
    obj <- function(logEff) {
      ObjEffort(logEff, Proj, sim, Year, TSIndex, stocks,
                TAC_by_Fleet, Effort_final, TACType)
    }
    upper_bound <- min(max(Effort[pos_idx]) * 1e3, .Machine$double.xmax)
    opt <- optimize(obj, interval = log(c(minEffort, upper_bound)))
    Effort[pos_idx] <- Effort_final[pos_idx] * exp(opt$minimum)
    return(Effort)
  }
  
  # Multi-fleet: Newton-Raphson 
  converged <- FALSE
  
  for (iter in seq_len(maxIter)) {
    
    Proj@Effort[sim, TSIndex, ] <- Effort
    Temp         <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
    CatchByFleet <- CalcCatchByFleet(Temp, sim, stocks, TSIndex, TACType)
    
    residual <- TAC_by_Fleet[pos_idx] - CatchByFleet[pos_idx]
    if (all(abs(residual) < tol)) {
      converged <- TRUE
      break
    }
    
    # Diagonal Jacobian: perturb all feasible fleets simultaneously 
    deltaF      <- pmax(Effort[pos_idx] * 1e-4, 1e-8)
    Effort_pert <- Effort
    Effort_pert[pos_idx] <- Effort[pos_idx] + deltaF
    
    Proj@Effort[sim, TSIndex, ] <- Effort_pert
    Temp_pert         <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
    CatchByFleet_pert <- CalcCatchByFleet(Temp_pert, sim, stocks, TSIndex, TACType)
    
    J_diag <- (CatchByFleet_pert[pos_idx] - CatchByFleet[pos_idx]) / deltaF
    J_diag[J_diag <= 0] <- 1e-8
    
    Eff_candidate <- pmax(Effort[pos_idx] + residual / J_diag, minEffort)
    
    # Accept diagonal step if it reduces the residual; otherwise fall back to
    # full Jacobian for this iteration
    Proj@Effort[sim, TSIndex, ]        <- Effort
    Proj@Effort[sim, TSIndex, pos_idx] <- Eff_candidate
    Temp_cand         <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
    CatchByFleet_cand <- CalcCatchByFleet(Temp_cand, sim, stocks, TSIndex, TACType)
    residual_cand     <- TAC_by_Fleet[pos_idx] - CatchByFleet_cand[pos_idx]
    
    if (sum(residual_cand^2) <= sum(residual^2)) {
      Effort[pos_idx] <- Eff_candidate
      next
    }
    
    # Full Jacobian: J[i, j] = dCatch_i / dEffort_j
    # Perturb each feasible fleet individually to capture cross-fleet effects
    nF <- length(pos_idx)
    J  <- matrix(0, nrow = nF, ncol = nF)
    diag(J) <- J_diag  # reuse already-computed diagonal
    
    for (k in seq_len(nF)) {
      fleet_k    <- pos_idx[k]
      deltaF_k   <- max(Effort[fleet_k] * 1e-4, 1e-8)
      Effort_pert <- Effort
      Effort_pert[fleet_k] <- Effort[fleet_k] + deltaF_k
      
      Proj@Effort[sim, TSIndex, ] <- Effort_pert
      Temp_pert <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
      CatchByFleet_pert <- CalcCatchByFleet(Temp_pert, sim, stocks, TSIndex, TACType)
      
      J[, k] <- (CatchByFleet_pert[pos_idx] - CatchByFleet[pos_idx]) / deltaF_k
    }
    
    # Newton step
    delta <- tryCatch(
      solve(J, residual),
      error = function(e) {
        residual / J_diag # singular — diagonal fallback
      }
    )
    
    Effort[pos_idx] <- pmax(Effort[pos_idx] + delta, minEffort)
  }
  
  # fallback to optim if derivative solver fails
  if (!converged) {
    
    # cli::cli_alert_warning(
    #   "OptEffort: Newton-Raphson did not converge after {maxIter} iterations \\
    #   (sim={sim}, TSIndex={TSIndex}). Falling back to optim()."
    # )
    
    objFun <- function(logEffortVec) {
      ObjEffort(logEffortVec, Proj, sim, Year, TSIndex, stocks,
                TAC_by_Fleet, Effort_final, TACType)
    }

    par_init <- log(Effort[pos_idx] / Effort_final[pos_idx])
    # par is log-scale offset from Effort_final; 0 => start at Effort_final
    par_init[!is.finite(par_init)] <- 0
    
    opt <- optim(
      par     = par_init,
      fn      = objFun,
      method  = "BFGS",
      control = list(maxit = 200)
    )

    Effort[pos_idx] <- Effort_final[pos_idx] * exp(opt$par)
    
  }
  Effort
}




