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
#' @param minEffort Small numeric to replace zero starting effort (default 1e-6)
#' @param tol Numeric, convergence tolerance for derivative solver (default 1e-6)
#' @param maxIter Integer, maximum iterations for derivative solver (default 20)
#' @return Numeric vector of optimized effort per fleet
#' @keywords internal
OptEffort <- function(Proj, Year, TSIndex, sim, stocks, TAC_by_Fleet,
                      minEffort = 1e-6, tol = 1e-2, maxIter = 50) {
  
  nFleet <- length(TAC_by_Fleet)
  
  # initial effort, replace zeros with a small value
  Effort_init <- Proj@Effort[sim, TSIndex-1, ]  # start at previous time step
  Effort_init[Effort_init < minEffort] <- minEffort
  
  # identify fleets to optimize vs. fleets forced to zero
  pos_idx <- which(TAC_by_Fleet > 0)
  zero_idx <- which(TAC_by_Fleet == 0)
  
  # Initialize final effort
  Effort_final <- Effort_init
  if (length(zero_idx) > 0) Effort_final[zero_idx] <- 0
  
  # skip if no fleets to optimize
  if (length(pos_idx) == 0) return(Effort_final)
  
  # nFleet = 1: use optimize
  if (length(pos_idx) == 1) {
    f <- pos_idx
    obj <- function(logEff) ObjEffort(logEff, Proj, sim, Year, TSIndex, stocks, TAC_by_Fleet, Effort_final)
    opt <- optimize(obj, interval = log(c(minEffort, Effort_final[f] * 10)))
    Effort_final[f] <- Effort_final[f] * exp(opt$minimum)
    return(Effort_final)
  }
  
  # multiple fleets: Newton-Raphson
  Effort <- Effort_final
  converged <- FALSE
  
  for (iter in seq_len(maxIter)) {
    Proj@Effort[sim, TSIndex, ] <- Effort
    Temp <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim, DoCalcaggF = FALSE)
    Removals <- Temp@Landings[sim, stocks, TSIndex, ] + Temp@Discards[sim, stocks, TSIndex, ]
    RemovalsByFleet <- colSums(Removals[, , drop = FALSE])
    diff <- TAC_by_Fleet[pos_idx] - RemovalsByFleet[pos_idx]
    
    if (all(abs(diff) < tol)) {
      converged <- TRUE
      break
    }

    # cbind(TAC_by_Fleet, RemovalsByFleet)
    
    deltaF <- pmax(Effort[pos_idx] * 1e-4, 1e-8)
    Effort_pert <- Effort
    Effort_pert[pos_idx] <- Effort[pos_idx] + deltaF
    Proj@Effort[sim, TSIndex, ] <- Effort_pert
    
    Temp_pert <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim, DoCalcaggF = FALSE)
    Removals_pert <- Temp_pert@Landings[sim, stocks, TSIndex, ] + Temp_pert@Discards[sim, stocks, TSIndex, ]
    RemovalsByFleet_pert <- colSums(Removals_pert[, , drop = FALSE])
    
    # approximate derivative (dC/dF) vector
    dC_dF <- (RemovalsByFleet_pert[pos_idx] - RemovalsByFleet[pos_idx]) / deltaF
    dC_dF[dC_dF <= 0] <- 1e-8
    
    Effort[pos_idx] <- pmax(Effort[pos_idx] + diff / dC_dF, minEffort)
  }
  
  # fallback to optim if derivative solver fails
  if (!converged) {
    objFun <- function(logEffortVec) {
      ObjEffort(logEffortVec, Proj, sim, Year, TSIndex, stocks, TAC_by_Fleet, Effort_final)
    }
    
    opt <- optim(
      par = rep(0, length(pos_idx)),
      fn = objFun,
      method = "BFGS",
      control = list(maxit = 100)
    )
    
    Effort[pos_idx] <- Effort[pos_idx] * exp(opt$par)
  }
  
  Effort_final[pos_idx] <- Effort[pos_idx]
  Effort_final
}

#' Objective function to match removals to TAC by fleet
#'
#' @param logEffortVec Numeric vector of log-scaled effort (length = number of fleets with positive TAC)
#' @param Proj Projection object
#' @param sim Integer, simulation index
#' @param TSIndex Integer, time step index
#' @param stocks Integer vector of stocks to consider
#' @param TAC_by_Fleet Numeric vector of TAC by fleet
#' @param Effort_init Numeric vector of initial effort
#' @return Sum of squared log differences between TAC and projected removals
#' @keywords internal
ObjEffort <- function(logEffortVec, Proj, sim, Year, TSIndex, stocks, TAC_by_Fleet, Effort_init) {
  
  pos_idx  <- which(TAC_by_Fleet > 0)
  zero_idx <- which(TAC_by_Fleet == 0)
  
  Effort <- Effort_init
  if (length(pos_idx) == 1) {
    Effort[pos_idx] <- Effort_init[pos_idx] * exp(logEffortVec)
  } else if (length(pos_idx) > 1) {
    Effort[pos_idx] <- Effort_init[pos_idx] * exp(logEffortVec)
  }
  
  if (length(zero_idx) > 0) {
    Effort[zero_idx] <- 0
  }
  
  Proj@Effort[sim, TSIndex, ] <- Effort
  
  Temp <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim, DoCalcaggF=FALSE)
  
  Removals <- Temp@Landings[sim, stocks, TSIndex, ] +  Temp@Discards[sim, stocks, TSIndex, ]
  RemovalsByFleet <- colSums(Removals[, , drop = FALSE])
  
  sum((log(TAC_by_Fleet[pos_idx]) - log(RemovalsByFleet[pos_idx]))^2)
}


