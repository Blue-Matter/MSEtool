
#' Objective function to match removals to TAC by fleet
#'
#' @param logEffortVec Numeric vector of log-scaled effort (length = number of fleets with positive TAC)
#' @param Proj Projection object
#' @param sim Integer, simulation index
#' @param TSIndex Integer, time step index
#' @param stocks Integer vector of stocks to consider
#' @param TAC_by_Fleet Numeric vector of TAC by fleet
#' @param Effort_init Numeric vector of initial effort
#' @param TACType Character. Does the TAC refer to `"Removals"` (default) or `"Landings"`.
#' @return Sum of squared log differences between TAC and projected removals
#' @keywords internal
ObjEffort <- function(logEffortVec, Proj, sim, Year, TSIndex, stocks, 
                      TAC_by_Fleet, Effort_init, TACType=c('Removals', 'Landings')) {
  
  TACType  <- match.arg(TACType)
  
  pos_idx  <- which(TAC_by_Fleet > 0)
  zero_idx <- which(TAC_by_Fleet == 0)
  
  Effort <- Effort_init
  Effort[pos_idx] <- Effort_init[pos_idx] * exp(logEffortVec)
  if (length(zero_idx) > 0) Effort[zero_idx] <- 0
  
  Proj@Effort[sim, TSIndex, ] <- Effort
  
  Temp <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)

  CatchByFleet <- CalcCatchByFleet(Temp, sim, stocks, TSIndex, TACType)
  
  catch <- CatchByFleet[pos_idx]
  tac   <- TAC_by_Fleet[pos_idx]
  
  if (any(!is.finite(catch)) || any(catch <= 0)) {
    return(sum(log(tac)^2) * 10)
  }
  
  log_resid <- log(tac) - log(catch)
  ss <- sum(log_resid^2)
  
  abs_penalty <- sum(((tac - catch) / tac)^2)
  ss + 0.01 * abs_penalty
  
  # out <- sum((log(TAC_by_Fleet[pos_idx]) - log(CatchByFleet[pos_idx]))^2)
  # out
}

#' Extract catch by fleet from a fishery dynamics object
#'
#' @param Temp     Output of `CalcFisheryDynamics()`
#' @param sim      Simulation index
#' @param stocks   Stock indices
#' @param TSIndex  Time-step index
#' @param TACType  `"Removals"` or `"Landings"`
#' @return Named numeric vector of catch per fleet
#' @keywords internal
CalcCatchByFleet <- function(Temp, sim, stocks, TSIndex, TACType) {
  Catch <- if (TACType == "Removals") {
    Temp@Landings[sim, stocks, TSIndex, ] + Temp@Discards[sim, stocks, TSIndex, ]
  } else {
    Temp@Landings[sim, stocks, TSIndex, ]
  }
  if (is.null(ncol(Catch))) Catch else colSums(Catch[, , drop = FALSE])
}