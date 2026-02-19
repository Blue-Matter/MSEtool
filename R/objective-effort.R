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
  
  TACType <- match.arg(TACType)
  
  pos_idx  <- which(TAC_by_Fleet > 0)
  zero_idx <- which(TAC_by_Fleet == 0)
  
  Effort <- Effort_init
  Effort[pos_idx] <- Effort_init[pos_idx] * exp(logEffortVec)
  
  if (length(zero_idx) > 0) {
    Effort[zero_idx] <- 0
  }

  Proj@Effort[sim, TSIndex, ] <- Effort
  
  Temp <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
  
  if (TACType =='Removals') {
    Catch <- Temp@Landings[sim, stocks, TSIndex, ] +  Temp@Discards[sim, stocks, TSIndex, ]
  } else {
    Catch <- Temp@Landings[sim, stocks, TSIndex, ]
  }
  
  if (is.null(ncol(Catch))) {
    CatchByFleet <- Catch
  } else {
    CatchByFleet <- colSums(Catch[, , drop = FALSE])
  }
  
  sum((log(TAC_by_Fleet[pos_idx]) - log(CatchByFleet[pos_idx]))^2)
}
