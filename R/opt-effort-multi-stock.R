#' Estimate fleet effort and stock-specific targeting weights
#'
#' `OptEffort_multi_stock()` solves for fleet effort levels `E_f` and
#' stock-specific targeting weights `delta_{s,f}` such that each active fleet
#' catches its TAC for every stock complex it participates in, subject to choke
#' constraints and a soft ridge penalty on year-to-year targeting changes.
#'
#' @param Proj       A projection object containing effort, targeting history,
#'   operating model settings, and stock/fleet dimensions.
#' @param Year       Integer. The simulation year index passed to
#'   [CalcFisheryDynamics()].
#' @param TSIndex    Integer. Time-step index into `Proj` arrays for the current
#'   year. `TSIndex - 1` is used to initialise the warm start.
#' @param sim        Integer. Simulation replicate index.
#' @param nStock     Integer. Total number of stocks in the model.
#' @param TAC_by_Complex Named list of length `nComplex`. Each element is a
#'   length-`nFleet` numeric vector of TAC values (one per fleet) for that
#'   complex, or `NULL` if no TAC applies to that complex.
#' @param TACType_by_Complex Character vector of length `nComplex`. Either
#'   `"Landings"` or `"Removals"` (landings + discards) for each complex.
#' @param Choke      Integer matrix `[nFleet x nComplex]`. A value of `1L`
#'   indicates that complex `i` is a choke constraint for fleet `f` — its TAC
#'   must not be exceeded.
#' @param UndershootPenalty Numeric matrix `[nFleet x nComplex]`. Penalty
#'   weight applied to proportional TAC undershoot for each fleet–complex pair.
#' @param OvershootPenalty  Numeric matrix `[nFleet x nComplex]`. Base penalty
#'   weight for proportional TAC overshoot. Choke complexes receive a ×1000
#'   multiplier internally.
#' @param PenaltyMode Character vector of length `nFleet`. Currently accepted
#'   for future extensibility; not used in the current implementation.
#' @param lambda     Numeric vector of length `nFleet` (or scalar, recycled), or
#'   `NULL` (default `1` for all fleets). Ridge penalty weight controlling
#'   resistance to year-to-year targeting changes. Larger values penalise
#'   deviations from the previous year's targeting mix more heavily.
#' @param n_recent   Integer (default `5`). Number of most-recent historical
#'   years examined to determine which stocks each fleet actively targets.
#' @param minEffort  Numeric (default `1e-8`). Floor applied to all effort
#'   values to avoid log(0) errors.
#' @param tol        Numeric (default `1e-6`). Convergence tolerance passed to
#'   optimisers and used as the feasibility tolerance in bisection.
#' @param maxEval    Integer (default `500`). Maximum function evaluations
#'   allowed across the BFGS and Nelder-Mead phases combined..
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`Effort`}{Numeric vector of length `nFleet`. Final effort per fleet,
#'     floored at `minEffort`.}
#'   \item{`Delta`}{Numeric matrix `[nFleet x nStock]`. Final targeting weights.
#'     Inactive stock entries are set to `0`.}
#'   \item{`Catch`}{Numeric matrix `[nComplex x nFleet]` of realised catches
#'     at the returned effort and targeting, with informative row/column names.}
#'   \item{`converged`}{Logical. `TRUE` if the BFGS or Nelder-Mead optimiser
#'     reported convergence in Phase 1.}
#'   \item{`ActiveStock`}{Logical matrix `[nFleet x nStock]`. Which stocks are
#'     considered active for each fleet based on recent targeting history.}
#' }
#'
#' @keywords internal
OptEffort_multi_stock <- function(Proj,
                                  Year,
                                  TSIndex,
                                  sim,
                                  StockNames,
                                  FleetNames,
                                  TAC_by_Complex,
                                  TACType_by_Complex,
                                  Choke,
                                  UndershootPenalty,
                                  OvershootPenalty,
                                  PenaltyMode,
                                  lambda        = NULL,  
                                  n_recent      = 5,
                                  minEffort     = 1e-8,
                                  tol           = 1e-6,
                                  maxEval       = 500) {
  
  Complexes   <- Proj@OM@Complexes
  nComplex    <- length(Complexes)
  nFleet      <- length(FleetNames)
  nStock      <- length(StockNames)
  
  # Normalise lambda to a length-nFleet vector
  if (is.null(lambda)) lambda <- rep(1, nFleet)
  if (length(lambda) == 1L) lambda <- rep(lambda, nFleet)
  if (length(lambda) != nFleet)
    cli::cli_abort("lambda must be length `nFleet`", .internal=TRUE)
  
  past_yr_idx  <- seq_len(TSIndex-1)
  n_years      <- length(past_yr_idx)

  # Active stocks per fleet
  active_stock <- GetActiveStocks(Proj, sim, 
                                  TSIndex, 
                                  StockNames, 
                                  FleetNames,
                                  n_recent)
  
  # Previous year effort and targeting 
  STarget    <- Proj@Misc$StockTargeting[sim,,,, drop = FALSE] |> abind::adrop(1)
  Effort_prev <- pmax(Proj@Effort[sim, TSIndex - 1, ], minEffort)
  Delta_prev  <- t(STarget[, , TSIndex - 1, drop = FALSE] |> abind::adrop(3))
  for (fl in seq_len(nFleet))
    Delta_prev[fl, !active_stock[fl, ]] <- 0
  
  # Mean-centred log_delta from last year — ridge penalty centre
  log_delta_prev <- GetLogDeltaPrev(Delta_prev, active_stock)

  # Determine Active fleets
  fleet_has_tac <- FleetHasTAC(nFleet, nComplex, TAC_by_Complex)
  active_fleets <- which(fleet_has_tac & apply(active_stock, 1, any))
  
  # No active fleets - keep things the same
  if (length(active_fleets) == 0L)
    return(list(Effort      = Effort_prev,
                Delta       = Delta_prev,
                converged   = TRUE,
                ActiveStock = active_stock))
  
  
  # Compute choke overshoot penalty
  choke_mult <- 1000
  OvershootPenalty[Choke == 1L] <- OvershootPenalty[Choke == 1L] * choke_mult
  
  active_stock_list <- purrr::map(active_fleets, \(fl) which(active_stock[fl, ]))

  # objective function arguments
  obj_args <- list(
    Proj               = Proj,
    sim                = sim,
    TSIndex            = TSIndex,
    Year               = Year,
    Complexes          = Complexes,
    TACType_by_Complex = TACType_by_Complex,
    TAC_by_Complex     = TAC_by_Complex,
    OvershootPenalty   = OvershootPenalty,
    UndershootPenalty  = UndershootPenalty,
    Effort_prev        = Effort_prev,
    nFleet             = nFleet,
    nStock             = nStock,
    log_delta_prev     = log_delta_prev,
    lambda_vec         = lambda,
    active_fleets      = active_fleets,
    active_stock_list  = active_stock_list,
    minEffort          = minEffort
  )
  
  params_init <- PackParams(Effort            = Effort_prev, 
                            Delta             = Delta_prev,
                            active_fleets     = active_fleets,
                            active_stock_list = active_stock_list,
                            minEffort         = minEffort)
                               
  obj1 <- function(p) {
    do.call(OptEffort_ms_objective, 
            c(list(params = p,
                   fixed_logeff  = NULL),
              obj_args))
  }
    
  result  <- OptEffort_ms_Solver(obj_fn=obj1, 
                                  params_init, 
                                  maxEval, 
                                  tol)
  
  state   <- UnpackParams(result$params, 
                          active_fleets, 
                          active_stock_list,
                          Effort_prev, 
                          nFleet, 
                          nStock,
                          minEffort = minEffort)
  
  Effort_final               <- state$Effort
  Delta_final                <- state$Delta
  Delta_final[!active_stock] <- 0

  
  list(Effort      = pmax(Effort_final, minEffort),
       Delta       = Delta_final,
       converged   = result$converged,
       ActiveStock = active_stock)
}