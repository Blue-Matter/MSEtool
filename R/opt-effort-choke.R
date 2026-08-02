#' Solve fleet effort and targeting under the choke rule
#'
#' Solves fleet effort under TAC management in a multi-complex model. Rather
#' than penalising TAC over/undershoot, effort is derived from the targeting
#' mix: for each
#' complex, the effort at which its catch equals its TAC is found by a
#' bracketed root find (`.SolveEffortByComplex()`), and those are combined by
#' `Imp@TAC@Compliance` (`.CombineEffortByCompliance()`). Full compliance stops
#' a fleet at the first TAC reached; zero compliance lets it run until the last
#' is reached.
#'
#' Only the targeting mix is optimised, maximising a concave transform of catch
#' against a penalty for drifting from last year's mix - the same objective
#' `.OptTargetingMultiStock()` uses under effort control, with effort supplied
#' by the choke rule instead of by the MP.
#'
#' @inheritParams .OptTargetingMultiStock
#' @param TAC_by_Complex Named list of per-fleet TAC vectors, `NULL` where a
#'   complex has no TAC.
#' @param TACType_by_Complex,TACUnit_by_Complex Per-complex `"Removals"`/
#'   `"Landings"` and `"Biomass"`/`"Number"` vectors.
#' @param Compliance `[nFleet x nComplex]` matrix from
#'   `.ResolveComplianceMatrix()`.
#' @param MaxFleetEffort Numeric vector (length `nFleet`), or `NULL`.
#' @param inner_reltol Numeric, or `NULL` (default). Relative convergence
#'   tolerance for the per-complex root finds during optimisation, as a
#'   fraction of the TAC being matched; `NULL` keeps `.OptEffortSinglestock()`'s
#'   absolute `tol`. 
#'
#' @return A named list with `Effort`, `Delta`, `converged` and `ActiveStock`.
#' @keywords internal
.OptEffortChoke <- function(Proj,
                            Year,
                            TSIndex,
                            sim,
                            StockNames,
                            FleetNames,
                            TAC_by_Complex,
                            TACType_by_Complex,
                            TACUnit_by_Complex,
                            Compliance,
                            MaxFleetEffort = NULL,
                            lambda        = NULL,
                            n_recent      = 5,
                            minEffort     = 1e-8,
                            tol           = 1e-6,
                            inner_reltol  = NULL,
                            maxEval       = 500) {

  nFleet <- length(FleetNames)
  nStock <- length(StockNames)

  if (is.null(lambda)) lambda <- 1
  if (is.null(dim(lambda))) {
    if (length(lambda) == 1L) lambda <- rep(lambda, nFleet)
    lambda <- matrix(lambda, nrow = nFleet, ncol = nStock)
  }

  # Effort implied by a trial targeting mix. Successive calls differ only
  # slightly in `Delta`, so each complex's root find warm-starts from the
  # previous call's solution, carried between evaluations in `cache`. Dropping
  # the warm start costs roughly a 5x slowdown.
  cache      <- new.env(parent = emptyenv())
  cache$warm <- NULL
  EffortFn <- function(Delta, reltol = inner_reltol) {
    res <- .ResolveChokeEffort(Proj, sim, TSIndex, Year, Delta,
                               TAC_by_Complex, TACType_by_Complex,
                               TACUnit_by_Complex, Compliance, MaxFleetEffort,
                               Effort_start = cache$warm, reltol = reltol)
    cache$warm <- res$EffortByComplex
    res$Effort
  }

  # The returned effort is the advice itself, so it is re-solved exactly
  FinalEffortFn <- function(Delta) EffortFn(Delta, reltol = NULL)

  active_stock <- .GetActiveStocks(Proj, sim, TSIndex, StockNames, FleetNames,
                                   n_recent)

  STarget    <- Proj@Misc$StockTargeting[sim, , , , drop = FALSE] |> abind::adrop(1)
  Delta_prev <- t(STarget[, , TSIndex - 1, drop = FALSE] |> abind::adrop(3))
  for (fl in seq_len(nFleet))
    Delta_prev[fl, !active_stock[fl, ]] <- 0

  log_delta_cap  <- .GetLogDeltaCap(Proj, sim)
  log_delta_prev <- .GetLogDeltaPrev(Delta_prev, active_stock, log_delta_cap)

  active_fleets <- which(apply(active_stock, 1, any))

  # Nothing to re-target: effort still follows from the existing mix
  if (length(active_fleets) == 0L)
    return(list(Effort      = FinalEffortFn(Delta_prev),
                Delta       = Delta_prev,
                converged   = TRUE,
                ActiveStock = active_stock))

  active_stock_list <- purrr::map(active_fleets, \(fl) which(active_stock[fl, ]))

  obj_args <- list(
    Proj              = Proj,
    sim               = sim,
    TSIndex           = TSIndex,
    Year              = Year,
    Effort_fixed      = NULL,
    Delta_base        = Delta_prev,
    log_delta_prev    = log_delta_prev,
    lambda_mat        = lambda,
    active_fleets     = active_fleets,
    active_stock_list = active_stock_list,
    log_delta_cap     = log_delta_cap,
    EffortFn          = EffortFn
  )

  params_init <- .PackDelta(Delta_prev, active_fleets, active_stock_list)

  obj <- function(p) do.call(.OptTargetingMsObjective, c(list(params = p), obj_args))

  result <- .OptEffortMsSolver(obj_fn = obj, params_init, maxEval, tol)

  Delta_final <- .UnpackDelta(result$params, active_fleets, active_stock_list,
                              Delta_prev, log_delta_cap)

  list(Effort      = FinalEffortFn(Delta_final),
       Delta       = Delta_final,
       converged   = result$converged,
       ActiveStock = active_stock)
}
