#' Solve fleet effort and targeting under the choke rule
#'
#' Solves fleet effort under TAC management in a multi-complex model. 
#' 
#' Effort is derived from the targeting mix: for each complex, the effort at 
#' which its catch equals its TAC is found by a bracketed root find (`.SolveEffortByComplex()`), and those are combined by `Imp@TAC@Compliance`
#' (`.CombineEffortByCompliance()`). Full compliance stops
#' a fleet at the first TAC reached.Zero compliance lets it run until the last
#' TAC is reached.
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
#'   fraction of the TAC being matched; `NULL` keeps `.OptEffortSingleStock()`'s
#'   absolute `tol`.
#'
#' @return A named list with `Effort`, `Delta` (`[nFleet x nStock]`, broadcast
#'   from the underlying per-complex solution), `converged` and
#'   `ActiveComplex`.
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

  nFleet    <- length(FleetNames)
  Complexes <- Proj@OM@Complexes
  nComplex  <- length(Complexes)

  if (is.null(lambda)) lambda <- 1
  if (is.null(dim(lambda))) {
    if (length(lambda) == 1L) lambda <- rep(lambda, nFleet)
    lambda <- matrix(lambda, nrow = nFleet, ncol = nComplex)
  }

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

  FinalSolve <- function(Delta) {
    res <- .ResolveChokeEffort(Proj, sim, TSIndex, Year, Delta,
                               TAC_by_Complex, TACType_by_Complex,
                               TACUnit_by_Complex, Compliance, MaxFleetEffort,
                               Effort_start = cache$warm, reltol = NULL)
    cache$warm <- res$EffortByComplex
    Attainment <- .CheckComplexTACAttainment(Proj, sim, TSIndex, Year, res$Effort, Delta,
                                             TAC_by_Complex, TACType_by_Complex, TACUnit_by_Complex,
                                             res$EffortByComplex)
    list(Effort = res$Effort, Attainment = Attainment)
  }

  state <- .PrepComplexTargetingState(Proj, sim, TSIndex, Complexes, FleetNames, n_recent)

  # Nothing to re-target: effort still follows from the existing mix
  if (length(state$active_fleets) == 0L) {
    Delta_prev_stock <- .ExpandComplexDelta(state$Delta_prev, Complexes, StockNames)
    Final <- FinalSolve(Delta_prev_stock)
    return(list(Effort        = Final$Effort,
                Delta         = Delta_prev_stock,
                converged     = Final$Attainment$converged,
                saturated     = Final$Attainment$saturated,
                TAC           = Final$Attainment$TAC,
                Catch         = Final$Attainment$Catch,
                ActiveComplex = state$active_complex))
  }

  obj_args <- list(
    Proj                = Proj,
    sim                 = sim,
    TSIndex             = TSIndex,
    Year                = Year,
    Effort_fixed        = NULL,
    Delta_base          = state$Delta_prev,
    log_delta_prev      = state$log_delta_prev,
    lambda_mat          = lambda,
    active_fleets       = state$active_fleets,
    active_complex_list = state$active_complex_list,
    Complexes           = Complexes,
    StockNames          = StockNames,
    log_delta_cap       = state$log_delta_cap,
    EffortFn            = EffortFn
  )

  params_init <- .PackDelta(state$Delta_prev, state$active_fleets, state$active_complex_list)

  obj <- function(p) do.call(.OptTargetingMsObjective, c(list(params = p), obj_args))

  result <- .OptEffortMsSolver(obj_fn = obj, params_init, maxEval, tol)

  DeltaComplex_final <- .UnpackDelta(result$params, state$active_fleets, state$active_complex_list,
                                     state$Delta_prev, state$log_delta_cap)
  Delta_final <- .ExpandComplexDelta(DeltaComplex_final, Complexes, StockNames)

  Final <- FinalSolve(Delta_final)

  list(Effort        = Final$Effort,
       Delta         = Delta_final,
       converged     = Final$Attainment$converged,
       saturated     = Final$Attainment$saturated,
       TAC           = Final$Attainment$TAC,
       Catch         = Final$Attainment$Catch,
       ActiveComplex = state$active_complex)
}
