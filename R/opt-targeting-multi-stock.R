#' Estimate stock-specific targeting weights for a fixed fleet effort
#'
#' `.OptTargetingMultiStock()` is the effort-control counterpart to
#' `.OptEffortChoke()`. Under TAC management, effort is derived from the
#' targeting mix by the choke rule. Under effort management there is no TAC to
#' solve for - fleet effort is instead fixed by the MP's `Effort` advice - but
#' a fleet fishing several complexes still has to decide how to point that
#' fixed effort across them. This function solves for the targeting weights
#' `delta_{c,f}` (one per fleet-complex pair - fleets don't target the
#' individual stocks *within* a complex, e.g. the sexes of a 2-sex stock; that
#' split is left to each stock's own catchability/selectivity) that maximise a
#' concave transform of total catch (an interim stand-in for catch *value*,
#' pending complex-specific bio-economic prices - see Details), subject to a
#' soft ridge penalty on year-to-year targeting change. Both paths share
#' `.OptTargetingMsObjective()`; they differ only in whether effort is
#' supplied or derived.
#'
#' @param Proj       A projection object containing effort, targeting
#'   history, operating model settings, and stock/fleet dimensions.
#' @param Year       Integer. The simulation year index passed to
#'   `.CalcFisheryDynamics()`.
#' @param TSIndex    Integer. Time-step index into `Proj` arrays for the
#'   current year. `TSIndex - 1` is used to initialise the warm start.
#' @param sim        Integer. Simulation replicate index.
#' @param StockNames Character vector of stock names.
#' @param FleetNames Character vector of fleet names.
#' @param Effort     Numeric vector of length `nFleet`. Fixed fleet effort
#'   for this year (from the MP's `Effort` advice) - not optimised.
#' @param lambda     Numeric matrix `[nFleet x nComplex]`, or a scalar or
#'   length-`nFleet` vector recycled across complexes, or `NULL` (default
#'   `1`). Ridge penalty weight controlling resistance to year-to-year
#'   targeting changes.
#' @param n_recent   Integer (default `5`). Number of most-recent historical
#'   years examined to determine which complexes each fleet actively targets.
#' @param minEffort  Numeric (default `1e-8`). Floor applied to fleet effort
#'   to avoid degenerate zero-effort fleets.
#' @param tol        Numeric (default `1e-6`). Convergence tolerance passed
#'   to the optimiser.
#' @param maxEval    Integer (default `500`). Maximum function evaluations
#'   allowed across the BFGS and Nelder-Mead phases combined.
#'
#' @details
#' The catch objective for each active fleet is `sum(log1p(Catch_s))` over
#' the member stocks of the fleet's active complexes, where `Catch_s` is
#' landings + discards (biomass) for stock `s` at the trial targeting
#' weights. `log1p` is a concave transform: it gives diminishing weight to
#' additional catch of an already-abundant complex, which discourages the
#' optimiser from collapsing onto whichever single complex is most
#' catchable.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`Delta`}{Numeric matrix `[nFleet x nStock]`. Final targeting
#'     weights, broadcast from the underlying `[nFleet x nComplex]` solution
#'     so every stock in a complex shares the same weight (see
#'     `.ExpandComplexDelta()`). Fleets/complexes not actively re-optimised
#'     retain their previous value.}
#'   \item{`converged`}{Logical. `TRUE` if the BFGS or Nelder-Mead optimiser
#'     reported convergence.}
#'   \item{`ActiveComplex`}{Logical matrix `[nFleet x nComplex]`. Which
#'     complexes are considered active for each fleet based on recent
#'     targeting history.}
#' }
#'
#' @seealso `.OptEffortChoke()` for the TAC-based counterpart this
#'   function mirrors.
#'
#' @keywords internal
.OptTargetingMultiStock <- function(Proj,
                                     Year,
                                     TSIndex,
                                     sim,
                                     StockNames,
                                     FleetNames,
                                     Effort,
                                     lambda        = NULL,
                                     n_recent      = 5,
                                     minEffort     = 1e-8,
                                     tol           = 1e-6,
                                     maxEval       = 500) {

  nFleet   <- length(FleetNames)
  Complexes <- Proj@OM@Complexes
  nComplex  <- length(Complexes)

  if (is.null(lambda)) lambda <- 1
  if (is.null(dim(lambda))) {
    if (length(lambda) == 1L) lambda <- rep(lambda, nFleet)
    if (length(lambda) != nFleet)
      cli::cli_abort("lambda must be length 1, `nFleet`, or [nFleet x nComplex]",
                     .internal = TRUE)
    lambda <- matrix(lambda, nrow = nFleet, ncol = nComplex)
  }
  if (length(dim(lambda)) != 2L || !all(dim(lambda) == c(nFleet, nComplex)))
    cli::cli_abort("lambda must be [nFleet x nComplex]", .internal = TRUE)

  state <- .PrepComplexTargetingState(Proj, sim, TSIndex, Complexes, FleetNames, n_recent)

  Effort <- pmax(Effort, minEffort)

  # No fleet has any active-complex history - nothing to (re)optimise
  if (length(state$active_fleets) == 0L)
    return(list(
      Delta       = .ExpandComplexDelta(state$Delta_prev, Complexes, StockNames),
      converged   = TRUE,
      ActiveComplex = state$active_complex
    ))

  obj_args <- list(
    Proj                = Proj,
    sim                 = sim,
    TSIndex             = TSIndex,
    Year                = Year,
    Effort_fixed        = Effort,
    Delta_base          = state$Delta_prev,
    log_delta_prev      = state$log_delta_prev,
    lambda_mat          = lambda,
    active_fleets       = state$active_fleets,
    active_complex_list = state$active_complex_list,
    Complexes           = Complexes,
    StockNames          = StockNames,
    log_delta_cap       = state$log_delta_cap
  )

  params_init <- .PackDelta(state$Delta_prev, state$active_fleets, state$active_complex_list)

  obj1 <- function(p) do.call(.OptTargetingMsObjective, c(list(params = p), obj_args))

  result <- .OptEffortMsSolver(obj_fn = obj1, params_init, maxEval, tol)

  DeltaComplex_final <- .UnpackDelta(result$params, state$active_fleets, state$active_complex_list,
                                     state$Delta_prev, state$log_delta_cap)

  list(Delta         = .ExpandComplexDelta(DeltaComplex_final, Complexes, StockNames),
       converged     = result$converged,
       ActiveComplex = state$active_complex)
}
