#' Estimate stock-specific targeting weights for a fixed fleet effort
#'
#' `.OptTargetingMultiStock()` is the effort-control counterpart to
#' `.OptEffortMultiStock()`. Under TAC management, fleet effort *and*
#' stock-specific targeting weights are solved jointly so each fleet catches
#' its TAC. Under effort management there is no TAC to solve for - fleet
#' effort is instead fixed by the MP's `Effort` advice - but a fleet fishing
#' several stocks still has to decide how to point that fixed effort across
#' them. This function solves for the targeting weights `delta_{s,f}` that
#' maximise a concave transform of total catch (an interim stand-in for
#' catch *value*, pending stock-specific bio-economic prices - see Details),
#' subject to the same soft ridge penalty on year-to-year targeting change
#' used by `.OptEffortMultiStock()`.
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
#' @param lambda     Numeric vector of length `nFleet` (or scalar, recycled),
#'   or `NULL` (default `1` for all fleets). Ridge penalty weight controlling
#'   resistance to year-to-year targeting changes - see
#'   `.OptEffortMultiStock()`.
#' @param n_recent   Integer (default `5`). Number of most-recent historical
#'   years examined to determine which stocks each fleet actively targets.
#' @param minEffort  Numeric (default `1e-8`). Floor applied to fleet effort
#'   to avoid degenerate zero-effort fleets.
#' @param tol        Numeric (default `1e-6`). Convergence tolerance passed
#'   to the optimiser.
#' @param maxEval    Integer (default `500`). Maximum function evaluations
#'   allowed across the BFGS and Nelder-Mead phases combined.
#'
#' @details
#' The catch objective for each active fleet is `sum(log1p(Catch_s))` over
#' the fleet's active stocks, where `Catch_s` is landings + discards
#' (biomass) for stock `s` at the trial targeting weights. `log1p` is a
#' concave transform: it gives diminishing weight to additional catch of an
#' already-abundant stock, which discourages the optimiser from collapsing
#' onto whichever single stock is most catchable.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{`Delta`}{Numeric matrix `[nFleet x nStock]`. Final targeting
#'     weights. Fleets/stocks not actively re-optimised retain their
#'     previous value.}
#'   \item{`converged`}{Logical. `TRUE` if the BFGS or Nelder-Mead optimiser
#'     reported convergence.}
#'   \item{`ActiveStock`}{Logical matrix `[nFleet x nStock]`. Which stocks
#'     are considered active for each fleet based on recent targeting
#'     history.}
#' }
#'
#' @seealso `.OptEffortMultiStock()` for the TAC-based counterpart this
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

  nFleet <- length(FleetNames)
  nStock <- length(StockNames)

  # Normalise lambda to a length-nFleet vector
  if (is.null(lambda)) lambda <- rep(1, nFleet)
  if (length(lambda) == 1L) lambda <- rep(lambda, nFleet)
  if (length(lambda) != nFleet)
    cli::cli_abort("lambda must be length `nFleet`", .internal = TRUE)

  # Active stocks per fleet
  active_stock <- .GetActiveStocks(Proj, sim, TSIndex, StockNames, FleetNames, n_recent)

  # Previous year targeting - the warm start and the ridge penalty's centre
  STarget    <- Proj@Misc$StockTargeting[sim,,,, drop = FALSE] |> abind::adrop(1)
  Delta_prev <- t(STarget[, , TSIndex - 1, drop = FALSE] |> abind::adrop(3))
  for (fl in seq_len(nFleet))
    Delta_prev[fl, !active_stock[fl, ]] <- 0

  log_delta_prev <- .GetLogDeltaPrev(Delta_prev, active_stock)

  active_fleets <- which(apply(active_stock, 1, any))

  Effort <- pmax(Effort, minEffort)

  # No fleet has any active-stock history - nothing to (re)optimise
  if (length(active_fleets) == 0L)
    return(list(Delta = Delta_prev, converged = TRUE, ActiveStock = active_stock))

  active_stock_list <- purrr::map(active_fleets, \(fl) which(active_stock[fl, ]))

  obj_args <- list(
    Proj              = Proj,
    sim               = sim,
    TSIndex           = TSIndex,
    Year              = Year,
    Effort_fixed      = Effort,
    Delta_base        = Delta_prev,
    log_delta_prev    = log_delta_prev,
    lambda_vec        = lambda,
    active_fleets     = active_fleets,
    active_stock_list = active_stock_list
  )

  params_init <- .PackDelta(Delta_prev, active_fleets, active_stock_list)

  obj1 <- function(p) do.call(.OptTargetingMsObjective, c(list(params = p), obj_args))

  result <- .OptEffortMsSolver(obj_fn = obj1, params_init, maxEval, tol)

  Delta_final <- .UnpackDelta(result$params, active_fleets, active_stock_list, Delta_prev)

  list(Delta       = Delta_final,
       converged   = result$converged,
       ActiveStock = active_stock)
}
