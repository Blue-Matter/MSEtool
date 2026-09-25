#' Optimise fleet effort to match TAC (single-stock)
#'
#' Calculates fleet-specific effort that achieves target removals (TAC) for a
#' single-complex (single-stock) model. For multi-complex models this is
#' still the underlying per-complex solver, called once per complex by
#' `.SolveEffortByComplex()` from within `.OptEffortChoke()`.
#'
#' @param Proj `hist-class` object containing projected effort and fishery
#'   dynamics.
#' @param Year Integer. Current projection year.
#' @param TSIndex Integer. Time-step index of `Year` in
#'   `c(YearsHist, YearsProj)`.
#' @param sim Integer. Simulation index.
#' @param TAC_by_Complex Named list of numeric vectors (length `nFleet`), one
#'   per complex. `NULL` entries indicate no TAC constraint for that complex,
#'   in which case the previous year's effort is returned unchanged.
#' @param TACType_by_Complex Character vector (length `nComplex`).
#'   `"Removals"` or `"Landings"` for each complex.
#' @param MaxFleetEffort Numeric vector (length `nFleet`). Optional per-fleet
#'   effort ceiling applied after optimisation; `NA` entries impose no ceiling.
#'   Fleets whose ceiling is exactly zero are excluded from optimisation.
#' @param cx Integer. Index of the complex to solve against, within
#'   `TAC_by_Complex` and the rows of the catch matrix. Default `1`. Values
#'   other than `1` are used when solving each complex separately to find the
#'   effort at which it reaches its own TAC.
#' @param Effort_start Numeric vector (length `nFleet`), or `NULL` (default) to
#'   start from the previous time step's effort. Supplies a warm start when this
#'   solve is nested inside an outer optimiser that re-solves a nearly identical
#'   problem each call - see `.OptEffortChoke()`.
#' @param reltol Numeric, or `NULL` (default). When supplied, convergence is
#'   judged relative to each fleet's TAC rather than against the absolute `tol`,
#'   making the criterion invariant to the units catch is measured in.
#' @param minEffort Numeric scalar. Floor substituted for near-zero effort
#'   values throughout; prevents log-scale arithmetic from collapsing to
#'   `-Inf`. Default `1e-8`.
#' @param tol Numeric scalar. Convergence tolerance: Newton-Raphson is
#'   considered converged when all active-fleet residuals satisfy
#'   `|TAC - catch| < tol`. Default `1e-3`.
#' @param maxIter Integer. Maximum Newton-Raphson iterations before the BFGS
#'   fallback is attempted. Default `100`.
#' @param max_log_step Numeric scalar. Maximum permitted log-scale step size
#'   per fleet per Newton-Raphson iteration. Caps multiplicative effort changes
#'   at `exp(max_log_step)` per iteration, preventing runaway effort when the
#'   Jacobian is near-singular (e.g. depleted stock). Default `log(10)`
#'   (i.e. at most a 10× change per iteration).
#' @param maxEffort Numeric scalar. Hard upper bound on any fleet's effort.
#'   Applied via `clamp_effort()` at every step and used as the bracket ceiling
#'   in the single-fleet case. Fleets that reach `99%` of this value while
#'   their TAC residual remains positive are declared saturated. Default `1e6`.
#'   This is an arbitrary backstop: it is tightened per fleet to
#'   `.MaxUsefulEffort()`, the effort past which the `maxF` cap leaves catch
#'   unchanged, so saturation is reached in a step or two instead of by climbing
#'   several decades.
#'
#' Effort is solved on the log scale so that candidate values are always
#' positive. The workflow is:
#'
#' 1. **Single active fleet**: `stats::optimize()` over the log-scale
#'    multiplier, with an automatic bracket search that detects whether the
#'    TAC is achievable within `maxEffort`.
#' 2. **Multiple active fleets**: an informed proportional rescale of the
#'    starting effort (see below), then diagonal Newton-Raphson with a
#'    full-Jacobian fallback, each step protected by a backtracking line
#'    search (see below). Effort values are clamped to `[minEffort,
#'    maxEffort]` throughout to prevent divergence when biomass is low.
#' 3. **`stats::optim()` (BFGS) fallback**: attempted whenever Newton-Raphson
#'    does not converge, unless the reason is a genuine effort ceiling
#'    (see "Saturation vs. stall" below). A solver stall is not evidence the
#'    TAC is unreachable, so it always gets a further, less-local search
#'    before the solve is given up on.
#'
#' ## Unreachable combined TAC.
#'
#' With multiple active fleets, the catch with every active fleet at its
#' effort ceiling is calculated first. If the combined TAC exceeds that
#' catch by more than the combined tolerance, the TAC cannot be reached at
#' any effort: every active fleet is set to its ceiling and the solve is
#' returned as saturated, without iterating.
#'
#' ## Warm-start rescale.
#' 
#' The Newton-Raphson loop starts from the previous
#' time step's effort (`Effort_start`, or `Proj@Effort[sim, TSIndex - 1L, ]`).
#'
#' ## Backtracking line search 
#' 
#' Each Newton-Raphson step (diagonal or full
#' Jacobian) is accepted only if it does not increase the total squared
#' residual across active fleets. 
#'
#' ## Saturation vs. stall  
#' 
#' Two distinct situations freeze a fleet out of
#' further iteration, and are tracked separately:
#'   * *Ceiling*: either (a) the fleet's effort reaches `99%` of
#'     `maxEffort_fl` (its `.MaxUsefulEffort()`-derived cap) while its TAC
#'     residual remains positive, or (b) after backtracking fails, the
#'     fleet's Jacobian entry is flat (`flat_grad`: near-zero derivative with
#'     a large residual). Both mean catch has genuinely stopped responding to
#'     this fleet's effort, ie biology-driven saturation.
#'   * *Stall*:  after backtracking fails and no fleet shows a flat
#'     gradient, the fleet with the largest residual is frozen instead. The
#'     Jacobian said effort should still matter, but the step search
#'     couldn't find an improving direction, ie a local solver difficulty, not
#'     evidence the TAC is unreachable. These fleets still get a global BFGS
#'     attempt before the solve is reported as failed.
#'
#' The `saturated` return value is `TRUE` only when at least one fleet was
#' frozen for a ceiling reason. A stall with no ceiling fleets involved is
#' reported as `converged = FALSE, saturated = FALSE`, a genuine solver
#' failure worth surfacing (see `.LogEffortConvergence()`), as distinct from
#' an expected, biomass-driven TAC shortfall.
#'
#' @param max_backtrack Integer. Number of times a rejected Newton-Raphson
#'   step is halved (in log-effort space) before it is treated as a stall.
#'   Default `6` (i.e. steps as small as `1/64` of the original are tried).
#'
#' @return A list with:
#'   * `Effort` - numeric vector of length `nFleet` giving optimised effort
#'     per fleet. For fleets with no TAC constraint (`BindingTAC` is `NA` or
#'     not in `pos_idx`), effort is carried forward from the previous time
#'     step. When the TAC is unachievable the two branches differ: the
#'     single-active-fleet branch cannot bracket the TAC and returns the
#'     previous time step's effort, while the multi-fleet Newton branch
#'     saturates and leaves effort at `maxEffort` (or `MaxFleetEffort[fl]`
#'     if lower).
#'   * `converged` - `TRUE` if the solve met `tol`/`reltol`, `FALSE` if not,
#'     `NA` if there was no active TAC to solve for.
#'   * `saturated` - `TRUE` if the TAC was unachievable within the effort
#'     ceiling (see "Saturation vs. stall" above) — `FALSE` for an
#'     unresolved solver stall, even though `converged` is also `FALSE` in
#'     that case.
#'   * `TAC`, `Catch` - numeric vectors (length `nFleet`, `NA` outside
#'     `pos_idx`) giving the target and achieved catch at the returned
#'     `Effort`, for diagnostic logging.
#'
#'   `.CalcFisheryDynamics()`
#'   overwrites `Hist@Effort` with the effort implied by the F actually
#'   realised, so an unachievable TAC is reported as the effort corresponding
#'   to the capped F rather than as `maxEffort`. See that function's
#'   `DoBackCalcEffort`.
#'
#' @keywords internal
.OptEffortSingleStock <- function(Proj,
                                  Year,
                                  TSIndex,
                                  sim,
                                  TAC_by_Complex,
                                  TACType_by_Complex,
                                  TACUnit_by_Complex,
                                  MaxFleetEffort = NULL,
                                  cx            = 1L,
                                  Effort_start  = NULL,
                                  minEffort     = 1e-8,
                                  tol           = 1e-3,
                                  reltol        = NULL,
                                  maxIter       = 100,
                                  max_log_step  = log(10),
                                  maxEffort = 1e6,
                                  max_backtrack = 6) {

  nFleet   <- nFleet(Proj)

  if (is.null(MaxFleetEffort))
    MaxFleetEffort <- rep(NA_real_, nFleet)

  # start from the previous solution when one is supplied
  Effort_ref <- if (is.null(Effort_start)) Proj@Effort[sim, TSIndex - 1L, ] else Effort_start

  maxEffort_fl <- rep(maxEffort, nFleet)
  useful       <- .MaxUsefulEffort(Proj, sim, TSIndex)
  if (!is.null(useful)) {
    ok <- is.finite(useful) & useful > 0
    maxEffort_fl[ok] <- pmin(maxEffort_fl[ok], useful[ok])
  }

  tac_vec <- TAC_by_Complex[[cx]]

  if (is.null(tac_vec)) {
    # No TAC has been set for any fleets
    # return previous effort unless an effort rec. has already been applied
    Effort_base <- pmax(Effort_ref, minEffort)
    has_ceiling <- !is.na(MaxFleetEffort)
    if (any(has_ceiling))
      Effort_base[has_ceiling] <- MaxFleetEffort[has_ceiling]
    return(list(Effort = Effort_base, converged = NA, saturated = FALSE,
                TAC = rep(NA_real_, nFleet), Catch = rep(NA_real_, nFleet)))
  }

  BindingTAC <- tac_vec
  pos_idx    <- which(is.finite(BindingTAC) & BindingTAC > 0)
  zero_idx   <- which(is.finite(BindingTAC) & BindingTAC == 0)

  Effort_base           <- pmax(Effort_ref, minEffort)
  Effort_base[zero_idx] <- 0
  Effort_curr           <- Effort_base

  has_ceiling <- !is.na(MaxFleetEffort)
  if (any(has_ceiling))
    Effort_curr[has_ceiling] <- pmin(Effort_curr[has_ceiling],
                                     MaxFleetEffort[has_ceiling])

  ceiling_zero <- has_ceiling & MaxFleetEffort <= 0
  Effort_curr[ceiling_zero] <- 0
  pos_idx <- setdiff(pos_idx, which(ceiling_zero))

  if (length(pos_idx) == 0L) {
    Catch_out <- rep(NA_real_, nFleet)
    Catch_out[zero_idx] <- 0
    Catch_out[ceiling_zero] <- 0
    return(list(Effort = Effort_curr, converged = NA, saturated = FALSE,
                TAC = BindingTAC, Catch = Catch_out))
  }
  
  if (length(pos_idx) == 1L) {
    fl  <- pos_idx[1L]
    tac <- BindingTAC[fl]
    
    catch_fl <- function(log_scale) {
      .OptSingleFleetCatch(log_scale, Effort_base, Proj, sim, TSIndex,
                          Year, TACType_by_Complex, TACUnit_by_Complex, fl,
                          cx = cx)
    }
    
    log_lo  <- log(minEffort)
    log_hi  <- 0
    
    max_log <- if (!is.na(MaxFleetEffort[fl])) {
      min(log(maxEffort_fl[fl] / Effort_base[fl]), log(MaxFleetEffort[fl] / Effort_base[fl]))
    } else {
      log(maxEffort_fl[fl] / Effort_base[fl])
    }
    
    if (catch_fl(0) > tac) {
      log_lo <- -log(10)
      while (catch_fl(log_lo) > tac && log_lo > log(minEffort))
        log_lo <- log_lo - log(10)
    } else {
      while (catch_fl(log_hi) < tac && log_hi < max_log)
        log_hi <- log_hi + log(10)
      
      if (catch_fl(log_hi) < tac) {
        Effort_curr[fl] <- Effort_base[fl]
        if (!is.na(MaxFleetEffort[fl]))
          Effort_curr[fl] <- min(Effort_curr[fl], MaxFleetEffort[fl])
        TAC_out   <- rep(NA_real_, nFleet); TAC_out[fl]   <- tac
        Catch_out <- rep(NA_real_, nFleet); Catch_out[fl] <- catch_fl(log(Effort_curr[fl] / Effort_base[fl]))
        return(list(Effort = Effort_curr, converged = FALSE, saturated = TRUE,
                    TAC = TAC_out, Catch = Catch_out))
      }
    }

    opt <- optimize(
      function(ls) (catch_fl(ls) - tac)^2,
      interval = c(log_lo, log_hi),
      tol = if (is.null(reltol)) 1e-8 else reltol
    )
    Effort_curr[fl] <- Effort_base[fl] * exp(opt$minimum)

    if (!is.na(MaxFleetEffort[fl]))
      Effort_curr[fl] <- min(Effort_curr[fl], MaxFleetEffort[fl])

    TAC_out   <- rep(NA_real_, nFleet); TAC_out[fl]   <- tac
    Catch_out <- rep(NA_real_, nFleet); Catch_out[fl] <- catch_fl(log(Effort_curr[fl] / Effort_base[fl]))
    return(list(Effort = Effort_curr, converged = TRUE, saturated = FALSE,
                TAC = TAC_out, Catch = Catch_out))
  }
  
  tol_vec <- if (is.null(reltol)) {
    rep(tol, length(pos_idx))
  } else {
    pmax(reltol * abs(BindingTAC[pos_idx]), .Machine$double.eps)
  }

  clamp_effort <- function(e, idx) pmin(pmax(e, minEffort), maxEffort_fl[idx])
  clamp_step   <- function(s) pmax(pmin(s, max_log_step), -max_log_step)
  
  residual_fn <- function(Effort_vec) {
    Proj     <- .WriteStateToProj(Proj, sim, TSIndex, Effort = Effort_vec)
    CatchMat <- .CalcFleetCatch(Proj, sim, TSIndex, Year, TACType_by_Complex,
                               TACUnit_by_Complex)[cx, ]
    (BindingTAC - CatchMat)[pos_idx]
  }
  
  apply_ceiling <- function(e, idx) {
    ceil <- MaxFleetEffort[idx]
    has  <- !is.na(ceil)
    if (any(has)) e[has] <- pmin(e[has], ceil[has])
    e
  }
  
  Effort_cap          <- Effort_curr
  Effort_cap[pos_idx] <- apply_ceiling(maxEffort_fl[pos_idx], pos_idx)
  residual_cap        <- residual_fn(Effort_cap)
  if (is.finite(sum(residual_cap)) && sum(residual_cap) > sum(tol_vec)) {
    Catch_out <- rep(NA_real_, nFleet); Catch_out[pos_idx] <- BindingTAC[pos_idx] - residual_cap
    if (length(zero_idx))  Catch_out[zero_idx]    <- 0
    if (any(ceiling_zero)) Catch_out[ceiling_zero] <- 0
    TAC_out <- rep(NA_real_, nFleet); TAC_out[pos_idx] <- BindingTAC[pos_idx]
    return(list(Effort = Effort_cap, converged = FALSE, saturated = TRUE,
                TAC = TAC_out, Catch = Catch_out))
  }

  # Informed warm-start rescale
  # A large, simultaneous multi-fleet TAC change can require the solver
  # to find a joint root far from the previous time step's effort. Rescale
  # all active fleets together by the ratio of total target TAC to total
  # catch achieved at the starting effort, on the log scale, before any
  # Newton-Raphson iteration. Only kept if it actually reduces the
  # total squared residual.
  residual0 <- residual_fn(Effort_curr)
  catch0    <- BindingTAC[pos_idx] - residual0
  total_tac <- sum(BindingTAC[pos_idx])
  total_catch0 <- sum(pmax(catch0, 0))
  if (is.finite(total_catch0) && total_catch0 > 0 && total_tac > 0) {
    rescale <- clamp_step(log(total_tac / total_catch0))
    if (abs(rescale) > 1e-6) {
      Eff_rescaled <- clamp_effort(Effort_curr[pos_idx] * exp(rescale), pos_idx)
      Eff_rescaled <- apply_ceiling(Eff_rescaled, pos_idx)
      Effort_test  <- Effort_curr
      Effort_test[pos_idx] <- Eff_rescaled
      residual_rescaled <- residual_fn(Effort_test)
      if (is.finite(sum(residual_rescaled^2)) &&
          sum(residual_rescaled^2) < sum(residual0^2)) {
        Effort_curr[pos_idx] <- Eff_rescaled
      }
    }
  }

  # Backtracking line search: halves a proposed log-scale step (for the
  # active fleets) up to `max_backtrack` times until it reduces the total
  # squared residual, or gives up.
  .BacktrackStep <- function(log_step, active_idx_pos, curr_ss) {
    step <- log_step
    for (b in seq_len(max_backtrack + 1L)) {
      Eff_try <- clamp_effort(Effort_curr[active_idx_pos] * exp(step), active_idx_pos)
      Eff_try <- apply_ceiling(Eff_try, active_idx_pos)

      Effort_test                <- Effort_curr
      Effort_test[active_idx_pos] <- Eff_try
      residual_test               <- residual_fn(Effort_test)
      test_ss                     <- sum(residual_test[active]^2)

      if (is.finite(test_ss) && test_ss <= curr_ss)
        return(list(improved = TRUE, Effort = Eff_try))

      step <- step / 2
    }
    list(improved = FALSE, Effort = Effort_curr[active_idx_pos])
  }

  converged      <- FALSE
  saturated_flag <- FALSE   # TRUE only for a genuine effort-ceiling fleet
  ceiling_fleets <- logical(length(pos_idx))  # frozen: real maxEffort_fl cap
  stalled_fleets <- logical(length(pos_idx))  # frozen: local step failed, no ceiling hit

  for (iter in seq_len(maxIter)) {

    # Early exit: all active fleets have hit maxEffort and TAC is
    # still unachievable (depleted stock scenario). 
    effort_at_cap <- Effort_curr[pos_idx] >= maxEffort_fl[pos_idx] * 0.99
    if (any(effort_at_cap & !ceiling_fleets)) {
      res_check <- residual_fn(Effort_curr)
      still_under <- effort_at_cap & (res_check > tol_vec)
      if (any(still_under)) {
        ceiling_fleets <- ceiling_fleets | still_under
      }
    }

    active <- !(ceiling_fleets | stalled_fleets)
    if (!any(active)) break

    residual <- residual_fn(Effort_curr)
    curr_ss  <- sum(residual[active]^2)

    if (all(abs(residual[active]) < tol_vec[active])) break

    # -- Diagonal Jacobian step, with backtracking --
    deltaF      <- pmax(Effort_curr[pos_idx] * 1e-4, 1e-8)
    Effort_pert <- Effort_curr
    Effort_pert[pos_idx[active]] <- Effort_curr[pos_idx[active]] + deltaF[active]
    residual_pert <- residual_fn(Effort_pert)

    J_diag    <- (residual - residual_pert) / deltaF
    flat_grad <- active & (abs(J_diag) < 1e-6) & (abs(residual) > tol_vec)
    tiny      <- abs(J_diag) < 1e-8
    J_diag[tiny] <- sign(J_diag[tiny]) * 1e-8
    J_diag[J_diag == 0] <- 1e-8

    log_step0 <- clamp_step(residual[active] / (J_diag[active] * Effort_curr[pos_idx[active]]))
    diag_result <- .BacktrackStep(log_step0, pos_idx[active], curr_ss)

    if (diag_result$improved) {
      Effort_curr[pos_idx[active]] <- diag_result$Effort
      next
    }

    # -- Full Jacobian step, with backtracking --
    nF_active  <- sum(active)
    active_idx <- which(active)
    J          <- matrix(0, nrow = nF_active, ncol = nF_active)
    diag(J)    <- J_diag[active]

    for (k in seq_len(nF_active)) {
      fleet_k                <- pos_idx[active_idx[k]]
      deltaF_k               <- max(Effort_curr[fleet_k] * 1e-4, 1e-8)
      Effort_pert_k          <- Effort_curr
      Effort_pert_k[fleet_k] <- Effort_curr[fleet_k] + deltaF_k
      res_pert_k             <- residual_fn(Effort_pert_k)
      J[, k]                 <- (residual[active] - res_pert_k[active]) / deltaF_k
    }

    delta_active <- tryCatch(
      solve(J, residual[active]),
      error = function(e) residual[active] / J_diag[active]
    )

    log_step_full0 <- clamp_step(delta_active / Effort_curr[pos_idx[active]])
    full_result    <- .BacktrackStep(log_step_full0, pos_idx[active], curr_ss)

    if (full_result$improved) {
      Effort_curr[pos_idx[active]] <- full_result$Effort
    } else {
      # Neither the diagonal nor the (repeatedly halved) full Newton step
      # could improve on the current residual. Two distinct causes land
      # here, and only one is a real ceiling:
      #  - flat_grad: catch genuinely stopped responding to this fleet's
      #    effort (zero derivative with residual still large) 
      #  - no flat_grad fleet to blame: the Jacobian says effort should
      #    still matter, but even a repeatedly-halved step couldn't find an
      #    improving direction. A genuine local solver difficulty,
      #    not evidence the TAC is unreachable, so the worst-residual fleet
      #    is frozen as *stalled* (not ceiling) and a global BFGS search is
      #    still attempted below before this is reported as a failure.
      if (any(flat_grad)) {
        ceiling_fleets <- ceiling_fleets | flat_grad
      } else {
        worst <- which.max(abs(residual) * as.numeric(active))
        stalled_fleets[worst] <- TRUE
      }
    }
  }

  # Convergence check across every fleet with a TAC
  final_residual <- residual_fn(Effort_curr)
  converged      <- all(abs(final_residual) < tol_vec)

  # BFGS fallback: attempted whenever the whole-fleet check above isn't
  # satisfied
  if (!converged) {
    obj_bfgs <- function(log_scale_vec) {
      log_scale_vec     <- clamp_step(log_scale_vec)
      Eff_test          <- Effort_base
      Eff_test[pos_idx] <- clamp_effort(Effort_base[pos_idx] * exp(log_scale_vec), pos_idx)
      Eff_test[pos_idx] <- apply_ceiling(Eff_test[pos_idx], pos_idx)
      sum(residual_fn(Eff_test)^2)
    }

    par_init <- log(pmax(Effort_curr[pos_idx], minEffort) /
                      pmax(Effort_base[pos_idx], minEffort))
    par_init[!is.finite(par_init)] <- 0
    par_init <- clamp_step(par_init)

    opt <- optim(par_init, obj_bfgs,
                 method  = "BFGS",
                 control = list(maxit = 200))

    Effort_curr[pos_idx] <- clamp_effort(Effort_base[pos_idx] * exp(clamp_step(opt$par)), pos_idx)
    Effort_curr[pos_idx] <- apply_ceiling(Effort_curr[pos_idx], pos_idx)

    final_residual <- residual_fn(Effort_curr)
    converged <- all(abs(final_residual) < tol_vec)
  }

  still_bad      <- abs(final_residual) >= tol_vec
  saturated_flag <- !converged && any(still_bad) && all(ceiling_fleets[still_bad])

  if (any(has_ceiling))
    Effort_curr[has_ceiling] <- pmin(Effort_curr[has_ceiling],
                                     MaxFleetEffort[has_ceiling])

  TAC_out   <- rep(NA_real_, nFleet); TAC_out[pos_idx]   <- BindingTAC[pos_idx]
  Catch_out <- rep(NA_real_, nFleet); Catch_out[pos_idx] <- BindingTAC[pos_idx] - final_residual
  if (length(zero_idx))  Catch_out[zero_idx]      <- 0
  if (any(ceiling_zero)) Catch_out[ceiling_zero]   <- 0

  list(Effort = Effort_curr, converged = converged, saturated = saturated_flag,
       TAC = TAC_out, Catch = Catch_out)
}
