#' Optimise fleet effort to match TAC (single-stock)
#'
#' Calculates fleet-specific effort that achieves target removals (TAC) for a
#' single-complex (single-stock) model. For multi-complex models use
#' `OptEffort_multistock()`.
#'
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
#' 1. **Single active fleet** — `stats::optimize()` over the log-scale
#'    multiplier, with an automatic bracket search that detects whether the
#'    TAC is achievable within `maxEffort`.
#' 2. **Multiple active fleets** — diagonal Newton-Raphson with a full-Jacobian
#'    fallback when the diagonal step fails to reduce the squared residual.
#'    Each iteration clamps step sizes to `max_log_step` and effort values to
#'    `[minEffort, maxEffort]` to prevent divergence when biomass is low.
#' 3. **`stats::optim()` (BFGS) fallback** — used only if Newton-Raphson
#'    neither converges nor saturates within `maxIter` iterations.
#'
#' Saturation (TAC unachievable) is detected in two ways: (a) a fleet's effort
#' reaches `99%` of `maxEffort` while its residual remains positive, or (b)
#' neither the diagonal nor the full-Jacobian step reduces the squared residual
#' and no flat-gradient fleet exists to blame — in which case the fleet with
#' the largest unresolved residual is force-saturated to break the stall.
#' Saturated fleets are frozen at their current effort and excluded from
#' subsequent iterations.
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
#'     ceiling.
#'
#'   `.CalcFisheryDynamics()`
#'   overwrites `Hist@Effort` with the effort implied by the F actually
#'   realised, so an unachievable TAC is reported as the effort corresponding
#'   to the capped F rather than as `maxEffort`. See that function's
#'   `DoBackCalcEffort`.
#'
#' @keywords internal
.OptEffortSinglestock <- function(Proj,
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
                                  maxEffort = 1e6) {

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
    return(list(Effort = Effort_base, converged = NA, saturated = FALSE))
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
  
  if (length(pos_idx) == 0L)
    return(list(Effort = Effort_curr, converged = NA, saturated = FALSE))
  
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
        return(list(Effort = Effort_curr, converged = FALSE, saturated = TRUE))
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

    return(list(Effort = Effort_curr, converged = TRUE, saturated = FALSE))
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
  
  converged        <- FALSE
  saturated_flag   <- FALSE
  saturated_fleets <- logical(length(pos_idx))
  
  for (iter in seq_len(maxIter)) {
    
    # Early exit: all active fleets have hit maxEffort and TAC is
    # still unachievable (depleted stock scenario).
    effort_at_cap <- Effort_curr[pos_idx] >= maxEffort_fl[pos_idx] * 0.99
    if (any(effort_at_cap & !saturated_fleets)) {
      res_check <- residual_fn(Effort_curr)
      still_under <- effort_at_cap & (res_check > tol_vec)
      if (any(still_under)) {
        saturated_fleets <- saturated_fleets | still_under
      }
    }
    
    active <- !saturated_fleets
    if (!any(active)) {
      saturated_flag <- TRUE
      break
    }
    
    residual <- residual_fn(Effort_curr)
    curr_ss  <- sum(residual[active]^2)
    
    if (all(abs(residual[active]) < tol_vec[active])) {
      converged <- TRUE
      break
    }
    
    # -- Diagonal Jacobian step --
    deltaF      <- pmax(Effort_curr[pos_idx] * 1e-4, 1e-8)
    Effort_pert <- Effort_curr
    Effort_pert[pos_idx[active]] <- Effort_curr[pos_idx[active]] + deltaF[active]
    residual_pert <- residual_fn(Effort_pert)
    
    J_diag    <- (residual - residual_pert) / deltaF
    flat_grad <- active & (abs(J_diag) < 1e-6) & (abs(residual) > tol_vec)
    tiny      <- abs(J_diag) < 1e-8
    J_diag[tiny] <- sign(J_diag[tiny]) * 1e-8
    J_diag[J_diag == 0] <- 1e-8
    
    log_step <- clamp_step(residual[active] / (J_diag[active] * Effort_curr[pos_idx[active]]))
    Eff_diag <- clamp_effort(Effort_curr[pos_idx[active]] * exp(log_step), pos_idx[active])
    Eff_diag <- apply_ceiling(Eff_diag, pos_idx[active])
    
    Effort_test                  <- Effort_curr
    Effort_test[pos_idx[active]] <- Eff_diag
    residual_diag                <- residual_fn(Effort_test)
    diag_ss                      <- sum(residual_diag[active]^2)
    
    if (is.finite(diag_ss) && diag_ss <= curr_ss) {
      Effort_curr[pos_idx[active]] <- Eff_diag
      next
    }
    
    # -- Full Jacobian step --
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
    
    log_step_full <- clamp_step(delta_active / Effort_curr[pos_idx[active]])
    new_effort    <- clamp_effort(Effort_curr[pos_idx[active]] * exp(log_step_full), pos_idx[active])
    new_effort    <- apply_ceiling(new_effort, pos_idx[active])
    
    Effort_test_full                  <- Effort_curr
    Effort_test_full[pos_idx[active]] <- new_effort
    residual_full                     <- residual_fn(Effort_test_full)
    full_ss                           <- sum(residual_full[active]^2)
    
    if (is.finite(full_ss) && full_ss <= curr_ss) {
      Effort_curr[pos_idx[active]] <- new_effort
    } else {
      if (any(flat_grad)) {
        saturated_fleets <- saturated_fleets | flat_grad
      } else {
        worst <- which.max(abs(residual) * as.numeric(active))
        saturated_fleets[worst] <- TRUE
      }
    }
  }
  
  if (!converged && !saturated_flag) {
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

    converged <- all(abs(residual_fn(Effort_curr)) < tol_vec)
  }

  if (any(has_ceiling))
    Effort_curr[has_ceiling] <- pmin(Effort_curr[has_ceiling],
                                     MaxFleetEffort[has_ceiling])

  list(Effort = Effort_curr, converged = converged, saturated = saturated_flag)
}
