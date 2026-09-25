
.LOG_DELTA_CAP <- log(10)

.GetLogDeltaCapComplex <- function(Proj, sim, Complexes, TruncSD = 2) {
  nF   <- nFleet(Proj)
  nC   <- length(Complexes)
  cap  <- matrix(.LOG_DELTA_CAP, nF, nC)

  Cov <- Proj@OM@StockTargeting@Covariance  # [sim, stock, stock, fleet]
  if (is.null(Cov)) return(cap)

  sim_cov <- if (dim(Cov)[1] == 1L) 1L else sim

  for (fl in seq_len(nF)) {
    var_s <- diag(Cov[sim_cov, , , fl])
    for (cx in seq_len(nC)) {
      var_cx <- max(pmax(var_s[Complexes[[cx]]], 0))
      cap_cx <- TruncSD * sqrt(var_cx)
      if (is.finite(cap_cx) && cap_cx > 0)
        cap[fl, cx] <- cap_cx
    }
  }
  cap
}

.MaxUsefulEffort <- function(Proj, sim, TSIndex, headroom = 10) {

  Misc <- Proj@Misc
  maxF <- Misc$maxF
  if (!length(maxF) || !is.finite(maxF) || maxF <= 0)
    return(NULL)

  q    <- Misc$Catchability     # Sim, Stock, Year, Fleet
  Dist <- Proj@Distribution     # Sim, Year, Fleet, Area
  RS   <- Misc$RelSize          # Sim, Area
  if (is.null(q) || is.null(Dist) || is.null(RS)) 
    return(NULL)

  nF <- nFleet(Proj)
  nS <- nStock(Proj)
  nA <- nArea(Proj)

  dens <- as.logical(Misc$Mode)
  if (length(dens) != nF) 
    dens <- rep(FALSE, nF)

  targ <- if (isTRUE(as.integer(Misc$StockTargetingFlag) == 1L))
    Misc$StockTargeting else NULL   # Sim, Stock, Fleet, Year

  # arrays may be broadcast over sim
  si <- function(A, d = 1L) min(sim, dim(A)[d])

  out <- rep(NA_real_, nF)

  for (fl in seq_len(nF)) {
    d <- Dist[si(Dist), TSIndex, fl, ]
    if (!any(is.finite(d) & d > 0) && TSIndex > 1L)
      d <- Dist[si(Dist), TSIndex - 1L, fl, ]

    w <- if (isTRUE(dens[fl]))
      1 / pmax(RS[si(RS), ], .Machine$double.eps) else rep(1, nA)

    ok <- is.finite(d) & d > 0
    if (!any(ok)) next

    best <- NA_real_
    for (st in seq_len(nS)) {
      qq <- q[si(q), st, TSIndex, fl]
      if (!is.finite(qq) || qq <= 0) next
      tg <- if (is.null(targ)) 1 else targ[si(targ), st, fl, TSIndex]
      if (!is.finite(tg) || tg <= 0) next
      best <- max(best, maxF / (qq * tg * d[ok] * w[ok]), na.rm = TRUE)
    }
    out[fl] <- best
  }

  headroom * out
}

.ApplyEffortCeiling <- function(Effort, MaxFleetEffort) {
  if (is.null(MaxFleetEffort)) return(Effort)
  has_ceiling <- !is.na(MaxFleetEffort)
  if (any(has_ceiling))
    Effort[has_ceiling] <- pmin(Effort[has_ceiling], MaxFleetEffort[has_ceiling])
  Effort
}

.WriteStateToProj <- function(Proj, sim, TSIndex, Effort, Delta = NULL) {
  Proj@Effort[sim, TSIndex, ] <- Effort
  
  if (!is.null(Delta))
    Proj@Misc$StockTargeting[sim, , , TSIndex] <- t(Delta)
  Proj
}


.OptSingleFleetCatch <- function(log_scale, Effort_base, Proj, sim, TSIndex,
                                Year, TACType_by_Complex, TACUnit_by_Complex,
                                fl, cx = 1L) {
  Eff <- Effort_base
  Eff[fl] <- Effort_base[fl] * exp(log_scale)
  Proj <- .WriteStateToProj(Proj, sim, TSIndex, Effort = Eff)
  .CalcFleetCatch(Proj, sim, TSIndex, Year, TACType_by_Complex,
                 TACUnit_by_Complex)[cx, fl]
}


# Effort at which each complex reaches its own TAC
#
# Solves, for every complex with a TAC, the fleet effort at which that
# complex's catch equals it.
.SolveEffortByComplex <- function(Proj, Year, TSIndex, sim,
                                  TAC_by_Complex,
                                  TACType_by_Complex,
                                  TACUnit_by_Complex,
                                  MaxFleetEffort = NULL,
                                  Effort_start = NULL,
                                  ...) {

  nComplex <- length(TAC_by_Complex)
  nFleet   <- nFleet(Proj)

  out          <- matrix(NA_real_, nrow = nComplex, ncol = nFleet)
  converged_cx <- rep(NA, nComplex)
  saturated_cx <- rep(NA, nComplex)

  for (cx in seq_len(nComplex)) {
    if (is.null(TAC_by_Complex[[cx]])) next

    start_cx <- if (is.null(Effort_start)) NULL else Effort_start[cx, ]
    if (!is.null(start_cx) && !all(is.finite(start_cx))) start_cx <- NULL

    solved <- .OptEffortSingleStock(
      Proj               = Proj,
      Year               = Year,
      TSIndex            = TSIndex,
      sim                = sim,
      TAC_by_Complex     = TAC_by_Complex,
      TACType_by_Complex = TACType_by_Complex,
      TACUnit_by_Complex = TACUnit_by_Complex,
      MaxFleetEffort     = MaxFleetEffort,
      cx                 = cx,
      Effort_start       = start_cx,
      ...
    )
    out[cx, ]        <- solved$Effort
    converged_cx[cx] <- solved$converged
    saturated_cx[cx] <- solved$saturated
  }

  attr(out, "converged") <- converged_cx
  attr(out, "saturated") <- saturated_cx
  out
}

# Combine per-complex efforts into one effort per fleet
#
# Compliance sets how far a fleet respects each complex's TAC as a cap.
# Full compliance stops the fleet at the first TAC reached; zero compliance
# lets it run until the last TAC is reached, overshooting the tighter ones.
#
# \deqn{E^*_f = \min_c \left[ \kappa_{fc} E_{fc} +
#   (1 - \kappa_{fc}) \max_c E_{fc} \right]}
.CombineEffortByCompliance <- function(EffortByComplex, Compliance,
                                       default = 1) {

  nFleet <- ncol(EffortByComplex)
  out    <- rep(NA_real_, nFleet)

  for (fl in seq_len(nFleet)) {
    E_c <- EffortByComplex[, fl]
    ok  <- which(is.finite(E_c))
    if (!length(ok)) next

    k <- Compliance[fl, ok]
    k[!is.finite(k)] <- default
    k <- pmin(pmax(k, 0), 1)

    E_max <- max(E_c[ok])
    out[fl] <- min(k * E_c[ok] + (1 - k) * E_max)
  }

  out
}

.CalcFleetCatch <- function(Proj, sim, TSIndex, Year,
                           TACType_by_Complex,
                           TACUnit_by_Complex ) {
  
  Temp <- .CalcFisheryDynamics(Proj, 
                           Years=Year, 
                           Sims = sim, 
                           DoCalcSpawnProduction = 1,
                           DoCalcRecruitment = 1,
                           DoCalcNumberNext = 0,
                           DoCalcBiomass = 0,
                           DoCalcOverallF = 0)
  
  .CatchMatrix(Temp, sim, TSIndex, TACType_by_Complex, TACUnit_by_Complex)
}


.CatchMatrix <- function(Temp, sim, TSIndex, TACType_by_Complex, TACUnit_by_Complex) {
  
  Complexes <- Temp@OM@Complexes
  nComplex  <- length(Complexes)
  nFleet    <- dim(Temp@Landings)[4]
  mat       <- matrix(0, nrow = nComplex, ncol = nFleet)
  
  for (i in seq_len(nComplex)) {
    stocks   <- Complexes[[i]]
    tac_type <- TACType_by_Complex[[i]]  
    tac_unit <- TACUnit_by_Complex[[i]] 
    
    for (fl in seq_len(nFleet)) {
      
      if (tac_unit[fl] == 'Biomass') {
        landings <- Temp@Landings[sim, stocks, TSIndex, fl, drop = FALSE]
        discards <- Temp@Discards[sim, stocks, TSIndex, fl, drop = FALSE]
      } else {
        landings <- purrr::map(Temp@LandingsAtAge[stocks], \(stock) {
          stock[sim, , TSIndex, fl, ,drop = FALSE] |>
          SumOverAge() |> SumOverArea()
        }) |> List2Array('Stock') |> SumOverStock()

        discards <- purrr::map(Temp@DiscardsAtAge[stocks], \(stock) {
          stock[sim, , TSIndex, fl, ,drop = FALSE] |>
            SumOverAge() |> SumOverArea()
        }) |> List2Array('Stock') |> SumOverStock()

      }
      
      if (tac_type[fl] == "Removals") {
        catch_val <- landings + discards
      } else {
        catch_val <- landings
      }
      
      mat[i, fl] <- SumOverStock(catch_val)
    }
  }
  mat
}

.GetActiveComplexes <- function(Proj, sim, TSIndex, Complexes, FleetNames,
                               n_recent = 5, tol = 1E-6) {

  nComplex <- length(Complexes)
  nFleet   <- length(FleetNames)
  active   <- matrix(FALSE, nFleet, nComplex,
                     dimnames = list(Fleet = FleetNames, Complex = names(Complexes)))

  recent_idx <- seq(max(1L, TSIndex - n_recent), TSIndex - 1)

  for (fl in seq_len(nFleet)) {
    for (cx in seq_len(nComplex)) {
      recent_mat <- Proj@Misc$StockTargeting[sim, Complexes[[cx]], fl, recent_idx, drop = FALSE]
      active[fl, cx] <- any(is.finite(recent_mat) & recent_mat > tol)
    }
  }

  active
}

.ExpandComplexDelta <- function(DeltaComplex, Complexes, StockNames) {
  DeltaStock <- matrix(1, nrow(DeltaComplex), length(StockNames),
                       dimnames = list(rownames(DeltaComplex), StockNames))
  for (cx in seq_along(Complexes))
    DeltaStock[, Complexes[[cx]]] <- DeltaComplex[, cx]
  DeltaStock
}

.PrepComplexTargetingState <- function(Proj, sim, TSIndex, Complexes, FleetNames,
                                      n_recent = 5) {

  nFleet   <- length(FleetNames)
  nComplex <- length(Complexes)

  active_complex <- .GetActiveComplexes(Proj, sim, TSIndex, Complexes, FleetNames, n_recent)

  STarget <- Proj@Misc$StockTargeting[sim, , , , drop = FALSE] |> abind::adrop(1)
  Delta_prev_stock <- t(STarget[, , TSIndex - 1, drop = FALSE] |> abind::adrop(3))  

  Delta_prev <- matrix(0, nFleet, nComplex,
                       dimnames = list(FleetNames, names(Complexes)))
  for (cx in seq_len(nComplex)) {
    stock_idx <- Complexes[[cx]]
    Delta_prev[, cx] <- if (length(stock_idx) == 1L) {
      Delta_prev_stock[, stock_idx]
    } else {
      exp(rowMeans(log(pmax(Delta_prev_stock[, stock_idx, drop = FALSE], 1e-10))))
    }
  }
  for (fl in seq_len(nFleet))
    Delta_prev[fl, !active_complex[fl, ]] <- 0

  log_delta_cap  <- .GetLogDeltaCapComplex(Proj, sim, Complexes)
  log_delta_prev <- .GetLogDeltaPrev(Delta_prev, active_complex, log_delta_cap)

  active_fleets       <- which(apply(active_complex, 1, any))
  active_complex_list <- purrr::map(active_fleets, \(fl) which(active_complex[fl, ]))

  list(
    active_complex      = active_complex,
    Delta_prev          = Delta_prev,
    log_delta_prev      = log_delta_prev,
    log_delta_cap       = log_delta_cap,
    active_fleets       = active_fleets,
    active_complex_list = active_complex_list
  )
}

.GetLogDeltaPrev <- function(Delta_prev, active_stock, log_delta_cap = NULL) {

  ld <- matrix(0, nrow(Delta_prev), ncol(Delta_prev))
  if (is.null(log_delta_cap))
    log_delta_cap <- matrix(.LOG_DELTA_CAP, nrow(Delta_prev), ncol(Delta_prev))

  for (fl in seq_len(nrow(Delta_prev))) {
    active_s <- which(active_stock[fl, ])
    if (length(active_s) == 0L) next
    lv    <- log(pmax(Delta_prev[fl, active_s], 1e-10))
    cap_s <- log_delta_cap[fl, active_s]
    ld[fl, active_s] <- pmin(pmax(lv - mean(lv), -cap_s), cap_s)
  }
  ld
}

.CheckComplexTACAttainment <- function(Proj, sim, TSIndex, Year, Effort_final, Delta_final,
                                       TAC_by_Complex, TACType_by_Complex, TACUnit_by_Complex,
                                       EbyC_final) {

  ComplexNames <- names(TAC_by_Complex)
  nComplex     <- length(TAC_by_Complex)

  ProjFinal <- .WriteStateToProj(Proj, sim, TSIndex, Effort = Effort_final, Delta = Delta_final)
  CatchMat  <- .CalcFleetCatch(ProjFinal, sim, TSIndex, Year, TACType_by_Complex, TACUnit_by_Complex)

  TAC_vec   <- vapply(TAC_by_Complex, function(x) if (is.null(x)) NA_real_ else sum(x, na.rm = TRUE), numeric(1))
  Catch_vec <- stats::setNames(rowSums(CatchMat, na.rm = TRUE), ComplexNames)

  has_tac <- is.finite(TAC_vec) & TAC_vec > 0
  tol_cx  <- pmax(1e-3, 1e-6 * abs(TAC_vec))
  hit     <- !has_tac | abs(Catch_vec - TAC_vec) < tol_cx

  converged_cx <- attr(EbyC_final, "converged")
  saturated_cx <- attr(EbyC_final, "saturated")
  if (is.null(converged_cx))
    converged_cx <- rep(NA, nComplex)
  if (is.null(saturated_cx))
    saturated_cx <- rep(NA, nComplex)

  choke_ok   <- !hit & !is.na(converged_cx) & converged_cx
  cap_ok     <- !hit & !is.na(saturated_cx) & saturated_cx
  unresolved <- !hit & !choke_ok & !cap_ok

  converged <- all(hit)
  saturated <- if (converged) NA else !any(unresolved)

  list(converged = converged, saturated = saturated, TAC = TAC_vec, Catch = Catch_vec)
}

.ResolveChokeEffort <- function(Proj, sim, TSIndex, Year, Delta,
                                TAC_by_Complex, TACType_by_Complex,
                                TACUnit_by_Complex, Compliance,
                                MaxFleetEffort = NULL, Effort_start = NULL, ...) {

  Effort_curr <- Proj@Effort[sim, TSIndex, ]
  ProjTmp     <- .WriteStateToProj(Proj, sim, TSIndex,
                                   Effort = Effort_curr, Delta = Delta)

  EbyC <- .SolveEffortByComplex(ProjTmp, Year, TSIndex, sim,
                                TAC_by_Complex, TACType_by_Complex,
                                TACUnit_by_Complex, MaxFleetEffort,
                                Effort_start = Effort_start, ...)

  Effort <- .CombineEffortByCompliance(EbyC, Compliance)

  unset <- !is.finite(Effort)
  if (any(unset)) Effort[unset] <- Effort_curr[unset]

  Effort  <- .ApplyEffortCeiling(Effort, MaxFleetEffort)
  ProjTmp <- .WriteStateToProj(ProjTmp, sim, TSIndex,
                               Effort = Effort, Delta = Delta)

  list(Effort = Effort, Proj = ProjTmp, EffortByComplex = EbyC)
}


# Solve the targeting-mix (Delta) objective, with multi-start restarts
#
# Minimises `obj_fn` (a concave-in-catch, penalised-for-drift objective
# over the packed log targeting-mix parameters) via BFGS, with a
# Nelder-Mead fallback when BFGS doesn't converge - as before. On top of
# that single search, a small, fixed number of additional restarts are
# tried from deterministically offset starting points, and the best result
# (lowest `obj_fn` value) across all of them is kept.
.OptEffortMsSolver <- function(obj_fn, params_init, maxEval, tol,
                               n_restarts = 3, restart_scale = 2) {

  .RunOnce <- function(p0, budget) {
    opt <- tryCatch(
      optim(p0, obj_fn, method = "BFGS",
            control = list(maxit  = budget,
                           reltol = tol,
                           ndeps  = rep(1e-4, length(p0)))),
      error = function(e) list(convergence = 1L, par = p0,
                               counts = c(`function` = 0L))
    )
    eval_used <- as.integer(opt$counts["function"])
    converged <- opt$convergence == 0L

    if (!converged && eval_used < budget) {
      opt2 <- tryCatch(
        optim(opt$par, obj_fn, method = "Nelder-Mead",
              control = list(maxit  = budget - eval_used,
                             reltol = tol)),
        error = function(e) opt
      )
      if (opt2$convergence == 0L) {
        converged <- TRUE
        opt       <- opt2
      }
    }
    list(params = opt$par, converged = converged, value = obj_fn(opt$par))
  }

  best <- .RunOnce(params_init, maxEval)

  np <- length(params_init)
  if (n_restarts > 0 && np > 0) {
    restart_budget <- max(20L, as.integer(maxEval / (n_restarts + 1)))
    offsets        <- .WeylOffsets(np, n_restarts)

    for (i in seq_len(n_restarts)) {
      p0   <- params_init + restart_scale * offsets[, i]
      cand <- .RunOnce(p0, restart_budget)
      if (is.finite(cand$value) && cand$value < best$value) best <- cand
    }
  }

  list(params = best$params, converged = best$converged)
}


.WeylOffsets <- function(np, n) {
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53)
  irr    <- sqrt(primes[(seq_len(np) - 1) %% length(primes) + 1])
  offsets <- matrix(NA_real_, nrow = np, ncol = n)
  for (i in seq_len(n)) {
    frac          <- ((i + 1) * irr) %% 1
    offsets[, i] <- 2 * frac - 1
  }
  offsets
}
