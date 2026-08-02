

.LOG_DELTA_CAP <- log(10)

# Per-(fleet, stock) log-targeting deviation bound, derived from the same
# historical `StockTargeting@Covariance` used by GenerateStockTargeting() to
# simulate stochastic historical/projection targeting. Ties how far the
# TAC/effort solvers may reallocate a fleet's targeting to how much that
# fleet's targeting has actually been observed to vary historically, rather
# than an arbitrary flat constant. Falls back to `.LOG_DELTA_CAP` for any
# (fleet, stock) with zero/non-finite historical variance (no covariance
# fitted, or a stock with no historical targeting record).
#
# Returns an [nFleet x nStock] matrix, mirroring the shape of `Delta`.
.GetLogDeltaCap <- function(Proj, sim, TruncSD = 2) {
  nF   <- nFleet(Proj)
  nS   <- nStock(Proj)
  cap  <- matrix(.LOG_DELTA_CAP, nF, nS)

  Cov <- Proj@OM@StockTargeting@Covariance  # [sim, stock, stock, fleet]
  if (is.null(Cov)) return(cap)

  sim_cov <- if (dim(Cov)[1] == 1L) 1L else sim

  for (fl in seq_len(nF)) {
    var_s <- diag(Cov[sim_cov, , , fl])
    cap_s <- TruncSD * sqrt(pmax(var_s, 0))
    valid <- is.finite(cap_s) & cap_s > 0
    cap[fl, valid] <- cap_s[valid]
  }
  cap
}

# Largest effort worth searching, per fleet.
#
# `maxF` caps total apical F per stock per area, so once
#
#   q(st,fl) * E * Dist(fl,ar) * targ(st,fl) [/ RelSize(ar)]  >=  maxF
#
# holds for every (stock, area) the fleet works, more effort buys no more F and
# therefore no more catch. Solving for E and taking the largest gives the point
# beyond which the search is pointless. `headroom` allows for other fleets
# diluting this one's share of the capped total, since the cap applies to their
# sum. Returns `NA` for fleets with no usable q, targeting or distribution, and
# `NULL` if the inputs are missing entirely - callers fall back to `maxEffort`.
#
# Without this the solvers climb to an arbitrary `maxEffort` (1e6) purely to
# discover a TAC is unachievable, which costs several Newton iterations per
# saturating fleet-year.
.MaxUsefulEffort <- function(Proj, sim, TSIndex, headroom = 10) {

  Misc <- Proj@Misc
  maxF <- Misc$maxF
  if (!length(maxF) || !is.finite(maxF) || maxF <= 0) return(NULL)

  q    <- Misc$Catchability     # Sim, Stock, Year, Fleet
  Dist <- Proj@Distribution     # Sim, Year, Fleet, Area
  RS   <- Misc$RelSize          # Sim, Area
  if (is.null(q) || is.null(Dist) || is.null(RS)) return(NULL)

  nF <- nFleet(Proj); nS <- nStock(Proj); nA <- nArea(Proj)

  dens <- as.logical(Misc$Mode)
  if (length(dens) != nF) dens <- rep(FALSE, nF)

  targ <- if (isTRUE(as.integer(Misc$StockTargetingFlag) == 1L))
    Misc$StockTargeting else NULL   # Sim, Stock, Fleet, Year

  # arrays may be broadcast over sim
  si <- function(A, d = 1L) min(sim, dim(A)[d])

  out <- rep(NA_real_, nF)

  for (fl in seq_len(nF)) {

    # The current year's distribution is only filled in by CalcSpatialDistribution
    # once the year is simulated, which is after effort has been chosen. Last
    # year's is a fine stand-in for a search bound.
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

# Applies a per-fleet effort ceiling (NA = no ceiling) to an Effort vector -
# lets a fleet be governed by both TAC and Effort advice at once, with
# Effort acting as the binding constraint whenever it is more restrictive
# than what TAC-solving would otherwise choose. See .UpdateTACSim().
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


#' Effort at which each complex reaches its own TAC
#'
#' Solves, for every complex with a TAC, the fleet effort at which that
#' complex's catch equals it. Catch is monotone in effort, so each solve is a
#' bracketed root find - the same operation `.OptEffortSinglestock()` performs,
#' applied to one complex at a time.
#'
#' @param Proj A `Proj` object, already sliced to `sim`.
#' @param Year Integer. Current projection year.
#' @param TSIndex Integer. Time-step index of `Year`.
#' @param sim Integer. Simulation index.
#' @param TAC_by_Complex Named list of per-fleet TAC vectors, `NULL` where a
#'   complex has no TAC.
#' @param TACType_by_Complex,TACUnit_by_Complex Per-complex `"Removals"`/
#'   `"Landings"` and `"Biomass"`/`"Number"` vectors.
#' @param MaxFleetEffort Numeric vector (length `nFleet`), or `NULL`.
#' @param Effort_start `[nComplex x nFleet]` matrix of warm-start efforts, or
#'   `NULL`. Each complex's root find starts from its own row; rows that are not
#'   fully finite fall back to a cold start.
#' @param ... Passed to `.OptEffortSinglestock()`.
#'
#' @return A `[nComplex x nFleet]` matrix of efforts. Rows for complexes
#'   without a TAC are `NA`, and are ignored when the efforts are combined.
#' @keywords internal
.SolveEffortByComplex <- function(Proj, Year, TSIndex, sim,
                                  TAC_by_Complex,
                                  TACType_by_Complex,
                                  TACUnit_by_Complex,
                                  MaxFleetEffort = NULL,
                                  Effort_start = NULL,
                                  ...) {

  nComplex <- length(TAC_by_Complex)
  nFleet   <- nFleet(Proj)

  out <- matrix(NA_real_, nrow = nComplex, ncol = nFleet)

  for (cx in seq_len(nComplex)) {
    if (is.null(TAC_by_Complex[[cx]])) next

    start_cx <- if (is.null(Effort_start)) NULL else Effort_start[cx, ]
    if (!is.null(start_cx) && !all(is.finite(start_cx))) start_cx <- NULL

    out[cx, ] <- .OptEffortSinglestock(
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
  }

  out
}

#' Combine per-complex efforts into one effort per fleet
#'
#' Compliance sets how far a fleet respects each complex's TAC as a cap.
#' Full compliance stops the fleet at the first TAC reached; zero compliance
#' lets it run until the last TAC is reached, overshooting the tighter ones.
#'
#' \deqn{E^*_f = \min_c \left[ \kappa_{fc} E_{fc} +
#'   (1 - \kappa_{fc}) \max_c E_{fc} \right]}
#'
#' Each complex contributes its own effort in proportion to how far the fleet
#' complies with it, and the effort that fills every quota in proportion to how
#' far it does not.
#'
#' @param EffortByComplex `[nComplex x nFleet]` matrix from
#'   `.SolveEffortByComplex()`. `NA` rows are ignored.
#' @param Compliance `[nFleet x nComplex]` matrix from
#'   `.ResolveComplianceMatrix()`. `NA` entries take `default`.
#' @param default Numeric. Compliance used where unset. Default `1`, i.e. a
#'   TAC is a hard cap.
#'
#' @return Numeric vector of length `nFleet`. `NA` for fleets with no TAC in
#'   any complex, which the caller leaves unchanged.
#' @keywords internal
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
    tac_type <- TACType_by_Complex[[i]]   # length nFleet
    tac_unit <- TACUnit_by_Complex[[i]]   # length nFleet
    
    for (fl in seq_len(nFleet)) {
      
      if (tac_unit[fl] == 'Biomass') {
        landings <- Temp@Landings[sim, stocks, TSIndex, fl, drop = FALSE]
        discards <- Temp@Discards[sim, stocks, TSIndex, fl, drop = FALSE]
      } else {
        landings <- purrr::map(Temp@LandingsAtAge, \(stock) {
          stock[sim, , TSIndex, fl, ,drop = FALSE] |>
          SumOverAge() |> SumOverArea()
        }) |> List2Array('Stock') |> SumOverStock()
        
        discards <- purrr::map(Temp@DiscardsAtAge, \(stock) {
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

.GetActiveStocks <- function(Proj, sim, TSIndex, StockNames, FleetNames, 
                            n_recent = 5, tol = 1E-6) {

  nStock <- length(StockNames)
  nFleet <- length(FleetNames)
  active <- matrix(FALSE, nFleet, nStock,
                   dimnames = list(Fleet = FleetNames, Stock = StockNames))
  
  
  recent_idx <- seq(max(1L, TSIndex - n_recent), TSIndex - 1)

  for (fl in seq_len(nFleet)) {
    recent_mat <- Proj@Misc$StockTargeting[sim, ,fl, recent_idx, drop = FALSE]
    active[fl, ] <- apply(recent_mat, 2,
                          function(x) any(is.finite(x) & x > tol))
  }
  
  active
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

#' Effort implied by a targeting mix under the choke rule
#'
#' Writes `Delta`, solves the effort at which each complex reaches its own TAC,
#' and combines those by compliance. Fleets with no TAC in any complex keep
#' their current effort.
#'
#' @return List with `Effort` (length `nFleet`), `Proj` (the object with both
#'   `Delta` and the resolved effort written in), and `EffortByComplex`, the
#'   per-complex efforts - which the caller can feed back as `Effort_start`.
#' @keywords internal
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


.OptEffortMsSolver <- function(obj_fn, params_init, maxEval, tol) {
    opt <- tryCatch(
    optim(params_init, obj_fn, method = "BFGS",
          control = list(maxit  = maxEval,
                         reltol = tol,
                         ndeps  = rep(1e-4, length(params_init)))),
    error = function(e) list(convergence = 1L, par = params_init,
                             counts = c(`function` = 0L))
  )
  eval_used <- as.integer(opt$counts["function"])
  converged <- opt$convergence == 0L
  
  if (!converged && eval_used < maxEval) {
    opt2 <- tryCatch(
      optim(opt$par, obj_fn, method = "Nelder-Mead",
            control = list(maxit  = maxEval - eval_used,
                           reltol = tol)),
      error = function(e) opt
    )
    if (opt2$convergence == 0L) {
      converged <- TRUE
      opt       <- opt2
    }
  }
  list(params = opt$par, converged = converged)
}
