# Pack per-fleet, per-active-stock log-targeting weights into a flat
# parameter vector for the optimiser. Effort is never a free parameter here -
# under effort control the MP fixes it, and under TAC control the choke rule
# derives it - so only log(Delta) is packed.
.PackDelta <- function(Delta, active_fleets, active_stock_list) {
  compute <- function(fl, active_s) {
    if (length(active_s) == 0L) return(numeric(0))
    log(pmax(Delta[fl, active_s], 1e-10))
  }
  purrr::map2(active_fleets, active_stock_list, compute) |> purrr::list_c()
}

# Unpack the flat parameter vector back into a full [nFleet x nStock] Delta
# matrix. `Delta_base` supplies entries not being optimised this call, so a
# partial re-optimisation never zeroes out untouched fleets/stocks. Each
# optimised fleet's log-Delta is mean-centred (geometric-mean-of-1, see
# .GetLogDeltaPrev()).
.UnpackDelta <- function(params, active_fleets, active_stock_list, Delta_base,
                        log_delta_cap = NULL) {
  Delta <- Delta_base
  pos   <- 1L
  if (is.null(log_delta_cap))
    log_delta_cap <- matrix(.LOG_DELTA_CAP, nrow(Delta_base), ncol(Delta_base))

  for (k in seq_along(active_fleets)) {
    fl       <- active_fleets[k]
    active_s <- active_stock_list[[k]]
    nA       <- length(active_s)
    if (nA == 0L) next

    log_d <- params[pos:(pos + nA - 1L)]
    pos   <- pos + nA
    cap_s <- log_delta_cap[fl, active_s]
    log_d <- pmin(pmax(log_d - mean(log_d), -cap_s), cap_s)

    Delta[fl, active_s] <- exp(log_d)
  }

  Delta
}

# Shared objective for .OptTargetingMultiStock() (effort control) and
# .OptEffortChoke() (TAC control): negative concave-transformed catch (so
# minimising this maximises catch), plus a ridge penalty on year-to-year
# targeting change. Catch
# is landings + discards (biomass) - an interim proxy for catch value; see
# .OptTargetingMultiStock()'s Details for how to switch to real
# stock-specific prices later.
.OptTargetingMsObjective <- function(params,
                                      Proj,
                                      sim,
                                      TSIndex,
                                      Year,
                                      Effort_fixed,
                                      Delta_base,
                                      log_delta_prev,
                                      lambda_mat,
                                      active_fleets,
                                      active_stock_list,
                                      log_delta_cap = NULL,
                                      EffortFn = NULL) {

  Delta <- .UnpackDelta(params, active_fleets, active_stock_list, Delta_base,
                        log_delta_cap)

  # Under effort control the MP fixes effort. Under TAC control effort follows
  # from the targeting mix, via the per-complex root find and the compliance
  # rule - `EffortFn` supplies it.
  Effort <- if (is.null(EffortFn)) Effort_fixed else EffortFn(Delta)

  ProjTmp <- .WriteStateToProj(Proj, sim, TSIndex, Effort, Delta)

  Temp <- .CalcFisheryDynamics(Hist = ProjTmp,
                              Years = Year,
                              Sims = sim,
                              DoCalcSpawnProduction = 1,
                              DoCalcRecruitment = 1,
                              DoCalcNumberNext = 0,
                              DoCalcBiomass = 0,
                              DoCalcOverallF = 0)

  fleet_catch_value <- function(fl, active_s) {
    catch_s <- vapply(active_s, function(st)
      Temp@Landings[sim, st, TSIndex, fl] + Temp@Discards[sim, st, TSIndex, fl],
      numeric(1))
    sum(log1p(pmax(catch_s, 0)))
  }

  catch_value <- purrr::map2_dbl(active_fleets, active_stock_list, \(fl, active_s) {
    if (!length(active_s)) return(0)
    fleet_catch_value(fl, active_s)
  }) |> sum()

  fleet_ridge <- function(fl, active_s) {
    if (!length(active_s)) return(0)
    log_d_cur  <- log(pmax(Delta[fl, active_s], 1e-10))
    log_d_cur  <- log_d_cur - mean(log_d_cur)
    log_d_prev <- log_delta_prev[fl, active_s]
    sum(lambda_mat[fl, active_s] * (log_d_cur - log_d_prev)^2)
  }

  ridge_pen <- if (!any(lambda_mat > 0)) {
    0
  } else {
    purrr::map2_dbl(active_fleets, active_stock_list, fleet_ridge) |> sum()
  }

  -catch_value + ridge_pen
}
