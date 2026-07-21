# Pack per-fleet, per-active-stock log-targeting weights into a flat
# parameter vector for the optimiser. Unlike .PackParams() (used by
# .OptEffortMultiStock()), effort is fixed here and excluded entirely from
# the parameter vector - only log(Delta) is packed.
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
.UnpackDelta <- function(params, active_fleets, active_stock_list, Delta_base) {
  Delta <- Delta_base
  pos   <- 1L

  for (k in seq_along(active_fleets)) {
    fl       <- active_fleets[k]
    active_s <- active_stock_list[[k]]
    nA       <- length(active_s)
    if (nA == 0L) next

    log_d <- params[pos:(pos + nA - 1L)]
    pos   <- pos + nA
    log_d <- log_d - mean(log_d)

    Delta[fl, active_s] <- exp(log_d)
  }

  Delta
}

# Objective for .OptTargetingMultiStock(): negative concave-transformed
# catch (so minimising this maximises catch), plus the same ridge penalty
# on year-to-year targeting change used by .OptEffortMsObjective(). Catch
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
                                      lambda_vec,
                                      active_fleets,
                                      active_stock_list) {

  Delta <- .UnpackDelta(params, active_fleets, active_stock_list, Delta_base)

  ProjTmp <- .WriteStateToProj(Proj, sim, TSIndex, Effort_fixed, Delta)

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
    lambda_vec[fl] * sum((log_d_cur - log_d_prev)^2)
  }

  ridge_pen <- if (!any(lambda_vec > 0)) {
    0
  } else {
    purrr::map2_dbl(active_fleets, active_stock_list, fleet_ridge) |> sum()
  }

  -catch_value + ridge_pen
}
