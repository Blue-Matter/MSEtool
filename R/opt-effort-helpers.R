
WriteStateToProj <- function(Proj, sim, TSIndex, Effort, Delta = NULL) {
  Proj@Effort[sim, TSIndex, ] <- Effort
  
  if (!is.null(Delta))
    Proj@Misc$StockTargeting[sim, , , TSIndex] <- t(Delta)
  Proj
}


OptSingleFleetCatch <- function(log_scale, Effort_base, Proj, sim, TSIndex,
                                Year, TACType_by_Complex, TACUnit_by_Complex,
                                fl) {
  Eff <- Effort_base
  Eff[fl] <- Effort_base[fl] * exp(log_scale)
  Proj <- WriteStateToProj(Proj, sim, TSIndex, Effort = Eff)
  CalcFleetCatch(Proj, sim, TSIndex, Year, TACType_by_Complex,
                 TACUnit_by_Complex)[1,fl]
}


CalcFleetCatch <- function(Proj, sim, TSIndex, Year, 
                           TACType_by_Complex,
                           TACUnit_by_Complex ) {
  
  Temp <- CalcFisheryDynamics(Proj, 
                           Years=Year, 
                           Sims = sim, 
                           DoCalcSpawnProduction = 1,
                           DoCalcRecruitment = 1,
                           DoCalcNumberNext = 0,
                           DoCalcBiomass = 0,
                           DoCalcOverallF = 0)
  
  CatchMatrix(Temp, sim, TSIndex, TACType_by_Complex, TACUnit_by_Complex)
}


CatchMatrix <- function(Temp, sim, TSIndex, TACType_by_Complex, TACUnit_by_Complex) {
  
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
      
      mat[i, fl] <- catch_val
    }
  }
  mat
}

GetActiveStocks <- function(Proj, sim, TSIndex, StockNames, FleetNames, 
                            n_recent = 5, tol = 1E-6) {

  nStock <- length(StockNames)
  nFleet <- length(FleetNames)
  active <- matrix(FALSE, nFleet, nStock,
                   dimnames = list(Fleet = FleetNames, Stock = StockNames))
  
  
  recent_idx <- seq(max(1L, TSIndex - n_recent), TSIndex - 1)
  
  for (fl in seq_len(nFleet)) {
    recent_mat <- Proj@OM@StockTargeting@Targeting[sim, ,fl, recent_idx, drop = FALSE] 
    active[fl, ] <- apply(recent_mat, 2,
                          function(x) any(is.finite(x) & x > tol))
  }
  
  active
}

GetLogDeltaPrev <- function(Delta_prev, active_stock) {

  ld <- matrix(0, nrow(Delta_prev), ncol(Delta_prev))
  
  for (fl in seq_len(nrow(Delta_prev))) {
    active_s <- which(active_stock[fl, ])
    if (length(active_s) == 0L) next
    lv <- log(pmax(Delta_prev[fl, active_s], 1e-10))
    ld[fl, active_s] <- lv - mean(lv)
  }
  ld
}

FleetHasTAC <- function(nFleet, nComplex, TAC_by_Complex) {
  vapply(seq_len(nFleet), function(fl) {
    any(vapply(seq_len(nComplex), function(i) {
      tac_vec <- TAC_by_Complex[[i]]
      if (is.null(tac_vec)) return(FALSE)
      !is.na(tac_vec[fl]) && tac_vec[fl] > 0
    }, logical(1)))
  }, logical(1))
}


PackParams <- function(Effort, 
                       Delta, 
                       active_fleets, 
                       active_stock_list,
                       fixed_logeff = NULL, 
                       minEffort = 1e-8) {
  
  compute_log_params <- function(fl, active_s) {
    if (length(active_s) == 0L) return(numeric(0))
    
    fixed      <- fixed_logeff[[as.character(fl)]]
    log_effort <- log(max(Effort[fl], minEffort))
    
    purrr::map(active_s, function(s) {
      if (!is.null(fixed) && !is.na(fixed[s])) return(NULL)
      log_effort + log(max(Delta[fl, s], 1e-10))
    }) |>
      purrr::list_c()
  }
   
  purrr::map2(active_fleets, active_stock_list, compute_log_params) |>
    purrr::list_c()

}

UnpackParams <- function(params, 
                         active_fleets, 
                         active_stock_list,
                         Effort_prev, 
                         nFleet, 
                         nStock,
                         fixed_logeff = NULL,
                         minEffort = 1e-8) {
  
  Effort <- Effort_prev
  Delta  <- matrix(0, nFleet, nStock)
  pos    <- 1L
  
  for (k in seq_along(active_fleets)) {
    fl       <- active_fleets[k]
    active_s <- active_stock_list[[k]]
    nA       <- length(active_s)
    if (nA == 0L) next
    
    fixed     <- fixed_logeff[[as.character(fl)]]
    log_eff_s <- numeric(nA)
    
    for (j in seq_len(nA)) {
      s <- active_s[j]
      if (!is.null(fixed) && !is.na(fixed[s])) {
        log_eff_s[j] <- fixed[s]
      } else {
        log_eff_s[j] <- params[pos]
        pos <- pos + 1L
      }
    }
    
    log_E_f     <- mean(log_eff_s)
    log_delta_s <- log_eff_s - log_E_f
    E_f         <- exp(log_E_f)
    delta_s     <- exp(log_delta_s)
    
    Effort[fl]          <- max(E_f, minEffort)
    Delta[fl, active_s] <- delta_s
  }
  
  list(Effort = Effort, Delta = Delta)
  
  
}

OptEffort_ms_objective <- function(
    params, 
    Proj, 
    sim, 
    TSIndex, 
    Year, 
    Complexes,
    TACType_by_Complex,
    TAC_by_Complex,
    TACUnit_by_Complex,
    OvershootPenalty,
    UndershootPenalty,
    Effort_prev,
    nFleet,
    nStock,
    log_delta_prev,
    lambda_vec,
    active_fleets,
    active_stock_list,
    minEffort = 1e-8,
    fixed_logeff = NULL
    ) {
  
  
  nComplex <- length(Complexes)
  
  # TAC miss penalty (proportional squared deviations)
  # plus ridge penalty on year-to-year log_delta change.
  state <- UnpackParams(params, 
                        active_fleets, 
                        active_stock_list,
                        Effort_prev, 
                        nFleet,
                        nStock,
                        fixed_logeff, 
                        minEffort)
  
  ProjTmp <- WriteStateToProj(Proj, sim, TSIndex, state$Effort, state$Delta)
  
  Temp    <- CalcFisheryDynamics(Hist = ProjTmp, 
                                 Years = Year,
                                 Sims = sim,
                                 DoCalcSpawnProduction = 1,
                                 DoCalcRecruitment = 1,
                                 DoCalcNumberNext = 0,
                                 DoCalcBiomass = 0,
                                 DoCalcOverallF = 0)
  cm <- CatchMatrix(Temp, sim, TSIndex, TACType_by_Complex, TACUnit_by_Complex)
  
  # TAC penalty: sum of squared proportional deviations across all relevant
  # complexes for each fleet
  complex_penalty <- function(i, fl, active_s) {
    tac_fi <- TAC_by_Complex[[i]]
    if (is.null(tac_fi) || is.na(tac_fi[fl]) || tac_fi[fl] <= 0) 
      return(NA_real_)
    
    if (!any(active_s %in% Complexes[[i]])) 
      return(NA_real_)
    
    tac_fi   <- tac_fi[fl]
    catch_fi <- cm[i, fl]
    
    if (!is.finite(catch_fi)) return(1)
    
    undershoot <- max(0, tac_fi - catch_fi) / tac_fi
    overshoot  <- max(0, catch_fi - tac_fi) / tac_fi
    
    UndershootPenalty[fl, i] * undershoot^2 +
      OvershootPenalty[fl, i]   * overshoot^2
  }
  
  fleet_penalty <- function(fl, active_s) {
    penalties <- purrr::map_dbl(seq_len(nComplex), \(i) 
                                complex_penalty(i, fl, active_s)
    )
    sum(penalties, na.rm = TRUE)
  }
  
  tac_pen <- purrr::map2_dbl(active_fleets, active_stock_list, fleet_penalty) |> sum()
  
  # Ridge penalty: sum of squared year-to-year log_delta changes for stocks,
  # scaled by fleet-specific lambda. 
  fleet_ridge <- function(fl, active_s) {
    fixed <- fixed_logeff[[as.character(fl)]]
    if (!is.null(fixed)) {
      free <- is.na(fixed[active_s]) 
    } else {
      free <- rep(TRUE, length(active_s))
    }
    
    log_d_cur  <- log(pmax(state$Delta[fl, active_s], 1e-10))
    log_d_cur  <- log_d_cur - mean(log_d_cur)
    log_d_prev <- log_delta_prev[fl, active_s]
    
    lambda_vec[fl] * sum((log_d_cur[free] - log_d_prev[free])^2)
  }
  
  if (!any(lambda_vec > 0)) {
    ridge_pen <- 0 
  } else {
    ridge_pen <- purrr::map2_dbl(active_fleets, active_stock_list, fleet_ridge) |> sum()
  }
  
  tac_pen + ridge_pen
}


OptEffort_ms_Solver <- function(obj_fn, params_init, maxEval, tol) {
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

