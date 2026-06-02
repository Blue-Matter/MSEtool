ResolveTACByComplex <- function(AdviceList, LastAdviceList, Complexes,
                                Proj, sim, FleetNames) {
  
  nFleet <- length(FleetNames)
  nComplex <- length(Complexes)
  ComplexNames <- names(Complexes)
  out <- vector("list", nComplex)
  
  for (i in seq_len(nComplex)) {
    Advice <- AdviceList[[i]]
    
    if (EmptyObject(Advice@TAC)) {
      prev <- LastAdviceList[[i]]
      if (!is.null(prev) && !EmptyObject(prev@TAC)) {
        Advice@TAC <- prev@TAC
      } else {
        next  
      }
    }
    
    TAC <- Advice@TAC
    dd  <- dim(TAC)
    
    if (is.null(dd) || length(dd) == 1) {
      
      if (length(TAC) == 1) {
        if (nFleet == 1) {
          out[[i]] <- as.numeric(TAC)
        } else {
          allocation <- Proj@OM@Allocation[[i]]
          if (is.null(allocation))
            cli::cli_abort("Proj@OM@Allocation[[{i}]] is NULL but TAC is a scalar with nFleet > 1", .internal=TRUE)
          all_sim  <- min(nrow(allocation), sim)
          out[[i]] <- as.numeric(TAC) * allocation[all_sim, ]
        }
      } else if (length(TAC) == nFleet) {
        out[[i]] <- as.numeric(TAC)
      } else {
        cli::cli_abort("Advice@TAC for complex {i} must be length 1 or length nFleet ({nFleet}); got {length(TAC)}", .internal=TRUE)
      }
      
    } else if (length(dd) == 2) {
      # Fleet x Area TAC 
      if (!all(dd == c(nFleet, length(Proj@OM@Areas)))) # adjust slot name as needed
        cli::cli_abort("Advice@TAC for complex {i} must be nFleet x nArea ({nFleet} x {length(nArea(Proj@OM))})", .internal=TRUE)
      cli::cli_abort("TAC by Fleet x Area optimization is not yet implemented")
      
    } else {
      cli::cli_abort("Advice@TAC for complex {i} has unexpected dimensions", .internal=TRUE)
    }
  }
  
  names(out) <- ComplexNames
  out
}

ResolveTACTypeByComplex <- function(AdviceList, Complexes, nFleet) {
  lapply(seq_along(Complexes), function(i) {
    recycle_to_fleets(AdviceList[[i]]@TACType, nFleet, 'TACType')
  })
}

ResolveTACUnitByComplex <- function(AdviceList, Complexes, nFleet) {
  lapply(seq_along(Complexes), function(i) {
    recycle_to_fleets(AdviceList[[i]]@TACUnit, nFleet, 'TACUnit')
  })
}

ResolveChokeMatrix <- function(Proj, nFleet, nComplex) {
  matrix(0L, nrow = nFleet, ncol = nComplex)
}

ResolveUndershootPenalty <- function(Proj, nFleet, nComplex) {
  matrix(1, nrow = nFleet, ncol = nComplex)
}

ResolveOvershootPenalty <- function(Proj, nFleet, nComplex, Choke) {
  matrix(1, nrow = nFleet, ncol = nComplex)
}

ResolvePenaltyMode <- function(Proj, nFleet) {
  rep("soft", nFleet)
}

ResolveLambda <- function(Proj, sim, TSIndex, 
                          StockNames, FleetNames, 
                          lambda_scale = 0.001, 
                          n_recent = 5) {
  
  # TODO - add user_defined lambda
  user_lambda <- NULL

  STarget <- Proj@Misc$StockTargeting[sim,,,seq_len(TSIndex-1), drop = FALSE] |> abind::adrop(1) 
  n_years <- dim(STarget)[3]
  
  nFleet <- length(FleetNames)
  
  raw <- setNames(rep(1, nFleet), FleetNames)
  
  active_stock <- GetActiveStocks(Proj, sim, TSIndex, StockNames, FleetNames,
                                  n_recent)
  
  for (fl in seq_len(nFleet)) {
    active_s <- which(active_stock[fl, ])
    
    if (length(active_s) == 0L || n_years < 2L) next
    
    # Mean-centred log_delta for each historical year
    log_delta_hist <- matrix(NA_real_, length(active_s), n_years)
    for (yr in seq_len(n_years)) {
      d <- STarget[active_s, fl, yr]
      if (any(!is.finite(d) | d <= 0)) next
      ld <- log(d)
      log_delta_hist[, yr] <- ld - mean(ld)
    }
    
    # Year-to-year changes across valid consecutive years
    valid_cols <- which(apply(log_delta_hist, 2, function(x) all(is.finite(x))))
    if (length(valid_cols) < 2L) next
    
    changes <- log_delta_hist[, valid_cols[-1], drop = FALSE] -
      log_delta_hist[, valid_cols[-length(valid_cols)], drop = FALSE]
    
    # 1/sd: inverse of typical targeting volatility
    sd_f <- sqrt(mean(changes^2))
    if (!is.finite(sd_f) || sd_f <= 0) next
    raw[fl] <- 1 / sd_f
    
    # --- Apply user override if provided 
    # if (!is.null(user_lambda) && !is.na(user_lambda[fl]))
    #   raw[fl] <- user_lambda[fl]
  }
  
  raw * (lambda_scale / mean(raw))
}
