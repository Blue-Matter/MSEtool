.ResolveTACByComplex <- function(AdviceList, LastAdviceList, Complexes,
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
      if (!all(dd == c(nFleet, nArea(Proj@OM)))) 
        cli::cli_abort("Advice@TAC for complex {i} must be nFleet x nArea ({nFleet} x {length(nArea(Proj@OM))})", .internal=TRUE)
      cli::cli_abort("TAC by Fleet x Area optimization is not yet implemented")
      
    } else {
      cli::cli_abort("Advice@TAC for complex {i} has unexpected dimensions", .internal=TRUE)
    }
  }
  
  names(out) <- ComplexNames
  out
}

.ResolveTACTypeByComplex <- function(AdviceList, Complexes, nFleet) {
  lapply(seq_along(Complexes), function(i) {
    TACType <- AdviceList[[i]]@TACType
    if (!length(TACType)) TACType <- 'Removals'
    .RecycleToFleets(TACType, nFleet, 'TACType')
  })
}

.ResolveTACUnitByComplex <- function(AdviceList, Complexes, nFleet) {
  lapply(seq_along(Complexes), function(i) {
    TACUnit <- AdviceList[[i]]@TACUnit
    if (!length(TACUnit)) TACUnit <- 'Biomass'
    .RecycleToFleets(TACUnit, nFleet, 'TACUnit')
  })
}


.ResolveComplianceMatrix <- function(Proj, FleetNames, ComplexNames, sim, Year, ControlType = 'TAC') {
  nFleet   <- length(FleetNames)
  nComplex <- length(ComplexNames)
  Compliance <- matrix(NA_real_, nrow = nFleet, ncol = nComplex,
                       dimnames = list(Fleet = FleetNames, Complex = ComplexNames))
  yr_chr <- as.character(Year)

  for (cx in seq_len(nComplex)) {
    ImpCx <- Proj@OM@Imp[[ComplexNames[cx]]]
    if (is.null(ImpCx)) next
    for (fl in seq_len(nFleet)) {
      ImpObj <- ImpCx[[FleetNames[fl]]]
      if (is.null(ImpObj)) next
      comp <- slot(ImpObj, ControlType)@Compliance
      if (!length(comp)) next
      Compliance[fl, cx] <- if (!is.null(dim(comp)) && yr_chr %in% dimnames(comp)$Year) {
        comp[min(sim, nrow(comp)), yr_chr]
      } else {
        as.numeric(comp)[1]
      }
    }
  }
  Compliance
}

# Applies Imp@<ControlType>@Error[sim, Year] as a multiplicative
# implementation-error factor to each complex's per-fleet advised value
# (TAC or Effort), before the advice is passed to effort-solving. Matches
# legacy's TACFrac/TACSD/TAC_y (and TAEFrac/TAESD/E_y) mechanism. A missing
# Imp object, Error slot, or year leaves that complex/fleet's value
# unchanged (multiplier of 1).
.ApplyImplementationError <- function(ValueByComplex, Proj, FleetNames, ComplexNames,
                                     sim, Year, ControlType = 'TAC') {
  # Interim (pre-MPStartYear) values represent actual/plausible realised
  # catch or effort, not a management recommendation - Imp error does not
  # apply to them.
  MPStartYear <- Proj@OM@MPStartYear
  if (!is.null(MPStartYear) && floor(Year) < MPStartYear)
    return(ValueByComplex)

  nFleet <- length(FleetNames)
  yr_chr <- as.character(Year)

  for (cx in seq_along(ValueByComplex)) {
    if (is.null(ValueByComplex[[cx]])) next
    ImpCx <- Proj@OM@Imp[[ComplexNames[cx]]]
    if (is.null(ImpCx)) next

    for (fl in seq_len(min(nFleet, length(ValueByComplex[[cx]])))) {
      ImpObj <- ImpCx[[FleetNames[fl]]]
      if (is.null(ImpObj)) next

      Error <- slot(ImpObj, ControlType)@Error
      if (!length(Error) || !yr_chr %in% dimnames(Error)$Year) next

      mult <- Error[min(sim, nrow(Error)), yr_chr]
      ValueByComplex[[cx]][fl] <- ValueByComplex[[cx]][fl] * mult
    }
  }
  ValueByComplex
}

.ResolveLambda <- function(Proj, sim, TSIndex,
                          StockNames, FleetNames, 
                          lambda_scale = 1, 
                          n_recent = 5) {
  
  STarget <- Proj@Misc$StockTargeting[sim,,,seq_len(TSIndex-1), drop = FALSE] |> abind::adrop(1)
  n_years <- dim(STarget)[3]

  nFleet <- length(FleetNames)
  nStock <- length(StockNames)

  raw <- setNames(rep(1, nFleet), FleetNames)
  
  active_stock <- .GetActiveStocks(Proj, sim, TSIndex, StockNames, FleetNames,
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
  }

  # derived per-fleet weight, normalised so mean(lambda) == lambda_scale
  derived <- raw * (lambda_scale / mean(raw))

  # Expand to [Fleet x Stock] and apply the user multiplier from
  # Effort@StockTargetingLambda. Absent/non-finite entries default to 1, so an
  # unset multiplier reproduces the derived value exactly.
  lambda <- matrix(derived, nrow = nFleet, ncol = nStock,
                   dimnames = list(Fleet = FleetNames, Stock = StockNames))

  Mult <- Proj@Misc$StockTargetingLambda
  if (!is.null(Mult)) {
    sim_m <- min(sim, dim(Mult)[1])
    yr_m  <- min(TSIndex, dim(Mult)[3])
    m     <- t(Mult[sim_m, , yr_m, , drop = FALSE] |> abind::adrop(c(1, 3)))
    m[!is.finite(m) | m < 0] <- 1
    lambda <- lambda * m
  }

  lambda
}
