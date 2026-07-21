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

.ResolveTACTypeByComplex <- function(AdviceList, Complexes, nFleet) {
  if (is.null(AdviceList[[1]]@TACType))
    AdviceList[[1]]@TACType <- 'Removals'
  
  lapply(seq_along(Complexes), function(i) {
    .RecycleToFleets(AdviceList[[i]]@TACType, nFleet, 'TACType')
  })
}

.ResolveTACUnitByComplex <- function(AdviceList, Complexes, nFleet) {
  if (is.null(AdviceList[[1]]@TACUnit))
    AdviceList[[1]]@TACUnit <- 'Biomass'
  
  lapply(seq_along(Complexes), function(i) {
    .RecycleToFleets(AdviceList[[i]]@TACUnit, nFleet, 'TACUnit')
  })
}

# [nFleet x nComplex] matrix of Imp@<ControlType>@Compliance for the current
# `sim`/`Year` (Compliance is populated to [Sim x Year] by PopulateImpSlot()),
# NA where unset (i.e. no complex/fleet pair configured, or that fleet/
# complex's Compliance is empty -- callers fall back to today's defaults).
# ControlType is 'TAC', 'Effort', or 'Size' -- for 'Size' the matrix isn't
# used by this helper's usual multi-stock-reconciliation callers, but the
# same lookup shape is available for anyone consuming Imp@Size@Compliance
# (see .UpdateSelectivitySim(), which does its own direct per-sim/fleet
# lookup instead of calling this, to avoid rebuilding the whole matrix on
# every sim).
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

.ResolveUndershootPenalty <- function(Proj, nFleet, nComplex) {
  matrix(1, nrow = nFleet, ncol = nComplex)
}

# Compliance is "how much this fleet reconciles its behaviour toward this
# complex's TAC when it competes with other complexes" (see ImpSlot()'s
# Compliance docs), continuously scaling the overshoot penalty:
# Compliance -> 0 means the fleet doesn't reconcile toward this complex at
# all, so overshoot is free (penalty = 0); Compliance = 0.5 reproduces
# today's pre-Compliance default (symmetric penalty = 1); Compliance -> 1
# approaches an effective hard choke, without needing a separate discrete
# mechanism. The ratio is capped at 1000x (matching the scale of the
# original discrete choke_mult in .OptEffortMultiStock()) rather than let
# it grow arbitrarily large -- an extreme penalty weight ill-conditions the
# optimiser and degrades constraint satisfaction rather than improving it.
.ResolveOvershootPenalty <- function(Proj, nFleet, nComplex, Compliance = NULL) {
  Penalty <- matrix(1, nrow = nFleet, ncol = nComplex)
  if (is.null(Compliance)) return(Penalty)

  set <- !is.na(Compliance)
  Penalty[set] <- Penalty[set] * Compliance[set] / pmax(1 - Compliance[set], 1e-3)
  Penalty
}

.ResolvePenaltyMode <- function(Proj, nFleet) {
  rep("soft", nFleet)
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
                          lambda_scale = 0.001, 
                          n_recent = 5) {
  
  # TODO - add user_defined lambda
  user_lambda <- NULL

  STarget <- Proj@Misc$StockTargeting[sim,,,seq_len(TSIndex-1), drop = FALSE] |> abind::adrop(1) 
  n_years <- dim(STarget)[3]
  
  nFleet <- length(FleetNames)
  
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
    
    # --- Apply user override if provided 
    # if (!is.null(user_lambda) && !is.na(user_lambda[fl]))
    #   raw[fl] <- user_lambda[fl]
  }
  
  raw * (lambda_scale / mean(raw))
}
