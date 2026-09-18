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
          allocation <- Proj@OM@FleetAllocation[[i]]
          if (is.null(allocation))
            cli::cli_abort("Proj@OM@FleetAllocation[[{i}]] is NULL but TAC is a scalar with nFleet > 1", .internal=TRUE)
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

.ApplyImplementationError <- function(ValueByComplex, Proj, FleetNames, ComplexNames,
                                     sim, Year, ControlType = 'TAC') {
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
                          Complexes, FleetNames,
                          lambda_scale = 1,
                          n_recent = 5) {

  STarget <- Proj@Misc$StockTargeting[sim,,,seq_len(TSIndex-1), drop = FALSE] |> abind::adrop(1)  # [nStock x nFleet x nYear]
  n_years <- dim(STarget)[3]

  nFleet   <- length(FleetNames)
  nComplex <- length(Complexes)

  raw <- setNames(rep(1, nFleet), FleetNames)

  active_complex <- .GetActiveComplexes(Proj, sim, TSIndex, Complexes, FleetNames, n_recent)

  CTarget <- array(NA_real_, dim = c(nComplex, nFleet, n_years))
  for (cx in seq_len(nComplex)) {
    stock_idx <- Complexes[[cx]]
    CTarget[cx, , ] <- if (length(stock_idx) == 1L) {
      STarget[stock_idx, , ]
    } else {
      exp(apply(log(pmax(STarget[stock_idx, , , drop = FALSE], 1e-10)), c(2, 3), mean))
    }
  }

  for (fl in seq_len(nFleet)) {
    active_cx <- which(active_complex[fl, ])

    if (length(active_cx) == 0L || n_years < 2L) next

    # Mean-centred log_delta for each historical year
    log_delta_hist <- matrix(NA_real_, length(active_cx), n_years)
    for (yr in seq_len(n_years)) {
      d <- CTarget[active_cx, fl, yr]
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

  lambda <- matrix(derived, nrow = nFleet, ncol = nComplex,
                   dimnames = list(Fleet = FleetNames, Complex = names(Complexes)))

  Mult <- Proj@Misc$StockTargetingLambda
  if (!is.null(Mult)) {
    sim_m   <- min(sim, dim(Mult)[1])
    yr_m    <- min(TSIndex, dim(Mult)[3])
    m_stock <- t(Mult[sim_m, , yr_m, , drop = FALSE] |> abind::adrop(c(1, 3)))  # [nFleet x nStock]
    m_stock[!is.finite(m_stock) | m_stock < 0] <- 1
    m <- matrix(1, nFleet, nComplex)
    for (cx in seq_len(nComplex))
      m[, cx] <- rowMeans(m_stock[, Complexes[[cx]], drop = FALSE])
    lambda <- lambda * m
  }

  lambda
}
