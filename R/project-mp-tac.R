#' Update effort to achieve TAC across all simulations
#'
#'
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years.
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names (unused here, kept for
#'   consistent `update_funs` signature).
#' @return Updated `Proj` object.
#' @keywords internal
Update_TAC <- function(Proj,
                       Year, 
                       AdviceSimList, 
                       LastAdviceSimList, 
                       YearsHist, 
                       YearsProj, 
                       Areas, 
                       FleetNames,
                       StockNames) {
  
  TSIndex <- match(Year, c(YearsHist, YearsProj))
  
  if (AllAdviceNull(AdviceSimList, 'TAC'))
    return(Proj)
  
  if (is.null(Proj@OM@Allocation) && !is.null(Proj@OM@CatchFrac))
    Proj@OM@Allocation <- Proj@OM@CatchFrac

  
  for (sim in seq_len(Proj@OM@nSim)) {
    
    # tictoc::tic("TAC Sim")
    
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    
    Proj <- Update_TAC_Sim(
      Proj            = Proj,
      sim             = sim,
      Year            = Year,
      TSIndex         = TSIndex,
      AdviceList      = AdviceList,
      LastAdviceList  = LastAdviceList,
      FleetNames      = FleetNames,
      Areas           = Areas
    )
    
    # tictoc::toc()
  }
  
  Proj
}

#' Update effort to achieve TAC for a single simulation
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param Year Integer. Current projection year.
#' @param TSIndex Integer. Time-step index of `Year` in `c(YearsHist, YearsProj)`.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param FleetNames Character vector of fleet names.
#' @param Areas Integer vector of area indices.
#' @return Updated `Proj` object.
#' @keywords internal
Update_TAC_Sim <- function(Proj,
                           sim,
                           Year, 
                           TSIndex,
                           AdviceList,
                           LastAdviceList,
                           FleetNames,
                           Areas) {
  
  Complexes    <- Proj@OM@Complexes
  ComplexNames <- names(Complexes)
  nComplex     <- length(Complexes)
  nFleet       <- length(FleetNames)
  nArea        <- length(Areas)
  
  chk <- purrr::map(AdviceList, \(Advice) inherits(Advice, 'advice')) |> unlist()
  if (any(!chk))
    return(Proj)
  
  TAC_by_Complex <- ResolveTACByComplex(
    AdviceList, LastAdviceList, Complexes, Proj, sim, nFleet
  )
  
  # Compute required effort per fleet x complex 
  RequiredEffort <- matrix(
    NA_real_, nrow = nFleet, ncol = nComplex,
    dimnames = list(Fleet = FleetNames, Complex = ComplexNames)
  )
  
  TACType_by_Complex <- vector("character", nComplex)
  
  for (i in seq_len(nComplex)) {
    if (is.null(TAC_by_Complex[[i]])) next
    
    stocks  <- Complexes[[i]]
    TAC_by_Fleet <- TAC_by_Complex[[i]]
    TACType <- AdviceList[[i]]@TACType
    TACType_by_Complex[i] <- TACType
    
    RequiredEffort[, i] <- OptEffort(
      Proj, Year, TSIndex, sim, stocks, TAC_by_Fleet, TACType
    )
    
  }
  
  # Binding effort = A fleet cannot exceed the effort implied by its most constraining TAC.
  FleetEffort <- apply(RequiredEffort, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else min(x)
  })
  
  # Constrain if there is an existing effort regulation
  for (fl in seq_len(nFleet)) {
    existing <- Proj@Effort[sim, TSIndex, fl]
    if (!is.na(existing)) {
      FleetEffort[fl] <- if (is.na(FleetEffort[fl])) existing else min(FleetEffort[fl], existing)
    }
  }

  Proj@Effort[sim, TSIndex, ] <- FleetEffort
  
  if (nComplex == 1) return(Proj)
  
  # multi-complex: choke-species effort scaling 
  stop("Multi-complex TAC not complete!")
  
  Proj
}


ResolveTACByComplex <- function(AdviceList, LastAdviceList, Complexes,
                                 Proj, sim, nFleet) {
  nComplex <- length(Complexes)
  out <- vector("list", nComplex)
  
  for (i in seq_len(nComplex)) {
    Advice <- AdviceList[[i]]
    
    # Fall back to previous advice if current is empty
    if (EmptyObject(Advice@TAC)) {
      prev <- LastAdviceList[[i]]
      if (!is.null(prev) && !EmptyObject(prev@TAC)) {
        Advice@TAC <- prev@TAC
      } else {
        next  # no usable advice for this complex
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
            stop("Proj@OM@Allocation[[", i, "]] is NULL but TAC is a scalar with nFleet > 1")
          all_sim  <- min(nrow(allocation), sim)
          out[[i]] <- as.numeric(TAC) * allocation[all_sim, ]
        }
      } else if (length(TAC) == nFleet) {
        out[[i]] <- as.numeric(TAC)
      } else {
        stop("Advice@TAC for complex ", i, " must be length 1 or length nFleet (", nFleet, "); got ", length(TAC))
      }
      
    } else if (length(dd) == 2) {
      # Fleet × Area TAC 
      if (!all(dd == c(nFleet, length(Proj@OM@Areas)))) # adjust slot name as needed
        stop("Advice@TAC for complex ", i, " must be nFleet × nArea (", nFleet, " × ", length(Proj@OM@Areas), ")")
      stop("TAC by Fleet × Area optimization is not yet implemented")
      
    } else {
      stop("Advice@TAC for complex ", i, " has unexpected dimensions")
    }
  }
  
  out
}




