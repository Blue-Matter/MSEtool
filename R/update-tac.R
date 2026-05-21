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
  
  for (sim in seq_len(Proj@OM@nSim)) {
    
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]

    Proj <- Update_TAC_Sim(
      Proj            = Proj,
      sim             = sim,
      Year            = Year,
      TSIndex         = TSIndex,
      AdviceList      = AdviceList,
      LastAdviceList  = LastAdviceList,
      StockNames      = StockNames,
      FleetNames      = FleetNames,
      Areas           = Areas
    )
  }
  
  Proj
}


# TODO 
# - add lambda_ascale to Fleet or Imp
# - add n_recent to OM@Control
# - add Choke to Imp

Update_TAC_Sim <- function(Proj, 
                           sim, 
                           Year,
                           TSIndex,
                           AdviceList,
                           LastAdviceList,
                           StockNames,
                           FleetNames, 
                           Areas,
                           lambda_scale = 0.001,
                           n_recent     = 5,
                           maxEval      = 500) {
  
  Complexes  <- Proj@OM@Complexes
  nComplex   <- length(Complexes)
  nFleet_loc <- length(FleetNames)
  nStock     <- length(StockNames)
  
  chk <- vapply(AdviceList, function(a) inherits(a, 'advice'), logical(1))
  if (any(!chk)) return(Proj)
  
  TAC_by_Complex     <- ResolveTACByComplex(AdviceList, LastAdviceList,
                                            Complexes, 
                                            Proj, 
                                            sim, 
                                            FleetNames)
  TACType_by_Complex <- ResolveTACTypeByComplex(AdviceList, Complexes)
  
  MaxFleetEffort     <- Proj@Effort[sim, TSIndex,]
  
  if (nComplex == 1) {
    
    Required_Effort <- OptEffort_singlestock(Proj, 
                                             Year, 
                                             TSIndex,
                                             sim, 
                                             TAC_by_Complex,
                                             TACType_by_Complex,
                                             MaxFleetEffort)
    
    Proj@Effort[sim, TSIndex, ] <- Required_Effort
    return(Proj)
  }
  
  # Multi-complex 
  Choke             <- ResolveChokeMatrix(Proj, nFleet_loc, nComplex)
  UndershootPenalty <- ResolveUndershootPenalty(Proj, nFleet_loc, nComplex)
  OvershootPenalty  <- ResolveOvershootPenalty(Proj, nFleet_loc, nComplex, Choke)
  PenaltyMode       <- ResolvePenaltyMode(Proj, nFleet_loc)
  lambda            <- ResolveLambda(Proj, sim, TSIndex, StockNames, FleetNames, lambda_scale, n_recent)

  result <- OptEffort_multi_stock(
    Proj               = Proj,
    Year               = Year,
    TSIndex            = TSIndex,
    sim                = sim,
    StockNames         = StockNames,
    FleetNames         = FleetNames,
    TAC_by_Complex     = TAC_by_Complex,
    TACType_by_Complex = TACType_by_Complex,
    Choke              = Choke,
    UndershootPenalty  = UndershootPenalty,
    OvershootPenalty   = OvershootPenalty,
    PenaltyMode        = PenaltyMode,
    lambda             = lambda,
    n_recent           = n_recent,
    maxEval            = maxEval
  )
  
  Proj@Effort[sim, TSIndex, ] <- result$Effort
  for (fl in seq_len(nFleet_loc))
    Proj@Misc$StockTargeting[sim, , fl, TSIndex] <- result$Delta[fl, ]

  Proj
}





 
