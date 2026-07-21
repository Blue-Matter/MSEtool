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
.UpdateTAC <- function(Proj,
                       Year, 
                       AdviceSimList, 
                       LastAdviceSimList, 
                       YearsHist, 
                       YearsProj, 
                       Areas, 
                       FleetNames,
                       StockNames) {
  
  TSIndex <- match(Year, c(YearsHist, YearsProj))
  
  if (.AllAdviceNull(AdviceSimList, 'TAC'))
    return(Proj)
  
  for (sim in seq_len(Proj@OM@nSim)) {
    
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]

    Proj <- .UpdateTACSim(
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

.UpdateTACSim <- function(Proj, 
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
  
  TAC_by_Complex     <- .ResolveTACByComplex(AdviceList, LastAdviceList,
                                            Complexes, Proj, sim, FleetNames)
  TAC_by_Complex     <- .ApplyImplementationError(TAC_by_Complex, Proj, FleetNames,
                                                 names(Complexes), sim, Year, 'TAC')

  TACType_by_Complex <- .ResolveTACTypeByComplex(AdviceList, Complexes, nFleet_loc)
  TACUnit_by_Complex <- .ResolveTACUnitByComplex(AdviceList, Complexes, nFleet_loc)

  # A fleet's effort is only ceiling-constrained by Effort advice if some
  # complex actually set Effort this year - .UpdateEffort() runs earlier in
  # the pipeline (see .ProjectMP()) and, if so, has already written the
  # resolved value into Proj@Effort by this point. Otherwise
  # Proj@Effort[sim, TSIndex, ] is just whatever was left there previously
  # (e.g. forward-filled from an earlier year's decision) and must not be
  # mistaken for a deliberate ceiling - so users can set both TAC and
  # Effort advice for the same stock/fleet, and whichever is more binding
  # applies: TAC-solving will never push a fleet's effort above an Effort
  # advice that was actually set this year.
  HasEffortAdvice <- any(purrr::map_lgl(AdviceList, \(a) {
    inherits(a, 'advice') && !is.null(a@Effort)
  }))
  MaxFleetEffort <- if (HasEffortAdvice) {
    Proj@Effort[sim, TSIndex, ]
  } else {
    rep(NA_real_, nFleet_loc)
  }

  if (nComplex == 1) {
    
    Required_Effort <- .OptEffortSinglestock(Proj, 
                                             Year, 
                                             TSIndex,
                                             sim, 
                                             TAC_by_Complex,
                                             TACType_by_Complex,
                                             TACUnit_by_Complex,
                                             MaxFleetEffort)
    
    Proj@Effort[sim, TSIndex, ] <- Required_Effort
    return(Proj)
  }
  
  # Multi-complex
  Compliance        <- .ResolveComplianceMatrix(Proj, FleetNames, names(Complexes), sim, Year)
  UndershootPenalty <- .ResolveUndershootPenalty(Proj, nFleet_loc, nComplex)
  OvershootPenalty  <- .ResolveOvershootPenalty(Proj, nFleet_loc, nComplex, Compliance)
  PenaltyMode       <- .ResolvePenaltyMode(Proj, nFleet_loc)
  lambda            <- .ResolveLambda(Proj, sim, TSIndex, StockNames, FleetNames, lambda_scale, n_recent)

  result <- .OptEffortMultiStock(
    Proj               = Proj,
    Year               = Year,
    TSIndex            = TSIndex,
    sim                = sim,
    StockNames         = StockNames,
    FleetNames         = FleetNames,
    TAC_by_Complex     = TAC_by_Complex,
    TACType_by_Complex = TACType_by_Complex,
    TACUnit_by_Complex = TACUnit_by_Complex,
    UndershootPenalty  = UndershootPenalty,
    OvershootPenalty   = OvershootPenalty,
    PenaltyMode        = PenaltyMode,
    MaxFleetEffort     = MaxFleetEffort,
    lambda             = lambda,
    n_recent           = n_recent,
    maxEval            = maxEval
  )
  
  Proj@Effort[sim, TSIndex, ] <- result$Effort
  for (fl in seq_len(nFleet_loc))
    Proj@Misc$StockTargeting[sim, , fl, TSIndex] <- result$Delta[fl, ]

  Proj
}





 
