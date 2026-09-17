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
#'
#' @details
#' For multi-complex `OM`s, the choke/targeting optimizer used to resolve
#' effort across complexes is tuned by `OM@Control$EffortOptim`, a named
#' list with elements:
#' - `lambda_scale`: `numeric(1)`, default `1`. Scales the complex-compliance
#'   penalty `lambda` passed to [.OptEffortChoke()].
#' - `n_recent`: `integer(1)`, default `5`. Number of recent *years* used to
#'   determine which complexes are currently active; converted to time
#'   steps (`n_recent * OM@Seasons`) before being passed to
#'   [.ResolveLambda()], and capped to however much history is available.
#' - `maxEval`: `integer(1)`, default `500`. Maximum solver evaluations
#'   passed to [.OptEffortChoke()].
#' @keywords internal
.UpdateTAC <- function(Proj,
                       Year,
                       AdviceSimList,
                       LastAdviceSimList,
                       YearsHist,
                       YearsProj,
                       Areas,
                       FleetNames,
                       StockNames,
                       EverySeason = FALSE) {

  TSIndex <- match(Year, c(YearsHist, YearsProj))
  Season  <- .SeasonOfYear(Year, c(YearsHist, YearsProj), Proj@OM@Seasons)

  if (.AllAdviceNull(AdviceSimList, 'TAC'))
    return(Proj)

  lambda_scale <- Proj@OM@Control$EffortOptim$lambda_scale %||% 1
  n_recent     <- Proj@OM@Control$EffortOptim$n_recent     %||% 5
  n_recent     <- n_recent * Proj@OM@Seasons
  maxEval      <- Proj@OM@Control$EffortOptim$maxEval      %||% 500

  for (sim in seq_len(Proj@OM@nSim)) {

    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]

    Proj <- .UpdateTACSim(
      Proj            = Proj,
      sim             = sim,
      Year            = Year,
      TSIndex         = TSIndex,
      Season          = Season,
      EverySeason     = EverySeason,
      AdviceList      = AdviceList,
      LastAdviceList  = LastAdviceList,
      StockNames      = StockNames,
      FleetNames      = FleetNames,
      Areas           = Areas,
      lambda_scale    = lambda_scale,
      n_recent        = n_recent,
      maxEval         = maxEval
    )
  }

  Proj
}


.UpdateTACSim <- function(Proj,
                           sim,
                           Year,
                           TSIndex,
                           AdviceList,
                           LastAdviceList,
                           StockNames,
                           FleetNames,
                           Areas,
                           Season      = NULL,
                           EverySeason = FALSE,
                           lambda_scale = 1,
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

  if (!EverySeason && !is.null(Season)) {
    for (i in seq_len(nComplex)) {
      SA <- Proj@OM@SeasonalAllocation[[i]]
      if (!is.null(SA))
        TAC_by_Complex[[i]] <- TAC_by_Complex[[i]] * SA[sim, Season, ]
    }
  }

  TACType_by_Complex <- .ResolveTACTypeByComplex(AdviceList, Complexes, nFleet_loc)
  TACUnit_by_Complex <- .ResolveTACUnitByComplex(AdviceList, Complexes, nFleet_loc)

  HasEffortAdvice <- any(purrr::map_lgl(AdviceList, \(a) {
    inherits(a, 'advice') && !is.null(a@Effort)
  }))
  MaxFleetEffort <- if (HasEffortAdvice) {
    Proj@Effort[sim, TSIndex, ]
  } else {
    rep(NA_real_, nFleet_loc)
  }

  # Single-sim slice for the repeated fishery-dynamics probe calls inside
  # the optimisers below
  ProjSim <- .SliceSim(Proj, sim, .DynamicsProbeSlots)

  if (nComplex == 1) {

    Solved <- .OptEffortSingleStock(ProjSim,
                                    Year,
                                    TSIndex,
                                    1L,
                                    TAC_by_Complex,
                                    TACType_by_Complex,
                                    TACUnit_by_Complex,
                                    MaxFleetEffort)

    Proj@Effort[sim, TSIndex, ] <- Solved$Effort
    Proj <- .LogEffortConvergence(Proj, Solved$converged, Solved$saturated, sim, Year,
                                  TAC = Solved$TAC, Catch = Solved$Catch, FleetNames = FleetNames)
    return(Proj)
  }

  # Multi-complex
  Compliance <- .ResolveComplianceMatrix(Proj, FleetNames, names(Complexes), sim, Year)
  lambda     <- .ResolveLambda(Proj, sim, TSIndex, Complexes, FleetNames, lambda_scale, n_recent)

  result <- .OptEffortChoke(
    Proj               = ProjSim,
    Year               = Year,
    TSIndex            = TSIndex,
    sim                = 1L,
    StockNames         = StockNames,
    FleetNames         = FleetNames,
    TAC_by_Complex     = TAC_by_Complex,
    TACType_by_Complex = TACType_by_Complex,
    TACUnit_by_Complex = TACUnit_by_Complex,
    Compliance         = Compliance,
    MaxFleetEffort     = MaxFleetEffort,
    lambda             = lambda,
    n_recent           = n_recent,
    maxEval            = maxEval
  )
  
  Proj@Effort[sim, TSIndex, ] <- result$Effort
  for (fl in seq_len(nFleet_loc))
    Proj@Misc$StockTargeting[sim, , fl, TSIndex] <- result$Delta[fl, ]

  Proj <- .LogEffortConvergence(Proj, result$converged, result$saturated, sim, Year,
                                TAC = result$TAC, Catch = result$Catch)

  Proj
}

# Log a failed TAC/Effort solve
.LogEffortConvergence <- function(Proj, converged, saturated, sim, Year,
                                  TAC = NULL, Catch = NULL, FleetNames = NULL) {
  if (is.na(converged) || converged)
    return(Proj)

  if (isTRUE(saturated))
    return(Proj)

  msg <- "Effort/TAC solver: did not converge within tolerance. Realised landings/removals may not match the TAC advised by the MP."

  if (!is.null(TAC) && !is.null(Catch)) {
    shortfall <- TAC - Catch
    ok <- is.finite(shortfall) & TAC > 0
    if (any(ok)) {
      idx  <- which(ok)
      idx  <- idx[order(-abs(shortfall[idx]))]
      nms  <- if (!is.null(names(TAC))) names(TAC)[idx] else
              if (!is.null(FleetNames)) FleetNames[idx] else paste0('Fleet_', idx)
      detail <- sprintf("%s: TAC = %s, achieved = %s (%s%%)",
                        nms,
                        format(round(TAC[idx], 1), big.mark = ','),
                        format(round(Catch[idx], 1), big.mark = ','),
                        round(100 * Catch[idx] / TAC[idx], 1))
      msg <- paste0(msg, " ", paste(detail, collapse = '; '))
    }
  }

  Proj@Log$warning <- c(
    Proj@Log$warning,
    list(.NewLogEntry(msg, name = 'EffortConvergence', sim = sim, year = Year))
  )
  Proj
}





 
