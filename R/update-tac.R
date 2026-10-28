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
#'   steps (`n_recent * OM@Seasons`) 
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
  Season  <- SeasonOfYear(c(YearsHist, YearsProj), Proj@OM@Seasons, Year)$SeasonInd

  if (.AllAdviceNull(AdviceSimList, 'TAC'))
    return(Proj)

  lambda_scale <- Proj@OM@Control$EffortOptim$lambda_scale %||NA% 1
  n_recent     <- Proj@OM@Control$EffortOptim$n_recent     %||NA% 5
  n_recent     <- n_recent * Proj@OM@Seasons
  maxEval      <- Proj@OM@Control$EffortOptim$maxEval      %||NA% 500

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
      YearIndex       = match(Year, YearsProj),
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
                           YearIndex   = NA,
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
  if (all(vapply(TAC_by_Complex, is.null, logical(1)))) return(Proj)
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

  EffortMax <- .FleetMaximumEffort(Proj, sim, TSIndex)

  # Single-sim slice for the repeated fishery-dynamics probe calls inside
  # the optimisers below
  ProjSim <- .SliceSim(Proj, sim, .DynamicsProbeSlots)

  # stop-mode bag limits shorten fishing time, so they scale the effort ceilings
  ScaleCap <- function(x, TimeFrac) if (is.null(x)) NULL else x * TimeFrac

  if (nComplex == 1) {
    SolveEffort <- function(P, TimeFrac = rep(1, nFleet_loc)) {
      Cap <- ScaleCap(EffortMax, TimeFrac)
      Res <- .OptEffortSingleStock(P,
                                   Year,
                                   TSIndex,
                                   1L,
                                   TAC_by_Complex,
                                   TACType_by_Complex,
                                   TACUnit_by_Complex,
                                   MaxFleetEffort * TimeFrac,
                                   EffortMax = Cap)
      list(Effort = .ApplyEffortCeiling(Res$Effort, Cap), Proj = P, Res = Res)
    }
  } else {
    Compliance <- .ResolveComplianceMatrix(Proj, FleetNames, names(Complexes), sim, Year)
    lambda     <- .ResolveLambda(Proj, sim, TSIndex, Complexes, FleetNames, lambda_scale, n_recent)

    SolveEffort <- function(P, TimeFrac = rep(1, nFleet_loc)) {
      Cap <- ScaleCap(EffortMax, TimeFrac)
      Res <- .OptEffortChoke(
        Proj               = P,
        Year               = Year,
        TSIndex            = TSIndex,
        sim                = 1L,
        StockNames         = StockNames,
        FleetNames         = FleetNames,
        TAC_by_Complex     = TAC_by_Complex,
        TACType_by_Complex = TACType_by_Complex,
        TACUnit_by_Complex = TACUnit_by_Complex,
        Compliance         = Compliance,
        MaxFleetEffort     = MaxFleetEffort * TimeFrac,
        EffortMax          = Cap,
        lambda             = lambda,
        n_recent           = n_recent,
        maxEval            = maxEval
      )
      for (fl in seq_len(nFleet_loc))
        P@Misc$StockTargeting[1, , fl, TSIndex] <- Res$Delta[fl, ]
      list(Effort = .ApplyEffortCeiling(Res$Effort, Cap), Proj = P, Res = Res)
    }
  }

  BagCons <- .BagLimitConstraints(Proj, sim, TSIndex, AdviceList, .AggBagLimitSim(Proj, Year, sim),
                                  FleetNames, StockNames)
  HasTAC  <- any(!vapply(TAC_by_Complex, is.null, logical(1)))

  if (length(BagCons) && HasTAC) {
    # last step's effort is carried forward for fleets without a TAC: restore its trips,
    # then shorten them by this step's stop-mode fishing-time fraction
    PrevYear    <- as.numeric(dimnames(Proj@Effort)[[2]][TSIndex - 1L])
    PrevNominal <- ProjSim@Effort[1, TSIndex - 1L, ] / .BagTimeFraction(Proj, PrevYear)[sim, ]
    SolveBag <- function(P, TimeFrac) {
      P@Effort[1, TSIndex - 1L, ] <- PrevNominal * TimeFrac
      SolveEffort(P, TimeFrac)
    }
    Sol    <- .SolveBagLimit(ProjSim, Year, TSIndex, BagCons, SolveEffort = SolveBag)
    Proj   <- .WriteBagLimitSolution(Proj, sim, TSIndex, Year, Sol, BagCons)
    Proj   <- .LogBagLimitConvergence(Proj, Sol, sim, Year)
    Solved <- Sol$Solved
    if (is.null(Proj@Misc$BagLimitSolvedTS))
      Proj@Misc$BagLimitSolvedTS <- rep(NA_integer_, Proj@OM@nSim)
    Proj@Misc$BagLimitSolvedTS[sim] <- TSIndex
  } else {
    Solved <- SolveEffort(ProjSim)
    Proj@Effort[sim, TSIndex, ] <- Solved$Effort
  }

  if (nComplex == 1) {
    Res <- Solved$Res
    Proj <- .RecordTACAttainment(Proj, sim, YearIndex,
                                 TAC       = matrix(Res$TAC, nrow = 1),
                                 Catch     = matrix(Res$Catch, nrow = 1),
                                 AtCeiling = Res$AtCeiling)
    Proj <- .LogEffortConvergence(Proj, Res$converged, Res$saturated, sim, Year,
                                  TAC = Res$TAC, Catch = Res$Catch, FleetNames = FleetNames)
    return(Proj)
  }

  result <- Solved$Res
  for (fl in seq_len(nFleet_loc))
    Proj@Misc$StockTargeting[sim, , fl, TSIndex] <- result$Delta[fl, ]

  Proj <- .RecordTACAttainment(Proj, sim, YearIndex,
                               TAC       = result$TACMatrix,
                               Catch     = result$CatchMatrix,
                               AtCeiling = result$AtCeiling)

  Proj <- .LogEffortConvergence(Proj, result$converged, result$saturated, sim, Year,
                                TAC = result$TAC, Catch = result$Catch)

  Proj
}

# Empty TAC attainment record for a projection: per complex, the effective
# TAC and the catch at the solved effort ([Sim, Year, Fleet]), and whether
# each fleet's effort is at its ceiling
.InitTACAttainment <- function(Proj, YearsProj) {
  DN  <- list(Sim = seq_len(Proj@OM@nSim), Year = YearsProj, Fleet = FleetNames(Proj))
  arr <- array(NA_real_, unname(lengths(DN)), dimnames = DN)
  Cx  <- names(Proj@OM@Complexes)
  list(TAC       = stats::setNames(rep(list(arr), length(Cx)), Cx),
       Catch     = stats::setNames(rep(list(arr), length(Cx)), Cx),
       AtCeiling = array(NA, unname(lengths(DN)), dimnames = DN))
}

# TAC and Catch are [nComplex x nFleet]
.RecordTACAttainment <- function(Proj, sim, YearIndex, TAC, Catch, AtCeiling) {
  Rec <- Proj@Misc$TACAttainment
  if (is.null(Rec) || is.na(YearIndex) || is.null(TAC)) return(Proj)
  for (cx in seq_along(Rec$TAC)) {
    Rec$TAC[[cx]][sim, YearIndex, ]   <- TAC[cx, ]
    Rec$Catch[[cx]][sim, YearIndex, ] <- Catch[cx, ]
  }
  Rec$AtCeiling[sim, YearIndex, ] <- AtCeiling
  Proj@Misc$TACAttainment <- Rec
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

  # Level: single-stock path passes FleetNames (per-fleet TAC/Catch);
  # the multi-complex/choke path doesn't (per-complex TAC/Catch instead).
  Level     <- if (!is.null(FleetNames)) 'Fleet' else 'Complex'
  Names     <- character(0)
  TAC_out   <- numeric(0)
  Catch_out <- numeric(0)

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

      Names     <- nms
      TAC_out   <- unname(TAC[idx])
      Catch_out <- unname(Catch[idx])
    }
  }

  entry        <- .NewLogEntry(msg, name = 'EffortConvergence', sim = sim, year = Year)
  entry$Level  <- Level
  entry$Names  <- Names
  entry$TAC    <- TAC_out
  entry$Catch  <- Catch_out

  Proj@Log$warning <- c(Proj@Log$warning, list(entry))
  Proj
}





 
