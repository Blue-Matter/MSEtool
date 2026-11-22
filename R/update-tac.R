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
#' - `lambda_scale`: `numeric(1)`, default `1`. Scales the penalty `lambda`
#'   on year-to-year change in a fleet's targeting mix, passed to
#'   [.OptEffortChoke()].
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

  TAC_by_Complex     <- .ResolveTACByComplex(AdviceList, Complexes, Proj, sim, FleetNames)
  if (all(vapply(TAC_by_Complex, is.null, logical(1)))) return(Proj)
  TAC_Advised        <- TAC_by_Complex
  TAC_by_Complex     <- .ApplyImplementationError(TAC_by_Complex, Proj, FleetNames,
                                                 names(Complexes), sim, Year, 'TAC')

  if (!EverySeason && !is.null(Season)) {
    for (i in seq_len(nComplex)) {
      SA <- Proj@OM@SeasonalAllocation[[i]]
      if (!is.null(SA) && !is.null(TAC_by_Complex[[i]])) {
        TAC_by_Complex[[i]] <- TAC_by_Complex[[i]] * SA[sim, Season, ]
        TAC_Advised[[i]]    <- TAC_Advised[[i]] * SA[sim, Season, ]
      }
    }
  }

  TACType_by_Complex <- .ResolveTACTypeByComplex(AdviceList, Complexes, nFleet_loc)
  TACUnit_by_Complex <- .ResolveTACUnitByComplex(AdviceList, Complexes, nFleet_loc)

  # an effort control caps a fleet's TAC effort; fleets without one have NA
  EffortSet <- Proj@Misc$EffortSet
  EffortSet <- if (is.null(EffortSet)) rep(FALSE, nFleet_loc) else EffortSet[sim, ]
  MaxFleetEffort <- ifelse(EffortSet, Proj@Effort[sim, TSIndex, ], NA_real_)

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
    # fleets without a TAC fish their nominal effort, shortened by this step's stop-mode fishing time
    Nominal <- ProjSim@Effort[1, TSIndex, ]
    SolveBag <- function(P, TimeFrac) {
      P@Effort[1, TSIndex, ] <- Nominal * TimeFrac
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
    Unresolved <- isFALSE(Res$converged) && !isTRUE(Res$saturated)
    Proj <- .RecordTACAttainment(Proj, sim, YearIndex,
                                 TAC        = matrix(Res$TAC, nrow = 1),
                                 Catch      = matrix(Res$Catch, nrow = 1),
                                 AtCeiling  = Res$AtCeiling,
                                 Advised    = .TACListMatrix(TAC_Advised, nFleet_loc),
                                 Unresolved = matrix(Unresolved, 1, nFleet_loc),
                                 Binding    = ifelse(is.finite(Res$TAC), 1L, NA_integer_),
                                 Type       = do.call(rbind, TACType_by_Complex),
                                 Unit       = do.call(rbind, TACUnit_by_Complex))
    Proj <- .LogEffortConvergence(Proj, Res$converged, Res$saturated, sim, Year,
                                  TAC = Res$TAC, Catch = Res$Catch, FleetNames = FleetNames)
    return(Proj)
  }

  result <- Solved$Res
  for (fl in seq_len(nFleet_loc))
    Proj@Misc$StockTargeting[sim, , fl, TSIndex] <- result$Delta[fl, ]

  nCx <- length(result$TAC)
  Proj <- .RecordTACAttainment(Proj, sim, YearIndex,
                               TAC        = result$TACMatrix,
                               Catch      = result$CatchMatrix,
                               AtCeiling  = result$AtCeiling,
                               Advised    = .TACListMatrix(TAC_Advised, nFleet_loc),
                               Unresolved = matrix(result$Unresolved, nCx, nFleet_loc),
                               Binding    = result$Binding,
                               Type       = do.call(rbind, TACType_by_Complex),
                               Unit       = do.call(rbind, TACUnit_by_Complex))

  Proj <- .LogEffortConvergence(Proj, result$converged, result$saturated, sim, Year,
                                TAC = result$TAC, Catch = result$Catch)

  Proj
}

# [nComplex x nFleet] matrix of a per-complex list of fleet TACs; NA for a complex without one
.TACListMatrix <- function(TAC_by_Complex, nFleet) {
  do.call(rbind, lapply(TAC_by_Complex, \(x) if (is.null(x)) rep(NA_real_, nFleet) else as.numeric(x)))
}

# Empty TAC attainment record ([Sim, Year, Fleet] arrays, per complex except AtCeiling/Binding);
# Advised is before implementation error, Catch is the solver's catch, Realised the engine's
.InitTACAttainment <- function(Proj, YearsProj) {
  DN  <- list(Sim = seq_len(Proj@OM@nSim), Year = YearsProj, Fleet = FleetNames(Proj))
  Cx  <- names(Proj@OM@Complexes)
  PerComplex <- function(value)
    stats::setNames(rep(list(array(value, unname(lengths(DN)), dimnames = DN)), length(Cx)), Cx)
  list(TAC        = PerComplex(NA_real_),
       Catch      = PerComplex(NA_real_),
       AtCeiling  = array(NA, unname(lengths(DN)), dimnames = DN),
       Advised    = PerComplex(NA_real_),
       Realised   = PerComplex(NA_real_),
       Type       = PerComplex(NA_character_),
       Unit       = PerComplex(NA_character_),
       Unresolved = PerComplex(NA),
       Binding    = array(NA_integer_, unname(lengths(DN)), dimnames = DN))
}

# TAC, Catch, Advised, Unresolved, Type and Unit are [nComplex x nFleet]; AtCeiling and Binding per fleet
.RecordTACAttainment <- function(Proj, sim, YearIndex, TAC, Catch, AtCeiling, Advised = NULL,
                                 Unresolved = NULL, Binding = NULL, Type = NULL, Unit = NULL) {
  Rec <- Proj@Misc$TACAttainment
  if (is.null(Rec) || is.na(YearIndex) || is.null(TAC)) return(Proj)
  PerComplex <- list(TAC = TAC, Catch = Catch, Advised = Advised, Unresolved = Unresolved,
                     Type = Type, Unit = Unit)
  for (nm in names(PerComplex)) {
    if (is.null(PerComplex[[nm]]) || is.null(Rec[[nm]])) next
    for (cx in seq_along(Rec$TAC))
      Rec[[nm]][[cx]][sim, YearIndex, ] <- PerComplex[[nm]][cx, ]
  }
  Rec$AtCeiling[sim, YearIndex, ] <- AtCeiling
  if (!is.null(Binding) && !is.null(Rec$Binding))
    Rec$Binding[sim, YearIndex, ] <- as.integer(Binding)
  Proj@Misc$TACAttainment <- Rec
  Proj
}

# Catch of each complex by fleet after the time step's dynamics, in the catch type and unit of its TAC
.RecordRealisedCatch <- function(Proj, YearIndex, TSIndex) {
  Rec <- Proj@Misc$TACAttainment
  if (is.null(Rec$Realised) || is.na(YearIndex) || is.na(TSIndex)) return(Proj)
  Complexes <- Proj@OM@Complexes
  nSim      <- Proj@OM@nSim
  nFleet    <- dim(Proj@Landings)[4]
  SumStocks <- \(x) matrix(apply(x, c(1, 4), sum), nSim, nFleet)
  AtAge     <- \(Slot, stocks) Reduce(`+`, lapply(Slot[stocks], \(x)
    matrix(apply(x[, , TSIndex, , , drop = FALSE], c(1, 4), sum), nSim, nFleet)))
  for (cx in seq_along(Rec$TAC)) {
    Has <- matrix(is.finite(Rec$TAC[[cx]][, YearIndex, ]), nSim, nFleet)
    if (!any(Has)) next
    stocks  <- Complexes[[cx]]
    Unit    <- matrix(Rec$Unit[[cx]][, YearIndex, ], nSim, nFleet)
    Removal <- matrix(Rec$Type[[cx]][, YearIndex, ], nSim, nFleet) == 'Removals'
    Removal[is.na(Removal)] <- TRUE
    Value <- SumStocks(Proj@Landings[, stocks, TSIndex, , drop = FALSE]) +
      Removal * SumStocks(Proj@Discards[, stocks, TSIndex, , drop = FALSE])
    Number <- Has & !is.na(Unit) & Unit == 'Number'
    if (any(Number)) {
      ValueN <- AtAge(Proj@LandingsAtAge, stocks) + Removal * AtAge(Proj@DiscardsAtAge, stocks)
      Value[Number] <- ValueN[Number]
    }
    Value[!Has] <- NA
    Rec$Realised[[cx]][, YearIndex, ] <- Value
  }
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





 
