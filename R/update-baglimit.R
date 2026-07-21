# Post-populate Sim x Year array (TripsScalar/AnglerPerTrip/Theta): Sim dim
# is 1 or nSim, Year dim is the full model time series - index with
# recycling on Sim only.
.GetEffortArrayValue <- function(x, sim, TSIndex) {
  x[min(sim, nrow(x)), TSIndex]
}

.CalcTrips <- function(Proj, sim, TSIndex, st, fl) {
  Eff <- Proj@OM@Fleet[[st]][[fl]]@Effort
  E <- Proj@Effort[sim, TSIndex, fl]

  if (identical(Eff@Units, "trips"))
    return(E)

  if (is.null(Eff@TripsScalar))
    cli::cli_abort(
      "`Fleet@Effort@TripsScalar` must be supplied when `Units != \"trips\"` for a bag-limit fleet",
      .internal = TRUE
    )

  .GetEffortArrayValue(Eff@TripsScalar, sim, TSIndex) * E
}

### LimitTypeOverride lets a stock's LimitType be substituted with an
# aggregate group's, when that stock's BagLimit is a species-specific
# sub-cap on a stock also covered by the group (the group's LimitType
# governs enforcement of both, per AggregateBagLimit()'s documented rule).
.CalcBagLimitCap <- function(Advice, Proj, sim, TSIndex, st, fl, LimitTypeOverride = NULL) {
  BagLimit  <- Advice@BagLimit
  BagLimit_fl <- if (length(BagLimit) == 1) BagLimit else BagLimit[fl]

  if (!is.null(LimitTypeOverride)) {
    LimitType_fl <- LimitTypeOverride
  } else {
    LimitType <- Advice@LimitType
    LimitType_fl <- if (length(LimitType) == 1) LimitType else LimitType[fl]
  }

  if (identical(LimitType_fl, "boat"))
    return(BagLimit_fl)

  Eff <- Proj@OM@Fleet[[st]][[fl]]@Effort
  if (is.null(Eff@AnglerPerTrip))
    cli::cli_abort(
      "`Fleet@Effort@AnglerPerTrip` must be supplied when `LimitType == \"angler\"`",
      .internal = TRUE
    )

  BagLimit_fl * .GetEffortArrayValue(Eff@AnglerPerTrip, sim, TSIndex)
}

# Resolves a character (name) or numeric (position) index against a names
# vector - used for `aggbaglimit@Fleet`/`@Stocks`, which may be either.
.ResolveIndex <- function(x, names) {
  if (is.character(x)) match(x, names) else as.integer(x)
}

.GroupKey <- function(st, fl) paste(st, fl, sep = "_")

# Maps every (stock, fleet) pair covered by an active aggregate group to
# that group - used so a stock's own species-specific BagLimit sub-cap is
# enforced with the group's LimitType/ClosureMode instead of its own, when
# that stock is a group member for the same fleet.
.BuildGroupLookup <- function(AggBagLimit, FleetNames, StockNames) {
  lookup <- list()
  for (grp in AggBagLimit) {
    if (!inherits(grp, 'aggbaglimit')) next
    fl     <- .ResolveIndex(grp@Fleet, FleetNames)
    stocks <- .ResolveIndex(grp@Stocks, StockNames)
    for (st in stocks)
      lookup[[.GroupKey(st, fl)]] <- grp
  }
  lookup
}

# Same as .CalcBagLimitCap() but for an `aggbaglimit` group, whose
# BagLimit/LimitType are scalar (not per-fleet vectors) and whose
# AnglerPerTrip is read from the first stock in the group (fleet trip
# behaviour is assumed consistent across the stocks it is pooled over).
.CalcGroupBagLimitCap <- function(grp, Proj, sim, TSIndex, st, fl) {
  if (identical(grp@LimitType, "boat"))
    return(grp@BagLimit)

  Eff <- Proj@OM@Fleet[[st]][[fl]]@Effort
  if (is.null(Eff@AnglerPerTrip))
    cli::cli_abort(
      "`Fleet@Effort@AnglerPerTrip` must be supplied when `LimitType == \"angler\"`",
      .internal = TRUE
    )

  grp@BagLimit * .GetEffortArrayValue(Eff@AnglerPerTrip, sim, TSIndex)
}

# E[max(X - Bcap, 0)] for X ~ NegBin(mean = mu, dispersion = theta), via
# truncated summation (no `actuar` dependency).
.CalcNegBinExcess <- function(mu, theta, Bcap, quantile = 0.9999) {
  if (mu <= 0 || Bcap < 0) return(0)

  xmax <- stats::qnbinom(quantile, size = theta, mu = mu)
  if (xmax <= Bcap) return(0)

  x <- (floor(Bcap) + 1):xmax
  sum((x - Bcap) * stats::dnbinom(x, size = theta, mu = mu))
}

# rho_f(t) = E[min(X, Bcap)] / mu, the fraction of unconstrained retained
# catch actually kept under ClosureMode = "discard" (Section 2.5).
.CalcDiscardModeRetention <- function(mu, Bcap, theta) {
  if (mu <= 0) return(1)
  excess <- .CalcNegBinExcess(mu, theta, Bcap)
  min(max((mu - excess) / mu, 0), 1)
}

# Retained (landed) numbers for one stock/fleet at the effort currently
# written into Proj@Effort[sim, TSIndex, fl] - a catch-only probe against
# .CalcFisheryDynamics(), matching the pattern in opt-effort-helpers.R.
# clone = 1 always, since (unlike .CalcFleetCatch()'s callers) this is called
# directly on the live Proj as well as on already-disposable copies - must
# never mutate Proj in place.
.CalcRetainedNumbers <- function(Proj, sim, TSIndex, Year, st, fl) {
  Temp <- .CalcFisheryDynamics(Proj,
                              Years = Year,
                              Sims = sim,
                              DoCalcSpawnProduction = 1,
                              DoCalcRecruitment = 1,
                              DoCalcNumberNext = 0,
                              DoCalcBiomass = 0,
                              DoCalcOverallF = 0,
                              clone = 1)

  Temp@LandingsAtAge[[st]][sim, , TSIndex, fl, , drop = FALSE] |>
    SumOverAge() |> SumOverArea() |> sum()
}

# ClosureMode = "stop" (Section 2.6): find the effort at or below the
# resolved effort such that retained numbers equal a FIXED aggregate target
# Bcap_f(t) * T_f(E_curr) - the trip count is evaluated once, at the
# resolved (baseline) effort, not re-derived at each candidate effort.
# Using T_f(E) instead (trips re-scaling with the very effort being solved
# for) makes the target shrink in lockstep with any effort cut, which is
# self-referential and drives effort to ~0 whenever the cap binds at all -
# not what "stop" mode is meant to represent (an early season closure once
# a season-length allowance, set from the trip activity that would have
# occurred, is reached).
.SolveBagLimitEffort <- function(Proj, sim, Year, TSIndex, st, fl, Advice,
                                  minEffort = 1e-8, tol = 1e-3,
                                  LimitTypeOverride = NULL) {
  E_curr <- Proj@Effort[sim, TSIndex, fl]
  if (E_curr <= minEffort) return(E_curr)

  Bcap       <- .CalcBagLimitCap(Advice, Proj, sim, TSIndex, st, fl, LimitTypeOverride = LimitTypeOverride)
  Trips_curr <- .CalcTrips(Proj, sim, TSIndex, st, fl)
  Bcap_total <- Bcap * Trips_curr

  residual <- function(E) {
    Eff <- Proj@Effort[sim, TSIndex, ]
    Eff[fl] <- E
    ProjE <- .WriteStateToProj(Proj, sim, TSIndex, Effort = Eff)
    .CalcRetainedNumbers(ProjE, sim, TSIndex, Year, st, fl) - Bcap_total
  }

  if (residual(E_curr) <= 0)
    return(E_curr)

  opt <- stats::optimize(function(E) residual(E)^2,
                         interval = c(minEffort, E_curr), tol = tol)
  opt$minimum
}

# Sum of .CalcRetainedNumbers() across all stocks in an aggregate group,
# same fleet - one trip catches several pooled species at once.
.CalcRetainedNumbersGroup <- function(Proj, sim, TSIndex, Year, stocks, fl) {
  sum(vapply(stocks, \(st) .CalcRetainedNumbers(Proj, sim, TSIndex, Year, st, fl), numeric(1)))
}

# Group version of .SolveBagLimitEffort(): one shared fleet effort is
# solved so the SUM of retained numbers across the group's stocks matches
# the fixed target Bcap * T_f(E_curr) - trips are counted once (the same
# trips catch every pooled species), not summed across stocks.
.SolveBagLimitEffortGroup <- function(Proj, sim, Year, TSIndex, stocks, fl, Bcap,
                                        minEffort = 1e-8, tol = 1e-3) {
  E_curr <- Proj@Effort[sim, TSIndex, fl]
  if (E_curr <= minEffort) return(E_curr)

  Trips_curr <- .CalcTrips(Proj, sim, TSIndex, stocks[1], fl)
  Bcap_total <- Bcap * Trips_curr

  residual <- function(E) {
    Eff <- Proj@Effort[sim, TSIndex, ]
    Eff[fl] <- E
    ProjE <- .WriteStateToProj(Proj, sim, TSIndex, Effort = Eff)
    .CalcRetainedNumbersGroup(ProjE, sim, TSIndex, Year, stocks, fl) - Bcap_total
  }

  if (residual(E_curr) <= 0)
    return(E_curr)

  opt <- stats::optimize(function(E) residual(E)^2,
                         interval = c(minEffort, E_curr), tol = tol)
  opt$minimum
}

# Scales retention by rho for this sim/Year only (a proportional reduction,
# so age/length/weight-at-age shape is preserved).
#
# Only Proj@Misc$RetAgeList[[st]]/RetSizeList[[st]][[fl]] is updated -
# .CalcFisheryDynamics() (C++) reads retention from these fleet-dimensioned
# Misc caches directly, not from Proj@OM@Fleet[[st]][[fl]]@Retention (which
# exists for other purposes but has no live reader in the current dynamics
# engine). No extension needed here: .ExtendHist() already sizes RetAgeList/
# RetSizeList to nSim x full-Years before .ProjectMP's loop ever starts.
.CalcScaleRetention <- function(Proj, sim, TSIndex, Year, st, fl, rho) {
  RetAge <- Proj@Misc$RetAgeList[[st]]
  if (!is.null(RetAge)) {
    RetAge[sim, , TSIndex, fl, ] <- RetAge[sim, , TSIndex, fl, ] * rho
    Proj@Misc$RetAgeList[[st]] <- RetAge
  }

  RetSize <- Proj@Misc$RetSizeList[[st]][[fl]]
  if (!is.null(RetSize)) {
    RetSize[sim, , TSIndex, ] <- RetSize[sim, , TSIndex, ] * rho
    Proj@Misc$RetSizeList[[st]][[fl]] <- RetSize
  }

  Proj
}

#' Apply bag-limit regulations across all simulations
#'
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years.
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateBagLimit <- function(Proj,
                            Year,
                            AdviceSimList,
                            LastAdviceSimList,
                            YearsHist,
                            YearsProj,
                            Areas,
                            FleetNames,
                            StockNames) {

  TSIndex <- match(Year, c(YearsHist, YearsProj))

  AggBagLimitSimList <- Proj@Misc$MPAggBagLimit[[as.character(Year)]]

  no_advice_bag <- .AllAdviceNull(AdviceSimList, 'BagLimit')
  no_agg_bag    <- is.null(AggBagLimitSimList) || !any(lengths(AggBagLimitSimList) > 0)

  if (no_advice_bag && no_agg_bag)
    return(Proj)

  for (sim in seq_len(Proj@OM@nSim)) {
    AdviceList     <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    AggBagLimit    <- if (!is.null(AggBagLimitSimList)) AggBagLimitSimList[[sim]] else NULL

    Proj <- .UpdateBagLimitSim(
      Proj           = Proj,
      sim            = sim,
      Year           = Year,
      TSIndex        = TSIndex,
      AdviceList     = AdviceList,
      LastAdviceList = LastAdviceList,
      AggBagLimit    = AggBagLimit,
      FleetNames     = FleetNames,
      StockNames     = StockNames,
      Complexes      = Proj@OM@Complexes,
      Areas          = Areas
    )
  }

  Proj
}

# rho_f(t) (Section 2.5) is recomputed every year the bag limit is active
# even when unchanged from LastAdviceList: unlike Retention/DiscardMortality,
# mu_f(t) moves with stock abundance under a constant regulation.
#
# Loops per-stock BagLimit advice first, then any `aggbaglimit` groups from
# an `mmp`-class MP. A stock in both an active group and its own per-stock
# BagLimit is sub-capped first (using the group's LimitType/ClosureMode, see
# AggregateBagLimit()), then the aggregate step runs on top of that state --
# for ClosureMode = "stop" the more restrictive of the two ends up binding.
.UpdateBagLimitSim <- function(Proj,
                                sim,
                                Year,
                                TSIndex,
                                AdviceList,
                                LastAdviceList,
                                AggBagLimit,
                                FleetNames,
                                StockNames,
                                Complexes,
                                Areas) {

  nFleet <- length(FleetNames)

  GroupLookup <- .BuildGroupLookup(AggBagLimit, FleetNames, StockNames)

  for (i in seq_along(AdviceList)) {
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]

    if (!inherits(Advice, 'advice')) next
    if (is.null(Advice@BagLimit))    next

    ClosureMode <- Advice@ClosureMode

    for (st in stocks) {
      for (fl in seq_len(nFleet)) {
        BagLimit_fl <- if (length(Advice@BagLimit) == 1) Advice@BagLimit else Advice@BagLimit[fl]
        if (is.na(BagLimit_fl)) next

        grp <- GroupLookup[[.GroupKey(st, fl)]]

        if (!is.null(grp)) {
          ClosureMode_fl    <- grp@ClosureMode
          LimitTypeOverride <- grp@LimitType
        } else {
          ClosureMode_fl    <- if (length(ClosureMode) == 1) ClosureMode else ClosureMode[fl]
          LimitTypeOverride <- NULL
        }

        if (identical(ClosureMode_fl, "stop")) {
          Proj@Effort[sim, TSIndex, fl] <- .SolveBagLimitEffort(
            Proj, sim, Year, TSIndex, st, fl, Advice, LimitTypeOverride = LimitTypeOverride
          )
          next
        }

        # ClosureMode == "discard" (default)
        Trips <- .CalcTrips(Proj, sim, TSIndex, st, fl)
        if (Trips <= 0) next

        mu    <- .CalcRetainedNumbers(Proj, sim, TSIndex, Year, st, fl) / Trips
        Bcap  <- .CalcBagLimitCap(Advice, Proj, sim, TSIndex, st, fl, LimitTypeOverride = LimitTypeOverride)
        theta <- .GetEffortArrayValue(Proj@OM@Fleet[[st]][[fl]]@Effort@Theta, sim, TSIndex)
        rho   <- .CalcDiscardModeRetention(mu, Bcap, theta)

        Proj <- .CalcScaleRetention(Proj, sim, TSIndex, Year, st, fl, rho)
      }
    }
  }

  for (grp in AggBagLimit) {
    if (!inherits(grp, 'aggbaglimit')) next

    fl     <- .ResolveIndex(grp@Fleet, FleetNames)
    stocks <- .ResolveIndex(grp@Stocks, StockNames)

    if (identical(grp@ClosureMode, "stop")) {
      Bcap <- .CalcGroupBagLimitCap(grp, Proj, sim, TSIndex, stocks[1], fl)
      Proj@Effort[sim, TSIndex, fl] <- .SolveBagLimitEffortGroup(
        Proj, sim, Year, TSIndex, stocks, fl, Bcap
      )
      next
    }

    # ClosureMode == "discard" (default)
    Trips <- .CalcTrips(Proj, sim, TSIndex, stocks[1], fl)
    if (Trips <= 0) next

    mu    <- .CalcRetainedNumbersGroup(Proj, sim, TSIndex, Year, stocks, fl) / Trips
    Bcap  <- .CalcGroupBagLimitCap(grp, Proj, sim, TSIndex, stocks[1], fl)
    theta <- .GetEffortArrayValue(Proj@OM@Fleet[[stocks[1]]][[fl]]@Effort@Theta, sim, TSIndex)
    rho   <- .CalcDiscardModeRetention(mu, Bcap, theta)

    for (st in stocks)
      Proj <- .CalcScaleRetention(Proj, sim, TSIndex, Year, st, fl, rho)
  }

  Proj
}
