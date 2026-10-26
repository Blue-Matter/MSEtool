.GetEffortArrayValue <- function(x, sim, TSIndex) {
  x[min(sim, nrow(x)), TSIndex]
}

# Trips per unit of (nominal) effort
.CalcTripsPerEffort <- function(Proj, sim, TSIndex, st, fl) {
  Eff <- Proj@OM@Fleet[[st]][[fl]]@Effort

  if (identical(Eff@Units, "trips"))
    return(1)

  if (is.null(Eff@TripsScalar))
    cli::cli_abort(c(
      "x" = "Bag limits need the number of trips for fleet {.val {fl}} (stock {.val {st}}).",
      "i" = "Set {.code Effort@TripsScalar} (trips per unit of effort) for this fleet, or {.code Effort@Units = \"trips\"} if effort is already in trips."
    ), call = NULL)

  .GetEffortArrayValue(Eff@TripsScalar, sim, TSIndex)
}

.CalcBagTheta <- function(Proj, sim, TSIndex, st, fl) {
  Theta <- Proj@OM@Fleet[[st]][[fl]]@Effort@Theta
  if (is.null(Theta))
    cli::cli_abort(c(
      "x" = "Bag limits need the catch-per-trip overdispersion for fleet {.val {fl}} (stock {.val {st}}).",
      "i" = "Set {.code Effort@Theta} for this fleet (see {.help [Effort()](MSEtool::Effort)})."
    ), call = NULL)
  theta <- .GetEffortArrayValue(Theta, sim, TSIndex)
  if (!is.numeric(theta) || is.na(theta) || theta <= 0)
    cli::cli_abort("{.code Effort@Theta} must be positive for fleet {.val {fl}} (stock {.val {st}})",
                   call = NULL)
  theta
}

.CalcAnglerCap <- function(BagLimit, Proj, sim, TSIndex, st, fl) {
  Eff <- Proj@OM@Fleet[[st]][[fl]]@Effort
  if (is.null(Eff@AnglerPerTrip))
    cli::cli_abort(c(
      "x" = "Per-angler bag limits need the number of anglers per trip for fleet {.val {fl}} (stock {.val {st}}).",
      "i" = "Set {.code Effort@AnglerPerTrip} for this fleet, or use {.code LimitType = \"boat\"}."
    ), call = NULL)

  BagLimit * .GetEffortArrayValue(Eff@AnglerPerTrip, sim, TSIndex)
}

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

  .CalcAnglerCap(BagLimit_fl, Proj, sim, TSIndex, st, fl)
}

.CalcGroupBagLimitCap <- function(grp, Proj, sim, TSIndex, st, fl) {
  if (identical(grp@LimitType, "boat"))
    return(grp@BagLimit)

  .CalcAnglerCap(grp@BagLimit, Proj, sim, TSIndex, st, fl)
}

.ResolveIndex <- function(x, names) {
  if (is.character(x)) match(x, names) else as.integer(x)
}

.GroupKey <- function(st, fl) paste(st, fl, sep = "_")

.BuildGroupLookup <- function(AggBagLimit, FleetNames, StockNames) {
  lookup <- list()
  for (grp in AggBagLimit) {
    if (!inherits(grp, 'aggbaglimit')) next
    fl     <- .ResolveIndex(grp@Fleet, FleetNames)
    stocks <- .ResolveIndex(grp@Stocks, StockNames)
    if (is.na(fl) || anyNA(stocks))
      cli::cli_abort("`AggregateBagLimit()` names a fleet or stock that is not in the OM", call = NULL)
    for (st in stocks) {
      key <- .GroupKey(st, fl)
      if (!is.null(lookup[[key]]))
        cli::cli_abort(c(
          "x" = "Stock {.val {StockNames[st]}} is in more than one `AggregateBagLimit()` for fleet {.val {FleetNames[fl]}}.",
          "i" = "Each stock can be pooled in at most one aggregate bag limit per fleet."
        ), call = NULL)
      lookup[[key]] <- grp
    }
  }
  lookup
}

# E[min(X, Bcap)] for X ~ NB(mu, theta): integral of P(X > t) over [0, Bcap]
.CalcExpectedMinNB <- function(mu, theta, Bcap) {
  if (mu <= 0 || Bcap <= 0) return(0)
  if (!is.finite(Bcap)) return(mu)

  Surv <- if (is.finite(theta))
    \(q) stats::pnbinom(q, size = theta, mu = mu, lower.tail = FALSE) else
    \(q) stats::ppois(q, lambda = mu, lower.tail = FALSE)
  # terms beyond this have P(X > k) < 1e-15
  kmax <- if (is.finite(theta))
    stats::qnbinom(1 - 1e-15, size = theta, mu = mu) else stats::qpois(1 - 1e-15, mu)

  n <- floor(Bcap)
  k <- if (n > 0) 0:min(n - 1, kmax) else integer(0)
  partial <- if (n <= kmax) (Bcap - n) * Surv(n) else 0
  min(sum(Surv(k)) + partial, mu)
}

# Fraction of the pre-bag catch per trip kept under a bag limit
.CalcBagLimitRho <- function(mu, Bcap, theta) {
  if (mu <= 0) return(1)
  min(max(.CalcExpectedMinNB(mu, theta, Bcap) / mu, 0), 1)
}

# One bag-limit constraint per regulated stock-fleet and per aggregate group
.BagLimitConstraints <- function(Proj, sim, TSIndex, AdviceList, AggBagLimit,
                                 FleetNames, StockNames) {
  Complexes   <- Proj@OM@Complexes
  GroupLookup <- .BuildGroupLookup(AggBagLimit, FleetNames, StockNames)
  Cons <- list()

  NewCon <- function(stocks, fl, cap, mode, level)
    list(stocks = stocks, fleet = fl, cap = cap, mode = mode, level = level,
         theta = .CalcBagTheta(Proj, sim, TSIndex, stocks[1], fl),
         TripsPerEffort = .CalcTripsPerEffort(Proj, sim, TSIndex, stocks[1], fl))

  for (i in seq_along(AdviceList)) {
    Advice <- AdviceList[[i]]
    if (!inherits(Advice, 'advice') || is.null(Advice@BagLimit)) next
    ClosureMode <- Advice@ClosureMode %||% 'discard'

    for (st in Complexes[[i]]) {
      for (fl in seq_along(FleetNames)) {
        BagLimit_fl <- if (length(Advice@BagLimit) == 1) Advice@BagLimit else Advice@BagLimit[fl]
        if (is.na(BagLimit_fl)) next

        # a species sub-cap inside an aggregate limit always releases its excess
        grp <- GroupLookup[[.GroupKey(st, fl)]]
        if (!is.null(grp)) {
          mode <- 'discard'
          cap  <- .CalcBagLimitCap(Advice, Proj, sim, TSIndex, st, fl, LimitTypeOverride = grp@LimitType)
        } else {
          mode <- if (length(ClosureMode) == 1) ClosureMode else ClosureMode[fl]
          cap  <- .CalcBagLimitCap(Advice, Proj, sim, TSIndex, st, fl)
        }
        Cons[[length(Cons) + 1]] <- NewCon(st, fl, cap, mode, 'stock')
      }
    }
  }

  for (grp in AggBagLimit) {
    if (!inherits(grp, 'aggbaglimit') || is.na(grp@BagLimit)) next
    fl     <- .ResolveIndex(grp@Fleet, FleetNames)
    stocks <- .ResolveIndex(grp@Stocks, StockNames)
    cap    <- .CalcGroupBagLimitCap(grp, Proj, sim, TSIndex, stocks[1], fl)
    Cons[[length(Cons) + 1]] <- NewCon(stocks, fl, cap, grp@ClosureMode %||% 'discard', 'group')
  }

  Cons
}

# Retained numbers in time step TSIndex for a single-sim Proj (`Retained`,
# [Stock, Fleet]) and the realised effort after any maxF clamp (`Effort`)
.CalcRetainedNumbersAll <- function(Proj, TSIndex, Year) {
  Temp <- .CalcFisheryDynamics(Proj,
                               Years = Year,
                               Sims = 1,
                               DoCalcSpawnProduction = 1,
                               DoCalcRecruitment = 1,
                               DoCalcNumberNext = 0,
                               DoCalcBiomass = 0,
                               DoCalcOverallF = 0,
                               DoBackCalcEffort = .BackCalcEffortFlag(Proj),
                               InPlace = TRUE)
  nFleet <- dim(Proj@Effort)[3]
  out <- vapply(Temp@LandingsAtAge, \(x) apply(x[1, , TSIndex, , , drop = FALSE], 4, sum),
                numeric(nFleet))
  list(Retained = matrix(out, ncol = nFleet, byrow = TRUE),
       Effort   = Temp@Effort[1, TSIndex, ])
}

.BagRetentionScale <- function(Cons, d, nStock, nFleet) {
  Scale <- matrix(1, nStock, nFleet)
  for (i in seq_along(Cons)) {
    if (Cons[[i]]$mode != 'discard') next
    Scale[Cons[[i]]$stocks, Cons[[i]]$fleet] <- Scale[Cons[[i]]$stocks, Cons[[i]]$fleet] * d[i]
  }
  Scale
}

# Sets retention at TSIndex to Base x Scale for the stock-fleets in Cons
.SetBagRetention <- function(Proj, sim, TSIndex, Base, Scale, Cons) {
  for (st in unique(unlist(lapply(Cons, `[[`, 'stocks')))) {
    fls <- unique(unlist(lapply(Cons, \(con) if (st %in% con$stocks) con$fleet)))
    RetAge <- Proj@Misc$RetAgeList[[st]]
    if (!is.null(RetAge)) {
      for (fl in fls)
        RetAge[sim, , TSIndex, fl, ] <- Base$RetAge[[st]][sim, , TSIndex, fl, ] * Scale[st, fl]
      Proj@Misc$RetAgeList[[st]] <- RetAge
    }
    for (fl in fls) {
      RetSize <- Proj@Misc$RetSizeList[[st]][[fl]]
      if (is.null(RetSize)) next
      RetSize[sim, , TSIndex, ] <- Base$RetSize[[st]][[fl]][sim, , TSIndex, ] * Scale[st, fl]
      Proj@Misc$RetSizeList[[st]][[fl]] <- RetSize
    }
  }
  Proj
}

#' Solve the bag-limit regulations for one simulation and time step
#'
#' Every constraint `c` gives a kept fraction \eqn{\rho_c = E[\min(X, B_c)] /
#' \mu_c}, with \eqn{X \sim NB(\mu_c, \theta_c)} the per-trip catch of fish
#' that clear all other retention rules. `"discard"` constraints scale
#' retention by \eqn{\rho_c}; `"stop"` constraints scale the fleet's fishing
#' time (effort) by the smallest \eqn{\rho_c} on that fleet, with the number of
#' trips unchanged. \eqn{\mu_c} is recomputed from the realised dynamics until
#' all \eqn{\rho_c} are self-consistent, so lower total mortality under the
#' bag limit is accounted for.
#'
#' @param Proj Single-sim `Proj` (from `.SliceSim()`).
#' @param Year,TSIndex Time step.
#' @param Cons Constraints from `.BagLimitConstraints()`.
#' @param EffortNominal Numeric `nFleet`. Effort before the bag limit, used
#'   when `SolveEffort` is `NULL`.
#' @param SolveEffort `NULL`, or `function(Proj, TimeFrac)` returning
#'   `list(Effort, Proj)`: effort (on the fishing-time scale) solved given the
#'   bag-limit retention already in `Proj` and the stop-mode fishing-time
#'   fraction `TimeFrac`. Used to nest the bag limit inside the TAC solve.
#' @return List with `Proj` (retention and effort for the solution), `Effort`,
#'   `Scale` (`[Stock, Fleet]` retention multiplier), `TimeFrac`, `Trips`
#'   (nominal trips per fleet, `NA` for fleets without a bag limit), `Solved`
#'   (last `SolveEffort()` result) and `converged`.
#' @keywords internal
.SolveBagLimit <- function(Proj, Year, TSIndex, Cons, EffortNominal = NULL,
                           SolveEffort = NULL, maxit = 50, tol = 1e-7) {
  nStock <- length(Proj@Misc$RetAgeList)
  nFleet <- dim(Proj@Effort)[3]
  Base   <- list(RetAge = Proj@Misc$RetAgeList, RetSize = Proj@Misc$RetSizeList)
  IsStop <- vapply(Cons, \(con) con$mode == 'stop', logical(1))

  d        <- rep(1, length(Cons))
  TimeFrac <- rep(1, nFleet)
  Solved   <- NULL
  converged <- FALSE

  for (it in seq_len(maxit)) {
    Scale <- .BagRetentionScale(Cons, d, nStock, nFleet)
    P <- .SetBagRetention(Proj, 1, TSIndex, Base, Scale, Cons)
    if (is.null(SolveEffort)) {
      Effort <- EffortNominal * TimeFrac
    } else {
      Solved <- SolveEffort(P, TimeFrac)
      Effort <- Solved$Effort
      P      <- Solved$Proj
    }
    P@Effort[1, TSIndex, ] <- Effort
    Probe    <- .CalcRetainedNumbersAll(P, TSIndex, Year)
    Retained <- Probe$Retained

    rho <- vapply(seq_along(Cons), \(i) {
      con   <- Cons[[i]]
      fl    <- con$fleet
      Trips <- con$TripsPerEffort * Probe$Effort[fl] / TimeFrac[fl]
      if (con$cap <= 0) return(0)
      if (!is.finite(Trips) || Trips <= 0) return(1)
      # undo this constraint's own effect (and, for a stock, any group above it)
      Undo <- if (con$level == 'stock') Scale[con$stocks, fl] else if (IsStop[i]) 1 else d[i]
      if (IsStop[i]) Undo <- Undo * TimeFrac[fl]
      # everything already released by a zero bag: nothing left to measure
      if (Undo <= 0) return(if (IsStop[i]) TimeFrac[fl] else d[i])
      mu <- sum(Retained[con$stocks, fl]) / Trips / Undo
      .CalcBagLimitRho(mu, con$cap, con$theta)
    }, numeric(1))

    d_new        <- ifelse(IsStop, 1, rho)
    TimeFrac_new <- rep(1, nFleet)
    for (i in which(IsStop))
      TimeFrac_new[Cons[[i]]$fleet] <- min(TimeFrac_new[Cons[[i]]$fleet], rho[i])

    # the returned state is the one built from TimeFrac, not TimeFrac_new
    TimeFracUsed <- TimeFrac
    converged <- max(abs(c(d_new - d, TimeFrac_new - TimeFrac))) < tol
    if (converged) break
    d        <- d_new
    TimeFrac <- TimeFrac_new
  }

  Trips <- rep(NA_real_, nFleet)
  for (con in Cons)
    Trips[con$fleet] <- con$TripsPerEffort * Probe$Effort[con$fleet] / TimeFracUsed[con$fleet]

  list(Proj = P, Effort = Effort, Scale = Scale, TimeFrac = TimeFracUsed, Trips = Trips,
       Solved = Solved, converged = converged)
}

# Write a single-sim bag-limit solution back to sim `sim` of the full Proj
.WriteBagLimitSolution <- function(Proj, sim, TSIndex, Year, Sol, Cons) {
  Base <- list(RetAge = Proj@Misc$RetAgeList, RetSize = Proj@Misc$RetSizeList)
  Proj <- .SetBagRetention(Proj, sim, TSIndex, Base, Sol$Scale, Cons)
  Proj@Effort[sim, TSIndex, ] <- Sol$Effort
  .RecordBagLimit(Proj, sim, Year, Sol)
}

# Bag-limit record for a projection ([Sim, Year, Fleet] arrays): nominal trips
# (`NA` where no bag limit applied), the stop-mode fraction of each trip's
# fishing time, and per stock the discard-mode fraction of retention kept
.InitBagLimitRecord <- function(Proj, YearsProj) {
  DN  <- list(Sim = seq_len(Proj@OM@nSim), Year = YearsProj, Fleet = FleetNames(Proj))
  arr <- \(value) array(value, unname(lengths(DN)), dimnames = DN)
  list(Trips             = arr(NA_real_),
       TimeFraction      = arr(1),
       RetentionFraction = stats::setNames(rep(list(arr(1)), nStock(Proj)), StockNames(Proj)))
}

.RecordBagLimit <- function(Proj, sim, Year, Sol) {
  Rec <- Proj@Misc$BagLimit
  if (is.null(Rec)) return(Proj)
  y <- match(round(Year, 6), round(as.numeric(dimnames(Rec$Trips)$Year), 6))
  if (is.na(y)) return(Proj)
  Rec$Trips[sim, y, ]        <- Sol$Trips
  Rec$TimeFraction[sim, y, ] <- Sol$TimeFrac
  for (st in seq_along(Rec$RetentionFraction))
    Rec$RetentionFraction[[st]][sim, y, ] <- Sol$Scale[st, ]
  Proj@Misc$BagLimit <- Rec
  Proj
}

# Stop-mode fishing-time fraction for the projection time step `Year` ([Sim, Fleet]); 1 elsewhere
.BagTimeFraction <- function(Proj, Year) {
  TF <- Proj@Misc$BagLimit$TimeFraction
  nFleet <- dim(Proj@Effort)[3]
  out <- matrix(1, Proj@OM@nSim, nFleet)
  if (is.null(TF)) return(out)
  y <- match(round(Year, 6), round(as.numeric(dimnames(TF)$Year), 6))
  if (is.na(y)) return(out)
  out[] <- TF[rep_len(seq_len(dim(TF)[1]), Proj@OM@nSim), y, ]
  out
}

.LogBagLimitConvergence <- function(Proj, Sol, sim, Year) {
  if (Sol$converged) return(Proj)
  msg <- "Bag-limit solver: kept fractions did not converge; realised retention may not match the bag limit."
  Proj@Log$warning <- c(Proj@Log$warning,
                        list(.NewLogEntry(msg, name = 'BagLimitConvergence', sim = sim, year = Year)))
  Proj
}

.AggBagLimitSim <- function(Proj, Year, sim) {
  AggBagLimitSimList <- Proj@Misc$MPAggBagLimit[[as.character(Year)]]
  if (length(AggBagLimitSimList) < sim) NULL else AggBagLimitSimList[[sim]]
}

#' Apply bag-limit regulations across all simulations
#'
#' Applies bag limits for simulations without a TAC this time step. Where a
#' TAC applies, the bag limit is solved inside the TAC solve
#' (`.UpdateTACSim()`), so the TAC is caught given the bag-limited retention
#' and fishing time.
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
                            StockNames,
                            EverySeason = FALSE) {

  TSIndex <- match(Year, c(YearsHist, YearsProj))

  AggBagLimitSimList <- Proj@Misc$MPAggBagLimit[[as.character(Year)]]

  no_advice_bag <- .AllAdviceNull(AdviceSimList, 'BagLimit')
  no_agg_bag    <- is.null(AggBagLimitSimList) || !any(lengths(AggBagLimitSimList) > 0)

  if (no_advice_bag && no_agg_bag)
    return(Proj)

  SolvedTS <- Proj@Misc$BagLimitSolvedTS

  for (sim in seq_len(Proj@OM@nSim)) {
    if (!is.null(SolvedTS) && identical(SolvedTS[sim], TSIndex)) next

    Proj <- .UpdateBagLimitSim(
      Proj        = Proj,
      sim         = sim,
      Year        = Year,
      TSIndex     = TSIndex,
      AdviceList  = AdviceSimList[[sim]],
      AggBagLimit = .AggBagLimitSim(Proj, Year, sim),
      FleetNames  = FleetNames,
      StockNames  = StockNames
    )
  }

  Proj
}

.UpdateBagLimitSim <- function(Proj,
                               sim,
                               Year,
                               TSIndex,
                               AdviceList,
                               AggBagLimit,
                               FleetNames,
                               StockNames) {

  Cons <- .BagLimitConstraints(Proj, sim, TSIndex, AdviceList, AggBagLimit, FleetNames, StockNames)
  if (!length(Cons)) return(Proj)

  ProjSim <- .SliceSim(Proj, sim, .DynamicsProbeSlots)
  Sol <- .SolveBagLimit(ProjSim, Year, TSIndex, Cons,
                        EffortNominal = Proj@Effort[sim, TSIndex, ])

  Proj <- .WriteBagLimitSolution(Proj, sim, TSIndex, Year, Sol, Cons)
  .LogBagLimitConvergence(Proj, Sol, sim, Year)
}
