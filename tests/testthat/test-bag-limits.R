# Bag-limit regulations: Advice(BagLimit, LimitType, ClosureMode) and
# AggregateBagLimit() for mmp MPs, enforced by .UpdateBagLimit().

.BagMP <- function(...) {
  args <- list(...)
  mp <- function(Data) do.call(Advice, args)
  class(mp) <- 'mp'
  mp
}

.BagTrips <- 20
.BagTheta <- 2

.BagOM <- function(om, nsim = 4, py = 3, DM = 1, Trips = .BagTrips, Theta = .BagTheta) {
  nSim(om)  <- nsim
  pYear(om) <- py
  for (st in seq_along(om@Fleet)) for (fl in seq_along(om@Fleet[[st]])) {
    om@Fleet[[st]][[fl]]@Effort@TripsScalar <- Trips
    om@Fleet[[st]][[fl]]@Effort@Theta       <- Theta
    if (!is.null(DM))
      om@Fleet[[st]][[fl]]@DiscardMortality <-
        DiscardMortality(MeanAtAge = rep(DM, nAge(om@Stock[[st]])))
  }
  om
}

.BagCache <- new.env()
.BagHist <- function(key, om) {
  if (is.null(.BagCache[[key]]))
    .BagCache[[key]] <- Simulate(om, silent = TRUE)
  .BagCache[[key]]
}

.BagProject <- function(Hist, MPs)
  suppressMessages(Project(Hist, MPs = MPs, parallel = FALSE, silent = TRUE))

# numbers summed over age and area: [Sim, Year, Fleet, MP]
.BagNum <- function(x, st = 1) apply(x[[st]], c(1, 3, 4, 6), sum)

# trips per sim/time step/fleet/MP: [Sim, Year, Fleet, MP]
.BagTripsArr <- function(MSE, nsim, Scalar = .BagTrips) {
  E <- MSE@Effort
  Scalar * E[rep_len(seq_len(dim(E)[1]), nsim), , , , drop = FALSE]
}

.BagExpected <- function(mu, Bcap, theta = .BagTheta)
  mu * vapply(mu, \(m) .CalcBagLimitRho(m, Bcap, theta), numeric(1))

.BagUpdateErrors <- function(MSE)
  Filter(\(e) grepl('UpdateError', e$name), MSE@Log$error)

# Effort with sims recycled to nsim: [Sim, Year, Fleet, MP]
.BagEffort <- function(MSE, nsim = 4)
  MSE@Effort[rep_len(seq_len(dim(MSE@Effort)[1]), nsim), , , , drop = FALSE]

test_that("the kept fraction is the exact negative-binomial E[min(X, Bag)] / mu", {
  expect_equal(.CalcBagLimitRho(0, 2, 2), 1)
  expect_equal(.CalcBagLimitRho(5, 1e6, 2), 1)
  expect_equal(.CalcBagLimitRho(5, Inf, 2), 1)
  expect_equal(.CalcBagLimitRho(5, 0, 2), 0)
  expect_equal(.CalcExpectedMinNB(0, 2, 2), 0)

  # brute-force sums, including heavy tails, fractional caps and small bags
  for (p in list(c(80, 2, 2), c(80, 2, 0.5), c(80, 0.5, 2), c(3, 2, 2), c(211, 199, 1),
                 c(50, 10, 0.3), c(3, 2.5, 2), c(5, 0.4, 1))) {
    mu <- p[1]; B <- p[2]; th <- p[3]
    x <- 0:2e5
    exact <- sum(pmin(x, B) * stats::dnbinom(x, size = th, mu = mu))
    expect_equal(.CalcExpectedMinNB(mu, th, B), exact, tolerance = 1e-8)
    expect_equal(.CalcBagLimitRho(mu, B, th), exact / mu, tolerance = 1e-8)
  }

  # Poisson limit
  x <- 0:500
  expect_equal(.CalcExpectedMinNB(20, Inf, 15), sum(pmin(x, 15) * stats::dpois(x, 20)), tolerance = 1e-10)

  # the kept mean never exceeds the bag
  expect_lte(80 * .CalcBagLimitRho(80, 2, 2), 2)

  # lower bag and more overdispersion both retain less
  expect_lt(.CalcBagLimitRho(10, 1, 2), .CalcBagLimitRho(10, 3, 2))
  expect_lt(.CalcBagLimitRho(10, 3, 0.5), .CalcBagLimitRho(10, 3, 50))
})

test_that("discard-mode boat limit on a single fleet", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  MSE <- .BagProject(Hist, list(SQ   = .BagMP(),
                                Bag  = .BagMP(BagLimit = 2,   LimitType = 'boat'),
                                Big  = .BagMP(BagLimit = 1e6, LimitType = 'boat'),
                                Zero = .BagMP(BagLimit = 0,   LimitType = 'boat')))
  expect_length(.BagUpdateErrors(MSE), 0)

  L <- .BagNum(MSE@LandingsAtAge)
  I <- .BagNum(MSE@InteractAtAge)
  D <- .BagNum(MSE@DiscardsAtAge)
  Trips <- .BagTripsArr(MSE, 4)

  # discard mortality 1: the limit does not change total removals, so interactions match SQ
  for (mp in c('Bag', 'Big', 'Zero'))
    expect_equal(I[, , , mp], I[, , , 'SQ'])

  # retained per trip is the negative-binomial expectation given the pre-limit mean
  mu <- I[, , , 'Bag'] / Trips[, , , 'Bag']
  expect_equal(L[, , , 'Bag'] / Trips[, , , 'Bag'], .BagExpected(mu, 2), tolerance = 1e-6)
  expect_true(all(L[, , , 'Bag'] / Trips[, , , 'Bag'] < 2))
  expect_equal(L[, , , 'Bag'] + D[, , , 'Bag'], I[, , , 'Bag'])

  # landings fall and discards rise relative to no bag limit
  expect_true(all(L[, , , 'Bag'] < L[, , , 'SQ']))
  expect_true(all(D[, , , 'SQ'] == 0))
  expect_true(all(D[, , , 'Bag'] > 0))
  LB <- apply(MSE@Landings, c(1, 3, 5), sum)
  DB <- apply(MSE@Discards, c(1, 3, 5), sum)
  expect_true(all(LB[, , 'Bag'] < LB[, , 'SQ']))
  expect_true(all(DB[, , 'Bag'] > 0))

  # non-binding bag changes nothing
  expect_equal(L[, , , 'Big'], L[, , , 'SQ'])
  expect_equal(D[, , , 'Big'], D[, , , 'SQ'])
  expect_equal(MSE@SBiomass[, , , 'Big'], MSE@SBiomass[, , , 'SQ'])

  # zero bag discards everything
  expect_true(all(L[, , , 'Zero'] == 0))
  expect_equal(D[, , , 'Zero'], I[, , , 'Zero'])
  expect_equal(I[, , , 'Zero'], I[, , , 'SQ'])
})

test_that("a bag limit of 0 removes all landings", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  MSE <- .BagProject(Hist, list(Zero = .BagMP(BagLimit = 0, LimitType = 'boat')))
  expect_true(all(.BagNum(MSE@LandingsAtAge) == 0))
})

test_that("an angler limit scales the cap by AnglerPerTrip", {
  skip_on_cran()
  om <- .BagOM(SingleStockOM)
  om@Fleet[[1]][[1]]@Effort@AnglerPerTrip <- 3
  Hist <- .BagHist('angler', om)
  MSE <- .BagProject(Hist, list(Angler = .BagMP(BagLimit = 2, LimitType = 'angler'),
                                Boat6  = .BagMP(BagLimit = 6, LimitType = 'boat'),
                                Boat2  = .BagMP(BagLimit = 2, LimitType = 'boat')))
  expect_length(.BagUpdateErrors(MSE), 0)
  L <- .BagNum(MSE@LandingsAtAge)
  I <- .BagNum(MSE@InteractAtAge)
  Trips <- .BagTripsArr(MSE, 4)

  expect_equal(L[, , , 'Angler'], L[, , , 'Boat6'])
  expect_true(all(L[, , , 'Angler'] > L[, , , 'Boat2']))
  mu <- I[, , , 'Angler'] / Trips[, , , 'Angler']
  expect_equal(L[, , , 'Angler'] / Trips[, , , 'Angler'], .BagExpected(mu, 2 * 3), tolerance = 1e-6)
})

test_that("per-fleet bag limits leave NA fleets unregulated", {
  skip_on_cran()
  Hist <- .BagHist('twofleet', .BagOM(TwoFleetOM))
  MSE <- .BagProject(Hist, list(SQ  = .BagMP(),
                                Fl1 = .BagMP(BagLimit = c(2, NA), LimitType = 'boat'),
                                Fl2 = .BagMP(BagLimit = c(NA, 2), LimitType = 'boat')))
  expect_length(.BagUpdateErrors(MSE), 0)
  L <- .BagNum(MSE@LandingsAtAge)
  I <- .BagNum(MSE@InteractAtAge)
  Trips <- .BagTripsArr(MSE, 4)

  expect_equal(L[, , 2, 'Fl1'], L[, , 2, 'SQ'])
  expect_equal(L[, , 1, 'Fl2'], L[, , 1, 'SQ'])
  expect_true(all(L[, , 1, 'Fl1'] < L[, , 1, 'SQ']))
  expect_true(all(L[, , 2, 'Fl2'] < L[, , 2, 'SQ']))

  # loose: sim 1 effort is back-calculated after the bag-limit step
  mu <- I[, , 1, 'Fl1'] / Trips[, , 1, 'Fl1']
  expect_equal(I[, , , 'Fl1'], I[, , , 'SQ'])
  expect_equal(L[, , 1, 'Fl1'] / Trips[, , 1, 'Fl1'], .BagExpected(mu, 2), tolerance = 5e-3)
})

test_that("discard mortality sets whether released fish survive", {
  skip_on_cran()
  MPs <- list(SQ = .BagMP(), Bag = .BagMP(BagLimit = 2, LimitType = 'boat'))
  M1 <- .BagProject(.BagHist('single', .BagOM(SingleStockOM)), MPs)
  M0 <- .BagProject(.BagHist('dm0', .BagOM(SingleStockOM, DM = 0)), MPs)
  expect_length(c(.BagUpdateErrors(M0), .BagUpdateErrors(M1)), 0)

  Biom <- \(x) apply(x, c(1, 3, 5), sum)

  # all released fish die: removals and SB unchanged, landings become dead discards
  expect_equal(Biom(M1@Landings)[, , 'Bag'] + Biom(M1@Discards)[, , 'Bag'],
               Biom(M1@Landings)[, , 'SQ'])
  expect_equal(M1@SBiomass[, , , 'Bag'], M1@SBiomass[, , , 'SQ'])

  # all released fish survive: no dead discards, lower removals, higher SB
  expect_true(all(Biom(M0@Discards) == 0))
  expect_true(all(Biom(M0@Landings)[, , 'Bag'] < Biom(M0@Landings)[, , 'SQ']))
  ny <- dim(M0@SBiomass)[3]
  expect_true(all(M0@SBiomass[, , ny, 'Bag'] > M0@SBiomass[, , ny, 'SQ']))
  expect_true(all(M0@SBiomass[, , ny, 'Bag'] > M1@SBiomass[, , ny, 'Bag']))
})

test_that("retained catch per trip is the bag-limited expectation when released fish survive", {
  skip_on_cran()
  Hist <- .BagHist('dm0', .BagOM(SingleStockOM, DM = 0))
  MSE <- .BagProject(Hist, list(SQ = .BagMP(), Bag = .BagMP(BagLimit = 2, LimitType = 'boat')))
  L <- .BagNum(MSE@LandingsAtAge)
  Trips <- .BagTripsArr(MSE, 4)
  expect_true(all(L[, , , 'Bag'] / Trips[, , , 'Bag'] <= 2))

  # released fish survive, so the pre-limit mean is the realised retained / kept fraction;
  # rho is self-consistent with the lower mortality under the limit
  I  <- .BagNum(MSE@InteractAtAge)
  mu <- I[, , , 'Bag'] / Trips[, , , 'Bag']
  expect_equal(L[, , , 'Bag'] / Trips[, , , 'Bag'], .BagExpected(mu, 2), tolerance = 1e-6)
})

test_that("stop mode shortens fishing time by the bag-limited kept fraction", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  MSE <- .BagProject(Hist, list(SQ      = .BagMP(),
                                Stop    = .BagMP(BagLimit = 2,   LimitType = 'boat', ClosureMode = 'stop'),
                                Disc    = .BagMP(BagLimit = 2,   LimitType = 'boat'),
                                StopBig = .BagMP(BagLimit = 1e4, LimitType = 'boat', ClosureMode = 'stop')))
  expect_length(.BagUpdateErrors(MSE), 0)
  L <- .BagNum(MSE@LandingsAtAge)
  D <- .BagNum(MSE@DiscardsAtAge)
  E <- .BagEffort(MSE)

  # trips are unchanged; fishing time per trip falls to the kept fraction
  TripsSQ  <- .BagTrips * E[, , , 'SQ']
  TimeFrac <- E[, , , 'Stop'] / E[, , , 'SQ']
  expect_true(all(TimeFrac < 1))
  mu_full <- L[, , , 'Stop'] / (TripsSQ * TimeFrac)
  expect_equal(TimeFrac, .BagExpected(mu_full, 2) / mu_full, tolerance = 1e-5, ignore_attr = TRUE)
  expect_equal(L[, , , 'Stop'] / TripsSQ, .BagExpected(mu_full, 2), tolerance = 1e-5, ignore_attr = TRUE)
  expect_true(all(L[, , , 'Stop'] / TripsSQ <= 2))

  # excess fish are never caught
  expect_true(all(D[, , , 'Stop'] == 0))
  expect_true(all(D[, , , 'Disc'] > 0))

  # same kept fraction in the first year: discard keeps it of the catch, stop of the time
  expect_equal(L[, 1, , 'Stop'], L[, 1, , 'Disc'], tolerance = 0.05)

  # non-binding: effort and catch unchanged
  expect_equal(E[, , , 'StopBig'], E[, , , 'SQ'])
  expect_equal(L[, , , 'StopBig'], L[, , , 'SQ'])
})

test_that("stop mode binds when the mean catch per trip is below the bag", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  SQ <- .BagProject(Hist, list(SQ = .BagMP()))
  mean_trip <- .BagNum(SQ@LandingsAtAge) / .BagTripsArr(SQ, 4)
  Bag <- 1.2 * max(mean_trip)

  MSE <- .BagProject(Hist, list(SQ = .BagMP(), Stop = .BagMP(BagLimit = Bag, LimitType = 'boat',
                                                              ClosureMode = 'stop')))
  E <- .BagEffort(MSE)
  expect_true(all(E[, , , 'Stop'] < E[, , , 'SQ']))
  expect_true(all(.BagNum(MSE@LandingsAtAge)[, , , 'Stop'] < .BagNum(MSE@LandingsAtAge)[, , , 'SQ']))
})

test_that("stop-mode bags on two stocks shortening one fleet take the binding one, not the product", {
  skip_on_cran()
  Hist <- .BagHist('multistock', .BagOM(MultiStockOM))
  SQ <- function(DataList) lapply(DataList, \(d) Advice())
  class(SQ) <- 'mmp'
  Stop <- function(DataList)
    lapply(DataList, \(d) Advice(BagLimit = c(2, NA), LimitType = 'boat', ClosureMode = 'stop'))
  class(Stop) <- 'mmp'
  MSE <- .BagProject(Hist, list(SQ = SQ, Stop = Stop))
  expect_length(.BagUpdateErrors(MSE), 0)

  E <- .BagEffort(MSE)
  TimeFrac <- E[, , 1, 'Stop'] / E[, , 1, 'SQ']
  Trips <- .BagTrips * E[, , 1, 'SQ']
  rho <- lapply(1:2, \(st) {
    mu_full <- .BagNum(MSE@LandingsAtAge, st)[, , 1, 'Stop'] / (Trips * TimeFrac)
    .BagExpected(mu_full, 2) / mu_full
  })
  expect_equal(TimeFrac, pmin(rho[[1]], rho[[2]]), tolerance = 1e-5, ignore_attr = TRUE)
  expect_true(all(TimeFrac > rho[[1]] * rho[[2]]))

  # fleet 2 has no bag
  expect_equal(E[, , 2, 'Stop'], E[, , 2, 'SQ'])
})

test_that("a species sub-cap inside a stop-mode aggregate releases its excess", {
  skip_on_cran()
  Hist <- .BagHist('multistock', .BagOM(MultiStockOM))
  SQ <- function(DataList) lapply(DataList, \(d) Advice())
  class(SQ) <- 'mmp'
  Agg <- function(DataList) {
    adv <- lapply(DataList, \(d) Advice())
    adv[[1]] <- Advice(BagLimit = c(1, NA), LimitType = 'boat')
    list(Advice = adv,
         AggregateBagLimit = list(AggregateBagLimit(Fleet = 1, Stocks = 1:2, BagLimit = 3,
                                                    ClosureMode = 'stop')))
  }
  class(Agg) <- 'mmp'
  MSE <- .BagProject(Hist, list(SQ = SQ, Agg = Agg))
  expect_length(.BagUpdateErrors(MSE), 0)

  D1 <- .BagNum(MSE@DiscardsAtAge, 1); D2 <- .BagNum(MSE@DiscardsAtAge, 2)
  L1 <- .BagNum(MSE@LandingsAtAge, 1); L2 <- .BagNum(MSE@LandingsAtAge, 2)
  E <- .BagEffort(MSE)
  expect_true(all(D1[, , 1, 'Agg'] > 0))
  expect_true(all(D2[, , 1, 'Agg'] == 0))
  expect_true(all(E[, , 1, 'Agg'] < E[, , 1, 'SQ']))

  # the aggregate time fraction follows from the kept catch after the sub-cap
  Trips    <- .BagTrips * E[, , 1, 'SQ']
  TimeFrac <- E[, , 1, 'Agg'] / E[, , 1, 'SQ']
  mu_full  <- (L1[, , 1, 'Agg'] + L2[, , 1, 'Agg']) / (Trips * TimeFrac)
  expect_equal(TimeFrac, .BagExpected(mu_full, 3) / mu_full, tolerance = 1e-5, ignore_attr = TRUE)
  expect_true(all(L1[, , 1, 'Agg'] / Trips <= 1))
})

test_that("a stock in two aggregate limits for one fleet is an error", {
  expect_error(.BuildGroupLookup(list(AggregateBagLimit(Fleet = 1, Stocks = 1:2, BagLimit = 3),
                                      AggregateBagLimit(Fleet = 1, Stocks = 2:3, BagLimit = 3)),
                                 FleetNames = 'F1', StockNames = c('a', 'b', 'c')),
               'more than one')
})

test_that("an aggregate bag limit pools retained catch across stocks for one fleet", {
  skip_on_cran()
  Hist <- .BagHist('multistock', .BagOM(MultiStockOM))
  SQ <- function(DataList) lapply(DataList, \(d) Advice())
  class(SQ) <- 'mmp'
  Agg <- function(DataList)
    list(Advice = lapply(DataList, \(d) Advice()),
         AggregateBagLimit = list(AggregateBagLimit(Fleet = 1, Stocks = 1:2, BagLimit = 2)))
  class(Agg) <- 'mmp'
  MSE <- .BagProject(Hist, list(SQ = SQ, Agg = Agg))
  expect_length(.BagUpdateErrors(MSE), 0)

  L1 <- .BagNum(MSE@LandingsAtAge, 1); L2 <- .BagNum(MSE@LandingsAtAge, 2)
  I1 <- .BagNum(MSE@InteractAtAge, 1); I2 <- .BagNum(MSE@InteractAtAge, 2)
  Trips <- .BagTripsArr(MSE, 4)

  # unregulated fleet unchanged
  expect_equal(L1[, , 2, 'Agg'], L1[, , 2, 'SQ'])
  expect_equal(L2[, , 2, 'Agg'], L2[, , 2, 'SQ'])

  # pooled mean drives one retention fraction applied to both stocks
  T1 <- Trips[, , 1, 'Agg']
  mu <- (I1[, , 1, 'Agg'] + I2[, , 1, 'Agg']) / T1
  expect_equal((L1[, , 1, 'Agg'] + L2[, , 1, 'Agg']) / T1, .BagExpected(mu, 2), tolerance = 1e-6)
  expect_equal(L1[, , 1, 'Agg'] / I1[, , 1, 'Agg'], L2[, , 1, 'Agg'] / I2[, , 1, 'Agg'])
  expect_true(all(L1[, , 1, 'Agg'] < L1[, , 1, 'SQ']))
  expect_true(all(L2[, , 1, 'Agg'] < L2[, , 1, 'SQ']))
})

test_that("a bag limit is applied in each season of a seasonal OM", {
  skip_on_cran()
  Hist <- .BagHist('seasonal', .BagOM(SeasonalSpatialOM, py = 1))
  MSE <- .BagProject(Hist, list(SQ = .BagMP(), Bag = .BagMP(BagLimit = 0.5, LimitType = 'boat')))
  expect_length(.BagUpdateErrors(MSE), 0)
  L <- .BagNum(MSE@LandingsAtAge)
  I <- .BagNum(MSE@InteractAtAge)
  Trips <- .BagTripsArr(MSE, 4)
  expect_equal(unname(dim(L)[2]), Hist@OM@Seasons)

  expect_true(all(L[, , , 'Bag'] < L[, , , 'SQ']))
  mu <- I[, , , 'Bag'] / Trips[, , , 'Bag']
  expect_equal(L[, , , 'Bag'] / Trips[, , , 'Bag'], .BagExpected(mu, 0.5), tolerance = 1e-6)
})

test_that("a bag limit persists through non-management years without compounding", {
  skip_on_cran()
  Hist <- .BagHist('single4', .BagOM(SingleStockOM, py = 4))
  # bag of 2 from the first management year, 1 thereafter
  Bag <- function(Data) Advice(BagLimit = if (isTRUE(Data@Misc$FirstAdvice)) 2 else 1, LimitType = 'boat')
  class(Bag) <- 'mp'

  M1 <- .BagProject(Hist, list(Bag = Bag))
  Hist3 <- Hist
  Hist3@OM@Interval <- 3
  M3 <- .BagProject(Hist3, list(Bag = Bag))
  expect_length(.BagUpdateErrors(M3), 0)

  Check <- function(MSE, Bags) {
    L <- .BagNum(MSE@LandingsAtAge)[, , 1, 1]
    I <- .BagNum(MSE@InteractAtAge)[, , 1, 1]
    Trips <- .BagTripsArr(MSE, 4)[, , 1, 1]
    for (y in seq_along(Bags))
      expect_equal(L[, y] / Trips[, y], .BagExpected(I[, y] / Trips[, y], Bags[y]),
                   tolerance = 1e-6, ignore_attr = TRUE)
    L
  }
  L1 <- Check(M1, c(2, 1, 1, 1))
  L3 <- Check(M3, c(2, 2, 2, 1))
  expect_equal(L3[, 1], L1[, 1])
  expect_true(all(L3[, 2:3] > L1[, 2:3]))
})

test_that("missing trip or angler inputs give a user error in the MP log", {
  skip_on_cran()
  om <- SingleStockOM
  nSim(om)  <- 4
  pYear(om) <- 2
  om@Fleet[[1]][[1]]@Effort@Theta <- 2
  Hist <- .BagHist('notrips', om)
  # relative effort trend sets Units to unitless, so TripsScalar is required
  expect_equal(Hist@OM@Fleet[[1]][[1]]@Effort@Units, 'unitless')

  MSE <- .BagProject(Hist, list(SQ = .BagMP(), Bag = .BagMP(BagLimit = 2, LimitType = 'boat')))
  err <- .BagUpdateErrors(MSE)
  expect_length(err, 1)
  expect_equal(err[[1]]$mp, 'Bag')
  expect_match(err[[1]]$message, 'Effort@TripsScalar', fixed = TRUE)
  expect_no_match(err[[1]]$message, 'internal error', fixed = TRUE)

  # effort already in trips needs no TripsScalar
  HistTrips <- Hist
  HistTrips@OM@Fleet[[1]][[1]]@Effort@Units <- 'trips'
  MSE <- .BagProject(HistTrips, list(SQ = .BagMP(), Bag = .BagMP(BagLimit = 40, LimitType = 'boat')))
  expect_length(.BagUpdateErrors(MSE), 0)
  L <- .BagNum(MSE@LandingsAtAge)
  expect_true(all(L[, , , 'Bag'] < L[, , , 'SQ']))

  MSE <- .BagProject(HistTrips, list(Angler = .BagMP(BagLimit = 2, LimitType = 'angler')))
  err <- .BagUpdateErrors(MSE)
  expect_length(err, 1)
  expect_match(err[[1]]$message, 'Effort@AnglerPerTrip', fixed = TRUE)
  expect_no_match(err[[1]]$message, 'internal error', fixed = TRUE)
})

test_that("Theta is required for a bag limit", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  Hist@OM@Fleet[[1]][[1]]@Effort@Theta <- NULL
  MSE <- .BagProject(Hist, list(Bag = .BagMP(BagLimit = 2, LimitType = 'boat')))
  err <- .BagUpdateErrors(MSE)
  expect_length(err, 1)
  expect_match(err[[1]]$message, 'Effort@Theta', fixed = TRUE)
})

test_that("a TAC is caught given the bag-limited retention", {
  skip_on_cran()
  om <- .BagOM(SingleStockOM, py = 4, DM = 0.5)
  Hist <- .BagHist('single4dm', om)
  TAC <- 300
  MSE <- .BagProject(Hist, list(TAC     = .BagMP(TAC = TAC, TACType = 'Landings'),
                                TACBag  = .BagMP(TAC = TAC, TACType = 'Landings', BagLimit = 2, LimitType = 'boat'),
                                TACStop = .BagMP(TAC = TAC, TACType = 'Landings', BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop'),
                                RemBag  = .BagMP(TAC = TAC, TACType = 'Removals', BagLimit = 2, LimitType = 'boat')))
  expect_length(MSE@Log$error, 0)
  LB <- apply(MSE@Landings, c(1, 3, 5), sum)
  DB <- apply(MSE@Discards, c(1, 3, 5), sum)
  E  <- .BagEffort(MSE)[, , 1, ]

  # landings TAC: caught with more trips; released excess adds dead discards
  expect_equal(LB[, , 'TACBag'], LB[, , 'TAC'], tolerance = 1e-4)
  expect_true(all(E[, , 'TACBag'] > E[, , 'TAC']))
  expect_true(all(DB[, , 'TACBag'] > 0))

  # removals TAC: landings plus dead discards equal the TAC
  expect_equal(LB[, , 'RemBag'] + DB[, , 'RemBag'], LB[, , 'TAC'], tolerance = 1e-4)
  expect_true(all(LB[, , 'RemBag'] < LB[, , 'TAC']))

  # retained per trip is still the bag-limited expectation at the solved effort
  L <- .BagNum(MSE@LandingsAtAge)[, , 1, 'TACBag']
  I <- .BagNum(MSE@InteractAtAge)[, , 1, 'TACBag']
  D <- .BagNum(MSE@DiscardsAtAge)[, , 1, 'TACBag']
  Trips <- .BagTrips * E[, , 'TACBag']
  rho <- L / (L + D / 0.5)
  expect_equal(L / Trips, .BagExpected(L / rho / Trips, 2), tolerance = 1e-5, ignore_attr = TRUE)

  # stop mode with no effort ceiling: more trips, same fishing mortality
  expect_equal(LB[, , 'TACStop'], LB[, , 'TAC'], tolerance = 1e-4)
  expect_equal(MSE@SBiomass[, , , 'TACStop'], MSE@SBiomass[, , , 'TAC'], tolerance = 1e-4)
})

test_that("a stop-mode bag limit lowers the effort ceiling for a TAC", {
  skip_on_cran()
  Hist <- .BagHist('single4', .BagOM(SingleStockOM, py = 4))
  Hist@OM@Fleet[[1]][[1]]@Effort@Maximum <- 1
  TAC <- 1e6
  MSE <- .BagProject(Hist, list(TAC     = .BagMP(TAC = TAC),
                                TACStop = .BagMP(TAC = TAC, BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop')))
  expect_length(MSE@Log$error, 0)
  E <- .BagEffort(MSE)[, , 1, ]
  expect_equal(E[, , 'TAC'], array(1, dim(E[, , 'TAC'])), ignore_attr = TRUE, tolerance = 1e-3)

  # nominal trips are at the maximum; fishing time is the kept fraction of it
  L <- .BagNum(MSE@LandingsAtAge)[, , 1, 'TACStop']
  Trips <- .BagTrips * 1
  mu_full <- L / (Trips * E[, , 'TACStop'])
  expect_equal(E[, , 'TACStop'], .BagExpected(mu_full, 2) / mu_full, tolerance = 1e-4, ignore_attr = TRUE)

  Att <- TACAttainment(MSE)
  Att <- Att[Att$MP == 'TACStop', ]
  expect_true(all(Att$Limit == 'Ceiling'))
})

test_that("a stop-mode bag limit shortens advised effort", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  MSE <- .BagProject(Hist, list(Eff  = .BagMP(Effort = 0.5),
                                Stop = .BagMP(Effort = 0.5, BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop')))
  E <- .BagEffort(MSE)[, , 1, ]
  L <- .BagNum(MSE@LandingsAtAge)[, , 1, 'Stop']
  Trips <- .BagTrips * E[, , 'Eff']
  TimeFrac <- E[, , 'Stop'] / E[, , 'Eff']
  mu_full <- L / (Trips * TimeFrac)
  expect_true(all(TimeFrac < 1))
  expect_equal(TimeFrac, .BagExpected(mu_full, 2) / mu_full, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("a bag limit inside a multi-complex TAC solve", {
  skip_on_cran()
  Hist <- .BagHist('multistock', .BagOM(MultiStockOM))
  TAC <- function(DataList) {
    lapply(DataList, \(d) {
      L <- d@Landings@Value
      Advice(TAC = 0.8 * L[LastHistYearInd(d), ], TACUnit = d@Landings@Units)
    })
  }
  class(TAC) <- 'mmp'
  TACBag <- function(DataList) {
    adv <- TAC(DataList)
    adv[[1]]@BagLimit <- 2; adv[[1]]@LimitType <- 'boat'
    adv
  }
  class(TACBag) <- 'mmp'
  MSE <- .BagProject(Hist, list(TAC = TAC, TACBag = TACBag))
  expect_length(MSE@Log$error, 0)

  # the bag-limited complex still catches its TAC as it does without the bag
  Att <- TACAttainment(MSE)
  Att <- Att[Att$Stock == names(Hist@OM@Complexes)[1] & Att$Fleet == FleetNames(Hist)[1], ]
  expect_equal(Att$Catch[Att$MP == 'TACBag'], Att$Catch[Att$MP == 'TAC'], tolerance = 1e-3)
  D <- .BagNum(MSE@DiscardsAtAge, 1)
  expect_true(all(D[, , , 'TACBag'] > D[, , , 'TAC']))
})

test_that("an annual TAC split over seasons is caught each season given the bag limit", {
  skip_on_cran()
  # released fish survive, so the extra trips needed under the bag do not deplete the stock
  Hist <- .BagHist('seasonal2dm0', .BagOM(SeasonalSpatialOM, py = 2, DM = 0))
  TACMP <- function(bag = NULL) {
    mp <- function(Data) {
      h <- LastHistYearInd(Data)
      Seasons <- SeasonOfYear(Data)$Seasons
      last <- sum(Data@Landings@Value[(h - Seasons + 1):h, ])
      a <- Advice(TAC = 0.5 * last, TACType = 'Landings', TACUnit = Data@Landings@Units)
      if (!is.null(bag)) { a@BagLimit <- bag; a@LimitType <- 'boat' }
      a
    }
    class(mp) <- 'mp'
    mp
  }
  MSE <- .BagProject(Hist, list(TAC = TACMP(), TACBag = TACMP(0.5)))
  expect_length(MSE@Log$error, 0)

  # the same seasonal split of the TAC is landed with and without the bag
  LB <- apply(MSE@Landings, c(1, 3, 5), sum)
  expect_equal(LB[, , 'TACBag'], LB[, , 'TAC'], tolerance = 1e-4)
  expect_gt(max(LB[1, , 'TAC']) / min(LB[1, , 'TAC']), 1.01)

  # each season is solved at its own catch per trip
  L <- .BagNum(MSE@LandingsAtAge)[, , 1, 'TACBag']
  I <- .BagNum(MSE@InteractAtAge)[, , 1, 'TACBag']
  Trips <- .BagTripsArr(MSE, 4)[, , 1, 'TACBag']
  expect_equal(L / Trips, .BagExpected(I / Trips, 0.5), tolerance = 1e-5, ignore_attr = TRUE)
  E <- .BagEffort(MSE)[, , 1, ]
  expect_true(all(E[, , 'TACBag'] > E[, , 'TAC']))
})

test_that("an angler limit with a TAC or stop mode equals the matching boat limit", {
  skip_on_cran()
  om <- .BagOM(SingleStockOM)
  om@Fleet[[1]][[1]]@Effort@AnglerPerTrip <- 3
  Hist <- .BagHist('angler', om)
  MSE <- .BagProject(Hist, list(
    AngTAC   = .BagMP(TAC = 300, TACType = 'Landings', BagLimit = 1, LimitType = 'angler'),
    BoatTAC  = .BagMP(TAC = 300, TACType = 'Landings', BagLimit = 3, LimitType = 'boat'),
    AngStop  = .BagMP(BagLimit = 1, LimitType = 'angler', ClosureMode = 'stop'),
    BoatStop = .BagMP(BagLimit = 3, LimitType = 'boat',   ClosureMode = 'stop')))
  expect_length(MSE@Log$error, 0)

  LB <- apply(MSE@Landings, c(1, 3, 5), sum)
  E  <- .BagEffort(MSE)
  expect_equal(LB[, , 'AngTAC'], LB[, , 'BoatTAC'])
  expect_equal(E[, , , 'AngTAC'], E[, , , 'BoatTAC'])
  expect_equal(LB[, , 'AngTAC'], array(300, dim(LB[, , 'AngTAC'])), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(E[, , , 'AngStop'], E[, , , 'BoatStop'])
  expect_true(all(E[, , , 'AngStop'] < 1))
})

test_that("bag limits start with the first MP year after interim advice", {
  skip_on_cran()
  Hist <- .BagHist('single4', .BagOM(SingleStockOM, py = 4))
  fl <- FleetNames(Hist)
  Hist@OM@MPStartYear <- 2029
  Hist@OM@InterimAdvice <- data.frame(Year = 2027:2028, Fleet = fl, Type = 'TAC', Mean = 250, EffType = NA)
  MSE <- .BagProject(Hist, list(
    TACBag  = .BagMP(TAC = 300, TACType = 'Landings', BagLimit = 2, LimitType = 'boat'),
    BagStop = .BagMP(BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop')))
  expect_length(MSE@Log$error, 0)

  LB <- apply(MSE@Landings, c(1, 3, 5), sum)
  DB <- apply(MSE@Discards, c(1, 3, 5), sum)
  Interim <- c('2027', '2028'); MPYears <- c('2029', '2030')

  # interim years: the interim TAC (removals) and no bag-limit releases
  for (mp in c('TACBag', 'BagStop')) {
    expect_true(all(DB[, Interim, mp] == 0))
    expect_equal(LB[, Interim, mp], array(250, dim(LB[, Interim, mp])), tolerance = 1e-4, ignore_attr = TRUE)
  }

  # MP years: the MP's TAC is caught given the bag; the stop bag shortens fishing time
  expect_equal(LB[, MPYears, 'TACBag'], array(300, dim(LB[, MPYears, 'TACBag'])), tolerance = 1e-4,
               ignore_attr = TRUE)
  expect_true(all(DB[, MPYears, 'TACBag'] > 0))
  E <- .BagEffort(MSE)
  expect_true(all(E[, MPYears, 1, 'BagStop'] < 1))
  expect_true(all(DB[, MPYears, 'BagStop'] == 0))
})

test_that("bag-limit projections in parallel match sequential", {
  skip_on_cran()
  skip_on_ci()
  skip_if(parallel::detectCores() < 2)
  skip_if(isNamespaceLoaded('pkgload') && pkgload::is_dev_package('MSEtool'),
          'parallel workers load the installed MSEtool, not the development copy')
  Hist <- .BagHist('multistock', .BagOM(MultiStockOM))
  TACBag <- function(DataList) {
    lapply(DataList, \(d) {
      L <- d@Landings@Value
      Advice(TAC = 0.8 * L[LastHistYearInd(d), ], TACType = 'Landings', TACUnit = d@Landings@Units,
             BagLimit = c(2, NA), LimitType = 'boat')
    })
  }
  class(TACBag) <- 'mmp'
  AggStop <- function(DataList)
    list(Advice = lapply(DataList, \(d) Advice()),
         AggregateBagLimit = list(AggregateBagLimit(Fleet = 1, Stocks = 1:2, BagLimit = 3,
                                                    ClosureMode = 'stop')))
  class(AggStop) <- 'mmp'
  MPs <- list(TACBag = TACBag, AggStop = AggStop,
              Disc = function(DataList) lapply(DataList, \(d) Advice(BagLimit = 2, LimitType = 'boat')))
  class(MPs$Disc) <- 'mmp'

  Seq <- .BagProject(Hist, MPs)
  SetupParallel(workers = 2, backend = 'multisession', silent = TRUE)
  on.exit(suppressMessages(DisableParallel()), add = TRUE)
  Par <- suppressMessages(Project(Hist, MPs = MPs, parallel = TRUE, silent = TRUE))
  expect_null(Par@Log$error)
  expect_equal(Par@Effort, Seq@Effort)
  expect_equal(Par@Landings, Seq@Landings)
  expect_equal(Par@Discards, Seq@Discards)
  expect_equal(Par@SBiomass, Seq@SBiomass)
  expect_equal(Par@Misc$BagLimit, Seq@Misc$BagLimit)
})

test_that("a fleet that retains nothing still fishes, distributed by the biomass it selects", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  nage <- nAge(Hist@OM@Stock[[1]])
  Release <- function(Data) Advice(Retention = Retention(MeanAtAge = rep(0, nage)))
  class(Release) <- 'mp'
  Keep <- function(Data) Advice(Retention = Retention(MeanAtAge = rep(1, nage)))
  class(Keep) <- 'mp'
  MSE <- .BagProject(Hist, list(Keep = Keep, Release = Release))
  expect_length(.BagUpdateErrors(MSE), 0)
  I <- .BagNum(MSE@InteractAtAge)
  # with full retention, selected and retained biomass coincide
  expect_equal(I[, , , 'Release'], I[, , , 'Keep'])
  expect_equal(MSE@Distribution[, , , , 'Release'], MSE@Distribution[, , , , 'Keep'])
  expect_true(all(.BagNum(MSE@LandingsAtAge)[, , , 'Release'] == 0))
  expect_true(all(MSE@Distribution[, , , , 'Release'] >= 0))
})

test_that("BagLimitEffect() reports trips, fishing-time and retention fractions", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  MSE <- .BagProject(Hist, list(SQ   = .BagMP(),
                                Stop = .BagMP(BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop'),
                                Disc = .BagMP(BagLimit = 2, LimitType = 'boat')))
  B <- BagLimitEffect(MSE)
  expect_false('SQ' %in% B$MP)
  E <- .BagEffort(MSE)[, , 1, ]
  BySimYear <- \(x) c(t(x))

  S <- B[B$MP == 'Stop', ]
  S <- S[order(S$Sim, S$Year), ]
  expect_equal(S$Trips, BySimYear(.BagTrips * E[, , 'SQ']), ignore_attr = TRUE)
  expect_equal(S$TimeFraction, BySimYear(E[, , 'Stop'] / E[, , 'SQ']), tolerance = 1e-6, ignore_attr = TRUE)
  expect_true(all(S$RetentionFraction == 1))

  D <- B[B$MP == 'Disc', ]
  D <- D[order(D$Sim, D$Year), ]
  L <- .BagNum(MSE@LandingsAtAge)[, , 1, 'Disc']
  I <- .BagNum(MSE@InteractAtAge)[, , 1, 'Disc']
  expect_true(all(D$TimeFraction == 1))
  expect_equal(D$RetentionFraction, BySimYear(L / I), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(D$Trips, BySimYear(.BagTrips * E[, , 'Disc']), ignore_attr = TRUE)

  # carried through subsetting
  expect_equal(nrow(BagLimitEffect(Subset(MSE, MPs = 'Stop'))), nrow(S))
  expect_equal(nrow(BagLimitEffect(Subset(MSE, Sims = 1:2))), sum(B$Sim %in% 1:2))
})

test_that("observed effort counts trips under a stop-mode bag limit", {
  skip_on_cran()
  om <- .BagOM(SingleStockOM)
  om@Obs[[1]][[1]]@Effort <- EffortObs(CV = 0.2, Bias = 1)
  Hist <- .BagHist('effortobs', om)
  MSE <- .BagProject(Hist, list(SQ   = .BagMP(),
                                Stop = .BagMP(BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop')))
  E <- .BagEffort(MSE)
  expect_true(all(E[, , 1, 'Stop'] < E[, , 1, 'SQ']))
  YearsP <- dimnames(MSE@Effort)$Year
  for (sim in 1:4) {
    Obs <- \(mp) MSE@PPD[[mp]][[sim]][[1]]@Effort@Value
    expect_equal(Obs('Stop')[YearsP, ], Obs('SQ')[YearsP, ])
  }
})

test_that("a stop-mode bag does not compound over non-management years", {
  skip_on_cran()
  Hist <- .BagHist('single4', .BagOM(SingleStockOM, py = 4))
  Hist@OM@Interval <- 3
  MSE <- .BagProject(Hist, list(Stop = .BagMP(BagLimit = 2, LimitType = 'boat', ClosureMode = 'stop')))
  B <- BagLimitEffect(MSE)
  expect_true(all(abs(B$Trips - .BagTrips) < 1e-8))
  E <- .BagEffort(MSE)[, , 1, 1]
  B <- B[order(B$Sim, B$Year), ]
  expect_equal(B$TimeFraction, c(t(E)), tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("a stop-mode bag on a fleet without a TAC keeps its trips when another fleet has one", {
  skip_on_cran()
  Hist <- .BagHist('twofleet4', .BagOM(TwoFleetOM, py = 4))
  MP <- function(Data) {
    L <- Data@Landings@Value[LastHistYearInd(Data), ]
    Advice(TAC = c(0.8 * L[1], NA), TACType = 'Landings', TACUnit = Data@Landings@Units,
           BagLimit = c(NA, 2), LimitType = 'boat', ClosureMode = 'stop')
  }
  class(MP) <- 'mp'
  MSE <- .BagProject(Hist, list(MP = MP))
  expect_length(MSE@Log$error, 0)

  B <- BagLimitEffect(MSE)
  expect_true(all(B$Fleet == FleetNames(Hist)[2]))
  # trips stay at the last historical effort; fishing time shortens each step
  nH <- dim(Hist@Effort)[2]
  E_hist <- Hist@Effort[rep_len(seq_len(dim(Hist@Effort)[1]), 4), nH, 2]
  B <- B[order(B$Sim, B$Year), ]
  expect_equal(B$Trips, rep(.BagTrips * E_hist, each = 4), tolerance = 1e-8, ignore_attr = TRUE)
  E <- .BagEffort(MSE)[, , 2, 1]
  expect_equal(c(t(E)), B$Trips / .BagTrips * B$TimeFraction, tolerance = 1e-6, ignore_attr = TRUE)
  expect_true(all(B$TimeFraction < 1))
})
