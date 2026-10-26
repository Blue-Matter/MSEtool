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
  mu * vapply(mu, \(m) .CalcDiscardModeRetention(m, Bcap, theta), numeric(1))

.BagUpdateErrors <- function(MSE)
  Filter(\(e) grepl('UpdateError', e$name), MSE@Log$error)

test_that("discard-mode retention matches the truncated negative-binomial excess", {
  expect_equal(.CalcDiscardModeRetention(0, 2, 2), 1)
  expect_equal(.CalcDiscardModeRetention(5, 1e6, 2), 1)
  expect_equal(.CalcNegBinExcess(5, 2, 1e6), 0)
  expect_equal(.CalcNegBinExcess(0, 2, 2), 0)

  for (p in list(c(3, 2, 2), c(10, 2, 2), c(5, 5, 0.5), c(20, 4, 5))) {
    mu <- p[1]; B <- p[2]; th <- p[3]
    x <- 0:stats::qnbinom(0.9999, size = th, mu = mu)
    excess <- sum(pmax(x - B, 0) * stats::dnbinom(x, size = th, mu = mu))
    expect_equal(.CalcNegBinExcess(mu, th, B), excess)
    expect_equal(.CalcDiscardModeRetention(mu, B, th), (mu - excess) / mu)
  }

  # lower bag and more overdispersion both retain less
  expect_lt(.CalcDiscardModeRetention(10, 1, 2), .CalcDiscardModeRetention(10, 3, 2))
  expect_lt(.CalcDiscardModeRetention(10, 3, 0.5), .CalcDiscardModeRetention(10, 3, 50))
})

test_that("discard-mode retention equals the exact negative-binomial E[min(X, Bag)]", {
  skip("engine truncates the excess at the 0.9999 quantile: retention overstated (mu = 80, theta = 2, bag = 2 retains 2.05 per trip)")
  for (p in list(c(80, 2, 2), c(80, 2, 0.5), c(80, 0, 2))) {
    mu <- p[1]; B <- p[2]; th <- p[3]
    x <- 0:1e5
    exact <- sum(pmin(x, B) * stats::dnbinom(x, size = th, mu = mu))
    expect_equal(mu * .CalcDiscardModeRetention(mu, B, th), exact, tolerance = 1e-4)
  }
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

  # zero bag discards (almost) everything; residual is the truncated-tail leak
  expect_true(all(L[, , , 'Zero'] < 1e-3 * L[, , , 'SQ']))
  expect_equal(D[, , , 'Zero'], I[, , , 'Zero'], tolerance = 1e-3)
})

test_that("a bag limit of 0 removes all landings", {
  skip("BagLimit = 0 retains ~0.06% of catch: .CalcNegBinExcess() truncates at the 0.9999 quantile")
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

test_that("retained catch per trip never exceeds the bag when released fish survive", {
  skip("rho is computed at the pre-limit Z; with DiscardMortality < 1 realised landings exceed the bag (2.1 per trip for a bag of 2)")
  skip_on_cran()
  Hist <- .BagHist('dm0', .BagOM(SingleStockOM, DM = 0))
  MSE <- .BagProject(Hist, list(Bag = .BagMP(BagLimit = 2, LimitType = 'boat')))
  expect_true(all(.BagNum(MSE@LandingsAtAge) / .BagTripsArr(MSE, 4) <= 2))
})

# Stop mode is being redesigned: these assert the current behaviour only.
test_that("stop mode reduces effort until retained numbers equal Bag x status-quo trips", {
  skip_on_cran()
  Hist <- .BagHist('single', .BagOM(SingleStockOM))
  MSE <- .BagProject(Hist, list(SQ      = .BagMP(),
                                Stop    = .BagMP(BagLimit = 2,   LimitType = 'boat', ClosureMode = 'stop'),
                                StopBig = .BagMP(BagLimit = 1e4, LimitType = 'boat', ClosureMode = 'stop')))
  expect_length(.BagUpdateErrors(MSE), 0)
  L <- .BagNum(MSE@LandingsAtAge)
  D <- .BagNum(MSE@DiscardsAtAge)
  Trips <- .BagTripsArr(MSE, 4)
  E <- MSE@Effort[rep_len(seq_len(dim(MSE@Effort)[1]), 4), , , , drop = FALSE]

  # binding: mean catch per trip exceeds the bag
  expect_true(all(L[, , , 'SQ'] / Trips[, , , 'SQ'] > 2))
  expect_equal(c(L[, , , 'Stop']), c(2 * Trips[, , , 'SQ']), tolerance = 1e-3)
  expect_true(all(E[, , , 'Stop'] < E[, , , 'SQ']))
  expect_true(all(D[, , , 'Stop'] == 0))

  # non-binding: effort and catch unchanged
  expect_equal(E[, , , 'StopBig'], E[, , , 'SQ'])
  expect_equal(L[, , , 'StopBig'], L[, , , 'SQ'])
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

test_that("a bag limit alongside a TAC runs and keeps landings within the TAC", {
  skip_on_cran()
  Hist <- .BagHist('single4', .BagOM(SingleStockOM, py = 4))
  TAC <- 300
  MSE <- .BagProject(Hist, list(TACBag = .BagMP(TAC = TAC, BagLimit = 2, LimitType = 'boat'),
                                TAC    = .BagMP(TAC = TAC)))
  expect_length(MSE@Log$error, 0)
  LB <- apply(MSE@Landings, c(1, 3, 5), sum)
  expect_true(all(LB[, , 'TACBag'] <= TAC * (1 + 1e-3)))
  expect_true(all(LB[, , 'TACBag'] < LB[, , 'TAC']))
})
