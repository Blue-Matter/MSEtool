# Seasonal allocation of periodically-set TAC/Effort: `.CheckSeasonalAllocation()`
# blends the historical seasonal removals pattern and the population biomass
# pattern, weighted per-fleet by `OM@HistoricalWeight`, into `OM@SeasonalAllocation`
# (a [Sim x Season x Fleet] array per complex), applied automatically to any
# periodic (non-`EverySeason`) MP's TAC/Effort.

test_that(".ResolveHistoricalWeight() expands scalar/partial/NULL correctly", {
  fleets <- c("F1", "F2", "F3")

  expect_equal(.ResolveHistoricalWeight(NULL, fleets), stats::setNames(c(1, 1, 1), fleets))
  expect_equal(.ResolveHistoricalWeight(0.5, fleets), stats::setNames(c(0.5, 0.5, 0.5), fleets))
  expect_equal(.ResolveHistoricalWeight(c(F2 = 0.2), fleets), stats::setNames(c(1, 0.2, 1), fleets))
  expect_equal(.ResolveHistoricalWeight(c(F2 = 0.2, 0.7), fleets), stats::setNames(c(0.7, 0.2, 0.7), fleets))

  expect_error(.ResolveHistoricalWeight(1.5, fleets))
  expect_error(.ResolveHistoricalWeight(-0.1, fleets))
})

test_that(".ValidateSeasonalAllocation() enforces shape and sums to 1", {
  fleets <- c("F1", "F2")
  ok <- array(0.25, dim = c(2, 4, 2), dimnames = list(Sim = 1:2, Season = 1:4, Fleet = fleets))

  expect_silent(.ValidateSeasonalAllocation(ok, nSimTot = 2, Seasons = 4, FleetNms = fleets))

  bad_sum <- ok
  bad_sum[1, 1, 1] <- 0.9
  expect_error(.ValidateSeasonalAllocation(bad_sum, nSimTot = 2, Seasons = 4, FleetNms = fleets))

  bad_dim <- array(0.5, dim = c(2, 3, 2))
  expect_error(.ValidateSeasonalAllocation(bad_dim, nSimTot = 2, Seasons = 4, FleetNms = fleets))

  recycled <- array(0.25, dim = c(1, 4, 2), dimnames = list(Sim = 1, Season = 1:4, Fleet = fleets))
  out <- .ValidateSeasonalAllocation(recycled, nSimTot = 3, Seasons = 4, FleetNms = fleets)
  expect_equal(unname(dim(out)), c(3, 4, 2))
})

test_that(".CheckSeasonalAllocation() defaults to pure historical share", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  expect_equal(hist@OM@HistoricalWeight[[1]], stats::setNames(1, FleetNames(om)))

  SA <- hist@OM@SeasonalAllocation[[1]]
  expect_equal(unname(dim(SA)), c(2, om@Seasons, 1))
  expect_equal(unname(apply(SA, c(1, 3), sum)), array(1, dim = c(2, 1)), tolerance = 1e-8)

  # Corrupting Biomass must not change the result when HistoricalWeight = 1
  # (the abundance computation should be skipped entirely).
  hist_corrupt <- hist
  hist_corrupt@Biomass[] <- NA_real_
  hist_corrupt@OM@SeasonalAllocation <- MakeNamedList(names(hist@OM@Complexes))
  hist_corrupt <- .CheckSeasonalAllocation(hist_corrupt)
  expect_equal(hist_corrupt@OM@SeasonalAllocation[[1]], SA)
})

test_that(".CheckSeasonalAllocation() blends historical and abundance shares", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  om@nSim <- 1
  om@HistoricalWeight <- list(0)
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  SA <- hist@OM@SeasonalAllocation[[1]]
  expect_equal(unname(apply(SA, c(1, 3), sum)), array(1, dim = c(1, 1)), tolerance = 1e-8)

  # HistoricalWeight = 0 -> pure abundance (Biomass) share, independent of
  # realised catch pattern.
  hist_corrupt <- hist
  hist_corrupt@Landings[] <- 0
  hist_corrupt@Discards[] <- 0
  hist_corrupt@OM@SeasonalAllocation <- MakeNamedList(names(hist@OM@Complexes))
  hist_corrupt <- .CheckSeasonalAllocation(hist_corrupt)
  expect_equal(hist_corrupt@OM@SeasonalAllocation[[1]], SA)
})

test_that("directly-specified OM@SeasonalAllocation overrides HistoricalWeight, allows per-sim variation", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  om@nSim <- 2
  Seasons <- om@Seasons
  fleets  <- FleetNames(om)

  direct <- array(0, dim = c(2, Seasons, 1), dimnames = list(Sim = 1:2, Season = 1:Seasons, Fleet = fleets))
  direct[1, 1, 1] <- 1
  direct[2, 2, 1] <- 1
  om@SeasonalAllocation <- list(direct)

  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  expect_equal(hist@OM@SeasonalAllocation[[1]], direct)
})

test_that("a periodic TAC MP with no seasonal logic still produces the historical shape", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  hist@OM@Interval <- 3

  Simple3yr <- function(Data) {
    CheckCatch(Data)
    Seasons <- MPSeasonIndex(Data)$Seasons
    n <- length(Data@Years)
    rows <- (n - Seasons + 1):n
    total <- sum(Data@Landings@Value[rows, , drop = FALSE], na.rm = TRUE) +
             sum(Data@Discards@Value[rows, , drop = FALSE], na.rm = TRUE)
    Advice(TAC = total / Seasons, TACUnit = Data@Landings@Units)
  }
  class(Simple3yr) <- 'mp'
  assign("Simple3yr", Simple3yr, envir = globalenv())
  on.exit(rm("Simple3yr", envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = "Simple3yr", parallel = FALSE, silent = TRUE)

  L <- mse@Landings[1, , , , 1]
  Seasons <- om@Seasons
  first_cycle  <- unname(L[seq_len(Seasons)])
  second_cycle <- unname(L[Seasons + seq_len(Seasons)])

  # Not flat: the historical seasonal shape should be present, not an equal split.
  expect_gt(max(first_cycle) / min(first_cycle), 1.3)
  # Repeats identically within the 3-year block.
  expect_equal(second_cycle, first_cycle, tolerance = 1e-8)
})

test_that("EverySeason MPs are unaffected by SeasonalAllocation", {
  skip_on_cran()
  om <- SeasonalSpatialOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  hist_flat <- hist
  hist_flat@OM@SeasonalAllocation[[1]][] <- 1 / om@Seasons

  mse_default <- Project(hist,      MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  mse_flat    <- Project(hist_flat, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  expect_equal(mse_default@Landings, mse_flat@Landings, tolerance = 1e-8)
})
