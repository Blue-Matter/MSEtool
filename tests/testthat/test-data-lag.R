# `DataLag` shifts the data year the MP sees back by a further whole
# `DataLag` years from the timestep immediately before the current
# management timestep -- see `.CalcDataYear()`/`.TrimMPData()` in
# R/helpers-mp.R. This must apply uniformly from the very first management
# year onward (including reaching into the operating model's historical
# data), must not depend on `Interval`, must convert cleanly to timesteps
# for seasonal OMs, and must lag from the last `InterimAdvice` year rather
# than the OM's true historical boundary when `MPStartYear` is set.

test_that(".CalcDataYear() applies a strict, unclamped annual lag", {
  YearsAll <- 2007:2026  # 20 historical years, last historical year 2026

  # DataLag=0: the timestep immediately before the last available one
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 0, Seasons = 1), 2026)

  # DataLag=1: one further whole year back, even though this reaches
  # *into* the historical record rather than stopping at its boundary
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 1, Seasons = 1), 2025)
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 2, Seasons = 1), 2024)
})

test_that(".CalcDataYear() converts DataLag to timesteps for seasonal OMs, preserving season", {
  YearsAll <- seq(2007, by = 1/4, length.out = 80)  # 20 years, 4 seasons/yr, last = 2026.75

  # DataLag=0 -> immediately preceding timestep (same rule as annual)
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 0, Seasons = 4), 2026.75)

  # DataLag=1 -> 4 timesteps back (one full year), landing on the same season
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 1, Seasons = 4), 2025.75)
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 2, Seasons = 4), 2024.75)
})

test_that(".CalcDataYear() errors clearly when DataLag exceeds the available data", {
  YearsAll <- 2007:2026  # 20 years of data -> max usable DataLag is 19

  expect_error(
    .CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 20, Seasons = 1),
    "DataLag"
  )
  expect_error(
    .CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 25, Seasons = 1),
    "DataLag"
  )
  # exactly at the boundary is fine
  expect_equal(.CalcDataYear(Year = 2027, YearsAll = YearsAll, DataLag = 19, Seasons = 1), 2007)
})

.RunDataLagMP <- function(om, DataLagVal, IntervalVal, MPStartYearVal = NULL) {
  om <- Simulate(om, silent = TRUE)

  assign(".dataLagLog", list(), envir = globalenv())
  on.exit(rm(".dataLagLog", envir = globalenv()), add = TRUE)

  logMP <- function(Data) {
    log <- get(".dataLagLog", envir = globalenv())
    log[[length(log) + 1]] <- max(Data@Years)
    assign(".dataLagLog", log, envir = globalenv())
    Advice(Effort = 1, EffType = 'Rel')
  }
  class(logMP) <- 'mp'
  assign(".logMP_dataLag", logMP, envir = globalenv())
  on.exit(rm(".logMP_dataLag", envir = globalenv()), add = TRUE)

  invisible(Project(om, MPs = ".logMP_dataLag", silent = TRUE))
  unlist(get(".dataLagLog", envir = globalenv()))
}

test_that("DataLag applies strictly (monotonically, matching the formula) from the first management year", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  pYear(om) <- 8

  CurrentYr <- CurrentYear(om)
  ManagementYears <- CurrentYr + seq_len(pYear(om))

  for (DataLagVal in c(0, 1, 2, 3)) {
    DataLag(om) <- DataLagVal
    Interval(om) <- 1
    set.seed(1)
    dataYearsUsedRaw <- .RunDataLagMP(om, DataLagVal, 1)

    # one call per sim per management year, grouped consecutively; every sim
    # within a management year must agree on the data year used
    byYear <- matrix(dataYearsUsedRaw, nrow = om@nSim)
    expect_true(all(apply(byYear, 2, \(x) length(unique(x)) == 1)),
                info = paste("DataLag =", DataLagVal))
    dataYearsUsed <- byYear[1, ]

    expected <- ManagementYears - 1 - DataLagVal
    expect_equal(dataYearsUsed, expected,
                 info = paste("DataLag =", DataLagVal))
    # must be strictly non-decreasing (no backward jump / plateau-then-dip)
    expect_true(all(diff(dataYearsUsed) >= 0),
                info = paste("DataLag =", DataLagVal))
  }
})

test_that("DataLag's data-year rule does not depend on Interval", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  pYear(om) <- 8
  DataLag(om) <- 1
  Interval(om) <- 2

  set.seed(1)
  dataYearsUsed <- .RunDataLagMP(om, 1, 2)

  CurrentYr <- CurrentYear(om)
  ManagementYears <- CurrentYr + seq(1, pYear(om), by = 2)
  expect_equal(dataYearsUsed, ManagementYears - 1 - 1)
})

test_that("DataLag lags from the last InterimAdvice year, not the OM's true historical year", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  pYear(om) <- 8
  DataLag(om) <- 2
  Interval(om) <- 1

  CurrentYr <- CurrentYear(om)
  MPStartYear(om) <- CurrentYr + 5  # 4 interim years before the MP starts
  InterimAdvice(om) <- data.frame(
    Year = (CurrentYr + 1):(MPStartYear(om) - 1), Type = "Effort", Mean = 1
  )

  set.seed(1)
  dataYearsUsed <- .RunDataLagMP(om, 2, 1)

  ManagementYears <- MPStartYear(om) + seq_len(pYear(om) - (MPStartYear(om) - CurrentYr - 1)) - 1
  expect_equal(dataYearsUsed, ManagementYears - 1 - 2)
  # in particular, the very first MP call must NOT fall back to lagging
  # from the OM's true CurrentYear (which would give CurrentYr here)
  expect_equal(dataYearsUsed[1], MPStartYear(om) - 1 - 2)
  expect_true(dataYearsUsed[1] > CurrentYr)
})

test_that("DataLag applies correctly on a seasonal OM, preserving the season", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 1
  pYear(om) <- 3
  DataLag(om) <- 1
  Interval(om) <- 1  # once per year, at the same season each time

  set.seed(1)
  dataYearsUsed <- .RunDataLagMP(om, 1, 1)

  # calendar year decreases by exactly DataLag relative to the (annual)
  # management schedule, and the within-year (season) fraction is identical
  # across every call
  seasonFrac <- dataYearsUsed - floor(dataYearsUsed)
  expect_equal(seasonFrac, rep(seasonFrac[1], length(seasonFrac)), tolerance = 1e-3)
  expect_equal(diff(floor(dataYearsUsed)), rep(1, length(dataYearsUsed) - 1))
})
