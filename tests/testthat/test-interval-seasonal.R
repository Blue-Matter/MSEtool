# `Interval` is always denominated in years, regardless of `Seasons` -- see
# `.ResolveInterval()`/`.CalcManagementYears()` in R/helpers-mp.R. MPs that
# must run at every native timestep of a seasonal OM (e.g. because they
# derive a season-specific value directly from history) declare
# `attr(mp, 'EverySeason') <- TRUE` instead of a small `Interval`, since a
# fixed fraction like `1/Seasons` can't be hard-coded on the MP without
# knowing which OM it will run under.

test_that(".CalcManagementYears() converts years to timesteps using Seasons", {
  YearsProj <- seq(2020, by = 1/4, length.out = 40)  # 10 years, 4 seasons/yr

  # annual (Interval=1yr, Seasons=4 -> every 4th timestep)
  mgmt <- .CalcManagementYears(YearsProj, Interval = 1, Seasons = 4)
  expect_equal(mgmt, YearsProj[seq(1, 40, by = 4)])

  # every 3 years (Interval=3yr, Seasons=4 -> every 12th timestep)
  mgmt3 <- .CalcManagementYears(YearsProj, Interval = 3, Seasons = 4)
  expect_equal(mgmt3, YearsProj[seq(1, 40, by = 12)])

  # every season (Interval = 1/Seasons -> every timestep)
  mgmt_every <- .CalcManagementYears(YearsProj, Interval = 1/4, Seasons = 4)
  expect_equal(mgmt_every, YearsProj)

  # non-seasonal OM (Seasons=1) behaves as plain annual stepping
  YearsAnnual <- seq(2020, by = 1, length.out = 10)
  expect_equal(.CalcManagementYears(YearsAnnual, Interval = 2, Seasons = 1),
               YearsAnnual[seq(1, 10, by = 2)])
})

test_that(".CalcManagementYears() errors on a non-whole-timestep Interval", {
  YearsProj <- seq(2020, by = 1/4, length.out = 20)
  expect_error(.CalcManagementYears(YearsProj, Interval = 1/3, Seasons = 4))
})

test_that(".CalcManagementYears() allows (and just notes) a non-annually-aligned step", {
  YearsProj <- seq(2020, by = 1/4, length.out = 20)
  # Interval=0.5yr, Seasons=4 -> step=2 timesteps: valid ("twice a year,
  # alternating Q1/Q3"), not a whole number of years, but not an error.
  expect_message(
    mgmt <- .CalcManagementYears(YearsProj, Interval = 0.5, Seasons = 4),
    "rotate across different calendar seasons"
  )
  expect_equal(mgmt, YearsProj[seq(1, 20, by = 2)])
})

test_that(".ResolveInterval() resolves EverySeason to 1/Seasons, with named overrides winning", {
  mp <- function(Data) NULL
  attr(mp, 'EverySeason') <- TRUE

  expect_equal(.ResolveInterval(1, "mp", mp, Seasons = 4), 0.25)
  expect_equal(.ResolveInterval(1, "mp", mp, Seasons = 1), 1)

  # explicit per-MP OM@Interval override still wins over EverySeason
  expect_equal(.ResolveInterval(c(mp = 2), "mp", mp, Seasons = 4), 2)

  # MP's own declared Interval (years) used when EverySeason is not set
  mp2 <- function(Data) NULL
  attr(mp2, 'Interval') <- 3
  expect_equal(.ResolveInterval(1, "mp2", mp2, Seasons = 4), 3)

  # falls back to the OM's generic Interval when neither is set
  mp3 <- function(Data) NULL
  expect_equal(.ResolveInterval(5, "mp3", mp3, Seasons = 4), 5)
})

test_that("built-in status-quo MPs declare EverySeason, not a fixed small Interval", {
  for (mp_name in c("CurrentEffort", "CurrentCatch", "CurrentLandings",
                     "AverageCatch", "refFCurr")) {
    mp <- get(mp_name)
    expect_true(isTRUE(attr(mp, 'EverySeason')),
                info = paste(mp_name, "should declare EverySeason"))
  }
})

test_that("CurrentCatch reproduces the exact historical seasonal pattern under Project()", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  L <- mse@Landings[, , , , 1]  # Sim x Year (single stock/fleet)
  Seasons <- om@Seasons
  nYearProj <- dim(L)[2] / Seasons

  # realised landings must repeat the same Seasons-length cycle every year
  first_cycle <- unname(L[1, seq_len(Seasons)])
  for (yr in seq_len(nYearProj) - 1) {
    idx <- yr * Seasons + seq_len(Seasons)
    expect_equal(unname(L[1, idx]), first_cycle, tolerance = 1e-8)
  }
})

test_that("a custom MP with a multi-year Interval stays phase-aligned on a seasonal OM", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  # Interval=3 now means "every 3 years" (not "every 3 timesteps"); this
  # must land management on the same calendar season every time.
  hist@OM@Interval <- 3

  AnnualEqualSplit <- function(Data) {
    CheckCatch(Data)
    si  <- SeasonOfYear(Data)
    LHY <- LastHistYearInd(Data)
    rows <- (LHY - si$Seasons + 1):LHY
    total <- sum(Data@Landings@Value[rows, , drop = FALSE], na.rm = TRUE) +
             sum(Data@Discards@Value[rows, , drop = FALSE], na.rm = TRUE)
    Advice(TAC = total / si$Seasons, TACUnit = Data@Landings@Units)
  }
  class(AnnualEqualSplit) <- 'mp'
  assign("AnnualEqualSplit", AnnualEqualSplit, envir = globalenv())
  on.exit(rm("AnnualEqualSplit", envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = "AnnualEqualSplit", parallel = FALSE, silent = TRUE)

  # Should complete without the "must correspond to a whole number of
  # timesteps" abort, and should not collapse (Seasons-fold overcatch would
  # otherwise crater biomass within the first Interval).
  expect_s4_class(mse, "mse")
  expect_true(all(is.finite(mse@SBiomass)))
  expect_gt(min(mse@SBiomass), 0)
})
