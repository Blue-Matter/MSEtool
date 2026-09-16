# Regression tests for the reference MPs (R/mp-reference.R). Real
# Simulate()/Project() calls on the seasonal example OM, so kept out of the
# CRAN check budget -- see skip_on_cran() below.

test_that("refMSY* divides TAC by Seasons on a seasonal OM", {
  skip_on_cran()
  # .RefMSYAdvice() returns a fixed annual removals figure (fraction * MSY)
  # as `Advice@TAC`, and .ApplyMP() reapplies whatever TAC an MP returns,
  # unchanged, at every timestep until the MP is next called. refMSY* has
  # `Interval = 1`, so on a seasonal OM it is called every season. Without
  # dividing by the number of seasons, the annual MSY figure would be
  # applied as the TAC at every season, overcatching by a factor of
  # `Seasons` each year.
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  # refMSY50 (fraction = 0.5) is used rather than refMSY (fraction = 1)
  # because fishing this particular stock at exactly 100% of MSY as a
  # constant catch is genuinely unstable (a known property of constant-
  # catch strategies under recruitment variability) and collapses the
  # stock after a few years regardless of the seasonal-scaling fix under
  # test. refMSY50 exercises the same `.RefMSYAdvice()` code path and
  # stays stable, isolating the behaviour this test targets.
  mse <- Project(hist, MPs = "refMSY50", parallel = FALSE, silent = TRUE)

  expect_s4_class(mse, "mse")
  expect_false(anyNA(mse@SBiomass))
  expect_gt(min(mse@SBiomass), 0)

  TotalRemovals <- mse@Landings + mse@Discards
  PerSeason     <- apply(TotalRemovals, 3, sum)

  Seasons      <- om@Seasons
  FirstYearIdx <- seq_len(Seasons)
  AnnualCatch  <- sum(PerSeason[FirstYearIdx])

  MSYTarget <- 0.5 * (as.numeric(MSYLandings(hist)) + as.numeric(MSYDiscards(hist)))

  # Under `FullComplianceImp`, realised removals should match the intended
  # annual target closely (not the ~Seasons-fold overcatch of the bug).
  expect_equal(AnnualCatch, MSYTarget, tolerance = 0.01)
})
