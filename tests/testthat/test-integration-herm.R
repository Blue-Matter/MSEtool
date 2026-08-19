# Full-pipeline Herm integration test: real 2-stock OM, real fishing effort,
# real historical Simulate() through the compiled CalcFisheryDynamics_()
# loop (CalcSpawnProduction -> CalcRecruitment -> CalcNumberNext ->
# CalcTransition -> CalcBiomass/CalcCatch/CalcOverallF), not the isolated
# .PrepHistMisc()-only checks in test-stocktransition.R or the standalone
# unfished-equilibrium checks in test-unfished-herm.R.
#
# Conservation/correctness check design: a naive "diff a Herm run against an
# independent no-Herm run of the same OM" does NOT isolate the transition's
# own effect, because CalcUnfishedNumber() (see R/calc-unfished-number.R)
# already makes the *initial* (first historical year) population Herm-aware
# -- the two runs' populations diverge from year 1 for a real, intentional
# reason unrelated to any bug, so a naive diff conflates "the equilibrium
# reference changed" with "the transition step is broken". Instead, this
# recomputes a single year's transition step twice on top of the SAME
# already-simulated real trajectory - once with DoCalcTransition=1 (matches
# the real run), once with DoCalcTransition=0 (skip transition) - and
# compares. Both start from identical prior-year state (real fishing/
# recruitment already baked in), so any difference is attributable only to
# the transition step itself. The target year (and beyond) must be zeroed
# out first: CalcNumberNext() silently no-ops when the write-target year is
# already populated (an early-exit performance guard, not a bug), so
# recomputing a year in-place onto an already-fully-simulated Hist without
# clearing it first would silently reuse-not-recompute.

skip_on_cran()

.make_herm_integration_om <- function() {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1
  sn <- StockNames(om)

  Ages(om@Stock[[1]]) <- Ages(MaxAge = 8, PlusGroup = TRUE)
  NaturalMortality(om@Stock[[1]]) <- NaturalMortality(Pars = list(M = 0.25))
  SRR(om@Stock[[1]])@R0 <- 1000

  Ages(om@Stock[[2]]) <- Ages(MaxAge = 8, PlusGroup = TRUE)
  NaturalMortality(om@Stock[[2]]) <- NaturalMortality(Pars = list(M = 0.35))
  SRR(om@Stock[[2]])@R0 <- 500

  om <- PopulateOM(om, silent = TRUE)
  nAge1 <- nAge(om@Stock[[1]])

  # smooth step-function Frac: onset around age 3, ~complete by the plus group
  Frac <- rep(1, nAge1)
  Frac[4:nAge1] <- seq(1, 0, length.out = nAge1 - 3)
  FracArr <- array(Frac, dim = c(1, nAge1))

  om@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = FracArr))
  list(om = om, Frac = FracArr, stock_names = sn)
}

# recompute one year's transition step twice (with/without) on top of an
# already-simulated Hist, returning both post-timestep Number slices
.recompute_transition_step <- function(hist, target_idx) {
  HistYears  <- Years(hist, 'H')
  driver_year <- HistYears[target_idx - 1]

  hist <- .PrepHistMisc(hist)
  hist@Number[[1]][, , target_idx:length(HistYears), ] <- 0
  hist@Number[[2]][, , target_idx:length(HistYears), ] <- 0

  withT <- .CalcFisheryDynamics(hist, Years = driver_year, DoCalcTransition = 1, clone = 1)
  noT   <- .CalcFisheryDynamics(hist, Years = driver_year, DoCalcTransition = 0, clone = 1)

  list(
    N1_withT = withT@Number[[1]][1, , target_idx, ],
    N2_withT = withT@Number[[2]][1, , target_idx, ],
    N1_noT   = noT@Number[[1]][1, , target_idx, ],
    N2_noT   = noT@Number[[2]][1, , target_idx, ]
  )
}

test_that("Simulate() completes with no error on a 2-stock Herm OM with real fishing effort", {
  fixture <- .make_herm_integration_om()
  set.seed(42)
  hist <- Simulate(fixture$om, silent = TRUE)

  expect_s4_class(hist, "hist")
  # real fishing actually happened (not just an unfished trajectory)
  expect_gt(sum(hist@Landings), 0)
})

test_that("no NaNs or negative values anywhere in Number/Biomass/Landings", {
  fixture <- .make_herm_integration_om()
  set.seed(42)
  hist <- Simulate(fixture$om, silent = TRUE)

  expect_false(anyNA(hist@Number[[1]]))
  expect_false(anyNA(hist@Number[[2]]))
  expect_false(any(hist@Number[[1]] < 0))
  expect_false(any(hist@Number[[2]] < 0))
  expect_false(anyNA(hist@Biomass))
  expect_false(any(hist@Biomass < 0))
  expect_false(anyNA(hist@Landings))
  expect_false(any(hist@Landings < 0))
})

test_that("the transition step conserves total numbers and exactly matches the hazard, mid-history", {
  fixture <- .make_herm_integration_om()
  set.seed(42)
  hist <- Simulate(fixture$om, silent = TRUE)

  hz <- as.numeric(.HermHazardRate(fixture$Frac)[1, ])

  for (target_idx in c(6, 15)) {
    r <- .recompute_transition_step(hist, target_idx)

    # conservation: total across both stocks unchanged by the transition step
    expect_equal(
      sum(r$N1_withT) + sum(r$N2_withT),
      sum(r$N1_noT) + sum(r$N2_noT),
      tolerance = 1e-8,
      info = paste("target_idx =", target_idx)
    )

    # exact hazard reconstruction: Nmov = raw_from * hazard
    Nmov <- r$N1_noT * hz
    expect_equal(as.numeric(r$N1_withT), as.numeric(r$N1_noT - Nmov),
                tolerance = 1e-8, info = paste("target_idx =", target_idx))
    expect_equal(as.numeric(r$N2_withT), as.numeric(r$N2_noT + Nmov),
                tolerance = 1e-8, info = paste("target_idx =", target_idx))

    # qualitative sanity: the To stock strictly gained wherever hazard > 0
    # and the From stock had individuals present
    onset <- which(hz > 0 & r$N1_noT[, 1] > 0)
    expect_true(length(onset) > 0, info = paste("target_idx =", target_idx))
    expect_true(all(Nmov[onset, ] > 0), info = paste("target_idx =", target_idx))
    expect_true(all(as.numeric(r$N2_withT) >= as.numeric(r$N2_noT) - 1e-8))

    # no NaN/negative in the recomputed step itself
    expect_false(anyNA(r$N1_withT) || anyNA(r$N2_withT))
    expect_false(any(r$N1_withT < 0) || any(r$N2_withT < 0))
  }
})

test_that("CalcUnfished_Equilibrium() converges without a warning on the same Herm OM", {
  fixture <- .make_herm_integration_om()

  msgs <- testthat::capture_messages(
    eq <- CalcUnfished_Equilibrium(fixture$om, silent = TRUE)
  )
  expect_false(any(grepl("not reached", msgs)))
  expect_false(anyNA(eq@Number[[1]]))
  expect_false(anyNA(eq@Number[[2]]))
  expect_false(any(eq@Number[[1]] < 0))
  expect_false(any(eq@Number[[2]] < 0))
})
