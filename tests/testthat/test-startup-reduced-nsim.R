# Setting `OM@nSim` directly to a value smaller than what's already baked
# into the OM (e.g. a loaded, previously-populated OM) and then calling
# `Simulate(OM)` used to fail: `PopulateOM()`'s cache machinery only ever
# grows/reuses Sim-dimensioned arrays, never shrinks them, so a manually
# lowered `OM@nSim` left some arrays (e.g. `Stock@Fecundity@MeanAtAge`)
# stuck at the original size while others picked up the new one. Fixed by
# reducing via `ReduceNSim(OM, OM@nSim)` before `PopulateOM()` runs in
# `.StartUp()` (R/utils-startup.R).

test_that("Simulate() works after manually lowering OM@nSim on a populated OM", {
  skip_on_cran()
  om <- PopulateOM(SingleStockOM, silent = TRUE)
  om@nSim <- 2

  hist <- Simulate(om, silent = TRUE)
  expect_s4_class(hist, "hist")
  expect_equal(unname(dim(hist@Landings)[1]), 2)
})

test_that("Simulate() works after lowering OM@nSim to 1", {
  skip_on_cran()
  om <- PopulateOM(SingleStockOM, silent = TRUE)
  om@nSim <- 1

  hist <- Simulate(om, silent = TRUE)
  expect_s4_class(hist, "hist")
  expect_equal(unname(dim(hist@Landings)[1]), 1)
  expect_false(anyNA(hist@SBiomass))
})

test_that("Simulate() works after raising nSim on built-in multi-stock OMs", {
  skip_on_cran()
  for (nm in c("MultiStockOM", "ComplexOM", "HermOM")) {
    om <- get(nm)
    nSim(om) <- nSim(om) + 3

    hist <- Simulate(om, silent = TRUE)
    expect_s4_class(hist, "hist")
    expect_equal(unname(dim(hist@OM@StockTargeting@Targeting)[1]), nSim(om),
                 label = nm)
  }
})

test_that("Simulate() works after raising nSim on a user-built multi-stock OM", {
  skip_on_cran()
  om <- OM(nSim = 3, nYear = 20, pYear = 5,
           Stock = list(AlbacoreExStock, ButterfishExStock),
           Fleet = list(list(AsympExFleet), list(AsympExFleet)))
  nSim(om) <- 5

  hist <- Simulate(om, silent = TRUE)
  expect_equal(unname(dim(hist@OM@StockTargeting@Targeting)[1]), 5)
})

test_that("Simulate() drops StockTargeting placeholders sized to an earlier nSim", {
  skip_on_cran()
  om <- MultiStockOM
  om@StockTargeting <- StockTargeting(om)
  om@nSim <- nSim(om) + 2

  hist <- Simulate(om, silent = TRUE)
  expect_equal(unname(dim(hist@OM@StockTargeting@Targeting)[1]), nSim(om))
})
