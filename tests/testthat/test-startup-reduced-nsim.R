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
