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

.MaxSimDim <- function(x) {
  if (isS4(x))
    return(max(0, vapply(methods::slotNames(x), \(s) .MaxSimDim(methods::slot(x, s)), numeric(1))))
  if (is.list(x) && !is.data.frame(x))
    return(max(0, vapply(x, .MaxSimDim, numeric(1))))
  n <- length(dimnames(x)$Sim)
  if (is.array(x) && n) n else 0
}

test_that("Populate() trims sim-dimensioned arrays after lowering OM@nSim", {
  skip_on_cran()
  for (nm in c("SingleStockOM", "HermOM", "ComplexOM")) {
    full <- Populate(get(nm), silent = TRUE)
    om <- full
    om@nSim <- 3

    om <- Populate(om, silent = TRUE)
    expect_equal(nSim(om), 3, label = nm)
    expect_equal(.MaxSimDim(om), 3, label = nm)
    expect_equal(om@Stock[[1]]@NaturalMortality@MeanAtAge,
                 full@Stock[[1]]@NaturalMortality@MeanAtAge[1:3, , , drop = FALSE],
                 label = nm)
    expect_equal(om@Stock[[1]]@SRR@RecDevHist,
                 full@Stock[[1]]@SRR@RecDevHist[1:3, , drop = FALSE], label = nm)
  }
})

test_that("Populate() trims a simulated OM after lowering nSim, matching Simulate()", {
  skip_on_cran()
  hist <- Simulate(SingleStockOM, silent = TRUE)
  om <- hist@OM
  nSim(om) <- 2

  om <- Populate(om, silent = TRUE)
  expect_equal(.MaxSimDim(om), 2)
  expect_equal(Simulate(om, silent = TRUE)@SBiomass,
               Simulate(Subset(SingleStockOM, Sims = 1:2), silent = TRUE)@SBiomass)
})

test_that("increasing nSim on a populated OM aborts with a clear message", {
  skip_on_cran()
  om <- OM(nSim = 3, nYear = 10, pYear = 3, Stock = ButterfishExStock,
           Fleet = AsympExFleet, Obs = CommercialFleetObs, Imp = FullComplianceImp)
  p <- Populate(om, silent = TRUE)
  nSim(p) <- 4
  expect_error(Populate(p, silent = TRUE), "cannot be increased on a populated OM")
  expect_error(Simulate(p, silent = TRUE), "cannot be increased on a populated OM")

  hist <- Simulate(om, silent = TRUE)
  om2 <- hist@OM
  nSim(om2) <- 5
  expect_error(Simulate(om2, silent = TRUE), "populated with 3 simulations")
})

test_that("increasing nSim before populating still works", {
  skip_on_cran()
  om <- OM(nSim = 3, nYear = 10, pYear = 3, Stock = ButterfishExStock,
           Fleet = AsympExFleet, Obs = CommercialFleetObs, Imp = FullComplianceImp)
  nSim(om) <- 4
  hist <- Simulate(om, silent = TRUE)
  expect_equal(unname(dim(hist@Landings)[1]), 4)
})
