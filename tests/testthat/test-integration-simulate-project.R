# Small-scale (nSim = 1) end-to-end integration tests. Real Simulate()/
# Project() calls, so kept out of the CRAN check budget -- see
# skip_on_cran() below. Not a substitute for the full multi-OM, realistic-
# scale regression sweep run by hand before a release (see tests/manual/).

test_that("Simulate() completes for a single-stock OM and produces valid output", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  expect_s4_class(hist, "hist")
  expect_false(is.null(hist@Reference@SPR0))
  expect_false(is.null(hist@Reference@MSY))
  expect_false(anyNA(hist@SBiomass))
})

test_that("Project() completes and produces a valid mse object", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  expect_s4_class(mse, "mse")
  expect_false(anyNA(mse@SBiomass))
})

test_that("CalcRefPoints() runs end-to-end and produces internally consistent output", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  hist <- CalcRefPoints(hist, silent = TRUE)

  expect_false(is.null(F01(hist)))
  expect_false(is.null(FMax(hist)))
  # F0.1 occurs before the yield-per-recruit peak
  expect_lt(as.numeric(F01(hist))[1], as.numeric(FMax(hist))[1])
})

test_that("Imp Compliance does not crash a multi-stock projection", {
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  ComplexNames <- names(Complexes(hist0@OM))
  FleetNames <- FleetNames(hist0@OM)

  om@Imp <- stats::setNames(
    list(MakeNamedList(FleetNames, Imp(TAC = ImpSlot(Compliance = 1))),
         MakeNamedList(FleetNames, Imp())),
    ComplexNames
  )
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  expect_s4_class(mse, "mse")
  expect_true(length(hist@OM@Imp) == length(ComplexNames))
})

test_that("Seasonal OM completes Simulate()+Project() without error", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  expect_s4_class(mse, "mse")
})
