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

test_that("back-calculated effort reproduces requested effort when maxF does not bind", {
  skip_on_cran()
  # The historical period takes effort as given, so back-calculating it from the
  # realised F must return the same numbers unless the maxF clamp binds. Covers
  # the multi-area sum and the density weighting.
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 2
  expect_gt(nArea(om), 1L)

  om@Control$BackCalcEffort <- FALSE
  set.seed(1)
  hist_off <- Simulate(om, silent = TRUE)

  om@Control$BackCalcEffort <- TRUE
  set.seed(1)
  hist_on <- Simulate(om, silent = TRUE)

  expect_equal(hist_on@Effort, hist_off@Effort, tolerance = 1e-10)
})

test_that("back-calculated effort keeps an unachievable TAC off the maxEffort cap", {
  skip_on_cran()
  # CurrentCatch on a stock that cannot sustain it drives effort to maxEffort;
  # the back-calculation reports the effort matching the capped F instead.
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  hist@OM@Control$BackCalcEffort <- TRUE
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  expect_true(all(is.finite(mse@Effort)))
  expect_lt(max(mse@Effort), 1e6)
})

test_that("implementation error applies to fleet-by-area effort advice", {
  skip_on_cran()
  # Imp@Effort@Error is a per-fleet multiplier on effort magnitude, so advice
  # given as a [nFleet x nArea] matrix must be scaled the same as the
  # equivalent per-fleet vector - it used to be skipped entirely.
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  nF <- length(FleetNames(hist)); nA <- nArea(hist); ns <- nSim(hist)
  AllY <- Years(hist@OM)
  Err <- array(0.5, dim = c(ns, length(AllY)),
               dimnames = list(Sim = seq_len(ns), Year = as.character(AllY)))
  for (fl in seq_len(nF)) hist@OM@Imp[[1]][[fl]]@Effort@Error <- Err

  EffVec <- function(Data) Advice(Effort = rep(0.4, 1), EffType = "Abs")
  EffMat <- function(Data) Advice(Effort = matrix(0.4 / 3, nrow = 1, ncol = 3),
                                  EffType = "Abs")
  class(EffVec) <- class(EffMat) <- "mp"
  assign("EffVec", EffVec, envir = globalenv())
  assign("EffMat", EffMat, envir = globalenv())
  on.exit(rm("EffVec", "EffMat", envir = globalenv()), add = TRUE)

  e_vec <- as.vector(Project(hist, MPs = "EffVec", silent = TRUE)@Effort)
  e_mat <- as.vector(Project(hist, MPs = "EffMat", silent = TRUE)@Effort)

  expect_equal(e_mat, e_vec, tolerance = 1e-8)
  expect_equal(e_vec[1], 0.2, tolerance = 1e-8)
})
