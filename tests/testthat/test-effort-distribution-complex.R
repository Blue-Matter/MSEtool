# Area distribution of effort advice in `.UpdateEffortSim()`: each fleet takes
# the [nFleet x nArea] allocation of the complex that binds its effort.

test_that("refFMSY projects a multi-complex OM without error", {
  skip_on_cran()
  om <- MultiStockOM
  om@nSim <- 4
  pYear(om) <- 5
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "refFMSY", silent = TRUE)

  expect_length(mse@Log$error, 0)
  expect_true(all(is.finite(mse@SBiomass)))
})

test_that("each fleet's area distribution follows its binding complex", {
  skip_on_cran()
  om <- MultiStockOM
  om@nSim <- 3
  pYear(om) <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  ProjYears <- as.character(Years(hist@OM, "Projection"))

  # complex 1 binds fleet 1 with a 0.25/0.75 area split; complex 2 binds fleet 2
  MixedEff <- function(Data) {
    if (grepl("Albacore", Data@Name))
      return(Advice(Effort = matrix(c(0.05, 1e3, 0.15, 1e3), 2, 2), EffType = "Abs"))
    Advice(Effort = 1, EffType = "Rel")
  }
  BothMat <- function(Data) {
    if (grepl("Albacore", Data@Name))
      return(Advice(Effort = matrix(c(0.05, 1e3, 0.15, 1e3), 2, 2), EffType = "Abs"))
    Advice(Effort = matrix(c(1e3, 0.3, 1e3, 0.1), 2, 2), EffType = "Abs")
  }
  class(MixedEff) <- class(BothMat) <- "mp"
  assign("MixedEff", MixedEff, envir = globalenv())
  assign("BothMat", BothMat, envir = globalenv())
  on.exit(rm("MixedEff", "BothMat", envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = c("MixedEff", "BothMat"), silent = TRUE)
  expect_length(mse@Log$error, 0)
  expect_true(all(is.finite(mse@SBiomass)))

  Eff  <- mse@Effort[, ProjYears, , , drop = FALSE]
  Dist <- mse@Distribution[, ProjYears, , , , drop = FALSE]

  # scalar Rel complex binds fleet 2: its distribution stays dynamic
  expect_equal(range(Eff[, , 1, "MixedEff"]), c(0.2, 0.2), tolerance = 1e-8)
  expect_equal(range(Eff[, , 2, "MixedEff"]), c(1, 1), tolerance = 1e-8)
  expect_equal(range(Dist[, , 1, 1, "MixedEff"]), c(0.25, 0.25), tolerance = 1e-8)
  expect_equal(range(Dist[, , 1, 2, "MixedEff"]), c(0.75, 0.75), tolerance = 1e-8)
  expect_gt(stats::sd(Dist[, , 2, 1, "MixedEff"]), 0)

  expect_equal(range(Eff[, , 1, "BothMat"]), c(0.2, 0.2), tolerance = 1e-8)
  expect_equal(range(Eff[, , 2, "BothMat"]), c(0.4, 0.4), tolerance = 1e-8)
  expect_equal(range(Dist[, , 1, 1, "BothMat"]), c(0.25, 0.25), tolerance = 1e-8)
  expect_equal(range(Dist[, , 2, 1, "BothMat"]), c(0.75, 0.75), tolerance = 1e-8)
})

test_that("a single-complex fleet x area effort matrix sets every fleet's distribution", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  om@maxF <- 100 # keep the per-area maxF clamp from scaling the requested effort
  pYear(om) <- 3
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  ProjYears <- as.character(Years(hist@OM, "Projection"))

  EffMat <- function(Data) Advice(Effort = matrix(c(0.05, 0.3, 0.15, 0.1), 2, 2),
                                  EffType = "Abs")
  class(EffMat) <- "mp"
  assign("EffMat", EffMat, envir = globalenv())
  on.exit(rm("EffMat", envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = "EffMat", silent = TRUE)
  expect_length(mse@Log$error, 0)

  Dist <- mse@Distribution[, ProjYears, , , 1, drop = FALSE]
  expect_equal(range(Dist[, , 1, 1, 1]), c(0.25, 0.25), tolerance = 1e-8)
  expect_equal(range(Dist[, , 2, 1, 1]), c(0.75, 0.75), tolerance = 1e-8)
  expect_equal(range(mse@Effort[, ProjYears, 1, 1]), c(0.2, 0.2), tolerance = 1e-8)
  expect_equal(range(mse@Effort[, ProjYears, 2, 1]), c(0.4, 0.4), tolerance = 1e-8)
})
