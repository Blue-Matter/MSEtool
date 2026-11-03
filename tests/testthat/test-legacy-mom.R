test_that("SimulateMOM returns a multiHist by stock and fleet", {
  skip_on_cran()
  multiHist <- .LegacyMultiHist()
  MOM <- MSEtool::Albacore_TwoFleet
  expect_s3_class(multiHist, "multiHist")
  expect_length(multiHist, length(MOM@Stocks))
  expect_length(multiHist[[1]], length(MOM@Fleets[[1]]))
  expect_s4_class(multiHist[[1]][[1]], "Hist")
  # stock dynamics are stored with the first fleet; each fleet keeps its own removals
  expect_null(multiHist[[1]][[2]]@TSdata$SBiomass)
  expect_equal(dim(multiHist[[1]][[2]]@TSdata$Removals), dim(multiHist[[1]][[1]]@TSdata$Removals))
  expect_false(isTRUE(all.equal(multiHist[[1]][[1]]@TSdata$Removals, multiHist[[1]][[2]]@TSdata$Removals)))
  SP <- multiHist[[1]][[1]]@SampPars$Stock
  nyears <- dim(multiHist[[1]][[1]]@TSdata$SBiomass)[2]
  achieved <- rowSums(multiHist[[1]][[1]]@TSdata$SBiomass[, nyears, ]) / SP$SSB0
  expect_equal(achieved, SP$D, tolerance = 1e-2)
})

test_that("ProjectMOM applies per-fleet MPs", {
  skip_on_cran()
  multiHist <- .LegacyMultiHist()
  MMSE <- ProjectMOM(multiHist, MPs = list(list("curEref", "NFref")), silent = TRUE, parallel = FALSE)
  expect_s4_class(MMSE, "MMSE")
  expect_equal(MMSE@nstocks, 1)
  expect_equal(MMSE@nfleets, 2)
  expect_equal(MMSE@nMPs, 1)
  nsim <- MMSE@nsim; proyears <- MMSE@proyears
  expect_equal(dim(MMSE@SB_SBMSY), c(nsim, 1, 1, proyears))
  expect_equal(dim(MMSE@Catch), c(nsim, 1, 2, 1, proyears))
  # fleet 1 keeps current effort, fleet 2 stops fishing
  expect_true(all(abs(MMSE@Effort[, 1, 1, 1, ] - 1) < 1e-2))
  expect_true(all(MMSE@Catch[, 1, 2, 1, ] < 1e-6))
})

test_that("ProjectMOM recycles a single MP across fleets", {
  skip_on_cran()
  multiHist <- .LegacyMultiHist()
  MMSE <- ProjectMOM(multiHist, MPs = c("NFref", "curEref"), silent = TRUE, parallel = FALSE)
  expect_equal(MMSE@nMPs, 2)
  expect_true(all(MMSE@Catch[, 1, , 1, ] < 1e-6))
  expect_true(all(MMSE@Catch[, 1, , 2, ] > 0))
  # no fishing rebuilds the stock relative to current effort
  expect_true(all(MMSE@SSB[, 1, 1, proyears <- MMSE@proyears] > MMSE@SSB[, 1, 2, proyears]))
})

test_that("multiMSE matches SimulateMOM then ProjectMOM", {
  skip_on_cran()
  MOM <- MSEtool::Albacore_TwoFleet
  MOM@nsim <- 3; MOM@proyears <- 5; MOM@interval <- 1
  MMSE <- multiMSE(MOM, MPs = c("NFref", "curEref"), silent = TRUE, parallel = FALSE)
  MMSE2 <- ProjectMOM(.LegacyMultiHist(), MPs = c("NFref", "curEref"), silent = TRUE, parallel = FALSE)
  expect_identical(MMSE@SSB, MMSE2@SSB)
  expect_identical(MMSE@Catch, MMSE2@Catch)
})

test_that("a single-stock, single-fleet MOM reproduces the equivalent OM", {
  skip_on_cran()
  OM <- .LegacyOM(nsim = 3, proyears = 5)
  MOM <- new("MOM",
             nsim = OM@nsim, proyears = OM@proyears, interval = OM@interval,
             Stocks = list(SubOM(OM, "Stock")),
             Fleets = list(list(SubOM(OM, "Fleet"))),
             Obs = list(list(SubOM(OM, "Obs"))),
             Imps = list(list(SubOM(OM, "Imp"))),
             cpars = list(list(OM@cpars)),
             maxF = OM@maxF)
  MOM@seed <- OM@seed

  Hist <- Simulate(OM, silent = TRUE)
  multiHist <- SimulateMOM(MOM, silent = TRUE, parallel = FALSE)
  expect_equal(multiHist[[1]][[1]]@AtAge$Number, Hist@AtAge$Number, tolerance = 0.01)
  expect_equal(multiHist[[1]][[1]]@TSdata$SBiomass, Hist@TSdata$SBiomass, tolerance = 0.01)

  MPs <- c("NFref", "FMSYref", "curEref")
  MSE <- Project(Hist, MPs = MPs, silent = TRUE)
  MMSE <- ProjectMOM(multiHist, MPs = MPs, silent = TRUE, parallel = FALSE)
  expect_equal(MMSE@F_FMSY[, 1, 1, 2, ], array(1, c(MMSE@nsim, MMSE@proyears)), ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(MMSE@FM[, 1, 1, 3, ], array(MSE@FM_hist[, OM@nyears], c(MMSE@nsim, MMSE@proyears)),
               ignore_attr = TRUE, tolerance = 0.01)
  expect_equal(MMSE@SSB[, 1, , ], MSE@SSB, ignore_attr = TRUE, tolerance = 0.01)
})
