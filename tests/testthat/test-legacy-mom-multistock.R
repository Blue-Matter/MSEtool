test_that("SimulateMOM simulates each stock of a multi-stock MOM", {
  skip_on_cran()
  multiHist <- .LegacyTwoStockHist()
  expect_length(multiHist, 2)
  for (p in 1:2) {
    H <- multiHist[[p]][[1]]
    SP <- H@SampPars$Stock
    nyears <- dim(H@TSdata$SBiomass)[2]
    achieved <- rowSums(H@TSdata$SBiomass[, nyears, ]) / SP$SSB0
    expect_equal(achieved, SP$D, tolerance = 1e-2, info = p)
  }
  expect_false(isTRUE(all.equal(multiHist[[1]][[1]]@SampPars$Stock$M, multiHist[[2]][[1]]@SampPars$Stock$M)))
})

test_that("ProjectMOM applies different MPs to each stock", {
  skip_on_cran()
  MMSE <- ProjectMOM(.LegacyTwoStockHist(), MPs = list(c("curEref", "NFref"), c("NFref", "curEref")),
                     silent = TRUE, parallel = FALSE)
  expect_equal(MMSE@nstocks, 2)
  expect_equal(MMSE@nMPs, 2)
  # MP 1: stock 1 at current effort, stock 2 unfished; MP 2 the reverse
  expect_true(all(MMSE@Catch[, 2, 1, 1, ] < 1e-6))
  expect_true(all(MMSE@Catch[, 1, 1, 2, ] < 1e-6))
  expect_true(all(MMSE@Catch[, 1, 1, 1, ] > 0))
  expect_true(all(MMSE@Catch[, 2, 1, 2, ] > 0))
})

test_that("ProjectMOM runs a multi-stock MMP", {
  skip_on_cran()
  .LocalLegacyMPs(MMP_half = function(x, DataList, reps = 1, ...)
    list(list(.LegacyRec(Effort = 1)), list(.LegacyRec(Effort = 0.5))))
  class(MMP_half) <- "MMP"
  assign("MMP_half", MMP_half, envir = globalenv())

  MMSE <- ProjectMOM(.LegacyTwoStockHist(), MPs = "MMP_half", silent = TRUE, parallel = FALSE)
  expect_equal(MMSE@nMPs, 1)
  expect_equal(MMSE@Effort[, 1, 1, 1, ], array(1, dim(MMSE@Effort[, 1, 1, 1, ])), ignore_attr = TRUE, tolerance = 1e-3)
  expect_equal(MMSE@Effort[, 2, 1, 1, ], array(0.5, dim(MMSE@Effort[, 2, 1, 1, ])), ignore_attr = TRUE, tolerance = 1e-3)
})

test_that("MOM cpars Data conditions one stock on real data", {
  skip_on_cran()
  RealData <- SubData_sim(1, .LegacyTwoStockHist()[[1]][[1]]@Data)
  MOM <- .LegacyTwoStockMOM()
  MOM@cpars <- list(list(list(Data = RealData)), list(list()))
  multiHist <- suppressMessages(SimulateMOM(MOM, parallel = FALSE, silent = TRUE))
  Data <- multiHist[[1]][[1]]@Data
  for (sim in seq_len(MOM@nsim)) {
    expect_equal(Data@Cat[sim, ], RealData@Cat[1, ], ignore_attr = TRUE)
    expect_equal(Data@Ind[sim, ], RealData@Ind[1, ], ignore_attr = TRUE)
  }
  expect_false(isTRUE(all.equal(multiHist[[2]][[1]]@Data@Cat[1, ], RealData@Cat[1, ])))
})
