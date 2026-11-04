# Real data are taken from the first simulation of the cached Hist
.LegacyRealData <- function() SubData_sim(1, .LegacyHist()@Data)

test_that("OM@cpars$Data replaces simulated historical data with the real data", {
  skip_on_cran()
  RealData <- .LegacyRealData()
  OM <- .LegacyOM()
  OM@cpars$Data <- RealData
  Hist <- Simulate(OM, silent = TRUE)
  Data <- Hist@Data
  nsim <- OM@nsim

  for (sim in seq_len(nsim)) {
    expect_equal(Data@Cat[sim, ], RealData@Cat[1, ], ignore_attr = TRUE)
    expect_equal(Data@Ind[sim, ], RealData@Ind[1, ], ignore_attr = TRUE)
    expect_equal(Data@CAA[sim, , ], RealData@CAA[1, , ], ignore_attr = TRUE)
    expect_equal(Data@CAL[sim, , ], RealData@CAL[1, , ], ignore_attr = TRUE)
  }
  expect_equal(Data@CAL_bins, RealData@CAL_bins)

  # index observation error is estimated from the fit to the real index
  IndStat <- Hist@SampPars$Obs$Ind_Stat
  expect_s3_class(IndStat, "data.frame")
  expect_equal(nrow(IndStat), nsim)
  expect_true(all(is.finite(as.matrix(IndStat))))
})

test_that("projections append simulated data to the real historical data", {
  skip_on_cran()
  RealData <- .LegacyRealData()
  OM <- .LegacyOM()
  OM@cpars$Data <- RealData
  Hist <- Simulate(OM, silent = TRUE)
  MSE <- Project(Hist, MPs = "curEref", silent = TRUE)
  PPD <- MSE@PPD[[1]]
  nyears <- OM@nyears
  expect_equal(PPD@Cat[, seq_len(nyears)], Hist@Data@Cat, ignore_attr = TRUE)
  expect_true(all(is.finite(PPD@Cat[, nyears + seq_len(OM@proyears - 1)])))
  expect_true(all(is.finite(PPD@Ind[, nyears + seq_len(OM@proyears - 1)])))
})

test_that("additional indices in the real data are conditioned", {
  skip_on_cran()
  OM <- .LegacyOM()
  RealData <- .LegacyRealData()
  nyears <- OM@nyears
  RealData@AddInd <- array(RealData@Ind, c(1, 1, nyears))
  RealData@CV_AddInd <- array(0.2, c(1, 1, nyears))
  RealData@AddIndV <- array(1, c(1, 1, OM@maxage + 1))
  RealData@AddIunits <- 1
  RealData@AddIndType <- 1
  OM@cpars$Data <- RealData

  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(dim(Hist@Data@AddInd), c(OM@nsim, 1, nyears))
  expect_equal(Hist@Data@AddInd[2, 1, ], RealData@AddInd[1, 1, ], ignore_attr = TRUE)
  AddStat <- Hist@SampPars$Obs$AddInd_Stat[[1]]
  expect_true(all(c("AC", "SD") %in% names(AddStat)))

  MSE <- Project(Hist, MPs = "curEref", silent = TRUE)
  expect_true(all(is.finite(MSE@PPD[[1]]@AddInd[, 1, nyears + seq_len(OM@proyears - 1)])))
})
