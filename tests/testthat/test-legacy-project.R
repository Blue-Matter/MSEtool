test_that("Project returns a legacy MSE with consistent dimensions", {
  skip_on_cran()
  OM <- .LegacyOM()
  MSE <- .LegacyMSE()
  expect_s4_class(MSE, "MSE")
  expect_identical(MSE@MPs, c("FMSYref", "curEref", "NFref"))
  expect_equal(MSE@nMPs, 3)
  for (nm in c("SB_SBMSY", "F_FMSY", "B", "SSB", "FM", "Catch", "Removals", "Effort", "TAC"))
    expect_equal(dim(slot(MSE, nm)), c(OM@nsim, 3, OM@proyears), info = nm)
  expect_equal(dim(MSE@SSB_hist), c(OM@nsim, OM@nyears))
  expect_length(MSE@PPD, 3)
  expect_s4_class(MSE@PPD[[1]], "Data")
  expect_equal(dim(MSE@PPD[[1]]@Cat), c(OM@nsim, OM@nyears + OM@proyears - 1))
})

test_that("reference MPs produce their defining fishing mortality", {
  skip_on_cran()
  OM <- .LegacyOM()
  MSE <- .LegacyMSE()
  # FMSYref fishes at FMSY
  expect_equal(MSE@F_FMSY[, 1, ], array(1, dim(MSE@F_FMSY[, 1, ])), ignore_attr = TRUE, tolerance = 1e-6)
  # curEref holds the last historical F
  expect_equal(MSE@FM[, 2, ], array(MSE@FM_hist[, OM@nyears], dim(MSE@FM[, 2, ])), ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(MSE@Effort[, 2, ], array(1, dim(MSE@Effort[, 2, ])), ignore_attr = TRUE)
  # NFref removes no catch and the stock rebuilds
  expect_true(all(MSE@Catch[, 3, ] < 1e-8))
  expect_true(all(MSE@FM[, 3, ] < 1e-8))
  expect_true(all(MSE@SSB[, 3, OM@proyears] > MSE@SSB[, 2, OM@proyears]))
})

test_that("SB_SBMSY uses the dynamic SSBMSY reference", {
  skip_on_cran()
  OM <- .LegacyOM()
  MSE <- .LegacyMSE()
  SSBMSY <- MSE@RefPoint$ByYear$SSBMSY[, OM@nyears + seq_len(OM@proyears)]
  for (mp in seq_len(MSE@nMPs))
    expect_equal(MSE@SB_SBMSY[, mp, ], MSE@SSB[, mp, ] / SSBMSY, ignore_attr = TRUE)
})

test_that("custom output and input controls are implemented", {
  skip_on_cran()
  .LocalLegacyMPs(
    fixTAC  = function(x, Data, reps = 1, ...) .LegacyRec(TAC = 100),
    halfE   = function(x, Data, reps = 1, ...) .LegacyRec(Effort = 0.5),
    sizeLim = function(x, Data, reps = 1, ...)
      .LegacyRec(LR5 = 0.9 * Data@vbLinf[x], LFR = 0.95 * Data@vbLinf[x]),
    closeA1 = function(x, Data, reps = 1, ...) .LegacyRec(Spatial = c(0, 1))
  )
  MSE <- Project(.LegacyHist(), MPs = c("curEref", "fixTAC", "halfE", "sizeLim", "closeA1"), silent = TRUE)

  expect_equal(MSE@TAC[, 2, ], array(100, dim(MSE@TAC[, 2, ])), ignore_attr = TRUE)
  expect_equal(MSE@Catch[, 2, ], array(100, dim(MSE@Catch[, 2, ])), ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(MSE@FM[, 3, ] / MSE@FM[, 1, ], array(0.5, dim(MSE@FM[, 3, ])), ignore_attr = TRUE, tolerance = 1e-6)
  expect_equal(MSE@Effort[, 3, ], array(0.5, dim(MSE@Effort[, 3, ])), ignore_attr = TRUE)
  expect_true(all(MSE@Catch[, 4, ] < MSE@Catch[, 1, ]))
  expect_false(isTRUE(all.equal(MSE@Catch[, 5, ], MSE@Catch[, 1, ])))
})

test_that("MPs are applied at the management interval with updated data", {
  skip_on_cran()
  .LocalLegacyMPs(nYrTAC = function(x, Data, reps = 1, ...) .LegacyRec(TAC = length(Data@Year)))
  OM <- .LegacyOM()
  OM@interval <- 3
  Hist <- Simulate(OM, silent = TRUE)
  MSE <- Project(Hist, MPs = "nYrTAC", silent = TRUE)
  TAC <- MSE@TAC[1, 1, ]
  expect_equal(unname(TAC), rep(OM@nyears + c(0, 3, 6, 9), each = 3))
})

test_that("implementation error scales the TAC", {
  skip_on_cran()
  .LocalLegacyMPs(fixTAC = function(x, Data, reps = 1, ...) .LegacyRec(TAC = 100))
  OM <- .LegacyOM()
  OM@TACFrac <- c(0.5, 0.5)
  OM@TACSD <- c(0, 0)
  MSE <- Project(Simulate(OM, silent = TRUE), MPs = "fixTAC", silent = TRUE)
  expect_equal(MSE@Catch[, 1, ], array(50, dim(MSE@Catch[, 1, ])), ignore_attr = TRUE, tolerance = 1e-4)
})

test_that("Project returns extended output and matches runMSE", {
  skip_on_cran()
  OM <- .LegacyOM()
  MSE <- Project(.LegacyHist(), MPs = "curEref", extended = TRUE, silent = TRUE)
  expect_true(all(c("N", "B", "SSB", "Catch", "FM") %in% names(MSE@Misc$extended)))

  MSE2 <- runMSE(OM, MPs = c("FMSYref", "curEref", "NFref"), silent = TRUE)
  expect_identical(MSE2@SSB, .LegacyMSE()@SSB)
  expect_identical(MSE2@Catch, .LegacyMSE()@Catch)
})

test_that("Project errors for an unknown MP", {
  skip_on_cran()
  expect_error(Project(.LegacyHist(), MPs = "notAnMP", silent = TRUE), "notAnMP")
})
