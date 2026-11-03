test_that("Simulate returns a legacy Hist with consistent dimensions", {
  skip_on_cran()
  OM <- .LegacyOM()
  Hist <- .LegacyHist()
  expect_s4_class(Hist, "Hist")
  expect_s4_class(Hist@Data, "Data")
  nareas <- Hist@SampPars$Stock$nareas

  for (nm in c("Biomass", "SBiomass", "VBiomass", "Removals", "Landings"))
    expect_equal(dim(Hist@TSdata[[nm]]), c(OM@nsim, OM@nyears, nareas), info = nm)
  expect_equal(dim(Hist@AtAge$Number), c(OM@nsim, OM@maxage + 1, OM@nyears, nareas))
  expect_equal(dim(Hist@Data@Cat), c(OM@nsim, OM@nyears))
  expect_equal(nrow(Hist@OMPars), OM@nsim)
  expect_true(all(is.finite(Hist@TSdata$SBiomass)))
  expect_true(all(Hist@TSdata$Removals >= 0))
})

test_that("Simulate conditions the stock on the sampled depletion", {
  skip_on_cran()
  OM <- .LegacyOM()
  Hist <- .LegacyHist()
  SP <- Hist@SampPars$Stock
  achieved <- rowSums(Hist@TSdata$SBiomass[, OM@nyears, ]) / SP$SSB0
  expect_equal(achieved, SP$D, tolerance = 1e-3)
  expect_true(all(SP$D >= OM@D[1] & SP$D <= OM@D[2]))
})

test_that("Simulate reports internally consistent reference points", {
  skip_on_cran()
  OM <- .LegacyOM()
  Hist <- .LegacyHist()
  RP <- Hist@Ref$ReferencePoints
  expect_equal(nrow(RP), OM@nsim)
  expect_true(all(RP$FMSY > 0 & RP$MSY > 0))
  expect_true(all(RP$SSBMSY < RP$SSB0))
  expect_true(all(RP$BMSY < RP$B0))
  expect_equal(RP$SSB0, Hist@SampPars$Stock$SSB0)

  ByYear <- Hist@Ref$ByYear
  expect_equal(dim(ByYear$SSBMSY), c(OM@nsim, OM@nyears + OM@proyears))
  expect_true(all(ByYear$FMSY > 0))
})

test_that("Simulate is reproducible for a given seed", {
  skip_on_cran()
  OM <- .LegacyOM()
  Hist <- .LegacyHist()
  Hist2 <- Simulate(OM, silent = TRUE)
  expect_identical(Hist2@TSdata$SBiomass, Hist@TSdata$SBiomass)
  expect_identical(Hist2@Data@Cat, Hist@Data@Cat)

  OM@seed <- OM@seed + 1
  Hist3 <- Simulate(OM, silent = TRUE)
  expect_false(isTRUE(all.equal(Hist3@SampPars$Stock$M, Hist@SampPars$Stock$M)))
})

test_that("Simulate uses cpars and the nsim argument", {
  skip_on_cran()
  OM <- .LegacyOM()
  OM@cpars <- list(M = c(0.25, 0.3, 0.35), D = c(0.2, 0.4, 0.6))
  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(Hist@SampPars$Stock$M, OM@cpars$M)
  achieved <- rowSums(Hist@TSdata$SBiomass[, OM@nyears, ]) / Hist@SampPars$Stock$SSB0
  expect_equal(achieved, OM@cpars$D, tolerance = 1e-3)

  Hist2 <- Simulate(.LegacyOM(), nsim = 2, silent = TRUE)
  expect_equal(nrow(Hist2@OMPars), 2)
})

test_that("runMSE(Hist = TRUE) matches Simulate", {
  skip_on_cran()
  Hist <- runMSE(.LegacyOM(), Hist = TRUE, silent = TRUE)
  expect_s4_class(Hist, "Hist")
  expect_identical(Hist@TSdata$SBiomass, .LegacyHist()@TSdata$SBiomass)
})

test_that("Simulate rejects an OM with one simulation", {
  expect_error(runMSE(.LegacyOM(nsim = 1), Hist = TRUE, silent = TRUE), "nsim")
})
