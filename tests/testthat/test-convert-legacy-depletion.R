.SBDepletion <- function(Hist, Year) {
  yr <- as.character(Year)
  Hist@SBiomass[, 1, yr] / Hist@Unfished@Equilibrium@SBiomass[, 1, yr]
}

test_that("converted legacy D is conditioned on SB/SB0", {
  skip_on_cran()
  om <- Convert(testOM, silent = TRUE)
  expect_identical(om@Stock[[1]]@Depletion@Reference, "SB0")

  Hist <- Simulate(om, silent = TRUE)
  Final <- as.numeric(Hist@OM@Stock[[1]]@Depletion@Final)
  achieved <- .SBDepletion(Hist, max(Hist@OM@Years[Hist@OM@Years <= Hist@OM@CurrentYear]))
  expect_equal(as.numeric(achieved), Final, tolerance = 1e-3)
})

test_that("converted legacy cpars D and initD use the SB0 reference", {
  skip_on_cran()
  OM <- testOM
  OM@cpars <- list(D = c(0.3, 0.4, 0.5), initD = c(0.6, 0.7, 0.8))
  om <- Convert(OM, silent = TRUE)
  Dep <- om@Stock[[1]]@Depletion
  expect_identical(Dep@Reference, "SB0")
  expect_equal(as.numeric(Dep@Final), c(0.3, 0.4, 0.5))
  expect_equal(as.numeric(Dep@Initial), c(0.6, 0.7, 0.8))

  Hist <- Simulate(om, silent = TRUE)
  Years <- Hist@OM@Years[Hist@OM@Years <= Hist@OM@CurrentYear]
  expect_equal(as.numeric(.SBDepletion(Hist, min(Years))), c(0.6, 0.7, 0.8), tolerance = 1e-3)
  expect_equal(as.numeric(.SBDepletion(Hist, max(Years))), c(0.3, 0.4, 0.5), tolerance = 1e-3)
})

test_that("converted legacy Data@Dep uses the SB0 reference", {
  D <- new("Data")
  D@Year <- 2001:2010
  D@LHYear <- 2010
  D@Cat <- matrix(1:10, 1)
  D@Dep <- 0.4
  d <- Convert(D, silent = TRUE)
  expect_equal(d@LifeHistory@Depletion@Final, 0.4)
  expect_identical(d@LifeHistory@Depletion@Reference, "SB0")
})
