test_that("Sub subsets an MSE by MP and by simulation", {
  skip_on_cran()
  MSE <- .LegacyMSE()
  byMP <- Sub(MSE, MPs = "curEref")
  expect_identical(byMP@MPs, "curEref")
  expect_equal(byMP@nMPs, 1)
  expect_equal(byMP@Catch[, 1, ], MSE@Catch[, 2, ])

  bySim <- Sub(MSE, sims = 2:3)
  expect_equal(bySim@nsim, 2)
  expect_equal(bySim@SSB, MSE@SSB[2:3, , , drop = FALSE], ignore_attr = TRUE)
})

test_that("joinMSE reassembles simulations split with Sub", {
  skip_on_cran()
  MSE <- .LegacyMSE()
  joined <- joinMSE(list(Sub(MSE, sims = 1:2), Sub(MSE, sims = 3)))
  expect_equal(joined@nsim, MSE@nsim)
  for (nm in c("SSB", "Catch", "FM", "SB_SBMSY", "F_FMSY"))
    expect_equal(slot(joined, nm), slot(MSE, nm), ignore_attr = TRUE, info = nm)
})

test_that("addMPs combines MSEs that share an OM", {
  skip_on_cran()
  MSE <- .LegacyMSE()
  combined <- addMPs(list(Sub(MSE, MPs = "FMSYref"), Sub(MSE, MPs = c("curEref", "NFref"))))
  expect_equal(combined@nMPs, 3)
  expect_identical(combined@MPs, MSE@MPs)
  expect_equal(combined@Catch, MSE@Catch, ignore_attr = TRUE)
})

test_that("joinHist reassembles Hist objects from split simulations", {
  skip_on_cran()
  OM <- .LegacyOM(nsim = 4)
  OM@cpars <- list(M = c(0.25, 0.3, 0.35, 0.4))
  H1 <- Simulate(suppressMessages(SubCpars(OM, sims = 1:2)), silent = TRUE)
  H2 <- Simulate(suppressMessages(SubCpars(OM, sims = 3:4)), silent = TRUE)
  H <- joinHist(list(H1, H2))
  expect_s4_class(H, "Hist")
  expect_equal(H@SampPars$Stock$M, OM@cpars$M)
  expect_equal(dim(H@TSdata$SBiomass)[1], 4)
  expect_equal(H@TSdata$SBiomass[3:4, , ], H2@TSdata$SBiomass, ignore_attr = TRUE)
})

test_that("checkMSE and summary run on a legacy MSE", {
  skip_on_cran()
  MSE <- .LegacyMSE()
  expect_no_error(checkMSE(MSE))
  smry <- summary(MSE, silent = TRUE)
  expect_s3_class(smry, "data.frame")
  expect_equal(nrow(smry), MSE@nMPs)
  expect_identical(as.character(smry$MP), MSE@MPs)
})

test_that("probability PMs match direct calculation", {
  skip_on_cran()
  MSE <- .LegacyMSE()
  Prob <- function(x) apply(x, c(1, 2), mean)
  expect_equal(PNOF(MSE)@Prob, Prob(MSE@F_FMSY < 1), ignore_attr = TRUE)
  expect_equal(P10(MSE)@Prob, Prob(MSE@SB_SBMSY > 0.1), ignore_attr = TRUE)
  expect_equal(P50(MSE)@Prob, Prob(MSE@SB_SBMSY > 0.5), ignore_attr = TRUE)
  expect_equal(P100(MSE)@Prob, Prob(MSE@SB_SBMSY > 1), ignore_attr = TRUE)
  expect_equal(PGK(MSE)@Prob, Prob(MSE@SB_SBMSY > 1 & MSE@F_FMSY < 1), ignore_attr = TRUE)

  pm <- PNOF(MSE)
  expect_s4_class(pm, "PMobj")
  expect_equal(pm@Mean, colMeans(pm@Prob), ignore_attr = TRUE)
  expect_identical(pm@MPs, MSE@MPs)
})

test_that("yield and variability PMs are bounded and order the reference MPs", {
  skip_on_cran()
  MSE <- .LegacyMSE()
  for (pm in c("LTY", "STY", "Yield", "AAVY", "AAVE")) {
    out <- get(pm)(MSE)
    expect_length(out@Mean, MSE@nMPs)
    expect_true(all(out@Mean >= 0 & out@Mean <= 1), info = pm)
  }
  # NFref takes no catch
  expect_equal(Yield(MSE)@Mean[[3]], 0)
  expect_equal(LTY(MSE)@Mean[[3]], 0)
  # Yield statistic is kept by simulation, MP and projection year
  yld <- Yield(MSE)
  expect_equal(dim(yld@Stat), c(MSE@nsim, MSE@nMPs, MSE@proyears))
})

test_that("PMs work on a single-MP MSE and a year subset", {
  skip_on_cran()
  MSE <- Sub(.LegacyMSE(), MPs = "FMSYref")
  expect_length(PNOF(MSE)@Mean, 1)
  expect_length(AAVY(MSE)@Mean, 1)
  pm <- P50(.LegacyMSE(), Yrs = c(1, 5))
  expect_equal(pm@Prob, apply(.LegacyMSE()@SB_SBMSY[, , 1:5] > 0.5, c(1, 2), mean), ignore_attr = TRUE)
})
