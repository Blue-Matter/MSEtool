context("Test MSE_functions")

library(openMSE)
MPs <- avail("MP", 'DLMtool')

Hist <- runMSE(testOM, Hist = TRUE, silent = TRUE)
MSE <- runMSE(testOM, MPs=MPs, checkMPs = FALSE)

testthat::test_that("joinHist", {
  testthat::expect_error(joinHist(list(Hist, Hist)), NA)
})

testthat::test_that("Converge", {
  testthat::expect_error(Converge(MSE), NA)
})


testthat::test_that("checkMSE", {
  testthat::expect_error(checkMSE(MSE), NA)
})


testthat::test_that("Sub by MP", {
  MPs1 <- MSE@MPs[1:3]
  MPs2 <- MSE@MPs[4:MSE@nMPs]
  testthat::expect_error(t1 <- Sub(MSE, MPs=MPs1), NA)
  testthat::expect_error(t2 <- Sub(MSE, MPs=MPs2), NA)
})

testthat::test_that("Sub by sim", {
  nsim <- MSE@nsim
  sims1 <- 1:ceiling((nsim/2))
  sims2 <- (max(sims1)+1):nsim
  testthat::expect_error(t1 <<- Sub(MSE, sim=sims1), NA)
  testthat::expect_error(t2 <<- Sub(MSE, sim=sims2), NA)
})

testthat::test_that("joinMSE", {
  testthat::expect_error(newMSE <<- joinMSE(list(t1, t2)), NA)
})

testthat::test_that("addMPs", {
  testthat::expect_error(addMPs(list(MSE, MSE)), NA)
})

testthat::test_that("joinMSE returns same object", {
  testthat::expect_true(all(summary(newMSE, silent=TRUE) == summary(MSE, silent=TRUE)))
})
