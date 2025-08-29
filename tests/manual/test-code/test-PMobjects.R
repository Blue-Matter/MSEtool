testthat::context("Test PM Functions")

library(MSEtool)

PMs <- avail("PM")

MSE <- runMSE(silent=TRUE)

for (pm in PMs) {
  testthat::test_that(paste("Test ", pm, " works with 6 MPs"), {
    testthat::expect_error( get(pm)(MSE), NA)
    pmout <- get(pm)(MSE)
    testthat::expect_equal(length(pmout@Mean), MSE@nMPs)
  })
}

MSE2 <- Sub(MSE, MPs=1)

for (pm in PMs) {
  testthat::test_that(paste("Test ", pm, " works with 1 MP"), {
    testthat::expect_error( get(pm)(MSE2), NA)
    pmout <- get(pm)(MSE2)
    testthat::expect_equal(length(pmout@Mean), MSE2@nMPs)
  })
}


