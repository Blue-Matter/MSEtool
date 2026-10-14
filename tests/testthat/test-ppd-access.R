test_that("PPD() keeps each MP's own Data@Misc", {
  skip_on_cran()

  OM <- SingleStockOM
  OM@nSim <- 2
  Hist <- Simulate(OM, silent = TRUE)

  MPa <- function(Data) {
    a <- Advice()
    a@Misc <- list(Cycles = c(Data@Misc$Cycles, Data@Misc$AdviceYear), x = 0.5)
    a
  }
  MPb <- function(Data) {
    a <- Advice()
    a@Misc <- list(x = 0.7)
    a
  }
  class(MPa) <- class(MPb) <- 'mp'
  attr(MPa, 'Interval') <- attr(MPb, 'Interval') <- 3

  MSE <- Project(Hist, MPs = list(MPa = MPa, MPb = MPb), silent = TRUE)
  ppd <- PPD(MSE)

  for (sim in 1:2) {
    expect_equal(ppd$MPa[[sim]][[1]]@Misc, MSE@PPD$MPa[[sim]][[1]]@Misc)
    expect_equal(ppd$MPb[[sim]][[1]]@Misc, list(x = 0.7))
  }
  expect_gt(length(ppd$MPa[[1]][[1]]@Misc$Cycles), 1)
  expect_equal(Years(ppd$MPb[[1]][[1]]), Years(ppd$MPa[[1]][[1]]))
})
