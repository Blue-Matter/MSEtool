testthat::context("slot descriptions")

library(MSEtool)
classes <- c("Stock", "Fleet", "Obs", "Imp", "Data")

for (cl in classes) {
  Desc <- get(paste0(cl, "Description"))
  slots <- slotNames(cl)
  testthat::test_that(paste("slots are in correct order for ", cl), {
    testthat::expect_true(all(slots == Desc[,1]))
  })
}
