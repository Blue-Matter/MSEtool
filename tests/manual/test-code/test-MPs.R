testthat::context("test MPs with testOM")

library(openMSE)
OM <- MSEtool::testOM
OM@nsim <- 6


MPs <- avail("MP", msg=FALSE)
output <- avail('Output', msg=FALSE)
input <- avail('Input', msg=FALSE)

mixed <- avail('Mixed', msg=FALSE)
reference <- avail('Reference', msg=FALSE)

testthat::test_that("avail MP returns correct length", {
  testthat::expect_equal(length(c(output, input, mixed, reference)), length(MPs))
})


for (mm in MPs) {
  testthat::test_that(paste("testOM works with ", mm), {
    testthat::expect_is(runMSE(OM, MPs=mm, silent=TRUE), "MSE")
  })
}
