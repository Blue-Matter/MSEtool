test_that("legacy Data indices convert to Survey indices with their units and type", {
  D <- new("Data")
  n <- 10
  D@Year <- 2001:2010
  D@LHYear <- 2010
  D@Cat <- matrix(1:n, 1)
  D@Ind <- matrix(stats::runif(n), 1)
  D@CV_Ind <- matrix(0.2, 1, n)
  D@AddInd <- array(stats::runif(2 * n), c(1, 2, n))
  D@CV_AddInd <- array(0.3, c(1, 2, n))
  D@AddIndType <- c(2, 1)
  D@AddIunits <- c(1, 0)

  d <- Convert(D, silent = TRUE)
  expect_null(d@CPUE@Value)
  expect_equal(d@Survey@Name, c("Ind", "AddInd1", "AddInd2"))
  expect_equal(d@Survey@Units, c("Biomass", "Biomass", "Number"))
  expect_equal(unname(d@Survey@Misc$IndexType), c("total", "spawning", "total"))
  expect_equal(unname(d@Survey@Value[, "AddInd2"]), D@AddInd[1, 2, ])
  expect_identical(.ResolveIndexSource(d, "auto"), "Survey")

  L <- Convert(d, silent = TRUE)
  expect_equal(as.numeric(L@Ind), as.numeric(D@Ind))
})
