test_that("CalcUnfished_Dynamic accepts an om object", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3

  u <- CalcUnfished_Dynamic(OM, silent = TRUE)
  expect_s4_class(u, "popdynamics")
  expect_true(all(is.finite(u@SBiomass)))
  expect_true(all(u@SBiomass > 0))
})
