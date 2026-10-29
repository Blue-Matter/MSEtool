test_that("runMSE() has no Do* arguments", {
  DoArgs <- c('DoDynamicUnfished', 'DoRefLandings', 'DoRefRemovals',
              'DoConditionObs', 'DoGenerateData', 'DoMSYRefs')
  expect_false(any(DoArgs %in% names(formals(runMSE))))
  expect_error(runMSE(SingleStockOM, Hist = TRUE, DoMSYRefs = FALSE), 'unused argument')
})

test_that("runMSE() returns hist and mse objects for an om", {
  skip_on_cran()
  Hist <- runMSE(SingleStockOM, Hist = TRUE, nSim = 3, silent = TRUE)
  expect_s4_class(Hist, 'hist')
  expect_equal(nSim(Hist@OM), 3)

  MSE <- runMSE(SingleStockOM, MPs = 'CurrentEffort', nSim = 3, silent = TRUE)
  expect_s4_class(MSE, 'mse')
  expect_equal(names(MSE@MPs), 'CurrentEffort')

  MSE2 <- runMSE(Hist, MPs = 'CurrentEffort', silent = TRUE)
  expect_s4_class(MSE2, 'mse')
  expect_equal(Landings(MSE2)$Value, Landings(MSE)$Value)
})
