skip_on_cran()

test_that("identical RecDevProj reduced to one sim still projects", {
  om <- SingleStockOM
  om@nSim <- 4
  om <- Populate(om)
  om@Stock[[1]]@SRR@RecDevProj[] <- 1
  hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
  expect_equal(nrow(hist@OM@Stock[[1]]@SRR@RecDevProj), 1)
  expect_equal(nrow(hist@OM@Stock[[1]]@SRR@RecDevHist), 4)

  mse <- Project(hist, MPs = 'CurrentEffort', silent = TRUE)
  expect_s4_class(mse, 'mse')
})
