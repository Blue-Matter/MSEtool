test_that("SpictControl() validates its arguments", {
  Ctl <- SpictControl(dteuler = 0.5, Inp = list(msytype = 'd'))
  expect_s3_class(Ctl, 'spictcontrol')
  expect_error(SpictControl(dteuler = 0), 'positive')
  expect_error(FitSP(SimSPData(Seed = 1)$Data, Control = SpictControl()), 'SPControl')
})

test_that(".SpictInp() maps FitSP settings to spict inputs", {
  Sim  <- SimSPData(Seed = 2, nIndex = 2, IndexStart = c(1, 10), Timing = c(0.5, 0))
  Prep <- .SPPrepData(Sim$Data, NULL, 'Survey', NULL, c(1, 4), NULL, c('Biomass', 'Number'),
                      'Removals', NULL)
  inp <- .SpictInp(Prep, Shape = 1.5, EstShape = FALSE, Depletion = 0.8, EstDepletion = FALSE,
                   IndexSD = 'estimate', Priors = list(FMSY = c(0.2, 0.5)),
                   Control = SpictControl(Priors = list(logalpha = c(0, 1, 1))), Start = NULL)
  expect_equal(inp$obsC, Prep$Catch)
  expect_length(inp$obsI, 2)
  expect_equal(inp$timeI[[1]][1], Prep$Years[1] + 0.5)
  expect_equal(inp$timeI[[2]][1], Prep$Years[10])
  expect_equal(unique(inp$stdevfacI[[2]]) / unique(inp$stdevfacI[[1]]), sqrt(Prep$Weight[1] / Prep$Weight[2]))
  expect_equal(inp$ini$logn, log(1.5))
  expect_equal(inp$phases$logn, -1)
  expect_equal(inp$priors$logbkfrac[1], log(0.8))
  expect_equal(inp$priors$logr[1], log(0.2 * 1.5))
  expect_equal(inp$priors$logalpha, c(0, 1, 1))
  expect_equal(inp$dteuler, 1)

  Est <- .SpictInp(Prep, Shape = 2, EstShape = TRUE, Depletion = 1, EstDepletion = TRUE,
                   IndexSD = 'data', Priors = list(Shape = c(2, 0.5)), Control = SpictControl(),
                   Start = c(FMSY = 0.2, MSY = 100))
  expect_equal(Est$priors$logn[3], 1)
  expect_null(Est$priors$logbkfrac)
  expect_equal(Est$ini$logm, log(100))
  expect_equal(Est$ini$logK, log(100 / (0.2 * 0.5)))
})

test_that("FitSP(Model = 'spict') agrees with the internal model", {
  skip_if_not_installed('spict')
  for (s in 1:3) {
    Sim <- SimSPData(Seed = s)
    Sp  <- FitSP(Sim$Data, Model = 'spict')
    Int <- FitSP(Sim$Data)
    expect_true(Sp$Converged)
    expect_equal(Sp$Shape, 2)
    expect_equal(Sp$Depletion, 1, tolerance = 0.05)
    expect_equal(Sp$MSY, Int$MSY, tolerance = 0.1)
    expect_equal(Sp$Terminal[['B_BMSY']], Int$Terminal[['B_BMSY']], tolerance = 0.15)
    expect_named(Sp$q, names(Int$q))
  }

  Sim <- SimSPData(Seed = 4)
  Unc <- FitSP(Sim$Data, Model = 'spict', Uncertainty = TRUE)
  expect_true(all(is.finite(Unc$SE)))
  Shape <- FitSP(Sim$Data, Model = 'spict', EstShape = TRUE)
  expect_true('Shape' %in% Shape$Est)
  expect_false(isTRUE(all.equal(Shape$Shape, 2)))
})

test_that("SurplusProduction(Model = 'spict') in a closed-loop projection", {
  skip_if_not_installed('spict')
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 2
  Hist <- Simulate(OM, silent = TRUE)
  MSE <- Project(Hist, MPs = list(SPspict = SetMPArgs(SurplusProduction, Model = 'spict')),
                 silent = TRUE)
  expect_length(MSE@Log$error, 0)
  E <- SPEstimates(MSE)
  expect_gt(mean(E$Converged), 0.8)
})
