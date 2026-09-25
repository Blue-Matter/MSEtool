test_that("SurplusProduction() returns a TAC from the fitted model on simulated data", {
  Sim <- SimSPData(Seed = 7)
  A <- SurplusProduction(Sim$Data)
  expect_s4_class(A, 'advice')
  S <- A@Misc$SurplusProduction$Summary
  expect_true(S$Converged)
  expect_equal(S$AdviceYear, max(Sim$Data@Years) + 1)

  Fit <- FitSP(Sim$Data)
  B   <- Fit$Terminal[['B_BMSY']]
  FHCR <- Fit$FMSY * min(1, B / 0.5)
  Expected <- .ProjectSP(Fit, FHCR, 1L)$Catch
  Prev <- LastTAC(Sim$Data)
  Expected <- ConstrainTAC(Prev, Expected / Prev, c(0.01, 0.5), c(0.01, 0.5),
                           c(0, 100 * max(Fit$Prep$CatchAll)))
  expect_equal(A@TAC, Expected, tolerance = 1e-6)

  Double <- SurplusProduction(Sim$Data, tunepar = 1.2, DeltaUp = c(0, 10), DeltaDown = c(0, 1))
  Single <- SurplusProduction(Sim$Data, DeltaUp = c(0, 10), DeltaDown = c(0, 1))
  expect_gt(Double@TAC, Single@TAC)
  expect_equal(Double@Misc$SurplusProduction$Summary$FAdvice,
               1.2 * Single@Misc$SurplusProduction$Summary$FAdvice, tolerance = 1e-8)
})

test_that("SurplusProduction() advice options", {
  Sim <- SimSPData(Seed = 8)
  D <- Sim$Data
  D@Discards <- CatchData(Name = 'Fleet1', Value = D@Landings@Value * 0.25, Units = 'Biomass')
  Free <- list(DeltaUp = c(0, 10), DeltaDown = c(0, 1))

  Rem  <- do.call(SurplusProduction, c(list(D), Free))
  Land <- do.call(SurplusProduction, c(list(D, TACType = 'Landings'), Free))
  expect_identical(Land@TACType, 'Landings')
  expect_equal(Land@TAC, Rem@TAC * 0.8, tolerance = 1e-6)

  Lag <- do.call(SurplusProduction, c(list(D, AdviceYear = max(D@Years) + 3, Interval = 2), Free))
  expect_equal(Lag@Misc$SurplusProduction$Summary$AdviceYear, max(D@Years) + 3)
  expect_false(isTRUE(all.equal(Lag@TAC, Rem@TAC)))

  Eff <- do.call(SurplusProduction, c(list(D, AdviceType = 'Effort'), Free))
  expect_null(Eff@TAC)
  expect_identical(Eff@EffType, 'Rel')
  S <- Eff@Misc$SurplusProduction$Summary
  Fit <- FitSP(D)
  expect_equal(Eff@Effort, S$FAdvice / unname(utils::tail(Fit$F, 1)), tolerance = 1e-6)

  Alloc <- do.call(SurplusProduction, c(list(D, Allocation = 1), Free))
  expect_equal(Alloc@TAC, Rem@TAC)

  Frac <- do.call(SurplusProduction, c(list(D, FractileB = 0.3, FractileF = 0.3), Free))
  expect_lt(Frac@Misc$SurplusProduction$Summary$FAdvice, Rem@Misc$SurplusProduction$Summary$FAdvice)

  Full <- SurplusProduction(D, Diagnostics = 'full')
  expect_s3_class(Full@Misc$SurplusProduction$Fit, 'spfit')
  None <- SurplusProduction(D, Diagnostics = 'none')
  expect_null(None@Misc$SurplusProduction$Summary)
  expect_named(None@Misc$SurplusProduction$par, c('FMSY', 'MSY', 'Depletion', 'Shape'))

  expect_error(SurplusProduction(D, tunepar = 0), 'tunepar')
  expect_error(SurplusProduction(D, FractileB = 1.5), 'FractileB')
})

test_that("SurplusProduction() falls back when the model cannot be fitted", {
  Sim <- SimSPData(Seed = 9)
  D <- Sim$Data
  D@Advice@TAC <- array(80, dim = c(1, 1), dimnames = list(Year = max(D@Years), NULL))

  Hold <- SurplusProduction(D, MinIndexYears = 100)
  expect_equal(Hold@TAC, 80)
  expect_true(Hold@Misc$SurplusProduction$Summary$Fallback)
  expect_match(Hold@Log$warning[[1]]$message, 'MinIndexYears')

  Trend <- SurplusProduction(D, MinIndexYears = 100, OnFail = 'trend', DeltaUp = c(0, 10),
                             DeltaDown = c(0, 1))
  Index <- D@Survey@Value
  n <- nrow(Index)
  Ratio <- colMeans(Index[(n - 1):n, , drop = FALSE], na.rm = TRUE) /
    colMeans(Index[(n - 4):(n - 2), , drop = FALSE], na.rm = TRUE)
  expect_equal(Trend@TAC, 80 * exp(mean(log(Ratio))), tolerance = 1e-8)

  NoIndex <- D
  NoIndex@Survey <- IndicesData()
  expect_equal(SurplusProduction(NoIndex)@TAC, 80)
})

test_that("SurplusProduction() warm-starts from the previous management cycle", {
  Sim <- SimSPData(Seed = 10)
  First <- SurplusProduction(Sim$Data)
  D <- Sim$Data
  D@Misc <- First@Misc
  Second <- SurplusProduction(D)
  expect_equal(Second@Misc$SurplusProduction$par, First@Misc$SurplusProduction$par,
               tolerance = 1e-3)
})

test_that("SurplusProduction() in closed-loop projections", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 4
  Hist <- Simulate(OM, silent = TRUE)
  MSE <- Project(Hist, MPs = list(SP = SurplusProduction,
                                  SPE = SetMPArgs(SurplusProduction, AdviceType = 'Effort')),
                 silent = TRUE)
  expect_s4_class(MSE, 'mse')
  expect_length(MSE@Log$error, 0)

  E <- SPEstimates(MSE)
  expect_setequal(unique(E$MP), c('SP', 'SPE'))
  expect_equal(nrow(E[E$MP == 'SP', ]), OM@nSim * length(Years(OM, 'Projection')))
  expect_gt(mean(E$Converged), 0.8)
  expect_true(all(c('OM_B_BMSY', 'OM_F_FMSY') %in% names(E)))
  expect_gt(stats::cor(log(E$B_BMSY), log(E$OM_B_BMSY), use = 'complete.obs'), 0.3)

  FF <- F_FMSY(MSE, df = TRUE)
  FF <- FF[FF$Period == 'Projection' & FF$MP == 'SP', ]
  expect_true(all(is.finite(FF$Value)))

  Lagged <- SingleStockOM
  Lagged@nSim <- 3
  Lagged@DataLag <- 1
  Lagged@Interval <- 3
  MSE <- Project(Simulate(Lagged, silent = TRUE), MPs = 'SurplusProduction', silent = TRUE)
  E <- SPEstimates(MSE)
  expect_true(all(E$AdviceYear - E$LastDataYear == 2))
  expect_equal(length(unique(E$AdviceYear)), length(seq(1, length(Years(Lagged, 'Projection')), by = 3)))
})

test_that("SurplusProduction() on a seasonal OM with a CPUE index", {
  skip_on_cran()
  OM <- SeasonalSpatialOM
  OM@nSim <- 2
  Hist <- Simulate(OM, silent = TRUE)
  MSE <- Project(Hist, MPs = list(SP = SetMPArgs(SurplusProduction, IndexSource = 'CPUE')),
                 silent = TRUE)
  expect_length(MSE@Log$error, 0)
  E <- SPEstimates(MSE)
  expect_true(all(E$AdviceYear == floor(E$AdviceYear)))
  expect_gt(mean(E$Converged), 0.8)
})
