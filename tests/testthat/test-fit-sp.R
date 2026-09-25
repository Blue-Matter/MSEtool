test_that("SPModel_cpp() matches the R reference implementation", {
  Sim  <- SimSPData(nYear = 30, nIndex = 3, CV = c(0.2, 0.3, 0.4), IndexStart = c(1, 8, 15),
                    Timing = c(0, 0.5, 1), IndexGap = 20:22)
  Prep <- .SPPrepData(Sim$Data, NULL, 'Survey', NULL, c(1, 2, 0.5), NULL,
                      c('Biomass', 'Number'), 'Removals', NULL)
  SD <- sqrt(log(1 + Prep$CV^2))
  set.seed(11)
  for (k in 1:8) {
    Pars <- c(stats::runif(1, 0.05, 0.6), stats::runif(1, 40, 250), stats::runif(1, 0.3, 1.2),
              c(0.6, 1, 1.0004, 1.5, 2, 3, 4, 2)[k])
    for (EstSD in c(TRUE, FALSE)) {
      Cpp <- SPModel_cpp(Pars, Prep$Catch, Prep$Index, SD, Prep$Timing, Prep$Weight,
                         rep(EstSD, 3), 4L, 5L, 3, 1e3, 1e-3, 0.05, TRUE)
      Ref <- SPReferenceModel(Pars, Prep$Catch, Prep$Index, SD, Prep$Timing, Prep$Weight,
                              rep(EstSD, 3))
      expect_equal(Cpp$NLL, Ref$NLL, tolerance = 1e-10)
      expect_equal(Cpp$B, Ref$B, tolerance = 1e-10)
      expect_equal(Cpp$F, Ref$F, tolerance = 1e-10)
    }
  }
})

test_that("SPProject_cpp() reproduces catch when projecting at the solved F", {
  Pars <- c(0.25, 120, 0.8, 1.7)
  K    <- 120 / (0.25 * 1.7^(1 / (1 - 1.7)))
  ByF  <- SPProject_cpp(Pars, 0.8 * K, c(0.1, 0.3, 0.5, 0.2), rep(1L, 4), 4L, 5L, 3, 1e-3)
  ByC  <- SPProject_cpp(Pars, 0.8 * K, ByF$Catch, rep(0L, 4), 4L, 5L, 3, 1e-3)
  expect_equal(ByC$F, ByF$F, tolerance = 1e-8)
  expect_equal(ByC$B, ByF$B, tolerance = 1e-8)
})

test_that("FitSP() recovers the parameters of simulated data", {
  Scenarios <- list(
    list(Sim = list(), Fit = list()),
    list(Sim = list(FPattern = 'contrast', IndexStart = c(10, 20), IndexGap = 25:28), Fit = list()),
    list(Sim = list(Dep = 0.5), Fit = list(EstDepletion = TRUE)),
    list(Sim = list(Shape = 1.3, FPattern = 'contrast'), Fit = list(EstShape = TRUE)),
    list(Sim = list(nIndex = 3, CV = c(0.2, 0.3, 0.5), IndexStart = c(1, 10, 20),
                    Timing = c(0, 0.5, 1)), Fit = list(IndexSD = 'data'))
  )
  for (Sc in Scenarios) {
    Res <- t(vapply(1:15, \(s) {
      Sim <- do.call(SimSPData, c(list(Seed = s), Sc$Sim))
      Fit <- do.call(FitSP, c(list(Data = Sim$Data), Sc$Fit))
      c(Fit$Converged, Fit$MSY / Sim$Truth$MSY - 1, Fit$FMSY / Sim$Truth$FMSY - 1,
        Fit$Terminal[['B_BMSY']] / Sim$Truth$B_BMSY - 1)
    }, numeric(4)))
    expect_gte(mean(Res[, 1]), 0.9)
    expect_lt(abs(stats::median(Res[, 2])), 0.05)
    expect_lt(abs(stats::median(Res[, 3])), 0.1)
    expect_lt(abs(stats::median(Res[, 4])), 0.1)
  }
})

test_that("FitSP() warm start reaches the cold-start optimum", {
  Sim  <- SimSPData(Seed = 3)
  Cold <- FitSP(Sim$Data)
  Warm <- FitSP(Sim$Data, Start = Cold$par * c(1.3, 0.8, 1, 1))
  expect_true(Warm$Converged)
  expect_equal(Warm$par, Cold$par, tolerance = 1e-3)
})

test_that("FitSP() standard errors, index selection, and input checks", {
  Sim <- SimSPData(Seed = 4, nIndex = 2)
  Fit <- FitSP(Sim$Data, Uncertainty = TRUE)
  expect_true(all(is.finite(Fit$SE)))
  expect_true(all(Fit$SE > 0))

  One <- FitSP(Sim$Data, Indices = 'Index2')
  expect_named(One$q, 'Index2')

  Sim$Data@Survey@Units <- c('Biomass', 'Recruitment')
  Fit <- FitSP(Sim$Data)
  expect_named(Fit$q, 'Index1')

  NoCV <- Sim$Data
  NoCV@Survey@CV <- NULL
  expect_error(FitSP(NoCV, IndexSD = 'data'), 'CV')

  Num <- Sim$Data
  Num@Landings@Units <- 'Number'
  expect_error(FitSP(Num), 'biomass')

  expect_error(FitSP(Sim$Data, Priors = list(r = c(1, 1))), 'Unknown')
  expect_error(FitSP(Sim$Data, Control = SPControl(Bounds = list(FMSY = c(1, 0.1)))), 'increasing')
})

test_that("FitSP() combines Survey and CPUE indices", {
  Sim <- SimSPData(Seed = 5, nIndex = 2, IndexStart = c(1, 12), Timing = c(0.5, 0))
  D <- Sim$Data
  D@CPUE <- IndicesData(Name = 'LonglineCPUE', Value = D@Survey@Value[, 2, drop = FALSE],
                        CV = D@Survey@CV[, 2, drop = FALSE], Units = 'Biomass', Timing = 0)
  D@Survey <- IndicesData(Name = 'Survey1', Value = D@Survey@Value[, 1, drop = FALSE],
                          CV = D@Survey@CV[, 1, drop = FALSE], Units = 'Biomass', Timing = 0.5)
  Both  <- FitSP(D, IndexSource = c('Survey', 'CPUE'))
  Named <- FitSP(D, IndexSource = c('Survey', 'CPUE'), Indices = c('LonglineCPUE', 'Survey1'))
  Orig  <- FitSP(Sim$Data)
  expect_named(Both$q, c('Survey1', 'LonglineCPUE'))
  expect_named(Named$q, c('LonglineCPUE', 'Survey1'))
  expect_equal(Both$par, Orig$par, tolerance = 1e-6)
  expect_equal(Named$par, Orig$par, tolerance = 1e-6)
})

test_that("FitSP() aggregates seasonal data to calendar years", {
  Sim <- SimSPData(Seed = 6, nIndex = 1, CV = 0.2, Timing = 0)
  D <- Sim$Data
  Years <- D@Years
  Seasons <- 4
  TS <- rep(Years, each = Seasons) + rep((0:3) / Seasons, length(Years))
  Catch <- matrix(rep(D@Landings@Value[, 1] / Seasons, each = Seasons), ncol = 1,
                  dimnames = list(Year = TS, Fleet = 'Fleet1'))
  Index <- matrix(NA_real_, length(TS), 1, dimnames = list(Year = TS, Index = 'Index1'))
  Index[seq(3, length(TS), by = Seasons), 1] <- D@Survey@Value[, 1]
  CV <- Index
  CV[!is.na(CV)] <- 0.2
  DS <- Data(Years = TS, YearLH = max(Years), Seasons = Seasons,
             Landings = CatchData(Name = 'Fleet1', Value = Catch, Units = 'Biomass'),
             Survey = IndicesData(Name = 'Index1', Value = Index, CV = CV, Units = 'Biomass',
                                  Timing = 0))
  Fit <- FitSP(DS, IndexSeasons = list(3))
  Ann <- FitSP(D)
  expect_equal(Fit$Prep$Timing, 0.5)
  expect_equal(unname(Fit$Prep$Catch), unname(D@Landings@Value[, 1]))
  expect_true(Fit$Converged)
  expect_equal(Fit$Years, Ann$Years)
})
