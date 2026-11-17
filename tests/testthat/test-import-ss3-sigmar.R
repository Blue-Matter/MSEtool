MockSSSigmaRReplist <- function() {
  Yr <- 1990:2009
  set.seed(1)
  Value <- stats::rnorm(length(Yr), 0, 0.3)
  SE <- seq(0.4, 0.1, length.out = length(Yr))
  Era <- ifelse(Yr < 1995, "Early", "Main")
  biasadj <- ifelse(Yr < 1998, 0.5, ifelse(Yr > 2006, 0.6, 0.9))
  recruit <- data.frame(Yr = Yr, era = Era, exp_recr = 1000,
                        pred_recr = 1000 * exp(Value - 0.5 * biasadj * 0.6^2),
                        biasadjuster = biasadj)
  parameters <- data.frame(Label = paste0(Era, "_RecrDev_", Yr), Value = Value,
                           Parm_StDev = SE)
  parameters <- rbind(data.frame(Label = "SR_LN(R0)", Value = 7, Parm_StDev = 0.1),
                      parameters)
  list(recruit = recruit, parameters = parameters, sigma_R_in = 0.6,
       breakpoints_for_bias_adjustment_ramp = data.frame(first_yr_full = 1998,
                                                         last_yr_full = 2006),
       nseasons = 1, startyr = 1990, Spawn_timing_in_season = 0)
}

.TestSigmaRArgs <- function() {
  list(YearsList = list(YearsHist = 1990:2009, Seasons = 1),
       Ages = Ages(MaxAge = 5, MinAge = 0, Units = "year"))
}

.LocalSS2SRRMocks <- function(env = parent.frame()) {
  local_mocked_bindings(.GetSSR0 = function(...) array(1, 1, dimnames = list(Year = 1990)),
                        .GetSSSRRPars = function(...) matrix(0.8, dimnames = list(NULL, "h")),
                        .GetSSRecDevsEarly = function(...) array(1, 1, dimnames = list(Age = 1)),
                        .GetSSRecDevs = function(...) array(1, 1, dimnames = list(Year = 1990)),
                        .package = "MSEtool", .env = env)
}

.TestSS2SRR <- function(replist, st = 1) {
  a <- .TestSigmaRArgs()
  .SS2SRR(st, list(`1` = replist), a$YearsList, a$Ages, nSim = 1)
}

test_that("rec-dev SD is sqrt(var + mean SE^2) of the Main-era rec devs", {
  .LocalSS2SRRMocks()
  replist <- MockSSSigmaRReplist()
  a <- .TestSigmaRArgs()
  Main <- replist$parameters[grepl("^Main_RecrDev", replist$parameters$Label), ]

  Stats <- .GetSSRecDevStats(replist, a$YearsList, a$Ages)
  expect_equal(Stats$SD, sqrt(var(Main$Value) + mean(Main$Parm_StDev^2)))
  expect_equal(Stats$Method, "SE")

  plateau <- 1998:2006
  logdev <- log(replist$recruit$pred_recr / replist$recruit$exp_recr)[replist$recruit$Yr %in% plateau]
  expect_equal(Stats$AC, stats::acf(logdev, lag.max = 1, plot = FALSE)$acf[2])

  expect_no_warning(SRR <- .TestSS2SRR(replist))
  expect_equal(unname(SRR@SD), Stats$SD)
})

test_that("rec-dev SEs are approximated from the bias adjustment when Parm_StDev is NA", {
  .LocalSS2SRRMocks()
  replist <- MockSSSigmaRReplist()
  replist$parameters$Parm_StDev <- NA
  replist$inputs$dir <- "path/to/base-case"
  a <- .TestSigmaRArgs()
  Main <- replist$parameters[grepl("^Main_RecrDev", replist$parameters$Label), ]
  b <- replist$recruit$biasadjuster[replist$recruit$era == "Main"]

  Stats <- .GetSSRecDevStats(replist, a$YearsList, a$Ages)
  expect_equal(Stats$SD, sqrt(var(Main$Value) + mean(0.6^2 * (1 - b))))
  expect_equal(Stats$Method, "BiasAdj")

  expect_warning(SRR <- .TestSS2SRR(replist), "base-case")
  expect_equal(unname(SRR@SD), Stats$SD)
  expect_no_warning(.TestSS2SRR(replist, st = 2))
})

test_that("rec-dev SD is sigma_R_in when neither SEs nor bias adjustment are available", {
  .LocalSS2SRRMocks()
  replist <- MockSSSigmaRReplist()
  replist$parameters$Parm_StDev <- NA
  replist$recruit$biasadjuster <- 0
  replist$inputs$dir <- "path/to/base-case"
  a <- .TestSigmaRArgs()

  expect_equal(.GetSSRecDevStats(replist, a$YearsList, a$Ages)$Method, "Input")
  expect_warning(SRR <- .TestSS2SRR(replist), "sigma_R_in")
  expect_equal(unname(SRR@SD), 0.6)
})

test_that("ImportSS rec-dev SD matches SS3 sigma_R_info (NP swordfish)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "WCNPOSWO-2023", "Final Base-case") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  replist <- suppressMessages(ImportSSReport(SSDir, silent = TRUE))[[1]]
  YearsList <- .GetSSYears(replist, 2)
  Ages <- .SS2Ages(1, list(replist), YearsList)

  Info <- replist$sigma_R_info
  expect_equal(.GetSSRecDevStats(replist, YearsList, Ages)$SD,
               as.numeric(Info$alternative_sigma_R[Info$period == "Main"]),
               tolerance = 1e-6)
})
