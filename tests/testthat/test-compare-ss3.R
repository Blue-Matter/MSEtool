test_that("CompareSS() simulates an om with optional calculations switched off", {
  skip_on_cran()
  SSDir <- file.path("../../..", "WCNPOSWO-2023", "Final Base-case") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  OM <- suppressMessages(ImportSS(SSDir, nSim = 2, pYear = 2,
                                  CatchUnits = "Biomass", silent = TRUE))
  FromOM   <- CompareSS(SSDir, OM, thresh = Inf, silent = TRUE)
  FromHist <- CompareSS(SSDir, Simulate(OM, silent = TRUE), thresh = Inf, silent = TRUE)
  for (nm in c('Recruits', 'Number', 'Biomass', 'Landings', 'Discards'))
    expect_equal(FromOM[[nm]]$df, FromHist[[nm]]$df, label = nm)
})
