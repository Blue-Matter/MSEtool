MockSSFecundityReplist <- function(nSeas = 1) {
  Ages <- 0:3
  eg <- expand.grid(Age = Ages, Seas = seq_len(nSeas))
  endgrowth <- data.frame(Seas = eg$Seas, Morph = 1, Sex = 1,
                          Age_Beg = eg$Age + (eg$Seas - 1) / nSeas,
                          Wt_Beg = c(0.1, 2, 6, 10)[eg$Age + 1],
                          Len_Mat = c(0, 0.1, 0.6, 0.95)[eg$Age + 1],
                          Age_Mat = -1,
                          `Mat*Fecund` = c(0, 0.3, 4, 9.6)[eg$Age + 1],
                          Mat_F_wtatage = c(0, 0.3, 4, 9.6)[eg$Age + 1],
                          check.names = FALSE)
  list(endgrowth = endgrowth, SpawnOutputUnits = "biomass")
}

test_that("SS3 fecundity uses Mat*Fecund when Age_Mat is a -1 placeholder", {
  replist <- MockSSFecundityReplist()
  Ages <- Ages(MaxAge = 3, MinAge = 0, Units = "year")
  Fec <- .GetSSFecundity(1, replist, list(YearsHist = 2000:2010), Ages)
  expect_equal(as.numeric(Fec), c(0, 0.3, 4, 9.6))

  replist$endgrowth[["Mat*Fecund"]] <- NULL
  replist$endgrowth$Mat_F_wtatage <- NULL
  Fec <- .GetSSFecundity(1, replist, list(YearsHist = 2000:2010), Ages)
  expect_true(all(Fec >= 0))
  expect_equal(as.numeric(Fec), c(0.1, 2, 6, 10) * c(0, 0.1, 0.6, 0.95))
})

test_that("seasonal SS3 fecundity uses Mat*Fecund when Age_Mat is a -1 placeholder", {
  replist <- MockSSFecundityReplist(nSeas = 4)
  Ages <- Ages(MaxAge = 3, MinAge = 0, Units = "year")
  Fec <- .GetSSFecunditySeasonal(1, replist, list(YearsHist = 2000:2010), Ages)
  expect_true(all(Fec >= 0))
  expect_equal(as.numeric(Fec), replist$endgrowth[["Mat*Fecund"]][order(replist$endgrowth$Age_Beg)])
})

.TestSSSP0 <- function(SSDir) {
  replist <- suppressMessages(r4ss::SS_output(SSDir, verbose = FALSE, printstats = FALSE,
                                              covar = FALSE, forecast = FALSE))
  OM <- suppressMessages(ImportSS(replist, nSim = 2, pYear = 2,
                                  CatchUnits = "Biomass", silent = TRUE))
  Hist <- Simulate(OM, silent = TRUE)
  SP0 <- SP0(Hist)
  list(OM = max(SP0[1, 1, seq_len(replist$nseasons)]),
       SS3 = replist$derived_quants["SSB_Virgin", "Value"],
       Fecundity = Hist@OM@Stock[[1]]@Fecundity@MeanAtAge)
}

test_that("ImportSS SP0 matches SSB_Virgin with Age_Mat = -1 (albMSE albacore, seasonal)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "albMSE", "boot", "data", "base") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  Out <- .TestSSSP0(SSDir)
  expect_true(all(Out$Fecundity >= 0))
  expect_equal(Out$OM, Out$SS3, tolerance = 1e-4)
})

test_that("ImportSS SP0 matches SSB_Virgin (ICCAT South Atlantic albacore, annual)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "iccatSALB", "data-raw", "assessment", "S05") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  Out <- .TestSSSP0(SSDir)
  expect_equal(Out$OM, Out$SS3, tolerance = 1e-4)
})
