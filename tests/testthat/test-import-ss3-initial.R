MockSSInitialReplist <- function() {
  AgeCols <- as.character(0:5)
  N0 <- 1000 * exp(-0.3 * 0:5)
  N1 <- N0 * c(0.95, 0.9, 0.7, 0.5, 0.4, 0.35)
  natage <- data.frame(Sex = 1, Yr = c(1998, 2000), Seas = 1, `Beg/Mid` = "B",
                       Era = c("VIRG", "TIME"), check.names = FALSE)
  natage <- cbind(natage, rbind(N0, N1) |> `colnames<-`(AgeCols))
  recruit <- data.frame(Yr = c(1997, 1998, 1999, 2000),
                        exp_recr = c(900, 900, 900, 1000),
                        pred_recr = c(990, 810, 945, 1100),
                        era = c("Init_age", "Init_age", "Init_age", "Main"))
  Dynamic_Bzero <- data.frame(Yr = 1998:2000, Era = c("VIRG", "INIT", "TIME"),
                              SSB = c(100, 40, 45), SSB_nofishing = c(100, 90, 91))
  list(natage = natage, recruit = recruit, Dynamic_Bzero = Dynamic_Bzero,
       nseasons = 1, startyr = 2000)
}

test_that("SS3 initial numbers-at-age split into rec devs and InitialAtAge", {
  replist <- MockSSInitialReplist()
  YearsList <- list(YearsHist = 2000:2010)
  Stock <- Stock()
  Stock@Ages <- Ages(MaxAge = 5, MinAge = 0, Units = "year")

  RecDevInit <- .GetSSRecDevsEarly(replist, YearsList, Stock@Ages)
  expect_equal(as.numeric(dimnames(RecDevInit)$Age), 1:5)
  expect_equal(as.numeric(RecDevInit), 0.9 * c(945 / 900, 810 / 900, 990 / 900, 1, 1))

  Stock@SRR@RecDevInit <- List2Array(list(RecDevInit), "Sim", pos = 1)
  InitialAtAge <- .SS2InitialAtAge(1, list(replist), YearsList, Stock)
  expect_equal(dimnames(InitialAtAge), dimnames(Stock@SRR@RecDevInit))
  expect_equal(as.numeric(InitialAtAge * Stock@SRR@RecDevInit),
               c(0.9, 0.7, 0.5, 0.4, 0.35))
})

test_that("SS3 rec devs default to 1 without initial-age deviations", {
  replist <- MockSSInitialReplist()
  replist$recruit <- replist$recruit[replist$recruit$era != "Init_age", ]
  replist$Dynamic_Bzero <- NULL
  RecDevInit <- .GetSSRecDevsEarly(replist, list(YearsHist = 2000:2010),
                                   Ages(MaxAge = 5, MinAge = 0, Units = "year"))
  expect_equal(as.numeric(RecDevInit), rep(1, 5))
})

test_that("ImportSS dynamic SB/SB_F=0 matches SS3 in the first year (NP swordfish)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "WCNPOSWO-2023", "Final Base-case") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))

  OM <- suppressMessages(ImportSS(SSDir, nSim = 2, pYear = 2,
                                  CatchUnits = "Biomass", silent = TRUE))
  Hist <- Simulate(OM, silent = TRUE)
  replist <- suppressMessages(r4ss::SS_output(SSDir, verbose = FALSE, printstats = FALSE,
                                              covar = FALSE, forecast = FALSE))

  for (st in 1:2) {
    Stock <- Hist@OM@Stock[[st]]
    Ratio <- .GetSSInitNumberRatio(replist, list(YearsHist = Years(Hist, "H")), Stock@Ages, st)
    expect_equal(as.numeric(Stock@SRR@RecDevInit[1, ] * Stock@Depletion@Misc$InitialAtAge[1, ]),
                 as.numeric(Ratio))
    Expected <- abind::adrop(Hist@Unfished@Equilibrium@Number[[st]][, , 1, , drop = FALSE], 3)[1, -1, ] *
      as.numeric(Ratio)
    expect_equal(unname(Hist@Number[[st]][1, -1, 1, ]), unname(Expected))
  }

  Dyn <- replist$Dynamic_Bzero
  Dyn <- Dyn[Dyn$Era == "TIME", ]
  SS3Ratio <- Dyn$SSB[1] / Dyn$SSB_nofishing[1]
  OMRatio <- SB_SB0(Hist, type = "Dynamic", df = FALSE)[1, 1, 1]
  expect_equal(OMRatio, SS3Ratio, tolerance = 0.02)
})
