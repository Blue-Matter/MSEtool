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
  skip_if_not_installed("r4ss", "1.52.1")

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

test_that("ImportSS first-year recruitment matches SS3 pred_recr when starting fished (SA albacore, annual)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "iccatSALB", "data-raw", "assessment", "S05") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  OM <- suppressMessages(ImportSS(SSDir, nSim = 2, pYear = 2, silent = TRUE))
  Hist <- Simulate(OM, control = SimControl(ConditionObs = FALSE, GenerateData = FALSE,
                                            MSYRefs = FALSE, RefPoints = FALSE, MGT = FALSE),
                   silent = TRUE)
  replist <- suppressMessages(r4ss::SS_output(SSDir, verbose = FALSE, printstats = FALSE,
                                              covar = FALSE, forecast = FALSE))
  rec1 <- replist$recruit[replist$recruit$Yr == min(Years(Hist, "H")), ]
  # starts from a fished state, so SS3 expected recruitment differs from R0
  expect_false(isTRUE(all.equal(rec1$exp_recr, OM@Stock[[1]]@SRR@R0[1, 1])))

  OMRec1 <- sum(Hist@Number[[1]][1, 1, 1, ])
  expect_equal(OMRec1, rec1$pred_recr, tolerance = 1e-4)
})

test_that("first-year births after the recruitment lag stay relative to SS3 exp_recr (seasonal)", {
  Mock <- function(birthseas, Spawn_month = 1) list(nseasons = 4, birthseas = birthseas, Spawn_month = Spawn_month)
  expect_true(.SSFirstBirthsFromR0(list(nseasons = 1)))
  expect_true(.SSFirstBirthsFromR0(Mock(1)))
  # default lag birthseas - 1: births come from the SRR
  expect_false(.SSFirstBirthsFromR0(Mock(3)))
  # spawning in season 1, births in season 3: lag 2 = birthseas - 1
  expect_null(.GetSSSpawnLag(Mock(3, Spawn_month = 1)))
  # spawning in season 2 (month 5), births in season 3: lag 1
  expect_equal(.GetSSSpawnLag(Mock(c(2, 3), Spawn_month = 5)), 1)
  expect_false(.SSFirstBirthsFromR0(Mock(c(2, 3), Spawn_month = 5)))
})

test_that("ImportSS first-year recruitment matches SS3 when births follow the recruitment lag (NP swordfish, seasonal)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "WCNPOSWO-2023", "Final Base-case") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  OM <- suppressMessages(ImportSS(SSDir, nSim = 2, pYear = 2, silent = TRUE))
  Hist <- Simulate(OM, control = SimControl(ConditionObs = FALSE, GenerateData = FALSE,
                                            MSYRefs = FALSE, RefPoints = FALSE, MGT = FALSE,
                                            DynamicUnfished = FALSE),
                   silent = TRUE)
  replist <- suppressMessages(r4ss::SS_output(SSDir, verbose = FALSE, printstats = FALSE,
                                              covar = FALSE, forecast = FALSE))
  birthseas <- max(replist$birthseas)
  FirstYear <- min(floor(Years(Hist, "H")))
  SS3Rec1 <- replist$natage[replist$natage$Yr == FirstYear & replist$natage$Seas == birthseas &
                              replist$natage$`Beg/Mid` == "B" & replist$natage$Sex == 1, "0"]
  OMRec1 <- sum(Hist@Number[[1]][1, 1, birthseas, ])
  expect_equal(OMRec1, SS3Rec1, tolerance = 0.01)
})

test_that("ImportSS imports SS3 hermaphroditism and matches SS3 by sex (Gag, protogynous)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "NOAA Workshop 2027", "SS3 Assessments", "Gag Gulf") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")

  OM <- suppressMessages(ImportSS(SSDir, nSim = 2, pYear = 2, silent = TRUE))
  expect_length(OM@Herm, 1)
  expect_equal(OM@Herm[[1]]@From, "Female")
  expect_equal(OM@Herm[[1]]@To, "Male")
  # males in spawning biomass (Hermaphro_maleSPB = 1): female fecundity, blended SPFrom
  expect_equal(OM@Stock[[2]]@Fecundity@MeanAtAge, OM@Stock[[1]]@Fecundity@MeanAtAge)
  expect_equal(OM@Stock[[1]]@SRR@SPFrom, c(Female = 0.5, Male = 0.5))

  Hist <- Simulate(OM, silent = TRUE)
  Out <- suppressMessages(CompareSS(SSDir, Hist, thresh = Inf, silent = TRUE))
  # trailing recruitment deviations (2018-2019) are generated, not imported
  for (nm in c("Number", "Biomass", "Landings")) {
    MARE <- Out[[nm]]$MARE
    expect_lt(max(MARE$MARE[MARE$Year <= 2017], na.rm = TRUE), 0.1)
  }

  # MSY reference points are finite and close to SS3
  R <- Hist@Reference@MSY
  expect_lt(R@FMSY[1], OM@maxF)
  replist <- suppressMessages(r4ss::SS_output(SSDir, verbose = FALSE, printstats = FALSE,
                                              covar = FALSE, forecast = FALSE))
  dq <- replist$derived_quants
  expect_equal(sum(R@SBMSY[1, , 1]), dq$Value[dq$Label == "SSB_MSY"], tolerance = 0.05)
  expect_equal(R@SPRMSY[1, 1, 1], dq$Value[dq$Label == "SPR_MSY"], tolerance = 0.05)
})

test_that("Herm per-recruit SPR includes production after sex change when recruiting from both sexes", {
  OM <- HermOM
  OM@nSim <- 1
  StockNms <- names(OM@Stock)
  OM@Stock[[1]]@SRR@SPFrom <- stats::setNames(c(0.5, 0.5), StockNms)
  OM@Stock[[2]]@SRR@SPFrom <- NULL
  OM@Stock[[2]]@SRR@ShareSPFrom <- FALSE
  OM@Stock[[2]]@SRR@R0 <- 1e-6
  OM@Stock[[2]]@SRR@Pars <- OM@Stock[[1]]@SRR@Pars
  yr <- max(Years(OM, "Hist"))
  pr <- suppressWarnings(CalcPerRecruit(OM, apicalF = c(1e-10, 0.2), Years = yr))
  SPR <- .Aperm(pr@SPR, c("Sim", "Stock", "Year", "F"))
  expect_equal(SPR[1, 1, 1, 1], 1, tolerance = 1e-6)
  expect_gt(SPR[1, 1, 1, 2], 0)
  expect_lt(SPR[1, 1, 1, 2], 1)

  # males (nearly all from sex change) do not trigger the collapse shortcut
  eq <- suppressWarnings(CalcEquilibrium(OM, apicalF = c(0, 0.1), Years = yr))
  expect_true(all(eq@Recruits[, 1, , ] > 0))
})
