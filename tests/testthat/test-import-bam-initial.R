test_that(".GetBAMInitRecDevs maps log.Nage.dev to age classes", {
  BAMdata <- list(parm.avec = list(age = 1:4, log.Nage.dev = c(NA, log(0.5), 0, log(2))))
  expect_equal(.GetBAMInitRecDevs(BAMdata, 1:5), c(1, 0.5, 1, 2, 1))
  expect_equal(.GetBAMInitRecDevs(list(), 1:3), c(1, 1, 1))
})

test_that("ImportBAM splits initial numbers-at-age into rec devs and InitialAtAge", {
  skip_on_cran()
  skip_if_not_installed("bamExtras")
  BAMdata <- GetBAMOutput("RedGrouper")

  OM <- suppressMessages(ImportBAM("RedGrouper", nSim = 2, pYear = 3, silent = TRUE))
  Stock <- OM@Stock[[1]]
  Ages <- Stock@Ages@Classes
  expect_equal(as.numeric(Stock@SRR@RecDevInit[1, ]),
               .GetBAMInitRecDevs(BAMdata, Ages)[-1])
  expect_false(is.null(Stock@Depletion@Misc$InitialAtAge))

  Hist <- Simulate(OM, silent = TRUE)
  N1 <- apply(Hist@Number[[1]][1, , 1, , drop = FALSE], 2, sum)
  expect_equal(unname(N1[-1]), unname(BAMdata$N.age[1, -1]), tolerance = 1e-6)

  Init <- .OM2Hist(OM, silent = TRUE)
  Init@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Init@OM, silent = TRUE)
  Unfished <- .CalcDynamicInitial(Init, Unfished = TRUE)
  Equil <- apply(Init@Unfished@Equilibrium@Number[[1]][1, , 1, , drop = FALSE], 2, sum)
  expect_equal(unname(apply(Unfished@Number[[1]][1, -1, 1, , drop = FALSE], 2, sum)),
               unname(Equil[-1] * Stock@SRR@RecDevInit[1, ]))
})

MockBAMUnitsData <- function() {
  list(t.series = data.frame(year = 2000:2002,
                             L.cHL.pr = c(10, 20, 30), L.rGN.pr = c(1, 2, 3),
                             L.rHB.pr = c(2, 4, 6), D.rGN.pr = c(5, 6, 7)),
       LD.pr.tseries = data.frame(year = 2000:2002,
                                  L.cHL.klb = c(10, 20, 30), L.cHL.knum = c(5, 9, 14),
                                  L.rGN.klb = c(4, 8, 12), L.rGN.knum = c(1, 2, 3),
                                  D.rGN.gutklb = c(5, 6, 7), D.rGN.knum = c(1, 1, 1)),
       info = list(units.landings = "1000 dead fish", units.discards = "1000 lb"))
}

test_that(".DetectBAMCatchUnits matches .pr series to LD.pr.tseries units", {
  BAMdata <- MockBAMUnitsData()
  expect_equal(.DetectBAMCatchUnits(BAMdata, c("cHL", "rGN"), c("cHL", "rGN"), "Landings"),
               c(cHL = "1000 lb", rGN = "1000 n"))
  expect_equal(.DetectBAMCatchUnits(BAMdata, "rGN", "rGN2", "Discards"),
               c(rGN2 = "1000 lb"))
  expect_message(Units <- .DetectBAMCatchUnits(BAMdata, "rHB", "rHB", "Landings"),
                 "Could not detect")
  expect_equal(Units, c(rHB = "1000 n"))
})

test_that("ImportBAM detects per-fleet catch units and keeps user overrides", {
  skip_on_cran()
  skip_if_not_installed("bamExtras")
  BAMdata <- GetBAMOutput("RedSnapper")

  OM <- suppressMessages(ImportBAM("RedSnapper", nSim = 2, pYear = 3, silent = TRUE,
                                   populate = FALSE))
  expect_equal(OM@Data[[1]]@Landings@Units,
               c(cHL = "Biomass", rHB = "Number", rGN = "Number"))
  expect_equal(OM@Data[[1]]@Discards@Units,
               c(cHL = "Number", rHB = "Number", rGN = "Number"))
  yr <- as.character(BAMdata$parms$styr:BAMdata$parms$endyr)
  expect_equal(unname(OM@Data[[1]]@Landings@Value[yr, "rGN"]),
               unname(BAMdata$t.series[yr, "L.rGN.ob"] * 1000))

  OM <- suppressMessages(ImportBAM("RedSnapper", nSim = 2, pYear = 3, silent = TRUE,
                                   populate = FALSE, UnitsLandings = c(rGN = "1000 lb")))
  expect_equal(OM@Data[[1]]@Landings@Units,
               c(cHL = "Biomass", rHB = "Number", rGN = "Biomass"))
})

test_that(".GetBAMSpawnOutput applies each year's Z, including the first year", {
  skip_if_not_installed("bamExtras")
  BAMdata <- GetBAMOutput("SnowyGrouper")
  SSB <- .GetBAMSpawnOutput(BAMdata)
  expect_equal(SSB[-1], BAMdata$t.series$SSB[-1], tolerance = 1e-5)
  N1 <- BAMdata$N.age[1, ] * exp(-BAMdata$Z.age[1, ] * BAMdata$parms$spawn.time)
  expect_equal(SSB[1], sum(N1 * BAMdata$a.series$reprod))
})

.BAMMaxMARE <- function(Out) {
  vapply(c("Recruits", "Number", "Biomass", "Landings", "Discards"), \(s) {
    m <- Out[[s]]$MARE$MARE
    if (is.null(m) || all(is.na(m))) 0 else max(m, na.rm = TRUE)
  }, numeric(1))
}

test_that("ImportBAM matches BAM with closed and discard-only fleets (Black Sea Bass)", {
  skip_on_cran()
  skip_if_not_installed("bamExtras")
  DiscMortDF <- data.frame(Fleet = c("cHL", "rHB", "rGN", "cPT", "cPT"),
                           Value = c(0.19, 0.152, 0.137, 0.14, 0.068),
                           Year  = c(1977, 1977, 1977, 1977, 2006))
  OM <- suppressMessages(ImportBAM("BlackSeaBass", nSim = 1, pYear = 1, silent = TRUE,
                                   DiscMortDF = DiscMortDF))
  expect_true(all(c("cTW", "cGN") %in% FleetNames(OM)))
  expect_gt(OM@maxF, 3)
  Out <- suppressMessages(CompareBAM("BlackSeaBass", OM))
  expect_true(all(.BAMMaxMARE(Out) < 0.01))
})

test_that("ImportBAM matches BAM with a recruitment lag (Snowy Grouper)", {
  skip_on_cran()
  skip_if_not_installed("bamExtras")
  OM <- suppressMessages(ImportBAM("SnowyGrouper", nSim = 1, pYear = 1, silent = TRUE))
  Out <- suppressMessages(CompareBAM("SnowyGrouper", OM))
  expect_true(all(.BAMMaxMARE(Out) < 0.01))
})
