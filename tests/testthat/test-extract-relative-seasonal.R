.DenomArray <- function(Years) {
  array(seq_along(Years), dim = c(1, 1, length(Years)),
        dimnames = list(Sim = 1, Stock = "S", Year = Years))
}

test_that(".AlignDenomYears maps projection steps to the same season of the last historical year", {
  Hist <- c("2020", "2020.2466", "2020.4959", "2020.7479",
            "2021", "2021.2466", "2021.4959", "2021.7479")
  Proj <- c("2022", "2022.2466", "2022.4959", "2022.7479",
            "2023", "2023.2466", "2023.4959", "2023.7479")
  out <- .AlignDenomYears(.DenomArray(Hist), Proj)
  expect_equal(dimnames(out)$Year, Proj)
  expect_equal(as.numeric(out), rep(5:8, 2))

  out <- .AlignDenomYears(.DenomArray(Hist), c(Hist[3:8], Proj[1:2]))
  expect_equal(as.numeric(out), c(3:8, 5:6))
})

test_that(".AlignDenomYears repeats the last value for annual models", {
  out <- .AlignDenomYears(.DenomArray(as.character(2018:2021)), as.character(2020:2024))
  expect_equal(as.numeric(out), c(3, 4, 4, 4, 4))

  out <- .AlignDenomYears(.DenomArray("2018"), c("2022", "2022.2466", "2022.4959"))
  expect_equal(as.numeric(out), c(1, 1, 1))
})

test_that("Projection denominators repeat the last historical year's seasonal values", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 3
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)

  Denom <- ExtendSims(mse@Unfished@Dynamic@SBiomass, 3)
  nHist <- dim(Denom)[3]
  LastYear <- Denom[, , (nHist - 3):nHist, drop = FALSE]
  expect_false(isTRUE(all.equal(as.numeric(LastYear[, , 1]), as.numeric(LastYear[, , 4]))))

  Ratio <- SB_SB0(mse, type = "Dynamic", df = FALSE)
  SB <- mse@SBiomass
  Implied <- SB[, , , 1] / Ratio[, , , 1]
  nProj <- length(dimnames(SB)$Year)
  expect_equal(nProj, 12)
  for (y in 0:2)
    expect_equal(unname(Implied[, 4 * y + 1:4]), unname(abind::adrop(LastYear, 2)))
})

test_that(".SumWithinCalendarYear sums complete calendar years only", {
  Years <- c("2020", "2020.25", "2020.5", "2020.75", "2021", "2021.25", "2021.5", "2021.75", "2022")
  arr <- array(seq_len(18), dim = c(2, 1, 9),
               dimnames = list(Sim = 1:2, Stock = "S", Year = Years))
  out <- .SumWithinCalendarYear(arr, 4)
  expect_equal(dimnames(out)$Year, c("2020", "2021"))
  expect_equal(out[, 1, "2020"], c(`1` = 1 + 3 + 5 + 7, `2` = 2 + 4 + 6 + 8))
  expect_equal(out[, 1, "2021"], c(`1` = 9 + 11 + 13 + 15, `2` = 10 + 12 + 14 + 16))
})

test_that("Seasonal MSY-relative ratios are annual and match the MSY reference basis", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 3
  om@Seasons <- 4
  om@RefSeason <- 2L
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)

  FMSY  <- as.numeric(hist@Reference@MSY@FMSY)
  SBMSY <- as.numeric(hist@Reference@MSY@SBMSY)
  SPMSY <- as.numeric(hist@Reference@MSY@SPMSY)
  expect_gt(length(unique(FMSY)), 1)
  SP0 <- ExtendSims(hist@Unfished@Equilibrium@SProduction, 3)
  expect_true(all(SPMSY < SP0[, 1, dim(SP0)[3] - 2]))

  ts  <- dimnames(hist@SBiomass)$Year
  cal <- floor(as.numeric(ts) + 1e-8)
  CalYears <- unique(cal)
  FTot <- SumOverFleet(hist@FDead)
  SB   <- ExtendSims(hist@SBiomass, 3)
  SP   <- ExtendSims(hist@SProduction, 3)

  ExpF  <- sapply(CalYears, \(y) rowSums(ExtendSims(FTot, 3)[, 1, cal == y])) / FMSY
  ExpSB <- sapply(CalYears, \(y) SB[, 1, which(cal == y)[2]]) / SBMSY
  ExpSP <- sapply(CalYears, \(y) SP[, 1, which(cal == y)[2]]) / SPMSY

  FF <- F_FMSY(hist, df = FALSE)
  expect_equal(dimnames(FF)$Year, as.character(CalYears))
  expect_equal(as.numeric(FF[, 1, ]), as.numeric(ExpF))
  expect_equal(as.numeric(SB_SBMSY(hist, df = FALSE)[, 1, ]), as.numeric(ExpSB))
  expect_equal(as.numeric(SP_SPMSY(hist, df = FALSE)[, 1, ]), as.numeric(ExpSP))

  df <- F_FMSY(mse, df = TRUE, Reduce = FALSE)
  ProjYears <- unique(floor(Years(mse@OM, "Projection")))
  expect_setequal(df$Year[df$Period == "Projection"], ProjYears)
  expect_true(all(df$Year == floor(df$Year)))

  sb <- SB_SBMSY(mse, df = TRUE, Reduce = FALSE)
  sb <- sb[sb$Period == "Projection", ]
  ff <- df[df$Period == "Projection", ]
  Joined <- merge(sb[, c("Sim", "Year", "MP", "Value")], ff[, c("Sim", "Year", "MP", "Value")],
                  by = c("Sim", "Year", "MP"))
  expect_equal(nrow(Joined), 3 * length(ProjYears))
  Status <- PM_Status(mse)
  expect_equal(as.numeric(Status@Mean),
               mean(Joined$Value.x > 1 & Joined$Value.y < 1))
  expect_equal(sort(Status@Years), ProjYears)
  expect_equal(sort(PM_FFMSY(mse)@Years), ProjYears)
})

test_that("Default RefSeason weights sum to one within each calendar year", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 2
  om@pYear <- 2
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  ts <- dimnames(hist@SBiomass)$Year
  W  <- .RefSeasonWeightArray(hist@OM, ts)
  Tot <- .SumWithinCalendarYear(W, 4)
  expect_equal(as.numeric(Tot), rep(1, length(Tot)))
})
