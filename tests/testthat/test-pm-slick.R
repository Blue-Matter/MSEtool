.PMFixture <- new.env()

# Seasonal OM, 4 sims, interim TAC in the first projection year, MPs every 2 years
.PMSeasonalMSE <- function() {
  if (!is.null(.PMFixture$mse))
    return(.PMFixture$mse)
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 4
  om@pYear <- 8
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  FirstYear <- floor(Years(hist, 'P')[1])
  hist@OM@MPStartYear <- FirstYear + 1
  hist@OM@InterimAdvice <- data.frame(Year = FirstYear, Type = 'TAC', Mean = 1000)
  hist@OM@Interval <- 2

  # 20% cuts while the TAC is above 800, otherwise increases capped at 10%
  Alt <- function(Data, DeltaDown = c(0, 0.3), DeltaUp = c(0, 0.1)) {
    Prev <- LastTAC(Data)
    Mod  <- if (Prev > 800) 0.8 else 1.5
    Advice(TAC = ConstrainTAC(Prev, Mod, DeltaDown, DeltaUp, c(0, Inf)))
  }
  class(Alt) <- 'mp'
  .PMFixture$mse <- Project(hist, MPs = list(Alt = Alt, CE = CurrentEffort),
                            parallel = FALSE, silent = TRUE)
  .PMFixture$mse
}

test_that("PMYears returns the windows of the MP-active calendar years", {
  om <- methods::new('om')
  om@nYear <- 10
  om@pYear <- 30
  om@CurrentYear <- 2020
  om@Seasons <- 4
  om@MPStartYear <- 2025
  All <- 2025:2050
  expect_equal(PMYears(om), All)
  expect_equal(PMYears(om, 'first', 10), 2025:2034)
  expect_equal(PMYears(om, 'last', 10), 2041:2050)
  expect_equal(PMYears(om, 'middle', 10), 2035:2040)
  expect_equal(PMYears(om, 'last', 1), 2050)
  expect_equal(PMYears(om, Skip = 5), 2030:2050)
  expect_equal(PMYears(om, 'first', 3, Skip = 5), 2030:2032)
  expect_error(PMYears(om, Skip = 26), "leaves no years")

  om@MPStartYear <- NULL
  expect_equal(PMYears(om), 2021:2050)
})

test_that(".KobeQuadrant partitions SB/SBMSY and F/FMSY, ties included", {
  SB <- c(2, 0.5, 0.5, 2, 1, 1, 2, 0.5, NA)
  F  <- c(0.5, 2, 0.5, 2, 0.5, 2, 1, 1, 1)
  expect_equal(.KobeQuadrant(SB, F),
               c('green', 'red', 'yellow', 'orange', 'yellow', 'orange', 'orange', 'yellow', NA))
})

test_that("Kobe quadrant probabilities sum to 1 per simulation and year", {
  skip_on_cran()
  mse <- .PMSeasonalMSE()
  Quadrants <- c('green', 'red', 'yellow', 'orange')
  Total <- Reduce(`+`, purrr::map(Quadrants, \(q) PM_Kobe(mse, q)@Prob))
  expect_equal(as.numeric(Total), rep(1, length(Total)))

  df <- .KobeStatusDF(mse, 'SBiomass')
  expect_setequal(unique(df$Year), PMYears(mse))
  Q <- table(df$Sim, df$Year, df$MP, .KobeQuadrant(df$SB, df$F))
  expect_true(all(apply(Q, 1:3, sum) == 1))

  expect_equal(PM_Status(mse)@Prob, PM_Kobe(mse, 'green')@Prob)
  expect_equal(PM_Red(mse)@Prob, PM_Kobe(mse, 'red')@Prob)
  yrs <- PMYears(mse, 'last', 3)
  expect_equal(PM_Red(mse, Years = yrs)@Years, yrs)
})

test_that("PM_MinStatus is the lowest annual SB/SBMSY or SB/SB0 per simulation", {
  skip_on_cran()
  mse <- .PMSeasonalMSE()
  yrs <- PMYears(mse)
  sb <- SB_SBMSY(mse, Reduce = FALSE)
  sb <- sb[sb$Period == 'Projection' & sb$Year %in% yrs, ]
  Min <- tapply(sb$Value, list(sb$Sim, sb$MP), min)
  pm <- PM_MinStatus(mse)
  expect_equal(unname(pm@Stat[, 1, colnames(Min)]), unname(Min))
  expect_true(all(is.na(pm@Prob)))
  expect_equal(pm@Years, yrs)

  # SB/SB0 in the reference season(s) of each year, the weights of SB/SBMSY
  dep <- SB_SB0(mse, Reduce = FALSE)
  dep <- dep[dep$Period == 'Projection', ]
  W <- .RefSeasonWeightDF(mse@OM, sort(unique(dep$Year)))
  dep$.Key <- round(dep$Year, 6)
  dep <- merge(dep, W, by = c('Sim', 'Stock', '.Key'))
  dep$Cal <- floor(dep$Year + 1e-8)
  dep <- dep[dep$Cal %in% yrs, ]
  Annual <- stats::aggregate(list(Value = dep$Value * dep$.W),
                             list(Sim = dep$Sim, MP = dep$MP, Cal = dep$Cal), sum)
  Min0 <- tapply(Annual$Value, list(Annual$Sim, Annual$MP), min)
  expect_equal(unname(PM_MinStatus(mse, 'Unfished')@Stat[, 1, colnames(Min0)]), unname(Min0))
})

test_that(".MPDeltaLimit reads the MP limits and applies overrides", {
  f <- function(Data, DeltaDown = c(0.01, 0.3), DeltaUp = c(0.01, 0.15)) NULL
  x <- list(A = f, B = SetMPArgs(f, DeltaDown = c(0, 0.2)), C = function(Data) NULL)
  expect_equal(.MPDeltaLimit(x, 'DeltaDown'), c(A = 0.3, B = 0.2, C = NA))
  expect_equal(.MPDeltaLimit(x, 'DeltaUp'), c(A = 0.15, B = 0.15, C = NA))
  expect_equal(.MPDeltaLimit(x, 'DeltaUp', 0.1), c(A = 0.1, B = 0.1, C = 0.1))
  expect_equal(.MPDeltaLimit(x, 'DeltaUp', c(B = 0.5)), c(A = 0.15, B = 0.5, C = NA))
})

test_that("PM_TACLimited compares increases with DeltaUp and decreases with DeltaDown", {
  skip_on_cran()
  mse <- .PMSeasonalMSE()
  tac <- TACs(mse)
  tac <- tac[tac$MP == 'Alt', ]
  Annual <- tapply(tac$Value, list(tac$Sim, tac$Year), sum, na.rm = TRUE)
  Steps <- as.character(c(max(Years(mse, 'P')[floor(Years(mse, 'P')) < mse@OM@MPStartYear]),
                          seq(mse@OM@MPStartYear, by = 2, length.out = 4)))
  Series <- Annual[, Steps]
  Change <- t(apply(Series, 1, \(x) diff(x) / utils::head(x, -1)))
  # the fixture has both 20% cuts and capped 10% increases
  expect_true(any(abs(Change + 0.2) < 1e-6) && any(abs(Change - 0.1) < 1e-6))

  Expected <- rowMeans((Change > 0 & Change >= 0.1 - 1e-6) | (Change < 0 & -Change >= 0.3 - 1e-6))
  pm <- PM_TACLimited(mse)
  expect_equal(unname(pm@Prob[, 1, 'Alt']), unname(Expected))
  expect_equal(pm@Stat, pm@Prob)
  expect_true(all(is.na(pm@Prob[, 1, 'CE'])))

  # symmetric limits count the 20% cuts as limited
  Sym <- rowMeans(abs(Change) >= 0.1 - 1e-6)
  expect_equal(unname(PM_TACLimited(mse, DeltaDown = 0.1)@Prob[, 1, 'Alt']), unname(Sym))
  expect_false(isTRUE(all.equal(Sym, Expected)))

  # without the change from the interim TAC
  Expected2 <- rowMeans(((Change > 0 & Change >= 0.1 - 1e-6) | (Change < 0 & -Change >= 0.3 - 1e-6))[, -1])
  expect_equal(unname(PM_TACLimited(mse, IncludeFirst = FALSE)@Prob[, 1, 'Alt']), unname(Expected2))

  # AAV from the interim TAC
  expect_equal(unname(PM_AAVY(mse, IncludeFirst = TRUE)@Stat[, 1, 'Alt']),
               unname(rowMeans(abs(Change))))
  expect_equal(unname(PM_AAVY(mse)@Stat[, 1, 'Alt']), unname(rowMeans(abs(Change[, -1]))))
})

test_that("PMs are unchanged per simulation after Subset()", {
  skip_on_cran()
  mse <- .PMSeasonalMSE()
  sub <- Subset(mse, Sims = c(2, 4))
  PMs <- list(\(x) PM_Status(x), \(x) PM_Red(x), \(x) PM_MinStatus(x),
              \(x) PM_MinStatus(x, 'Unfished'), \(x) PM_TACLimited(x),
              \(x) PM_AAVY(x, IncludeFirst = TRUE), \(x) PM_Yield(x),
              \(x) PM_FFMSY(x, Ref = 1.4))
  for (f in PMs) {
    Full <- f(mse)
    Sub  <- f(sub)
    expect_equal(unname(Sub@Stat), unname(Full@Stat[c(2, 4), , , drop = FALSE]))
    expect_equal(unname(Sub@Prob), unname(Full@Prob[c(2, 4), , , drop = FALSE]))
  }
})

test_that("MSE2Slick handles PM specs, mixed metric types, and annual time series", {
  skip_on_cran()
  skip_if_not_installed('Slick')
  mse <- .PMSeasonalMSE()
  yrs <- PMYears(mse, 'last', 3)
  PMs <- list(
    list(PM = PM_Status, Code = 'PGK'),
    list(PM = PM_Status, Args = list(Years = yrs), Code = 'PGK_Last'),
    quote(PM_Red(Years = yrs)),
    list(PM = PM_MinStatus, Code = 'Min'),
    list(fun = PM_FFMSY, args = list(Ref = 1.4), Code = 'PLim_F'),
    list(PM = PM_Yield, Code = 'Yield'),
    list(PM = PM_TACLimited, Code = 'Limited')
  )
  sl <- MSE2Slick(mse, PMs = PMs,
                  TimeseriesCode = c('SB_SBMSY', 'F_FMSY', 'SB_SB0', 'Removals', 'TAC'),
                  TimeseriesLimit = c(F_FMSY = 1.4), KobeLimit = c(0.4, 1.4))
  expect_equal(sl@Boxplot@Code, c('PGK', 'PGK_Last', 'KobeRed', 'Min', 'PLim_F', 'Yield', 'Limited'))
  expect_equal(sl@MPs@Code, names(mse@MPs))

  Value <- sl@Boxplot@Value
  expect_equal(Value[, 1, , 5], PM_FFMSY(mse, Ref = 1.4)@Prob[, 1, names(mse@MPs)], ignore_attr = TRUE)
  expect_equal(Value[, 1, , 4], PM_MinStatus(mse)@Stat[, 1, names(mse@MPs)], ignore_attr = TRUE)
  expect_equal(sl@Quilt@MinValue, c(0, 0, 0, NA, 0, NA, 0))
  expect_equal(sl@Quilt@MaxValue, c(1, 1, 1, NA, 1, NA, 1))
  expect_true(all(sl@Spider@Value >= 0 & sl@Spider@Value <= 1, na.rm = TRUE))
  Yield <- sl@Tradeoff@Value[1, , 6]
  expect_equal(sl@Spider@Value[1, , 6], Yield / max(Yield), ignore_attr = TRUE)

  expect_error(MSE2Slick(mse, PMs = list(PM_Status, PM_Status)), "Duplicated PM code")

  # annual time series, historical and projection
  Time <- sl@Timeseries@Time
  expect_true(all(Time == floor(Time)))
  expect_equal(sl@Timeseries@Limit, c(0.4, 1.4, NA, NA, NA))
  sb <- SB_SBMSY(mse, Reduce = FALSE)
  sb <- sb[sb$Sim == 3 & sb$MP %in% c('Historical', 'Alt'), ]
  expect_equal(sl@Timeseries@Value[3, 1, 1, 1, ], sb$Value[match(Time, sb$Year)])

  rem <- PM_Removals(mse)
  RemTS <- sl@Timeseries@Value[, 1, , 4, Time %in% PMYears(mse), drop = FALSE]
  expect_equal(apply(RemTS, c(1, 3), mean)[, 1], rem@Stat[, 1, 'Alt'], ignore_attr = TRUE)
  expect_true(all(is.na(sl@Timeseries@Value[, 1, , 5, Time <= max(.SlickTime(mse@OM, 'Historical'))])))

  # Kobe from the first MP year
  expect_equal(sl@Kobe@Time, PMYears(mse))
  expect_equal(sl@Kobe@Limit, c(0.4, 1.4))
  df <- .KobeStatusDF(mse, 'SBiomass')
  df <- df[df$Sim == 2 & df$MP == 'CE', ]
  expect_equal(sl@Kobe@Value[2, 1, 2, 2, ], df$F[match(sl@Kobe@Time, df$Year)])
})
