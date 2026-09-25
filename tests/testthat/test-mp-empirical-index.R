# 3 full years of 4 seasons plus 2 seasons of a partial final year; fleet 2
# has all-NA discards; survey 2 is observed in season 3 only.
.SeasonalIndexData <- function() {
  Seasons <- 4
  Years   <- c(rep(2020:2022, each = Seasons) + (0:3) / Seasons, 2023, 2023.25)
  nTS     <- length(Years)
  rn      <- list(Year = as.character(Years), Fleet = c('F1', 'F2'))

  Land <- array(c(seq_len(nTS), 10 * seq_len(nTS)), c(nTS, 2), rn)
  Disc <- array(c(rep(1, nTS), rep(NA, nTS)), c(nTS, 2), rn)

  Season <- rep_len(1:4, nTS)
  Surv <- array(NA_real_, c(nTS, 2), list(Year = rn$Year, Fleet = c('S1', 'S2')))
  Surv[, 1] <- 1 + Season / 10
  Surv[Season == 3, 2] <- 2

  D <- Data(Years = Years, YearLH = 2023, Seasons = Seasons,
            Landings = CatchData(Name = rn$Fleet, Value = Land),
            Discards = CatchData(Name = rn$Fleet, Value = Disc),
            Survey   = IndicesData(Name = c('S1', 'S2'), Value = Surv))
  D@Misc$Sim <- 1
  D
}

.AnnualIndexData <- function(IndexValue, Ref = NULL) {
  Years <- 2001:2020
  n     <- length(Years)
  rn    <- list(Year = as.character(Years), Fleet = 'F1')
  D <- Data(Years = Years, YearLH = 2020,
            Landings = CatchData(Name = 'F1', Value = array(100, c(n, 1), rn)),
            Discards = CatchData(Name = 'F1', Value = array(NA_real_, c(n, 1), rn)),
            Survey   = IndicesData(Name = 'S1', Ref = Ref,
                                   Value = array(IndexValue, c(n, 1), list(Year = rn$Year, Fleet = 'S1'))))
  D@Misc$Sim <- 1
  D
}

test_that("AnnualData returns non-seasonal data unchanged", {
  D <- .AnnualIndexData(rep(1, 20))
  expect_identical(AnnualData(D), D)
})

test_that("AnnualData sums catch over complete calendar years and keeps all-NA cells NA", {
  A <- AnnualData(.SeasonalIndexData())
  expect_equal(A@Years, 2020:2022)
  expect_equal(A@Seasons, 1)
  expect_equal(unname(A@Landings@Value[, 'F1']), c(sum(1:4), sum(5:8), sum(9:12)))
  expect_equal(unname(A@Landings@Value[, 'F2']), 10 * c(sum(1:4), sum(5:8), sum(9:12)))
  expect_equal(unname(A@Discards@Value[, 'F1']), rep(4, 3))
  expect_true(all(is.na(A@Discards@Value[, 'F2'])))

  Partial <- AnnualData(.SeasonalIndexData(), CompleteYears = FALSE)
  expect_equal(Partial@Years, 2020:2023)
  expect_equal(unname(Partial@Landings@Value['2023', 'F1']), 13 + 14)
})

test_that("AnnualData averages indices over the requested seasons", {
  D <- .SeasonalIndexData()

  All <- AnnualData(D)
  expect_equal(unname(All@Survey@Value[, 'S1']), rep(1.25, 3))
  expect_equal(unname(All@Survey@Value[, 'S2']), rep(2, 3))

  S3 <- AnnualData(D, IndexSeasons = 3)
  expect_equal(unname(S3@Survey@Value[, 'S1']), rep(1.3, 3))

  PerIndex <- AnnualData(D, IndexSeasons = list(Survey = list(c(1, 2), NULL)))
  expect_equal(unname(PerIndex@Survey@Value[, 'S1']), rep(1.15, 3))
  expect_equal(unname(PerIndex@Survey@Value[, 'S2']), rep(2, 3))

  expect_error(AnnualData(D, IndexSeasons = 5), "IndexSeasons")
  expect_error(AnnualData(D, IndexSeasons = list(Survey = list(1))), "one element per index")
  expect_error(AnnualData(D, IndexSeasons = list(Index = 1)), "Survey")
})

test_that("LastTAC falls back to the last complete year's removals, ignoring NA discards", {
  expect_equal(LastTAC(.AnnualIndexData(rep(1, 20))), 100)

  D <- .SeasonalIndexData()
  expect_equal(LastTAC(D), sum(9:12) + 10 * sum(9:12) + 4)

  D@Advice@TAC <- array(c(500, 700), c(2, 1), list(Year = c('2021', '2022'), Fleet = 'Total'))
  expect_equal(LastTAC(D), 700)
})

test_that("IndexTarget defaults the target to the last historical status", {
  Flat <- .AnnualIndexData(rep(1, 20))
  expect_equal(IndexTarget(Flat)@TAC, 100)
  expect_equal(IndexTarget(Flat, Smooth = FALSE)@TAC, 100)

  # index at half its supplied Ref: TAC cut by the maximum DeltaDown
  expect_equal(IndexTarget(.AnnualIndexData(rep(1, 20), Ref = 2))@TAC, 50)
  expect_equal(IndexTarget(Flat, IndexTarget = 2)@TAC, 50)

  expect_true(is.finite(IndexRate(Flat)@TAC))
})

test_that("TACType sets the catch used for calibration, the previous TAC, and the advice", {
  D <- .AnnualIndexData(rep(1, 20))
  D@Discards@Value[] <- 50
  expect_equal(LastTAC(D), 150)
  expect_equal(LastTAC(D, 'Landings'), 100)
  expect_equal(LastTAC(.SeasonalIndexData(), 'Landings'), sum(9:12) + 10 * sum(9:12))

  for (MP in list(IndexRate, IndexTarget)) {
    Rem  <- MP(D, Smooth = FALSE)
    Land <- MP(D, Smooth = FALSE, TACType = 'Landings')
    expect_equal(c(Rem@TAC, Land@TAC), c(150, 100))
    expect_equal(c(TACType(Rem), TACType(Land)), c('Removals', 'Landings'))
  }
})

test_that("IndexRate keeps the previous TAC when the trial TAC is infinite", {
  D <- .AnnualIndexData(c(rep(1, 18), 0, 0))
  expect_equal(IndexRate(D, Smooth = FALSE, RecentYears = 3)@TAC, 100)
})

test_that(".ResolveIndexTarget keeps supplied values and falls back past trailing NAs", {
  IndexHist <- rbind(c(1, 2, 3, 4, 5, NA), c(2, 2, 2, 2, NA, NA))
  Ref <- .ResolveIndexTarget(c(10, NA), NULL, c(TRUE, TRUE), IndexHist, LHInd = 6,
                             Smooth = FALSE, ENPMult = 0.3, RecentYears = 2)
  expect_equal(Ref, c(10, 2))

  Ref <- .ResolveIndexTarget(c(NA, NA), NULL, c(TRUE, TRUE), IndexHist, LHInd = 5,
                             Smooth = FALSE, ENPMult = 0.3, RecentYears = 2)
  expect_equal(Ref, c(4.5, 2))

  expect_equal(.ResolveIndexTarget(c(NA, NA), 3, c(FALSE, TRUE), IndexHist[2, , drop = FALSE],
                                   LHInd = 5, Smooth = FALSE, ENPMult = 0.3, RecentYears = 1), 3)
})

test_that("seasonal MPs match the same MP applied to AnnualData()", {
  D <- .SeasonalIndexData()

  expect_equal(IndexTarget(D, Smooth = FALSE)@TAC,
               IndexTarget(AnnualData(D), Smooth = FALSE)@TAC)
  expect_equal(IndexRate(D, Smooth = FALSE)@TAC,
               IndexRate(AnnualData(D), Smooth = FALSE)@TAC)

  expect_equal(IndexTarget(D, Smooth = FALSE, IndexSeasons = list(c(1, 2), NULL))@TAC,
               IndexTarget(AnnualData(D, IndexSeasons = list(Survey = list(c(1, 2), NULL))),
                           Smooth = FALSE)@TAC)
  expect_error(IndexTarget(D, IndexSeasons = list(1)), "one element per selected index")
})

test_that("index MPs project non-zero annual-scale TACs in annual and seasonal OMs", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())

  AnnualLanding <- function(mse) {
    L <- mse@Landings
    dn <- names(dimnames(L))
    Tot <- apply(L, match(c("Sim", "Year", "MP"), dn), sum, na.rm = TRUE)
    Yr  <- floor(as.numeric(dimnames(Tot)[[2]]) + 1e-6)
    apply(Tot, c(1, 3), function(x) tapply(x, Yr, sum))
  }

  for (Seasons in c(1, 4)) {
    om <- TwoFleetOM
    om@nSim <- 3
    om@pYear <- 3
    om@Seasons <- Seasons
    set.seed(1)
    hist <- Simulate(om, silent = TRUE)
    LastRemovals <- vapply(hist@Data, \(d) LastTAC(d[[1]]), numeric(1))

    mse <- Project(hist, MPs = c("IndexTarget", "IndexRate"), parallel = FALSE, silent = TRUE)
    Annual <- AnnualLanding(mse)

    expect_true(all(is.finite(Annual)), info = paste("Seasons =", Seasons))
    expect_true(all(Annual > 0), info = paste("Seasons =", Seasons))
    FirstYear <- Annual[1, , ]
    expect_true(all(FirstYear > 0.4 * LastRemovals & FirstYear < 2 * LastRemovals),
                info = paste("Seasons =", Seasons))
  }
})

test_that("tunepar scales the IndexRate rate and the IndexTarget target", {
  D <- .AnnualIndexData(seq(2, 1, length.out = 20), Ref = 1.5)
  Free <- list(DeltaUp = c(0, 10), DeltaDown = c(0, 1), Smooth = FALSE)

  Rate1 <- do.call(IndexRate, c(list(D), Free))
  Rate2 <- do.call(IndexRate, c(list(D, tunepar = 1.25), Free))
  expect_equal(Rate2@TAC, 1.25 * Rate1@TAC)

  Target1 <- do.call(IndexTarget, c(list(D), Free))
  Target2 <- do.call(IndexTarget, c(list(D, tunepar = 1.25), Free))
  Target3 <- do.call(IndexTarget, c(list(D, IndexTarget = 1.5 / 1.25), Free))
  expect_equal(Target2@TAC, 1.25 * Target1@TAC)
  expect_equal(Target2@TAC, Target3@TAC)

  expect_error(IndexRate(D, tunepar = -1), 'tunepar')
  expect_error(IndexTarget(D, tunepar = c(1, 2)), 'tunepar')
})

test_that("catch in the first projection years increases with tunepar for every MP", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 3
  Hist <- Simulate(OM, silent = TRUE)
  for (MP in c('IndexRate', 'IndexTarget', 'SurplusProduction')) {
    Fn <- get(MP)
    MPs <- list(Low = SetMPArgs(Fn, tunepar = 0.6), Mid = Fn, High = SetMPArgs(Fn, tunepar = 1.6))
    MSE <- Project(Hist, MPs = MPs, silent = TRUE)
    Yield <- PM_Removals(MSE, Years = Years(OM, 'Projection')[1:10])@Mean[1, c('Low', 'Mid', 'High')]
    expect_true(all(diff(Yield) > 0), label = MP)
  }
})
