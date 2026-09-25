
.InterimRows <- function(Mean, CV = NA_real_, Max = NA_real_)
  data.frame(Mean = Mean, CV = CV, Max = Max)

test_that("interim multiplier is deterministic when CV is NA/0 or all Mean are 0", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 10
  expect_equal(.SampleInterimMultiplier(om, 2025, "S", "TAC", .InterimRows(100), 1), 1)
  expect_equal(.SampleInterimMultiplier(om, 2025, "S", "TAC", .InterimRows(100, 0), 1), 1)
  expect_equal(.SampleInterimMultiplier(om, 2025, "S", "TAC", .InterimRows(c(0, 0), 0.3), 1), 1)
})

test_that("interim multiplier has mean 1 and the requested CV", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 20000
  rows <- .InterimRows(c(600, 400), 0.2)
  m <- .SampleInterimMultiplier(om, 2025, "S", "TAC", rows, seq_len(om@nSim))
  expect_equal(mean(m), 1, tolerance = 0.01)
  expect_equal(sd(m) / mean(m), 0.2, tolerance = 0.03)
})

test_that("interim multiplier is truncated at the smallest Max / Mean", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 5000
  rows <- .InterimRows(c(600, 400, 0), 0.3, c(720, 500, NA))
  m <- .SampleInterimMultiplier(om, 2025, "S", "TAC", rows, seq_len(om@nSim))
  expect_true(all(m <= 1.2))
  expect_true(mean(m) < 1)
})

test_that("interim multiplier seeds differ across years, stocks, and types", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 5
  rows <- .InterimRows(100, 0.3)
  draw <- function(y, s, t) .SampleInterimMultiplier(om, y, s, t, rows, seq_len(om@nSim))
  expect_false(isTRUE(all.equal(draw(2024, "S", "TAC"), draw(2042, "S", "TAC"))))
  expect_false(isTRUE(all.equal(draw(2025, "S1", "TAC"), draw(2024, "S2", "TAC"))))
  expect_false(isTRUE(all.equal(draw(2025, "S", "TAC"), draw(2025, "S", "Effort"))))
  expect_equal(draw(2025, "S", "TAC"), draw(2025, "S", "TAC"))
})

test_that("InterimAdvice validation rejects SD, negative CV, mixed CV, and Max < Mean", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  yr <- CurrentYear(om) + 1
  MPStartYear(om) <- yr + 1
  set_ia <- function(df) { om@InterimAdvice <- df; methods::validObject(om) }

  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, SD = 10)), "use `CV`")
  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, CV = -0.1)), "CV")
  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Fleet = c("A", "B"),
                                 Mean = c(100, 50), CV = c(0.1, 0.2))), "identical")
  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, Max = 90)), "Max")

  expect_no_error(set_ia(data.frame(Year = yr, Type = "TAC", Fleet = c("A", "B", "C"),
                                Mean = c(100, 50, 0), CV = c(0.2, 0.2, NA), Max = c(100, NA, NA))))
})

test_that("InterimAdvice Complex column: Stock is deprecated, both is an error, names are validated", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  yr <- CurrentYear(om) + 1
  MPStartYear(om) <- yr + 1
  om <- PopulateOM(om, silent = TRUE)
  cx <- names(om@Complexes)
  set_ia <- function(df) { om@InterimAdvice <- df; methods::validObject(om); om }

  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, Complex = cx, Stock = cx)),
               "both `Complex` and `Stock`")

  proj <- methods::new("hist")
  proj@OM <- set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, Stock = cx))
  expect_warning(out <- .CheckInterimAdvice(proj), "deprecated")
  expect_true("Complex" %in% names(out@OM@InterimAdvice))
  expect_false("Stock" %in% names(out@OM@InterimAdvice))

  proj@OM <- set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, Complex = "NotAComplex"))
  expect_error(.CheckInterimAdvice(proj), "NotAComplex")

  proj@OM <- set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, Complex = cx))
  expect_no_warning(.CheckInterimAdvice(proj))
})

.InterimSeasonalHist <- function(Seasons = 4) {
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 4
  om@Seasons <- Seasons
  set.seed(1)
  Simulate(om, silent = TRUE)
}

.InterimCheck <- function(hist, IA, MPStartYear = floor(Years(hist, "P")[1]) + 2) {
  hist@OM@MPStartYear <- MPStartYear
  hist@OM@InterimAdvice <- IA
  .CheckInterimAdvice(hist)
}

test_that("InterimAdvice seasonal/annual form, coverage, and MPStartYear pairing are enforced", {
  skip_on_cran()
  hist <- .InterimSeasonalHist()
  yp <- Years(hist, "P")
  fl <- FleetNames(hist)
  ok <- data.frame(Year = c(2027, 2028), Type = "TAC", Mean = 1000)

  expect_no_error(.InterimCheck(hist, ok))
  expect_no_error(.InterimCheck(hist, rbind(
    data.frame(Year = yp[1:4], Fleet = fl[1], Type = "TAC", Mean = 100),
    data.frame(Year = 2027, Fleet = fl[2], Type = "TAC", Mean = 400),
    data.frame(Year = 2028, Fleet = NA, Type = "TAC", Mean = 1000))))

  expect_error(.InterimCheck(hist, rbind(ok, data.frame(Year = yp[2], Type = "TAC", Mean = 10))),
               "single row for the calendar year")
  expect_error(.InterimCheck(hist, rbind(ok[2, ], data.frame(Year = yp[2:4], Type = "TAC", Mean = 10))),
               "single row for the calendar year")
  expect_error(.InterimCheck(hist, rbind(ok, data.frame(Year = 2027.3, Type = "TAC", Mean = 10))),
               "not interim timesteps")
  expect_error(.InterimCheck(hist, rbind(ok, data.frame(Year = 2029, Type = "TAC", Mean = 10))),
               "not interim timesteps")
  expect_error(.InterimCheck(hist, ok[1, ]), "no rows for Complex")
  expect_error(.InterimCheck(hist, data.frame(Year = c(2027, 2028), Fleet = fl[1], Type = "TAC", Mean = 10)),
               "do not cover fleet")
  expect_error(.InterimCheck(hist, rbind(data.frame(Year = 2027, Fleet = c(fl, NA), Type = "TAC", Mean = 10),
                                         ok[2, , drop = FALSE] |> transform(Fleet = NA))),
               "also a `Fleet = NA` total")
  expect_error(.InterimCheck(hist, ok, MPStartYear = NULL), "no interim years")
  expect_error(.InterimCheck(hist, NULL), "not specified")
  expect_no_error(.InterimCheck(hist, NULL, MPStartYear = NULL))
})

test_that("seasonal OM: seasonal rows used as given, annual rows split by SeasonalAllocation, totals by FleetAllocation", {
  skip_on_cran()
  hist <- .InterimSeasonalHist()
  yp <- Years(hist, "P")
  fl <- FleetNames(hist)
  hist@OM@MPStartYear <- 2029
  hist@OM@InterimAdvice <- rbind(
    data.frame(Year = yp[1:4], Fleet = fl[1], Type = "TAC", Mean = c(300, 400, 500, 200)),
    data.frame(Year = 2027, Fleet = fl[2], Type = "TAC", Mean = 1200),
    data.frame(Year = 2028, Fleet = NA, Type = "TAC", Mean = 500)
  )
  alloc <- .CheckFleetAllocation(hist) |> .CheckSeasonalAllocation()
  SA <- alloc@OM@SeasonalAllocation[[1]]
  FA <- alloc@OM@FleetAllocation[[1]]

  removals <- function(mp) {
    mse <- Project(hist, MPs = mp, parallel = FALSE, silent = TRUE)
    rem <- ArraySum(mse@Landings, mse@Discards)
    apply(rem[, , as.character(yp[1:8]), , 1, drop = FALSE], c(1, 3, 4), sum)
  }
  rem <- removals("refMSY50")

  for (sim in 1:3) {
    expect_equal(unname(rem[sim, 1:4, fl[1]]), c(300, 400, 500, 200), tolerance = 1e-3)
    expect_equal(unname(rem[sim, 1:4, fl[2]]), 1200 * unname(SA[sim, , fl[2]]), tolerance = 1e-3)
    expected <- 500 * cbind(FA[sim, 1] * SA[sim, , fl[1]], FA[sim, 2] * SA[sim, , fl[2]])
    expect_equal(unname(rem[sim, 5:8, ]), unname(expected), tolerance = 1e-3)
  }

  # interim years do not depend on the MP's EverySeason attribute
  expect_equal(removals("CurrentCatch"), rem, tolerance = 1e-6)
})

test_that("annual OM: per-fleet TAC and relative/absolute Effort are applied", {
  skip_on_cran()
  hist <- .InterimSeasonalHist(Seasons = 1)
  fl <- FleetNames(hist)
  hist@OM@MPStartYear <- 2029
  hist@OM@InterimAdvice <- rbind(
    data.frame(Year = 2027, Fleet = fl, Type = "TAC", Mean = c(300, 150), EffType = NA),
    data.frame(Year = 2028, Fleet = fl, Type = "Effort", Mean = c(0.5, 0.2), EffType = c("Rel", "Abs"))
  )
  mse <- Project(hist, MPs = "refMSY50", parallel = FALSE, silent = TRUE)
  rem <- ArraySum(mse@Landings, mse@Discards)
  rem27 <- apply(rem[, , "2027", , 1, drop = FALSE], c(1, 4), sum)
  for (sim in 1:3) expect_equal(unname(rem27[sim, ]), c(300, 150), tolerance = 1e-3)

  nH <- length(Years(hist, "H"))
  expect_equal(unname(mse@Effort[, "2028", fl[1], 1]), 0.5 * unname(hist@Effort[, nH, fl[1]]), tolerance = 1e-6)
  expect_equal(unname(mse@Effort[, "2028", fl[2], 1]), rep(0.2, 3), tolerance = 1e-6)
})

test_that("InterimAdvice TACUnit accepts catch units and validates TACType", {
  skip_on_cran()
  hist <- .InterimSeasonalHist(Seasons = 1)
  fl <- FleetNames(hist)
  IA <- data.frame(Year = rep(2027:2028, each = 2), Fleet = fl, Type = "TAC", Mean = 100,
                   TACUnit = c("t", "1000 n", NA, "Number"))
  out <- .InterimCheck(hist, IA)
  expect_identical(out@OM@InterimAdvice$TACUnit, c("Biomass", "Number", "Biomass", "Number"))

  expect_error(.InterimCheck(hist, transform(IA, TACUnit = "tonnes")), "TACUnit")
  expect_error(.InterimCheck(hist, transform(IA, TACType = "Catch")), "TACType")
})

test_that("annual OM: interim TAC in numbers and biomass are applied per fleet", {
  skip_on_cran()
  hist <- .InterimSeasonalHist(Seasons = 1)
  fl <- FleetNames(hist)
  hist@OM@MPStartYear <- 2029
  hist@OM@InterimAdvice <- data.frame(Year = rep(2027:2028, each = 2), Fleet = fl, Type = "TAC",
                                      Mean = c(20, 100, 100, 10),
                                      TACUnit = c("n", "t", "Biomass", "Number"))
  mse <- Project(hist, MPs = "refMSY50", parallel = FALSE, silent = TRUE)
  remB <- apply(ArraySum(mse@Landings, mse@Discards)[, , c("2027", "2028"), , 1, drop = FALSE], c(1, 3, 4), sum)
  remN <- ArraySum(mse@LandingsAtAge[[1]], mse@DiscardsAtAge[[1]])
  remN <- apply(remN[, , c("2027", "2028"), , , 1, drop = FALSE], c(1, 3, 4), sum)
  for (sim in 1:3) {
    expect_equal(unname(remN[sim, "2027", fl[1]]), 20, tolerance = 1e-3)
    expect_equal(unname(remB[sim, "2027", fl[2]]), 100, tolerance = 1e-3)
    expect_equal(unname(remB[sim, "2028", fl[1]]), 100, tolerance = 1e-3)
    expect_equal(unname(remN[sim, "2028", fl[2]]), 10, tolerance = 1e-3)
  }
})
