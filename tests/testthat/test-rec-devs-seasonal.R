# Seasonal models: one recruitment deviation per calendar year, shared by its
# time steps, so SD and AC describe the annual deviations (as in SS3 3.30).

.WithinYearRange <- function(Dev, Years) {
  L <- log(Dev)
  max(apply(L, 1, \(r) tapply(r, floor(as.numeric(Years) + 1e-8), \(v) diff(range(v)))))
}

test_that("GenRecDevs: seasonal deviations are annual with the given SD and AC", {
  for (S in c(4, 12)) {
    a  <- Ages(5, Units = if (S == 4) "quarter" else "month")
    hy <- seq(1990, by = 1 / S, length.out = 20 * S)
    py <- seq(2010, by = 1 / S, length.out = 150 * S)
    d  <- GenRecDevs(SD = 0.6, AC = 0.5, Ages = a, HistYears = hy, ProjYears = py,
                     nSim = 60, Seed = 11)
    expect_lt(.WithinYearRange(d$RecDevHist, hy), 1e-12)
    expect_lt(.WithinYearRange(d$RecDevProj, py), 1e-12)
    Ann <- log(d$RecDevProj[, seq(1, length(py), by = S)])
    expect_equal(mean(apply(Ann, 1, stats::sd)), 0.6, tolerance = 0.05)
    expect_equal(mean(apply(Ann, 1, \(r) stats::acf(r, 1, plot = FALSE)$acf[2])), 0.5,
                 tolerance = 0.1)
  }
})

test_that("GenRecDevs: initial-age deviations are shared by cohorts born in the same year", {
  a <- Ages(12, Units = "quarter")   # 3 years
  d <- GenRecDevs(SD = 0.6, AC = 0.3, Ages = a, HistYears = seq(2000, by = 0.25, length.out = 8),
                  ProjYears = seq(2002, by = 0.25, length.out = 8), nSim = 2, Seed = 1)
  Age <- as.numeric(colnames(d$RecDevInit))
  expect_lt(.WithinYearRange(d$RecDevInit, 2000 - Age), 1e-12)
  expect_gt(length(unique(round(d$RecDevInit[1, ], 10))), 1)

  # youngest class above 0 (e.g. births in season 3): entry year from it
  a2 <- Ages(12, MinAge = 2, Units = "quarter")
  d2 <- GenRecDevs(SD = 0.6, AC = 0.3, Ages = a2, HistYears = seq(2000, by = 0.25, length.out = 8),
                   ProjYears = seq(2002, by = 0.25, length.out = 8), nSim = 2, Seed = 1)
  Age2 <- as.numeric(colnames(d2$RecDevInit))
  expect_lt(.WithinYearRange(d2$RecDevInit, 2000 - (Age2 - min(a2@Classes))), 1e-12)
})

test_that("ImportSS: seasonal plus group is selected in every season (NP swordfish)", {
  skip_on_cran()
  SSDir <- file.path("../../..", "WCNPOSWO-2023", "Final Base-case") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))
  skip_if_not_installed("r4ss", "1.52.1")
  OM <- suppressMessages(ImportSS(SSDir, nSim = 1, pYear = 2, silent = TRUE))
  for (Fleet in OM@Fleet[[1]]) {
    S <- Fleet@Selectivity@MeanAtAge
    A <- as.numeric(dimnames(S)$Age)
    Plus <- abind::asub(S, which(A == max(A)), which(names(dimnames(S)) == "Age"))
    Age15 <- abind::asub(S, which(A >= 15), which(names(dimnames(S)) == "Age"))
    # each time step: the plus-group class has the season's maximum-age value
    expect_equal(as.numeric(Plus), as.numeric(apply(Age15, 2:length(dim(Age15)), max)))
  }
})

test_that("GenRecDevs: seasonal supplied deviations are kept and NA years generated", {
  S  <- 4
  a  <- Ages(5, Units = "quarter")
  hy <- seq(1990, by = 1 / S, length.out = 20 * S)
  py <- seq(2010, by = 1 / S, length.out = 5 * S)
  Ann <- exp(stats::rnorm(20, 0, 0.5))
  sup <- matrix(rep(Ann, each = S), 1)
  sup[, 1 + S * (0:19)] <- 0          # season 1 does not recruit
  sup[, 71:80] <- NA                  # last 2.5 years unknown
  d <- GenRecDevs(SD = 0.6, AC = 0.99, Ages = a, HistYears = hy, ProjYears = py,
                  nSim = 3, RecDevHist = sup, Seed = 2)
  Known <- which(!is.na(sup[1, ]))
  for (i in 1:3) expect_equal(unname(d$RecDevHist[i, Known]), as.numeric(sup[1, Known]))
  # the NA steps of the partly known year take its supplied (non-zero) deviation
  expect_equal(unname(d$RecDevHist[1, 71:72]), rep(Ann[18], 2))
  # generated years are annual and conditioned on the last known deviation
  expect_lt(.WithinYearRange(d$RecDevHist[, 73:80, drop = FALSE], hy[73:80]), 1e-12)
  expect_lt(abs(log(d$RecDevHist[1, 73]) - log(Ann[18])), 0.3)
})

test_that("GenMultiStockRecDevs: seasonal projection deviations are annual on recruiting seasons", {
  skip_on_cran()
  om <- MultiStockOM
  om@nSim <- 2
  om@Seasons <- 4
  om <- suppressMessages(Populate(om, silent = TRUE))
  Hist <- om@Stock[[1]]@SRR@RecDevHist
  Hist[, seq(2, ncol(Hist), by = 4)] <- 0   # season 2 does not recruit
  om@Stock[[1]]@SRR@RecDevHist <- Hist
  om <- GenMultiStockRecDevs(om, silent = TRUE, Correlated = TRUE)
  P <- om@Stock[[1]]@SRR@RecDevProj
  py <- as.numeric(colnames(P))
  Season2 <- seq(2, ncol(P), by = 4)
  expect_true(all(P[, Season2] == 0))
  expect_lt(.WithinYearRange(P[, -Season2, drop = FALSE], py[-Season2]), 1e-12)
  P2 <- om@Stock[[2]]@SRR@RecDevProj
  expect_lt(.WithinYearRange(P2, py), 1e-12)
})

test_that(".SimulateRecDevAR1 (CombineOM) fills seasonal years with annual deviations", {
  om <- SeasonalSpatialOM
  om@nSim <- 2
  om <- suppressMessages(Populate(om, silent = TRUE))
  st <- om@Stock[[1]]
  HY <- Years(om, "H")
  Fwd <- tail(HY, 36)
  out <- .SimulateRecDevAR1(st@SRR@RecDevHist[, seq_len(length(HY) - 36), drop = FALSE],
                            Fwd, HY, st)
  expect_lt(.WithinYearRange(out[, as.character(Fwd), drop = FALSE], Fwd), 1e-12)
})
