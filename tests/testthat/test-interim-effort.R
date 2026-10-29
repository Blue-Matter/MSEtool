# Interim Effort rows are applied the same way as MP effort advice of the same value

test_that("seasonal OM: annual interim Effort follows the historical seasonal effort pattern", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim  <- 3
  om@pYear <- 2
  Imp(om) <- FullComplianceImp
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  Seasons <- om@Seasons
  nHist   <- dim(hist@Effort)[2]
  Year1   <- floor(Years(hist, "P")[1])
  hist@OM@MPStartYear <- Year1 + 1
  InterimEffort <- function(IA) {
    hist@OM@InterimAdvice <- IA
    mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
    expect_length(mse@Log$error, 0)
    ExtendSims(mse@Effort, om@nSim)[, seq_len(Seasons), 1, 1]
  }

  Share <- .SeasonalEffortAllocation(hist)[[1]][, , 1]
  expect_gt(max(apply(Share, 1, stats::sd)), 1e-3)
  Abs <- InterimEffort(data.frame(Year = Year1, Type = "Effort", Mean = 2, EffType = "Abs"))
  expect_equal(unname(rowSums(Abs)), rep(2, om@nSim), tolerance = 1e-10)
  expect_equal(unname(Abs), unname(2 * Share), ignore_attr = TRUE, tolerance = 1e-10)

  LastYr <- ExtendSims(hist@Effort, om@nSim)[, (nHist - Seasons + 1):nHist, 1]
  Rel <- InterimEffort(data.frame(Year = Year1, Type = "Effort", Mean = 1, EffType = "Rel"))
  expect_equal(unname(Rel), unname(LastYr), ignore_attr = TRUE, tolerance = 1e-10)
})

test_that("seasonal OM: an annual Fleet = NA Effort total is split by EffortAllocation, then by season", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim    <- 3
  om@pYear   <- 2
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  yp <- Years(hist, "P")
  fl <- FleetNames(hist)
  hist@OM@MPStartYear <- floor(yp[1]) + 1
  hist@OM@InterimAdvice <- data.frame(Year = floor(yp[1]), Fleet = NA, Type = "Effort", Mean = 0.8)
  EA    <- .CheckFleetAllocation(hist)@OM@EffortAllocation[[1]]
  Share <- .SeasonalEffortAllocation(hist)[[1]]

  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  expect_length(mse@Log$error, 0)
  Eff <- ExtendSims(mse@Effort, om@nSim)[, 1:4, , 1]
  for (sim in seq_len(om@nSim))
    for (f in 1:2)
      expect_equal(unname(Eff[sim, , f]), unname(0.8 * EA[min(sim, nrow(EA)), f] * Share[sim, , f]),
                   tolerance = 1e-10)
})

test_that("spatial OM: interim Effort leaves the area split to the engine, as MP effort does", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 3
  pYear(om) <- 4
  Imp(om) <- FullComplianceImp
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  expect_equal(nArea(om), 2)

  AbsEffort <- function(Data) Advice(Effort = 0.8, EffType = "Abs")
  class(AbsEffort) <- "mp"
  assign("AbsEffort", AbsEffort, envir = globalenv())
  on.exit(rm("AbsEffort", envir = globalenv()), add = TRUE)

  yp <- Years(hist, "P")
  Years <- as.character(yp[1:2])
  mseMP <- Project(hist, MPs = "AbsEffort", parallel = FALSE, silent = TRUE)
  hist@OM@MPStartYear   <- yp[1] + 2
  hist@OM@InterimAdvice <- data.frame(Year = yp[1:2], Type = "Effort", Mean = 0.8)
  mseIA <- Project(hist, MPs = "AbsEffort", parallel = FALSE, silent = TRUE)
  expect_length(mseIA@Log$error, 0)

  expect_equal(mseIA@Distribution[, Years, , , 1], mseMP@Distribution[, Years, , , 1], tolerance = 1e-10)
  expect_equal(mseIA@SBiomass, mseMP@SBiomass, tolerance = 1e-8)

  nHist <- dim(hist@Effort)[2]
  for (sim in seq_len(om@nSim))
    expect_gt(max(abs(mseIA@Distribution[sim, Years, 1, 1, 1] - hist@Distribution[sim, nHist, 1, 1])), 1e-4)
})

test_that("interim Effort keeps a user-specified historical distribution", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  pYear(om) <- 4
  HistYears <- Years(om, "Historical")
  om@Fleet[[1]][[1]]@Effort@Distribution <-
    array(c(0.3, 0.7), c(1, 1, 2), dimnames = list(Sim = 1, Year = HistYears[1], Area = 1:2))
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  yp <- Years(hist, "P")
  fl <- FleetNames(hist)
  hist@OM@MPStartYear   <- yp[1] + 2
  hist@OM@InterimAdvice <- data.frame(Year = rep(yp[1:2], each = 2), Fleet = fl,
                                      Type = "Effort", Mean = c(0.2, 0.3))
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  expect_length(mse@Log$error, 0)

  Years <- as.character(yp[1:2])
  Dist <- mse@Distribution[, Years, , , 1, drop = FALSE]
  expect_equal(range(Dist[, , 1, 1, 1]), c(0.3, 0.3), tolerance = 1e-10)
  expect_equal(range(Dist[, , 1, 2, 1]), c(0.7, 0.7), tolerance = 1e-10)

  nHist <- dim(hist@Effort)[2]
  for (sim in seq_len(om@nSim))
    expect_gt(max(abs(Dist[sim, , 2, 1, 1] - hist@Distribution[sim, nHist, 2, 1])), 1e-4)
  expect_equal(range(ExtendSims(mse@Effort, om@nSim)[, Years, 1, 1]), c(0.2, 0.2), tolerance = 1e-10)
})
