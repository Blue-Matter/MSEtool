# Status-quo effort (empty advice, e.g. `CurrentEffort`) sets only the effort
# level; the area split is left to the engine unless the OM specifies a
# historical distribution. Area-matrix advice sets the split only while in force.

.RegisterMPs <- function(...) {
  MPs <- list(...)
  for (nm in names(MPs)) {
    class(MPs[[nm]]) <- "mp"
    assign(nm, MPs[[nm]], envir = globalenv())
  }
  names(MPs)
}

test_that(".CheckTACEffort() turns empty advice into flagged relative effort 1", {
  Fleets <- c("F1", "F2")
  Out <- .CheckTACEffort(list(list(A = Advice(), B = Advice(TAC = 10))), Fleets)
  sq <- Out[[1]]$A
  expect_equal(as.numeric(sq@Effort), c(1, 1))
  expect_identical(names(dimnames(sq@Effort)), "Fleet")
  expect_identical(sq@EffType, "Rel")
  expect_true(sq@Misc$StatusQuoEffort)
  expect_null(Out[[1]]$B@Effort)

  Data <- .ApplyAdviceMiscToData(list(A = new("data")), list(A = sq))
  expect_null(Data$A@Misc$StatusQuoEffort)
})

test_that("CurrentEffort keeps the status-quo level with a dynamic area split", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 3
  pYear(om) <- 15
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  expect_equal(nArea(om), 2)

  MPNames <- .RegisterMPs(RelOne = function(Data) Advice(Effort = 1, EffType = "Rel"))
  on.exit(rm(list = MPNames, envir = globalenv()), add = TRUE)
  mse <- Project(hist, MPs = c("CurrentEffort", "RelOne"), parallel = FALSE, silent = TRUE)
  expect_length(mse@Log$error, 0)

  ProjYears <- as.character(Years(hist@OM, "Projection"))
  nHist     <- dim(hist@Effort)[2]
  Dist <- mse@Distribution[, ProjYears, , , , drop = FALSE]

  expect_equal(Dist[, , , , "CurrentEffort"], Dist[, , , , "RelOne"], tolerance = 1e-10)
  expect_equal(mse@SBiomass[, , , "CurrentEffort"], mse@SBiomass[, , , "RelOne"], tolerance = 1e-10)

  LastDist <- hist@Distribution[, nHist, 1, 1]
  for (sim in seq_len(om@nSim)) {
    Share <- Dist[sim, , 1, 1, "CurrentEffort"]
    expect_gt(stats::sd(Share), 1e-4)
    expect_gt(max(abs(Share - LastDist[sim])), 1e-4)
  }
  expect_equal(apply(Dist[, , 1, , "CurrentEffort"], c(1, 2), sum),
               array(1, dim(Dist)[1:2]), ignore_attr = TRUE, tolerance = 1e-10)

  LastEff <- ExtendSims(hist@Effort, om@nSim)[, nHist, 1]
  expect_equal(unname(ExtendSims(mse@Effort, om@nSim)[, ProjYears, 1, "CurrentEffort"]),
               matrix(LastEff, om@nSim, length(ProjYears)), ignore_attr = TRUE, tolerance = 1e-10)
})

test_that("status-quo effort keeps the last historical seasonal pattern", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim  <- 2
  om@pYear <- 2
  Imp(om) <- FullComplianceImp
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  # an annual MP returning empty advice is split by season the same way
  MPNames <- .RegisterMPs(AnnualEmpty = function(Data) Advice())
  on.exit(rm(list = MPNames, envir = globalenv()), add = TRUE)
  mse <- Project(hist, MPs = c("CurrentEffort", "AnnualEmpty"), parallel = FALSE, silent = TRUE)
  expect_length(mse@Log$error, 0)

  Seasons <- om@Seasons
  nHist   <- dim(hist@Effort)[2]
  LastYr  <- hist@Effort[, (nHist - Seasons + 1):nHist, 1]
  LastDist <- hist@Distribution[, (nHist - Seasons + 1):nHist, 1, 1]
  for (mp in c("CurrentEffort", "AnnualEmpty")) {
    for (yr in seq_len(om@pYear) - 1) {
      idx <- yr * Seasons + seq_len(Seasons)
      expect_equal(unname(mse@Effort[, idx, 1, mp]), unname(LastYr), tolerance = 1e-8)
      expect_gt(max(abs(mse@Distribution[, idx, 1, 1, mp] - LastDist)), 1e-4)
    }
  }
})

test_that("a user-specified historical distribution is kept under CurrentEffort", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  pYear(om) <- 5
  HistYears <- Years(om, "Historical")
  om@Fleet[[1]][[1]]@Effort@Distribution <-
    array(c(0.3, 0.7), c(1, 1, 2), dimnames = list(Sim = 1, Year = HistYears[1], Area = 1:2))
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  expect_equal(range(hist@Distribution[, , 1, 1]), c(0.3, 0.3), tolerance = 1e-10)

  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  expect_length(mse@Log$error, 0)

  ProjYears <- as.character(Years(hist@OM, "Projection"))
  Dist <- mse@Distribution[, ProjYears, , , 1, drop = FALSE]
  expect_equal(range(Dist[, , 1, 1, 1]), c(0.3, 0.3), tolerance = 1e-10)
  expect_equal(range(Dist[, , 1, 2, 1]), c(0.7, 0.7), tolerance = 1e-10)

  # the fleet without a specified distribution keeps the engine's area split
  nHist <- dim(hist@Effort)[2]
  for (sim in seq_len(om@nSim))
    expect_gt(max(abs(Dist[sim, , 2, 1, 1] - hist@Distribution[sim, nHist, 2, 1])), 1e-4)
})

test_that("an area-matrix effort advice sets the area split only while it is in force", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  om@maxF <- 100
  pYear(om) <- 6
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  Mat <- matrix(c(0.05, 0.3, 0.15, 0.1), 2, 2)
  MatThenRel <- function(Data) if (isTRUE(Data@Misc$FirstAdvice))
    Advice(Effort = Mat, EffType = "Abs") else Advice(Effort = 1, EffType = "Rel")
  MatThenEmpty <- function(Data) if (isTRUE(Data@Misc$FirstAdvice))
    Advice(Effort = Mat, EffType = "Abs") else Advice()
  attr(MatThenRel, "Interval") <- attr(MatThenEmpty, "Interval") <- 3
  MPNames <- .RegisterMPs(MatThenRel = MatThenRel, MatThenEmpty = MatThenEmpty)
  on.exit(rm(list = MPNames, envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = c("MatThenRel", "MatThenEmpty"), parallel = FALSE, silent = TRUE)
  expect_length(mse@Log$error, 0)

  ProjYears <- as.character(Years(hist@OM, "Projection"))
  First <- ProjYears[1:3]
  Later <- ProjYears[4:6]
  nHist <- dim(hist@Effort)[2]
  LastEff <- ExtendSims(hist@Effort, om@nSim)[, nHist, ]
  Eff     <- ExtendSims(mse@Effort, om@nSim)

  for (mp in c("MatThenRel", "MatThenEmpty")) {
    Dist <- mse@Distribution[, , , , mp]
    expect_equal(range(Dist[, First, 1, 1]), c(0.25, 0.25), tolerance = 1e-8)
    expect_equal(range(Dist[, First, 2, 1]), c(0.75, 0.75), tolerance = 1e-8)
    expect_gt(min(abs(Dist[, Later, 1, 1] - 0.25)), 1e-4)
    expect_gt(min(abs(Dist[, Later, 2, 1] - 0.75)), 1e-4)
    expect_false(anyNA(Dist[, Later, , ]))

    expect_equal(range(Eff[, First, 1, mp]), c(0.2, 0.2), tolerance = 1e-8)
    expect_equal(range(Eff[, First, 2, mp]), c(0.4, 0.4), tolerance = 1e-8)
    for (fl in 1:2)
      expect_equal(unname(Eff[, Later, fl, mp]),
                   matrix(LastEff[, fl], om@nSim, length(Later)), ignore_attr = TRUE,
                   tolerance = 1e-8)
  }
})
