# Conditioning catch/effort observation Bias on real data (R/condition-obs-catch.R,
# R/condition-obs-effort.R) and its use when generating projection data.

skip_on_cran()

.BuildConditionedCatchEffort <- function(scale = 0.5) {
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  nSim(OM) <- 3
  OM@Obs[[1]][[1]]@Landings@Bias <- NULL

  set.seed(101)
  H0 <- Simulate(OM, silent = TRUE)
  HistYears <- Years(OM, 'Historical')
  FleetName <- FleetNames(OM)
  HY <- as.character(HistYears)

  L <- apply(H0@Landings, c('Sim', 'Year'), sum)[1, HY] * scale
  E <- H0@Effort[1, HY, 1] * scale
  mm <- function(v) matrix(v, ncol = 1, dimnames = list(Year = HistYears, Fleet = FleetName))

  Data(OM) <- Data(Name = 'Real', Years = HistYears,
                   Landings = CatchData(Name = FleetName, Units = 'Biomass', Value = mm(L)),
                   Effort   = EffortData(Name = FleetName, Value = mm(E)))

  Hist <- Simulate(OM, silent = TRUE)
  list(Hist = Hist, HY = HY, L = L, E = E, ProjYears = as.character(Years(OM, 'Projection')))
}

built <- .BuildConditionedCatchEffort()

.MeanRatio <- function(num, den) {
  r <- num / den
  r[!is.finite(r) | r < 0.001] <- NA
  rowMeans(r, na.rm = TRUE)
}

test_that("conditioned catch and effort Bias is observed / simulated", {
  Hist <- built$Hist
  nSim <- 3
  o <- Obs(Hist@OM)[[1]][[1]]

  SimL <- apply(Hist@Landings, c('Sim', 'Year'), sum)[, built$HY]
  ExpL <- .MeanRatio(matrix(built$L, nSim, length(built$HY), byrow = TRUE), SimL)
  expect_equal(as.numeric(o@Landings@Bias), unname(ExpL), tolerance = 1e-6)
  expect_equal(as.numeric(o@Landings@Bias)[1], 0.5, tolerance = 1e-6)

  SimE <- Hist@Effort[, built$HY, 1]
  ExpE <- .MeanRatio(matrix(built$E, nSim, length(built$HY), byrow = TRUE), SimE)
  expect_equal(as.numeric(o@Effort@Bias), unname(ExpE), tolerance = 1e-6)
  expect_equal(as.numeric(o@Effort@Bias)[1], 0.5, tolerance = 1e-6)
})

test_that("projected catch and effort data are scaled by the conditioned Bias", {
  MSE <- Project(built$Hist, 'CurrentCatch', silent = TRUE)
  PPD <- Data(MSE)[[1]]
  o <- Obs(built$Hist@OM)[[1]][[1]]

  TrueL <- apply(MSE@Landings, c('Sim', 'Year', 'MP'), sum)
  for (x in seq_along(PPD)) {
    vl <- PPD[[x]][[1]]@Landings@Value
    ve <- PPD[[x]][[1]]@Effort@Value
    py <- head(intersect(built$ProjYears, rownames(vl)), -1)
    expect_true(length(py) > 0)

    ErrL <- o@Landings@Error[x, py]
    ErrE <- o@Effort@Error[x, py]
    expect_equal(unname(vl[py, 1]),
                 unname(TrueL[x, py, 1] * o@Landings@Bias[x] * ErrL), tolerance = 1e-6)
    expect_equal(unname(ve[py, 1]),
                 unname(MSE@Effort[x, py, 1, 1] * o@Effort@Bias[x] * ErrE), tolerance = 1e-6)
  }
})

.CatchEffortBase <- function() {
  data(SingleStockOM, envir = environment())
  OM <- SingleStockOM
  nSim(OM) <- 3
  OM@Obs[[1]][[1]]@Landings@Bias <- NULL
  set.seed(101)
  H0 <- Simulate(OM, silent = TRUE)
  HistYears <- Years(OM, 'Historical')
  HY <- as.character(HistYears)
  list(OM = OM, HistYears = HistYears, FleetName = FleetNames(OM),
       L = apply(H0@Landings, c('Sim', 'Year'), sum)[1, HY] * 0.5,
       E = H0@Effort[1, HY, 1] * 0.5)
}

base <- .CatchEffortBase()

.WithData <- function(RowYears, DataYears = base$HistYears, ObsYears = NULL) {
  OM <- base$OM
  RY <- as.character(RowYears)
  mm <- function(v) matrix(v[RY], ncol = 1, dimnames = list(Year = RowYears, Fleet = base$FleetName))
  if (!is.null(ObsYears)) {
    OM@Obs[[1]][[1]]@Landings@Years <- ObsYears
    OM@Obs[[1]][[1]]@Effort@Years   <- ObsYears
  }
  args <- list(Name = 'Real',
               Landings = CatchData(Name = base$FleetName, Units = 'Biomass', Value = mm(base$L)),
               Effort   = EffortData(Name = base$FleetName, Value = mm(base$E)))
  args$Years <- DataYears
  Data(OM) <- do.call(Data, args)
  OM
}

test_that("Data without Years, or with rows outside Data@Years, is rejected", {
  expect_error(Simulate(.WithData(base$HistYears, DataYears = NULL), silent = TRUE),
               "Years.*not set")
  expect_error(Simulate(.WithData(base$HistYears, DataYears = tail(base$HistYears, 10)), silent = TRUE),
               "not in")
})

test_that("years without data rows are not imputed when conditioning catch and effort", {
  HistYears <- base$HistYears
  for (RowYears in list(head(HistYears, 10), tail(HistYears, 10))) {
    Hist <- Simulate(.WithData(RowYears), silent = TRUE)
    o <- Obs(Hist@OM)[[1]][[1]]
    for (sl in c('Landings', 'Effort')) {
      co  <- slot(o, sl)
      obs <- if (sl == 'Landings') base$L else base$E
      CondYears <- RowYears[obs[as.character(RowYears)] > 0]
      expect_equal(as.numeric(co@Years), as.numeric(CondYears))
      expect_equal(as.numeric(co@Bias)[1], 0.5, tolerance = 1e-6)
      expect_equal(colnames(co@Error), as.character(c(HistYears, Years(Hist, 'Projection'))))
      NoData <- as.character(setdiff(HistYears, RowYears))
      expect_true(all(is.na(co@Error[, NoData])))
    }
  }
})

test_that("Obs@Years restricts Bias to those years and Error spans all historical years", {
  HistYears <- base$HistYears
  ObsYears  <- tail(HistYears, 10)
  Hist <- Simulate(.WithData(HistYears, ObsYears = ObsYears), silent = TRUE)
  o  <- Obs(Hist@OM)[[1]][[1]]
  OY <- as.character(ObsYears)

  SimL <- apply(Hist@Landings, c('Sim', 'Year'), sum)[, OY]
  expect_equal(as.numeric(o@Landings@Bias),
               unname(.MeanRatio(matrix(base$L[OY], 3, 10, byrow = TRUE), SimL)), tolerance = 1e-6)
  SimE <- Hist@Effort[, OY, 1]
  expect_equal(as.numeric(o@Effort@Bias),
               unname(.MeanRatio(matrix(base$E[OY], 3, 10, byrow = TRUE), SimE)), tolerance = 1e-6)

  for (co in list(o@Landings, o@Effort)) {
    expect_equal(as.numeric(co@Years), as.numeric(ObsYears))
    expect_true(all(as.character(HistYears) %in% colnames(co@Error)))
  }
})

test_that("OM data rows are matched to projection years by calendar year", {
  DY <- 2017:2029
  d  <- Data(Years = DY, Landings = CatchData(Name = "F1", Value = matrix(seq_along(DY), ncol = 1,
                                              dimnames = list(Year = DY, Fleet = "F1"))))
  expect_equal(.OMDataRow(d, "Landings", 2027), 11L)
  expect_true(is.na(.OMDataRow(d, "Landings", 2016)))
  expect_true(is.na(.OMDataRow(d, "Landings", 2027, "CV")))
  expect_true(is.na(.OMDataRow(NULL, "Landings", 2027)))
})

test_that("real catch and effort supplied for projection years are used when Data@Years starts late", {
  HistYears <- base$HistYears
  PY <- head(Years(base$OM, 'Projection'), 3)
  DY <- c(tail(HistYears, 10), PY)
  OM <- base$OM
  L  <- c(base$L[as.character(tail(HistYears, 10))], 111, 222, 333)
  E  <- c(base$E[as.character(tail(HistYears, 10))], 0.11, 0.22, 0.33)
  mm <- function(v) matrix(v, ncol = 1, dimnames = list(Year = DY, Fleet = base$FleetName))
  Data(OM) <- Data(Name = 'Real', Years = DY,
                   Landings = CatchData(Name = base$FleetName, Units = 'Biomass', Value = mm(L)),
                   Effort   = EffortData(Name = base$FleetName, Value = mm(E)))
  MSE <- Project(Simulate(OM, silent = TRUE), 'CurrentCatch', silent = TRUE)
  for (d in Data(MSE)[[1]]) {
    expect_equal(unname(d[[1]]@Landings@Value[as.character(PY), 1]), c(111, 222, 333))
    expect_equal(unname(d[[1]]@Effort@Value[as.character(PY), 1]), c(0.11, 0.22, 0.33))
  }
})
