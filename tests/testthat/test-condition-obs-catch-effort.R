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
