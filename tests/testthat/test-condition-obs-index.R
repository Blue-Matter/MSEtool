# Coverage for the index observation-conditioning pipeline (R/condition-obs-index.R,
# R/index-nominal.R, R/plot-index-fit.R). The `EstimateBeta()`/`CalcIndexResiduals()`
# tests are pure-function and need no OM. The wiring tests are real Simulate() calls
# (small, nSim = 1) and follow the skip_on_cran() convention used elsewhere -- see
# tests/testthat/setup.R for why a bare test_dir() run skips them.

# ---- EstimateBeta() ----

test_that("EstimateBeta fixes Beta at the user-supplied value", {
  logSim <- matrix(rnorm(2 * 10), nrow = 2)
  fit <- EstimateBeta(logObs = rnorm(10), logSim = logSim, beta = 0.7)

  expect_equal(fit$Beta, c(0.7, 0.7))
  expect_equal(fit$Status, c("fixed_user", "fixed_user"))
  expect_true(all(is.na(fit$R2)))
  expect_true(all(is.na(fit$PValue)))
})

test_that("EstimateBeta fixes Beta at 1 with too few usable points", {
  logSim <- matrix(rnorm(5), nrow = 1)
  fit <- EstimateBeta(logObs = rnorm(5), logSim = logSim, MinPoints = 8)

  expect_equal(fit$Beta, 1)
  expect_equal(fit$Status, "fixed_insufficient_data")
  expect_equal(fit$nPoints, 5)
})

test_that("EstimateBeta fixes Beta at 1 for a flat (near-zero-variance) nominal index", {
  n <- 10
  logSim <- matrix(rep(0, n), nrow = 1)
  fit <- EstimateBeta(logObs = rnorm(n), logSim = logSim)

  expect_equal(fit$Beta, 1)
  expect_equal(fit$Status, "fixed_low_variance")
  expect_true(is.na(fit$R2))
})

test_that("EstimateBeta recovers a strong, significant hyperstability signal", {
  set.seed(1)
  n <- 20
  x <- seq(0, 2, length.out = n)
  trueBeta <- 0.5
  logSim <- matrix(x, nrow = 1)
  logObs <- trueBeta * x + rnorm(n, sd = 0.01)

  fit <- EstimateBeta(logObs, logSim)

  expect_equal(fit$Status, "estimated")
  expect_equal(fit$Beta, trueBeta, tolerance = 0.05)
  expect_true(fit$PValue < 0.05)
  expect_true(fit$R2 > 0.9)
  expect_false(is.na(fit$SE_Beta))
  expect_true(fit$CI_Lower < trueBeta && trueBeta < fit$CI_Upper)
})

test_that("EstimateBeta fixes Beta at 1 when the true relationship is proportional (Beta = 1)", {
  set.seed(1)
  n <- 20
  x <- seq(0, 2, length.out = n)
  logSim <- matrix(x, nrow = 1)
  logObs <- x + rnorm(n, sd = 0.3)

  fit <- EstimateBeta(logObs, logSim)

  expect_equal(fit$Status, "fixed_not_significant")
  expect_equal(fit$Beta, 1)
})

test_that("EstimateBeta clamps an extreme, significant slope and flags fixed_bounds", {
  set.seed(1)
  n <- 20
  x <- seq(0.01, 0.5, length.out = n)
  trueBeta <- 5
  logSim <- matrix(x, nrow = 1)
  logObs <- trueBeta * x + rnorm(n, sd = 0.001)

  fit <- EstimateBeta(logObs, logSim)

  expect_equal(fit$Status, "fixed_bounds")
  expect_equal(fit$Beta, 3)
})

test_that("EstimateBeta handles simulations independently", {
  set.seed(1)
  n <- 20
  x <- seq(0, 2, length.out = n)
  logObs <- 0.5 * x
  logSim <- rbind(x, rep(0, n))

  fit <- EstimateBeta(logObs, logSim)

  expect_equal(unname(fit$Status), c("estimated", "fixed_low_variance"))
  expect_equal(unname(fit$Beta[2]), 1)
})

# ---- CalcIndexResiduals() ----

test_that("CalcIndexResiduals recovers Efficiency = 1 and zero residuals for an exact fit", {
  n <- 20
  nomIndex <- matrix(exp(seq(0, 1, length.out = n)), nrow = 1)
  observed <- as.numeric(nomIndex[1, ])

  res <- CalcIndexResiduals(observed, nomIndex, beta = 1)

  expect_equal(res$Beta, 1)
  expect_equal(res$Efficiency, 1, tolerance = 1e-8)
  expect_true(all(abs(res$LogResiduals) < 1e-8))
})

# ---- Wiring: SimControl(EstimateBeta) through Simulate() ----

test_that("SimControl(EstimateBeta = FALSE/TRUE) is wired through a real Simulate() run", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)

  stocks    <- 1L
  fl        <- FleetNames(hist0)[1]
  HistYears <- Years(hist0, 'H')
  ProjYears <- Years(hist0, 'P')
  expect_gte(length(HistYears), 8L)

  # Build a synthetic "real" index with a known Beta from the true nominal
  # index (via the same shared .CalcNomIndex() core used by conditioning),
  # so estimation has a noiseless target to recover.
  NomIndex <- .CalcNomIndex(
    Number_List = hist0@Number[stocks],
    object      = hist0,
    stocks      = stocks,
    fleet       = fl,
    IndexObs    = IndicesObs(),
    Years       = HistYears
  )
  expect_true(all(is.finite(NomIndex)) && all(NomIndex > 0))

  trueBeta  <- 0.6
  synthetic <- as.numeric(NomIndex[1, ])^trueBeta
  synthetic <- synthetic / mean(synthetic)

  om@Data <- list(Data(
    Years = c(HistYears, ProjYears),
    CPUE  = IndicesData(Name = fl,
                        Value = matrix(synthetic, ncol = 1,
                                       dimnames = list(as.character(HistYears), fl)))
  ))

  set.seed(1)
  hist_off <- Simulate(om, control = SimControl(EstimateBeta = FALSE), silent = TRUE)
  fitted_off <- hist_off@OM@Obs[[1]][[fl]]@CPUE
  expect_equal(as.numeric(fitted_off@Beta), 1)

  set.seed(1)
  hist_on <- Simulate(om, control = SimControl(EstimateBeta = TRUE), silent = TRUE)
  fitted_on <- hist_on@OM@Obs[[1]][[fl]]@CPUE
  expect_equal(as.numeric(fitted_on@Beta), trueBeta, tolerance = 0.05)
  expect_equal(unname(fitted_on@Misc$BetaFit$Status), "estimated")
  expect_false(is.na(fitted_on@Misc$BetaFit$R2))

  tbl <- IndexFitTable(hist_on, type = 'CPUE', print = FALSE)
  expect_true(all(c("R2", "PValue", "nYears", "Status") %in% colnames(tbl)))
  expect_false("Efficiency" %in% colnames(tbl))
  expect_equal(tail(colnames(tbl), 4), c("Beta", "R2", "PValue", "Status"))
  expect_equal(tbl$Status[1], "estimated")

  p <- PlotIndexFit(hist_on, type = 'CPUE')
  expect_s3_class(p, "ggplot")
})

test_that(".GenHistDataIndices() handles real data per fleet, not per index type", {
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1
  # AsympExFleet has no default Obs@CV/@Error in MultiStockOM; give it one so
  # it's a genuine simulation candidate, matching DomeExFleet's default.
  om@Obs[[1]][["AsympExFleet"]]@CPUE <- IndicesObs(CV = 0.2)

  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  stocks <- hist0@OM@Complexes[[1]]
  HistYears <- Years(hist0, 'H')
  ProjYears <- Years(hist0, 'P')
  AllYears  <- c(HistYears, ProjYears)

  NomIndex <- .CalcNomIndex(
    Number_List = hist0@Number[stocks], object = hist0, stocks = stocks,
    fleet = "DomeExFleet", IndexObs = IndicesObs(), Years = HistYears
  )
  synthetic <- as.numeric(NomIndex[1, ])
  synthetic <- synthetic / mean(synthetic)
  blank2 <- Data(Years = AllYears)

  # Scenario A: DomeExFleet has real data; AsympExFleet has no real column at
  # all (not even NA) -- it should still be simulated from its own Obs@CV,
  # not silently skipped because a sibling fleet has real data.
  om@Data <- list(
    Data(Years = AllYears,
         CPUE = IndicesData(Name = "DomeExFleet",
                            Value = matrix(synthetic, ncol = 1,
                                           dimnames = list(as.character(HistYears), "DomeExFleet")))),
    blank2
  )
  set.seed(1)
  histA <- Simulate(om, silent = TRUE)
  simDataA <- histA@Data[["1"]][[1]]@CPUE
  expect_true("AsympExFleet" %in% colnames(simDataA@Value))
  expect_false(all(is.na(simDataA@Value[, "AsympExFleet"])))

  # Scenario B: AsympExFleet has a real column that is entirely NA -- the
  # user supplied it but has no observations. It must stay unchanged: no
  # conditioning (Beta/Efficiency stay unset) and no simulated fill-in.
  Val <- cbind(rep(NA_real_, length(HistYears)), synthetic)
  dimnames(Val) <- list(as.character(HistYears), c("AsympExFleet", "DomeExFleet"))
  om2 <- om
  om2@Data <- list(
    Data(Years = AllYears, CPUE = IndicesData(Name = c("AsympExFleet", "DomeExFleet"), Value = Val)),
    blank2
  )
  set.seed(1)
  histB <- Simulate(om2, silent = TRUE)
  obsA <- histB@OM@Obs[[1]][["AsympExFleet"]]@CPUE
  expect_true(is.null(obsA@Beta))
  simDataB <- histB@Data[["1"]][[1]]@CPUE
  expect_true(all(is.na(simDataB@Value[, "AsympExFleet"])))
  obsD <- histB@OM@Obs[[1]][["DomeExFleet"]]@CPUE
  expect_false(is.null(obsD@Beta))
})

test_that("PlotIndexFit() applies Timing decay for mse objects, historical and projection", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)

  fl <- FleetNames(hist0)[1]
  HistYears <- Years(hist0, 'H')
  ProjYears <- Years(hist0, 'P')
  AllYears  <- c(HistYears, ProjYears)

  NomIndex <- .CalcNomIndex(
    Number_List = hist0@Number[1], object = hist0, stocks = 1, fleet = fl,
    IndexObs = IndicesObs(), Years = HistYears
  )
  synthetic <- as.numeric(NomIndex[1, ])
  synthetic <- synthetic / mean(synthetic)

  # Timing = 0.5 (mid-timestep) so decay has something to do.
  om@Data <- list(Data(
    Years = AllYears,
    CPUE  = IndicesData(Name = fl, Timing = 0.5,
                        Value = matrix(synthetic, ncol = 1,
                                       dimnames = list(as.character(HistYears), fl)))
  ))

  set.seed(1)
  hist_t <- Simulate(om, silent = TRUE)
  mse <- Project(hist_t, MPs = "NFref", parallel = FALSE, silent = TRUE)

  # Must not error -- mse@Hist is a stripped `timeseries` object with no
  # @OM, and mse@FDeadArea carries an extra MP dimension; both used to break
  # PlotIndexFit() for mse objects before this fix.
  p <- expect_no_error(PlotIndexFit(mse, type = 'CPUE'))
  expect_s3_class(p, "ggplot")

  # Decay must be real, not a silent no-op: build the same decay-capable
  # object PlotIndexFit() constructs internally and check the nominal index
  # actually differs from the no-decay case.
  mp <- dimnames(mse@Number[[1]])$MP[1]
  decayObj <- mse
  decayObj@FDeadArea[1] <- purrr::map(mse@FDeadArea[1], \(arr) {
    .ArraySubsetMP(arr, mp) |> DropDimension('MP', warn = FALSE)
  })
  IndexObs <- mse@OM@Obs[[1]][[fl]]@CPUE

  withDecay <- .CalcNomIndex(Number_List = mse@Hist@Number[1], object = decayObj,
                             stocks = 1, fleet = fl, IndexObs = IndexObs,
                             Years = HistYears, timing = 0.5, Units = "Biomass")
  noDecay <- .CalcNomIndex(Number_List = mse@Hist@Number[1], object = decayObj,
                           stocks = 1, fleet = fl, IndexObs = IndexObs,
                           Years = HistYears, timing = NA_real_, Units = "Biomass")
  expect_gt(max(abs(withDecay - noDecay)), 0)
})

test_that(".CalcNomIndex(sim=x) matches the corresponding sim=NULL row for multi-year Years", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  fl <- FleetNames(hist0)[1]
  HistYears <- Years(hist0, 'H')
  expect_gt(length(HistYears), 1L)

  nomAll <- .CalcNomIndex(Number_List = hist0@Number[1], object = hist0, stocks = 1,
                          fleet = fl, IndexObs = IndicesObs(), Years = HistYears)
  nomSim <- .CalcNomIndex(Number_List = hist0@Number[1], object = hist0, stocks = 1,
                          fleet = fl, IndexObs = IndicesObs(), Years = HistYears, sim = 1)

  # A single-sim, multi-year call must return a per-year vector, not a
  # scalar -- `.AggregateNomIndex()` used to collapse Year away too when
  # summing across stocks for the sim-set path.
  expect_length(nomSim, length(HistYears))
  expect_equal(as.numeric(nomSim), as.numeric(nomAll[1, ]))
})

test_that("simulating an index then conditioning on it recovers the generating Beta/SD/AC", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  fl <- FleetNames(om)[1]

  trueBeta <- 0.5
  om@Obs[[1]][[fl]]@CPUE <- IndicesObs(Beta = trueBeta, CV = 0.01, AC = 0)

  set.seed(42)
  hist_sim <- Simulate(om, control = SimControl(GenerateData = TRUE), silent = TRUE)
  HistYears <- Years(hist_sim, 'H')
  ProjYears <- Years(hist_sim, 'P')

  # The simulated series must actually track the true population trend
  # (previously it collapsed to noise around a flat line for the historical
  # period -- see the .CalcNomIndex(sim=x) test above).
  simVal <- hist_sim@Data[["1"]][[1]]@CPUE@Value[, fl]
  NomIndexTrue <- .CalcNomIndex(Number_List = hist_sim@Number[1], object = hist_sim,
                                stocks = 1, fleet = fl, IndexObs = IndicesObs(), Years = HistYears)
  expect_gt(cor(log(simVal), log(as.numeric(NomIndexTrue[1, ]))), 0.99)

  # Feed the simulated series back in as "real" data and re-condition on it.
  om2 <- om
  om2@Obs[[1]][[fl]]@CPUE <- IndicesObs()
  om2@Data <- list(Data(
    Years = c(HistYears, ProjYears),
    CPUE  = IndicesData(Name = fl,
                        Value = matrix(simVal, ncol = 1, dimnames = list(as.character(HistYears), fl)))
  ))

  set.seed(1)
  hist_cond <- Simulate(om2, control = SimControl(EstimateBeta = TRUE), silent = TRUE)
  obsC <- hist_cond@OM@Obs[[1]][[fl]]@CPUE

  expect_equal(as.numeric(obsC@Beta), trueBeta, tolerance = 0.05)
  expect_equal(unname(obsC@Misc$BetaFit$Status), "estimated")
})

test_that("PlotIndexFit() standardizes every simulation's True series to the reference years, not just sim 1", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 5
  fl <- FleetNames(om)[1]
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  HistYears <- Years(hist0, 'H')
  subYears  <- HistYears[(length(HistYears) - 9):length(HistYears)]

  NomIndex <- .CalcNomIndex(Number_List = hist0@Number[1], object = hist0, stocks = 1,
                            fleet = fl, IndexObs = IndicesObs(), Years = subYears, sim = 1)
  synthetic <- as.numeric(NomIndex)
  synthetic <- synthetic / mean(synthetic)

  Value <- matrix(NA_real_, nrow = length(HistYears), ncol = 1,
                  dimnames = list(as.character(HistYears), fl))
  Value[as.character(subYears), 1] <- synthetic
  om@Data <- list(Data(Years = HistYears, CPUE = IndicesData(Name = fl, Value = Value)))

  set.seed(1)
  hist_c <- Simulate(om, silent = TRUE)

  # Real data identical across sims is often stored as a single replicate
  # regardless of nSim (this is how .GenerateHistoricalData() de-duplicates
  # it) -- force that here since it's the scenario that broke.
  hist_c@Data <- list("1" = hist_c@Data[[1]])

  df <- MSEtool:::.BuildIndexFitDF(hist_c, 'CPUE')
  for (s in unique(df$Sim[df$Series == 'True'])) {
    sub <- df[df$Sim == s & df$Period == 'Historical' & df$Series == 'True' & df$Year %in% subYears, ]
    expect_equal(mean(sub$Value), 1, tolerance = 1e-8, label = paste0("sim ", s, " mean over reference years"))
  }
})

test_that("IndexFitTable() drops all-NA columns and has no Efficiency column", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  fl <- FleetNames(om)[1]
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  HistYears <- Years(hist0, 'H')
  ProjYears <- Years(hist0, 'P')

  NomIndex <- .CalcNomIndex(Number_List = hist0@Number[1], object = hist0, stocks = 1,
                            fleet = fl, IndexObs = IndicesObs(), Years = HistYears)
  synthetic <- as.numeric(NomIndex[1, ])
  synthetic <- synthetic / mean(synthetic)
  om@Data <- list(Data(
    Years = c(HistYears, ProjYears),
    CPUE  = IndicesData(Name = fl,
                        Value = matrix(synthetic, ncol = 1, dimnames = list(as.character(HistYears), fl)))
  ))

  # SimControl's default EstimateBeta = FALSE -> Beta fixed everywhere
  # ("fixed_user" status, matching a real conditioned run), so R2/PValue are
  # NA for every row and should be dropped.
  set.seed(1)
  hist_c <- Simulate(om, silent = TRUE)
  tbl <- IndexFitTable(hist_c, type = 'CPUE', print = FALSE)

  expect_false("Efficiency" %in% colnames(tbl))
  expect_false(any(c("R2", "PValue") %in% colnames(tbl)))
  expect_true(all(c("Stock", "Fleet", "Beta", "Status") %in% colnames(tbl)))
  expect_equal(tbl$Status[1], "fixed_user")
})

test_that("PlotIndexFit() keeps the Index on its natural scale and scales True to match", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  fl <- FleetNames(om)[1]
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  HistYears <- Years(hist0, 'H')
  subYears  <- HistYears[(length(HistYears) - 9):length(HistYears)]

  NomIndex <- .CalcNomIndex(Number_List = hist0@Number[1], object = hist0, stocks = 1,
                            fleet = fl, IndexObs = IndicesObs(), Years = subYears)
  # A deliberately non-mean-1 natural scale (e.g. CPUE units around 5.5),
  # so a bug that quietly re-normalizes the Index back to mean 1 shows up.
  natScale  <- 5.5
  synthetic <- as.numeric(NomIndex[1, ])
  synthetic <- synthetic / mean(synthetic) * natScale

  Value <- matrix(NA_real_, nrow = length(HistYears), ncol = 1,
                  dimnames = list(as.character(HistYears), fl))
  Value[as.character(subYears), 1] <- synthetic
  om@Data <- list(Data(Years = HistYears, CPUE = IndicesData(Name = fl, Value = Value)))

  set.seed(1)
  hist_c <- Simulate(om, silent = TRUE)
  df  <- MSEtool:::.BuildIndexFitDF(hist_c, 'CPUE')
  sub <- df[df$Period == 'Historical' & df$Year %in% subYears, ]

  # Index untouched, on its own units -- not renormalized to mean 1.
  expect_equal(sub$Value[sub$Series == 'Index'], synthetic, tolerance = 1e-8)
  # True scaled to share the Index's mean over the same years.
  expect_equal(mean(sub$Value[sub$Series == 'True']), natScale, tolerance = 1e-8)
})
