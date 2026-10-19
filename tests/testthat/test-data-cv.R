# CV reported in the CV slot of generated catch, effort, and index data.

skip_on_cran()

data(SingleStockOM, envir = environment())
OM0 <- SingleStockOM
nSim(OM0) <- 2
HistYears <- Years(OM0, 'Historical')
ProjYears <- Years(OM0, 'Projection')
FleetName <- FleetNames(OM0)[1]

# reported CVs of a data series, sim 1, split into historical and projection years
.ReportedCV <- function(OM, type, column = FleetName, control = SimControl()) {
  MSE <- Project(Simulate(OM, silent = TRUE, control = control), MPs = 'refFCurr', silent = TRUE)
  CV  <- slot(Data(MSE)[[1]][[1]][[1]], type)@CV[, column]
  Yr  <- as.numeric(names(CV))
  list(Hist = CV[Yr %in% HistYears], Proj = CV[Yr %in% ProjYears])
}

test_that("SimControl(DataCV) sets the default reported CV", {
  CV <- .ReportedCV(OM0, 'Landings', control = SimControl(DataCV = 0.35))
  expect_true(all(CV$Hist == 0.35))
  expect_true(all(CV$Proj == 0.35))

  CV <- .ReportedCV(OM0, 'Survey')
  expect_true(all(CV$Hist == 0.2))
  expect_true(all(CV$Proj == 0.2))
})

test_that("SimControl(DataCV) can be set by data type", {
  control <- SimControl(DataCV = list(Landings = 0.05, Survey = 0.3))
  expect_equal(control$DataCV, c(Effort = 0.2, Landings = 0.05, Discards = 0.2, CPUE = 0.2, Survey = 0.3))

  MSE <- Project(Simulate(OM0, silent = TRUE, control = control), MPs = 'refFCurr', silent = TRUE)
  d <- Data(MSE)[[1]][[1]][[1]]
  expect_true(all(d@Landings@CV[, FleetName] == 0.05))
  expect_true(all(d@Discards@CV[, FleetName] == 0.2))
  expect_true(all(d@Survey@CV[, FleetName] == 0.3))

  expect_error(SimControl(DataCV = list(Landing = 0.1)), "DataCV")
  expect_error(SimControl(DataCV = c(0.1, 0.2)), "DataCV")
})

test_that("an Obs DataCV overrides the default, including by year", {
  OM <- OM0
  OM@Obs[[1]][[FleetName]]@Landings@DataCV <- 0.05
  CV <- .ReportedCV(OM, 'Landings')
  expect_true(all(CV$Hist == 0.05))
  expect_true(all(CV$Proj == 0.05))

  Change <- HistYears[10]
  OM@Obs[[1]][[FleetName]]@Survey@DataCV <- stats::setNames(c(0.4, 0.1), c(min(HistYears), Change))
  CV <- .ReportedCV(OM, 'Survey')
  expect_true(all(CV$Hist[as.numeric(names(CV$Hist)) < Change] == 0.4))
  expect_true(all(CV$Hist[as.numeric(names(CV$Hist)) >= Change] == 0.1))
  expect_true(all(CV$Proj == 0.1))
})

test_that("real index CVs are kept, with missing ones and projections from the default or DataCV", {
  set.seed(7)
  Value <- stats::setNames(runif(length(HistYears), 0.5, 1.5), HistYears)
  RealCV <- stats::setNames(rep(0.15, length(HistYears)), HistYears)
  RealCV[1:3] <- NA
  OM <- AddIndex(OM0, Value, Name = 'Surv', Selectivity = 'Biomass', CV = RealCV)

  CV <- .ReportedCV(OM, 'Survey', 'Surv', control = SimControl(DataCV = 0.3))
  expect_true(all(CV$Hist[1:3] == 0.3))
  expect_true(all(CV$Hist[-(1:3)] == 0.15))
  expect_true(all(CV$Proj == 0.15))

  OM@Obs[[1]][['Surv']]@Survey@DataCV <- 0.25
  CV <- .ReportedCV(OM, 'Survey', 'Surv')
  expect_true(all(CV$Hist[1:3] == 0.25))
  expect_true(all(CV$Hist[-(1:3)] == 0.15))
  expect_true(all(CV$Proj == 0.25))
})

test_that("DataCV is validated", {
  expect_error(SimControl(DataCV = -1), "DataCV")
  expect_error(CatchObs(DataCV = c(0.1, 0.2)), "DataCV")
  expect_error(IndicesObs(DataCV = -0.1), "DataCV")
  expect_equal(.DataCVYears(CatchObs(DataCV = c('2010' = 0.3, '2000' = 0.5)), c(1999, 2000, 2009, 2010, 2020), 0.2),
               c(0.2, 0.5, 0.5, 0.3, 0.3))
})

test_that("DataCV() gets and sets the slot on catch, effort, and index obs", {
  for (obj in list(CatchObs(), EffortObs(), IndicesObs())) {
    expect_null(DataCV(obj))
    DataCV(obj) <- c('2000' = 0.3, '2010' = 0.1)
    expect_equal(DataCV(obj), c('2000' = 0.3, '2010' = 0.1))
    expect_error(DataCV(obj) <- -0.1, "DataCV")
  }
  expect_error(DataCV(CompObs()), "DataCV")
})
