# User-supplied observation settings for projection years, and MaxGap for
# data series that end before the last historical year.

skip_on_cran()

data(SingleStockOM, envir = environment())
OM0 <- SingleStockOM
nSim(OM0) <- 3
HistYears <- Years(OM0, 'Historical')
ProjYears <- Years(OM0, 'Projection')
LastHist  <- max(HistYears)
FleetName <- FleetNames(OM0)[1]

set.seed(101)
IndexValue <- setNames(runif(length(HistYears), 0.5, 1.5), HistYears)

.SurveyOM <- function(Gap = 0, IndexObs = IndicesObs()) {
  v <- IndexValue
  v[HistYears > LastHist - Gap] <- NA
  AddIndex(OM0, v, Name = 'Surv', Selectivity = 'Biomass', IndexObs = IndexObs)
}

# number of finite projected survey values, per sim
.nProjSurvey <- function(OM) {
  MSE <- Project(Simulate(OM, silent = TRUE), MPs = 'refFCurr', silent = TRUE)
  vapply(Data(MSE)[[1]], \(d) {
    val <- d[[1]]@Survey@Value
    sum(is.finite(val[as.numeric(rownames(val)) > LastHist, 'Surv']))
  }, numeric(1))
}

test_that("indices are projected up to MaxGap missing years (default 5)", {
  expect_true(all(.nProjSurvey(.SurveyOM(5)) == length(ProjYears)))
  expect_true(all(.nProjSurvey(.SurveyOM(6)) == 0))
  expect_true(all(.nProjSurvey(.SurveyOM(6, IndicesObs(MaxGap = 10))) == length(ProjYears)))
  expect_true(all(.nProjSurvey(.SurveyOM(15, IndicesObs(MaxGap = Inf))) == length(ProjYears)))
  expect_true(all(.nProjSurvey(.SurveyOM(1, IndicesObs(MaxGap = 0))) == 0))
  expect_true(all(.nProjSurvey(.SurveyOM(0, IndicesObs(MaxGap = 0))) == length(ProjYears)))
})

test_that("the observation gap is counted in calendar years for seasonal time steps", {
  Years <- seq(2010, 2020.75, by = 0.25)
  Obs   <- Years <= 2015.75
  expect_equal(.ObsGap(Years, Obs, 2020.75), 5)
  expect_equal(.ObsGap(Years, Years <= 2014.5, 2020.75), 6)
  expect_equal(.ObsGap(Years, rep(FALSE, length(Years)), 2020.75), Inf)
  expect_equal(.ObsGap(c(Years, 2021, 2022), c(Obs, TRUE, TRUE), 2020.75), 5)
})

test_that("MaxGap is validated", {
  expect_error(IndicesObs(MaxGap = -1), "MaxGap")
  expect_error(CompObs(MaxGap = c(1, 2)), "MaxGap")
  expect_true(is.na(IndicesObs(MaxGap = NA)@MaxGap))
})

test_that("user-supplied index CV and AC are kept for projection years", {
  Hist <- Simulate(.SurveyOM(IndexObs = IndicesObs(CV = 0.05, AC = 0)), silent = TRUE)
  io   <- Hist@OM@Obs[[1]][['Surv']]@Survey

  expect_equal(unique(as.numeric(io@CV)), 0.05)
  expect_equal(unique(as.numeric(io@AC)), 0)
  expect_true(all(io@Stats$SD > 0.2))

  ProjSD <- apply(log(io@Error[, as.character(ProjYears)]), 1, sd)
  expect_true(all(abs(ProjSD - sdconv(1, 0.05)) < 0.03))

  # historical errors still reproduce the observed index
  HistData <- Hist@Data[[1]][[1]]@Survey@Value[, 'Surv']
  expect_equal(unname(HistData[names(IndexValue)]), unname(IndexValue), tolerance = 1e-6)
})

test_that("conditioned index CV and AC replace values the user did not supply", {
  Hist <- Simulate(.SurveyOM(), silent = TRUE)
  io   <- Hist@OM@Obs[[1]][['Surv']]@Survey
  expect_equal(as.numeric(io@AC), io@Stats$AC)
  expect_equal(as.numeric(io@CV), sqrt(exp(io@Stats$SD^2) - 1))
})

test_that("a user-supplied index Error is used for projection years", {
  AllYears <- c(HistYears, ProjYears)
  Error <- array(1.1, dim = c(1, length(AllYears)), dimnames = list(Sim = 1, Year = AllYears))
  Hist  <- Simulate(.SurveyOM(IndexObs = IndicesObs(Error = Error)), silent = TRUE)
  io    <- Hist@OM@Obs[[1]][['Surv']]@Survey
  expect_true(all(io@Error[, as.character(ProjYears)] == 1.1))
  expect_false(all(io@Error[, as.character(HistYears)] == 1.1))
})

test_that("user-supplied catch CV and Bias are kept for projection years", {
  H0 <- Simulate(OM0, silent = TRUE)
  Landings <- apply(H0@Landings, c('Sim', 'Year'), sum)[1, ]
  Landings <- Landings[as.character(HistYears)] * exp(rnorm(length(HistYears), 0, 0.2))
  OM <- OM0
  Data(OM) <- Data(Years = HistYears, Landings = CatchData(Name = FleetName, Units = 'Biomass',
                                        Value = matrix(Landings, ncol = 1,
                                                       dimnames = list(Year = HistYears, Fleet = FleetName))))

  Fit <- Simulate(OM, silent = TRUE)@OM@Obs[[1]][[FleetName]]@Landings
  expect_false(isTRUE(all.equal(unique(as.numeric(Fit@CV)), 0.02)))

  OM@Obs[[1]][[FleetName]]@Landings <- CatchObs(CV = 0.02, Bias = rep(1.2, nSim(OM)))
  co <- Simulate(OM, silent = TRUE)@OM@Obs[[1]][[FleetName]]@Landings
  expect_equal(unique(as.numeric(co@CV)), 0.02)
  expect_equal(unique(as.numeric(co@Bias)), 1.2)
  ProjSD <- apply(log(co@Error[, as.character(ProjYears)]), 1, sd)
  expect_true(all(abs(ProjSD - sdconv(1, 0.02)) < 0.01))
})

test_that("user-supplied effort CV and Bias are kept for projection years", {
  H0 <- Simulate(OM0, silent = TRUE)
  Effort <- H0@Effort[1, as.character(HistYears), 1] * exp(rnorm(length(HistYears), 0, 0.2))
  OM <- OM0
  Data(OM) <- Data(Years = HistYears,
                   Effort = EffortData(Name = FleetName,
                                       Value = matrix(Effort, ncol = 1,
                                                      dimnames = list(Year = HistYears, Fleet = FleetName))))

  OM@Obs[[1]][[FleetName]]@Effort <- EffortObs(CV = 0.02, Bias = rep(0.8, nSim(OM)))
  eo <- Simulate(OM, silent = TRUE)@OM@Obs[[1]][[FleetName]]@Effort
  expect_equal(unique(as.numeric(eo@CV)), 0.02)
  expect_equal(unique(as.numeric(eo@Bias)), 0.8)
  ProjSD <- apply(log(eo@Error[, as.character(ProjYears)]), 1, sd)
  expect_true(all(abs(ProjSD - sdconv(1, 0.02)) < 0.01))
})

# Sim-1 catch-at-age as [Year x Fleet x Age] counts, all-NA from `EndGap` years before the last historical year
.CompOM <- function(H0, EndGap = 0, CompObs = NULL) {
  Ages <- as.numeric(dimnames(H0@LandingsAtAge[[1]])$Age)
  caa  <- H0@LandingsAtAge[[1]][1, , , 1, , drop = FALSE]
  dn   <- names(dimnames(caa))
  obs  <- apply(caa, match(c('Year', 'Age'), dn), sum)[as.character(HistYears), ]
  comp <- array(round(obs / rowSums(obs) * 200), dim = c(length(HistYears), 1, length(Ages)),
                dimnames = list(Year = HistYears, Fleet = FleetName, Age = Ages))
  comp[HistYears > LastHist - EndGap, , ] <- NA
  OM <- OM0
  Data(OM) <- Data(Name = 'Real', Years = HistYears,
                   LandingsAtAge = CompData(Name = FleetName, Value = comp, Classes = Ages,
                                            Units = 'years'))
  if (!is.null(CompObs))
    OM@Obs[[1]][[FleetName]]@LandingsAtAge <- CompObs
  OM
}

H0 <- Simulate(OM0, silent = TRUE)

test_that("user-supplied SampleSize and ESS are kept for projection years", {
  co <- Simulate(.CompOM(H0, CompObs = CompObs(SampleSize = 50, ESS = 20)), silent = TRUE)@
    OM@Obs[[1]][[FleetName]]@LandingsAtAge
  expect_true(all(co@SampleSize[, as.character(ProjYears)] == 50))
  expect_true(all(co@ESS[, as.character(ProjYears)] == 20))
  expect_gt(stats::median(co@SampleSize[, as.character(HistYears)]), 150)

  co <- Simulate(.CompOM(H0, CompObs = CompObs(SampleSize = 50)), silent = TRUE)@
    OM@Obs[[1]][[FleetName]]@LandingsAtAge
  expect_true(all(co@SampleSize[, as.character(ProjYears)] == 50))
  expect_true(all(co@ESS[, as.character(ProjYears)] <= 50))

  co <- Simulate(.CompOM(H0), silent = TRUE)@OM@Obs[[1]][[FleetName]]@LandingsAtAge
  expect_true(all(co@SampleSize[, as.character(ProjYears)] > 150))
})

# number of projected years with catch-at-age, per sim
.nProjCAA <- function(OM) {
  MSE <- Project(Simulate(OM, silent = TRUE), MPs = 'refFCurr', silent = TRUE)
  vapply(Data(MSE)[[1]], \(d) {
    val <- d[[1]]@LandingsAtAge@Value
    tot <- apply(val[as.numeric(dimnames(val)$Year) > LastHist, 1, , drop = FALSE], 1, sum)
    sum(is.finite(tot) & tot > 0)
  }, numeric(1))
}

test_that("composition data are projected up to MaxGap missing years", {
  expect_true(all(.nProjCAA(.CompOM(H0, EndGap = 5)) == length(ProjYears)))
  expect_true(all(.nProjCAA(.CompOM(H0, EndGap = 8)) == 0))
  expect_true(all(.nProjCAA(.CompOM(H0, EndGap = 8, CompObs(MaxGap = Inf))) == length(ProjYears)))
})

test_that("MaxGap() gets and sets the slot on index and composition obs", {
  io <- IndicesObs()
  expect_null(MaxGap(io))
  MaxGap(io) <- 10
  expect_equal(MaxGap(io), 10)
  MaxGap(io) <- NA
  expect_true(is.na(MaxGap(io)))
  expect_error(MaxGap(io) <- -1, "MaxGap")

  co <- CompObs(SampleSize = 100)
  MaxGap(co) <- Inf
  expect_equal(MaxGap(co), Inf)
  expect_error(MaxGap(CatchObs()), "MaxGap")
})
