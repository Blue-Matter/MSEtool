# Projected CPUE and Survey index values continue the historical observation
# process: same Efficiency and Beta, and errors with the same mean, SD and AC.

# [Sim, Year, Period, Value, Nom, Resid, LogError] for one index of the first MP
.IndexResiduals <- function(mse, type, cx, fl) {
  Complexes <- .ResolveComplexes(mse@OM)
  cxName <- names(Complexes)[cx]
  io  <- slot(mse@OM@Obs[[cx]][[fl]], type)
  idx <- .ExtractIndexSeries(mse, TRUE, type, cxName, fl)
  nom <- .ExtractTrueSeries(mse, TRUE, type, cxName, fl, Complexes)
  idx <- idx[idx$MP %in% c("Historical", names(mse@PPD)[1]), c("Sim", "Year", "Period", "Value")]
  nom <- nom[nom$MP %in% c("Historical", names(mse@PPD)[1]), c("Sim", "Year", "Value")]
  idx$Sim <- as.numeric(idx$Sim)
  nom$Sim <- as.numeric(nom$Sim)
  names(nom)[3] <- "Nom"
  d <- merge(idx[is.finite(idx$Value), ], nom, by = c("Sim", "Year"))
  Beta <- .SimValues(io@Beta %||% 1, nSim(mse))
  Eff  <- .SimValues(io@Efficiency, nSim(mse))
  d$Resid    <- log(d$Value) - log(Eff[d$Sim]) - Beta[d$Sim] * log(d$Nom)
  d$LogError <- log(io@Error[cbind(d$Sim, match(as.character(d$Year), colnames(io@Error)))])
  d[order(d$Sim, d$Year), ]
}

# per-sim moments of log residuals about `mu`, scaled by `var`
.ResidMoments <- function(d, mu, var = 1) {
  sims <- sort(unique(d$Sim))
  mu  <- rep_len(mu, max(sims))
  var <- rep_len(var, max(sims))
  do.call(rbind, lapply(sims, \(s) {
    r <- d$Resid[d$Sim == s]
    z <- r - mu[s]
    data.frame(Sim = s,
               MeanExp = mean(exp(r)),
               Var     = mean(z^2) / var[s],
               Cov1    = mean(z[-1] * z[-length(z)]) / var[s])
  }))
}

# mean of per-sim values within 4 standard errors of `target`
.ExpectMeanNear <- function(x, target, label) {
  se <- sd(x) / sqrt(length(x))
  expect_true(abs(mean(x) - target) < 4 * se,
              label = sprintf("%s: mean %.4f, expected %.4f (4 SE = %.4f)",
                              label, mean(x), target, 4 * se))
}

.ExpectSameCatchability <- function(d, label) {
  expect_setequal(unique(d$Period), c("Historical", "Projection"))
  expect_equal(d$Resid, d$LogError, tolerance = 1e-8, label = label)
}

# stationary AR(1) log errors with marginal SD sdconv(1, CV) and mean mconv(1, CV)
.AR1LogError <- function(n, CV, AC) {
  s <- sdconv(1, CV)
  z <- numeric(n)
  z[1] <- rnorm(1, 0, s)
  for (t in seq_len(n)[-1]) z[t] <- AC * z[t - 1] + rnorm(1, 0, s * sqrt(1 - AC^2))
  z + mconv(1, CV)
}

.CachedFixture <- function(build) {
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- build()
    cache
  }
}

# ---- simulated indices ----

.SimInputs <- list(CPUE   = list(CV = 0.5,  AC = 0.7, Beta = 0.6),
                   Survey = list(CV = 0.25, AC = 0,   Beta = 1.3))

.SimulatedFixture <- .CachedFixture(function() {
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim  <- 100
  om@pYear <- 30
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@CPUE   <- do.call(IndicesObs, .SimInputs$CPUE)
  om@Obs[[1]][[fl]]@Survey <- do.call(IndicesObs, c(.SimInputs$Survey, Selectivity = "Biomass"))
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  set.seed(2)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  list(hist = hist, mse = mse, fl = fl)
})

test_that("simulated CPUE and Survey indices use the same Efficiency and Beta in historical and projection years", {
  skip_on_cran()
  fx <- .SimulatedFixture()
  for (type in c("CPUE", "Survey")) {
    io <- slot(fx$hist@OM@Obs[[1]][[fx$fl]], type)
    expect_equal(.SimValues(io@Beta, nSim(fx$hist)), rep(.SimInputs[[type]]$Beta, nSim(fx$hist)))
    expect_length(io@Efficiency, nSim(fx$hist))
    d <- .IndexResiduals(fx$mse, type, 1, fx$fl)
    expect_equal(as.numeric(table(d$Period)),
                 nSim(fx$hist) * c(length(Years(fx$hist, "H")), length(Years(fx$hist, "P"))))
    .ExpectSameCatchability(d, type)
    # historical values are standardized to mean 1 by the same Efficiency
    Hist <- d[d$Period == "Historical", ]
    expect_equal(as.numeric(tapply(Hist$Value, Hist$Sim, mean)), rep(1, nSim(fx$hist)),
                 tolerance = 1e-10)
  }
})

test_that("simulated index errors have the same mean, SD and lag-1 autocorrelation in historical and projection years", {
  skip_on_cran()
  fx <- .SimulatedFixture()
  for (type in c("CPUE", "Survey")) {
    inp   <- .SimInputs[[type]]
    sigma <- sdconv(1, inp$CV)
    d <- .IndexResiduals(fx$mse, type, 1, fx$fl)
    Mom <- lapply(c(Historical = "Historical", Projection = "Projection"), \(p)
      .ResidMoments(d[d$Period == p, ], mconv(1, inp$CV), sigma^2))
    for (p in names(Mom)) {
      lab <- paste(type, p)
      .ExpectMeanNear(Mom[[p]]$MeanExp, 1, paste(lab, "E[error]"))
      .ExpectMeanNear(Mom[[p]]$Var, 1, paste(lab, "Var(log error) / sigma^2"))
      .ExpectMeanNear(Mom[[p]]$Cov1, inp$AC, paste(lab, "lag-1 AC"))
    }
    for (m in c("MeanExp", "Var", "Cov1"))
      .ExpectMeanNear(Mom$Projection[[m]] - Mom$Historical[[m]], 0, paste(type, m, "projection - historical"))
  }
})

# ---- indices conditioned on real data ----

.RealInputs <- list(CPUE   = list(q = 0.5,  Beta = 0.6, CV = 0.5,  AC = 0.6),
                    Survey = list(q = 2e-4, Beta = 1,   CV = 0.25, AC = 0))

# synthetic real indices from sim 1: CPUE with missing years, Survey for the last 12 years only
.RealIndexData <- .CachedFixture(function() {
  data(SingleStockOM, envir = environment())
  om <- UpdateObject(SingleStockOM)
  om@nSim  <- 100
  om@pYear <- 30
  fl <- FleetNames(om)[1]
  set.seed(1)
  hist0 <- Simulate(om, silent = TRUE)
  HY <- Years(hist0, "H")
  Sel <- list(CPUE = IndicesObs(), Survey = IndicesObs(Selectivity = "Biomass"))
  set.seed(11)
  Real <- lapply(c(CPUE = "CPUE", Survey = "Survey"), \(type) {
    inp <- .RealInputs[[type]]
    Nom <- .CalcNomIndex(hist0@Number[1], hist0, 1, fl, Sel[[type]], HY)[1, ]
    e   <- .AR1LogError(length(HY), inp$CV, inp$AC)
    Obs <- if (type == "CPUE") !seq_along(HY) %in% c(3, 7, 12) else seq_along(HY) > length(HY) - 12
    Val <- inp$q * Nom^inp$Beta * exp(e)
    Val[!Obs] <- NA
    list(Value = setNames(Val, HY), LogError = e[Obs])
  })
  list(om = om, fl = fl, Real = Real, HY = HY, PY = Years(hist0, "P"))
})

.ConditionedOM <- function(rd, CPUEObs = IndicesObs(), SurveyObs = IndicesObs(Beta = 1, Selectivity = "Biomass")) {
  om <- rd$om
  Mat <- \(type) matrix(rd$Real[[type]]$Value, ncol = 1, dimnames = list(rd$HY, rd$fl))
  om@Data <- list(Data(Years = c(rd$HY, rd$PY),
                       CPUE = IndicesData(Name = rd$fl, Value = Mat("CPUE")),
                       Survey = IndicesData(Name = rd$fl, Value = Mat("Survey"))))
  om@Obs[[1]][[rd$fl]]@CPUE   <- CPUEObs
  om@Obs[[1]][[rd$fl]]@Survey <- SurveyObs
  om
}

.RunConditioned <- function(om) {
  set.seed(1)
  hist <- Simulate(om, control = SimControl(EstimateBeta = TRUE), silent = TRUE)
  set.seed(2)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  list(hist = hist, mse = mse)
}

.ConditionedFixture <- .CachedFixture(function() {
  rd <- .RealIndexData()
  c(.RunConditioned(.ConditionedOM(rd)), list(rd = rd))
})

test_that("indices conditioned on real data project with the estimated Efficiency and Beta", {
  skip_on_cran()
  fx <- .ConditionedFixture()
  rd <- fx$rd
  for (type in c("CPUE", "Survey")) {
    io <- slot(fx$hist@OM@Obs[[1]][[rd$fl]], type)
    d  <- .IndexResiduals(fx$mse, type, 1, rd$fl)
    .ExpectSameCatchability(d, type)

    Hist <- d[d$Period == "Historical", ]
    Real <- rd$Real[[type]]$Value
    expect_equal(Hist$Value, unname(Real[as.character(Hist$Year)]))
    expect_equal(nrow(Hist), nSim(fx$hist) * sum(is.finite(Real)))
    expect_equal(nrow(d[d$Period == "Projection", ]), nSim(fx$hist) * length(rd$PY))

    # Beta is the log-linear fit; Efficiency makes the historical errors mean 1
    for (s in 1:5) {
      Hs  <- Hist[Hist$Sim == s, ]
      Fit <- coef(lm(log(Value) ~ log(Nom), data = Hs))
      if (io@Misc$BetaFit$Status[s] == "estimated")
        expect_equal(unname(Fit[2]), as.numeric(io@Beta[s]), tolerance = 1e-8)
      expect_equal(as.numeric(io@Efficiency[s]), mean(Hs$Value / Hs$Nom^as.numeric(io@Beta[s])),
                   tolerance = 1e-8)
    }
    expect_equal(as.numeric(tapply(exp(Hist$Resid), Hist$Sim, mean)), rep(1, nSim(fx$hist)),
                 tolerance = 1e-8)
  }
  expect_equal(as.numeric(fx$hist@OM@Obs[[1]][[rd$fl]]@Survey@Beta), rep(1, nSim(fx$hist)))
  expect_true(any(fx$hist@OM@Obs[[1]][[rd$fl]]@CPUE@Misc$BetaFit$Status == "estimated"))
})

test_that("conditioning on real data generated by the OM recovers the generating values", {
  skip_on_cran()
  fx <- .ConditionedFixture()
  rd <- fx$rd
  # sim 1 has the dynamics the real data were generated from; Survey Beta is fixed at its true value
  inp <- .RealInputs$Survey
  e   <- rd$Real$Survey$LogError
  io  <- fx$hist@OM@Obs[[1]][[rd$fl]]@Survey
  expect_equal(as.numeric(io@Efficiency[1]), inp$q * mean(exp(e)), tolerance = 1e-8)
  expect_equal(io@Stats$SD[1], sd(e), tolerance = 1e-8)
  expect_equal(io@Stats$AC[1], acf(e, plot = FALSE)$acf[2], tolerance = 1e-8)
  expect_lt(abs(io@Stats$SD[1] - sdconv(1, inp$CV)), 3 * sdconv(1, inp$CV) / sqrt(2 * length(e)))
  expect_lt(abs(io@Stats$AC[1] - inp$AC), 3 / sqrt(length(e)))

  inp <- .RealInputs$CPUE
  io  <- fx$hist@OM@Obs[[1]][[rd$fl]]@CPUE
  Fit <- io@Misc$BetaFit
  expect_true(Fit$CI_Lower[1] < inp$Beta && inp$Beta < Fit$CI_Upper[1])
  # estimated Beta shifts the residuals slightly from the generating errors
  e <- rd$Real$CPUE$LogError
  expect_lt(abs(io@Stats$SD[1] - sd(e)), 0.02)
  expect_lt(abs(io@Stats$AC[1] - acf(e, plot = FALSE)$acf[2]), 0.05)
  expect_lt(abs(io@Stats$SD[1] - sdconv(1, inp$CV)), 3 * sdconv(1, inp$CV) / sqrt(2 * length(e)))
})

# projection residuals after AR(1) burn-in from the last historical residual
.ProjAfterBurnIn <- function(d, PY, BurnIn = 10) d[d$Period == "Projection" & d$Year > PY[BurnIn], ]

test_that("projected conditioned index errors have the estimated SD and AC and are unbiased", {
  skip_on_cran()
  fx <- .ConditionedFixture()
  rd <- fx$rd
  for (type in c("CPUE", "Survey")) {
    io <- slot(fx$hist@OM@Obs[[1]][[rd$fl]], type)
    SD <- io@Stats$SD
    AC <- io@Stats$AC
    expect_equal(as.numeric(io@CV), sqrt(exp(SD^2) - 1))
    expect_equal(as.numeric(io@AC), AC)

    # historical residuals are the conditioned estimates
    d <- .IndexResiduals(fx$mse, type, 1, rd$fl)
    Hist <- d[d$Period == "Historical", ]
    expect_equal(as.numeric(tapply(Hist$Resid, Hist$Sim, sd)), SD, tolerance = 1e-8)
    expect_equal(as.numeric(tapply(Hist$Resid, Hist$Sim, \(r) acf(r, plot = FALSE)$acf[2])), AC,
                 tolerance = 1e-8)

    Proj <- .ProjAfterBurnIn(d, rd$PY)
    Mom <- .ResidMoments(Proj, -SD^2 / 2, SD^2)
    .ExpectMeanNear(Mom$MeanExp, 1, paste(type, "E[error]"))
    .ExpectMeanNear(Mom$Var, 1, paste(type, "Var(log error) / SD^2"))
    .ExpectMeanNear(Mom$Cov1 - AC, 0, paste(type, "lag-1 AC - estimated AC"))

    # same error level in both periods: no step at the last historical year
    LogMean <- \(x) as.numeric(tapply(x$Resid, x$Sim, mean))
    .ExpectMeanNear(LogMean(Proj) - LogMean(Hist), 0, paste(type, "projection - historical log error"))
  }
})

test_that("user-supplied CV and AC replace the conditioned values for projected index errors", {
  skip_on_cran()
  rd <- .RealIndexData()
  User <- list(CPUE = list(CV = 0.15, AC = 0.3), Survey = list(CV = 0.6, AC = 0.8))
  om <- .ConditionedOM(rd, CPUEObs = do.call(IndicesObs, User$CPUE),
                       SurveyObs = do.call(IndicesObs, c(User$Survey, Beta = 1, Selectivity = "Biomass")))
  om@nSim <- 60
  fx <- .RunConditioned(om)
  for (type in c("CPUE", "Survey")) {
    inp <- User[[type]]
    io  <- slot(fx$hist@OM@Obs[[1]][[rd$fl]], type)
    expect_equal(unique(as.numeric(io@CV)), inp$CV)
    expect_equal(unique(as.numeric(io@AC)), inp$AC)
    expect_gt(abs(mean(io@Stats$SD) - sdconv(1, inp$CV)), 0.1)

    d <- .IndexResiduals(fx$mse, type, 1, rd$fl)
    .ExpectSameCatchability(d, type)
    sigma <- sdconv(1, inp$CV)
    Mom <- .ResidMoments(.ProjAfterBurnIn(d, rd$PY), mconv(1, inp$CV), sigma^2)
    .ExpectMeanNear(Mom$MeanExp, 1, paste(type, "E[error]"))
    .ExpectMeanNear(Mom$Var, 1, paste(type, "Var(log error) / user sigma^2"))
    .ExpectMeanNear(Mom$Cov1, inp$AC, paste(type, "lag-1 AC"))
  }
})

# ---- seasonal and multi-stock OMs ----

test_that("a populated index with Beta can be populated again", {
  for (Beta in list(0.7, c(0.6, 0.8))) {
    set.seed(1)
    io <- PopulateIndexObs(IndicesObs(CV = 0.2, Beta = Beta), 8, 2001:2010, 2011:2015)
    expect_identical(PopulateIndexObs(io, 8, 2001:2010, 2011:2015)@Beta, io@Beta)
  }
  Reduced <- array(0.7, 1, list(Sim = "1"))
  expect_identical(.PopulateObsBeta(Reduced, 8), Reduced)
})

test_that("seasonal indices keep the same catchability and observed seasons in projection years", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- UpdateObject(SeasonalSpatialOM)
  om@nSim  <- 4
  om@pYear <- 3
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@CPUE <- IndicesObs(CV = 0.2, AC = 0.5, Beta = 0.7)
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  expect_equal(.SimValues(hist@OM@Obs[[1]][[fl]]@CPUE@Beta, nSim(hist)), rep(0.7, nSim(hist)))
  mse  <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  d <- .IndexResiduals(mse, "CPUE", 1, fl)
  .ExpectSameCatchability(d, "simulated")
  expect_equal(nrow(d), nSim(hist) * (length(Years(hist, "H")) + length(Years(hist, "P"))))

  # real index observed in seasons 3 and 9 only
  HY <- Years(hist, "H")
  Season <- (seq_along(HY) - 1) %% Seasons(hist) + 1
  Nom <- .CalcNomIndex(hist@Number[1], hist, 1, fl, IndicesObs(), HY)[1, ]
  set.seed(3)
  Val <- 0.01 * Nom^0.8 * exp(.AR1LogError(length(HY), 0.2, 0.3))
  Val[!Season %in% c(3, 9)] <- NA
  om@Obs[[1]][[fl]]@CPUE <- IndicesObs()
  om@Data <- list(Data(Years = c(HY, Years(hist, "P")),
                       CPUE = IndicesData(Name = fl, Value = matrix(Val, ncol = 1, dimnames = list(HY, fl)))))
  set.seed(1)
  hist <- Simulate(om, control = SimControl(EstimateBeta = TRUE), silent = TRUE)
  mse  <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  d <- .IndexResiduals(mse, "CPUE", 1, fl)
  .ExpectSameCatchability(d, "conditioned")
  AllYears <- c(HY, Years(hist, "P"))
  AllSeason <- (seq_along(AllYears) - 1) %% Seasons(hist) + 1
  ObsYears <- AllYears[AllSeason %in% c(3, 9)]
  for (p in c("Historical", "Projection")) {
    Years_p <- if (p == "Historical") HY else Years(hist, "P")
    expect_equal(sort(unique(d$Year[d$Period == p])), ObsYears[ObsYears %in% Years_p], label = p)
  }
})

test_that("an index of a multi-stock complex keeps the same catchability in projection years", {
  skip_on_cran()
  data(ComplexOM, envir = environment())
  om <- UpdateObject(ComplexOM)
  om@nSim  <- 4
  om@pYear <- 5
  fl <- FleetNames(om)[1]
  om@Obs[[1]][[fl]]@Survey <- IndicesObs(CV = 0.2, AC = 0.5, Beta = 0.7, Selectivity = "Biomass")
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  HY <- Years(hist, "H")

  # the complex index sums its stocks
  StockNom <- \(io, st) .CalcNomIndex(hist@Number[st], hist, st, fl, io, HY)
  for (io in list(IndicesObs(), IndicesObs(Selectivity = "Biomass")))
    expect_equal(.CalcNomIndex(hist@Number[1:2], hist, 1:2, fl, io, HY),
                 StockNom(io, 1) + StockNom(io, 2), tolerance = 1e-10)

  Nom <- .CalcNomIndex(hist@Number[1:2], hist, 1:2, fl, IndicesObs(), HY)[1, ]
  set.seed(3)
  Val <- 0.3 * Nom^1.4 * exp(.AR1LogError(length(HY), 0.2, 0.4))
  Val[c(2, 5)] <- NA
  D <- Data(Years = c(HY, Years(hist, "P")),
            CPUE = IndicesData(Name = fl, Value = matrix(Val, ncol = 1, dimnames = list(HY, fl))))

  # OM@Data is named by complex, whether supplied as an object or an unnamed list
  for (Supplied in list(D, list(D)))
    expect_named(PopulateOM(`slot<-`(om, "Data", value = Supplied), silent = TRUE)@Data,
                 names(om@Complexes))
  expect_error(PopulateOM(`slot<-`(om, "Data", value = list(D, D)), silent = TRUE), "complex")

  om@Data <- list(D)
  set.seed(1)
  hist <- Simulate(om, control = SimControl(EstimateBeta = TRUE), silent = TRUE)
  expect_false(is.null(hist@OM@Obs[[1]][[fl]]@CPUE@Stats))
  mse  <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)
  for (type in c("CPUE", "Survey"))
    .ExpectSameCatchability(.IndexResiduals(mse, type, 1, fl), type)
})
