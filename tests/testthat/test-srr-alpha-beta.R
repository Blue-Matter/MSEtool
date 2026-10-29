.ABEffort <- function() {
  Effort(Effort = data.frame(Year = c(2000, 2024), Lower = c(0.2, 1), Upper = c(0.2, 1), CV = 0.1))
}

.ABOM <- function(SRR, AtAge = FALSE, MinAge = 0, nSim = 3) {
  ages <- MinAge:20
  if (AtAge) {
    w <- 0.008 * (120 * (1 - exp(-0.2 * (ages + 0.1))))^3
    m <- 1 / (1 + exp(-log(19) * (ages - 4) / 2))
    stk <- Stock(Name = "s", Ages = Ages(MinAge = MinAge, MaxAge = 20),
                 Weight = Weight(MeanAtAge = w), Maturity = Maturity(MeanAtAge = m),
                 NaturalMortality = NaturalMortality(Pars = list(M = 0.2)),
                 SRR = SRR, Depletion = Depletion(Final = 0.4))
  } else {
    stk <- Stock(Name = "s", Ages = Ages(MinAge = MinAge, MaxAge = 20),
                 Length = Length(Pars = list(Linf = 120, K = 0.2, t0 = -0.1)),
                 Weight = Weight(Pars = list(a = 0.008, b = 3)),
                 NaturalMortality = NaturalMortality(Pars = list(M = 0.2)),
                 Maturity = Maturity(Pars = list(L50 = 50, L50_95 = 10)),
                 SRR = SRR, Depletion = Depletion(Final = 0.4))
  }
  OM(Stock = stk,
     Fleet = Fleet(Name = "F", Effort = .ABEffort(),
                   Selectivity = Selectivity(Pars = list(SA50 = 4, SA50_95 = 2))),
     nSim = nSim, CurrentYear = 2024, nYear = 25, pYear = 5)
}

test_that("SRRSteepness and SRRAlphaBeta are inverses", {
  phi0 <- c(2, 5, 12)
  for (Model in c("BevertonHolt", "Ricker")) {
    h <- if (Model == "Ricker") c(0.5, 1, 1.4) else c(0.4, 0.7, 0.95)
    ab <- SRRAlphaBeta(h, R0 = 1000, phi0 = phi0, Model = Model)
    back <- SRRSteepness(ab$alpha, ab$beta, phi0 = phi0, Model = Model)
    expect_equal(back[[1]], h)
    expect_equal(back$R0, rep(1000, 3))
  }
  expect_error(SRRSteepness(0.1, 1e-3, phi0 = 2), "must be > 1")
})

test_that("converted steepness reproduces the alpha-beta recruitment curve", {
  phi0 <- 4; S <- c(1, 100, 1e4)
  ab <- list(alpha = 2, beta = 1e-3)
  bh <- SRRSteepness(ab$alpha, ab$beta, phi0, "BevertonHolt")
  expect_equal(BevertonHolt(S, phi0 * bh$R0, bh$R0, bh$h),
               ab$alpha * S / (1 + ab$beta * S))
  rk <- SRRSteepness(ab$alpha, ab$beta, phi0, "Ricker")
  expect_equal(Ricker(S, phi0 * rk$R0, rk$R0, rk$hR),
               ab$alpha * S * exp(-ab$beta * S))
})

test_that("CalcSPR0 works for a minimal at-age OM with no Length", {
  om <- .ABOM(SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4), AtAge = TRUE)
  ages <- 0:20
  w <- 0.008 * (120 * (1 - exp(-0.2 * (ages + 0.1))))^3
  m <- 1 / (1 + exp(-log(19) * (ages - 4) / 2))
  expected <- sum(w * m * exp(-0.2 * ages) * c(rep(1, 20), 1 / (1 - exp(-0.2))))
  expect_equal(unique(c(CalcSPR0(om, silent = TRUE))), expected)
})

test_that("Populate converts alpha-beta Pars to steepness and R0", {
  omH  <- .ABOM(SRR(Pars = list(h = c(0.5, 0.7, 0.9)), R0 = 2000, SD = 0.4))
  phi0 <- apply(CalcSPR0(omH, silent = TRUE), "Sim", \(x) x[1]) |> rep_len(3)
  ab   <- SRRAlphaBeta(c(0.5, 0.7, 0.9), 2000, phi0)
  srr  <- Populate(.ABOM(SRR(Pars = list(alpha = ab$alpha, beta = ab$beta), SD = 0.4)),
                   silent = TRUE)@Stock[[1]]@SRR
  expect_equal(unname(srr@Pars$h[, 1]), c(0.5, 0.7, 0.9))
  expect_equal(unique(round(c(srr@R0), 6)), 2000)
  expect_equal(unname(srr@Misc$AlphaBeta$phi0), unname(phi0))
})

test_that("alpha-beta errors are informative", {
  expect_error(Populate(.ABOM(SRR(Pars = list(alpha = 1, beta = 1e-3), R0 = 1000)), silent = TRUE),
               "must be")
  expect_error(Populate(.ABOM(SRR(Pars = list(alpha = 1e-3, beta = 1e-3))), silent = TRUE),
               "invalid")
  expect_error(Populate(.ABOM(SRR(Pars = list(alpha = 1, beta = 1e-3), Model = "HockeyStick")),
                        silent = TRUE), "BevertonHolt")
})

test_that("Simulate with alpha-beta matches the equivalent steepness OM", {
  skip_on_cran()
  for (Model in c("BevertonHolt", "Ricker")) {
    hnm <- if (Model == "Ricker") "hR" else "h"
    hv  <- if (Model == "Ricker") c(0.6, 1, 1.4) else c(0.5, 0.7, 0.9)
    omH <- .ABOM(SRR(Pars = stats::setNames(list(hv), hnm), Model = Model, R0 = 5000, SD = 0.4))
    phi0 <- apply(CalcSPR0(omH, silent = TRUE), "Sim", \(x) x[1]) |> rep_len(3)
    ab   <- SRRAlphaBeta(hv, 5000, phi0, Model)
    omAB <- .ABOM(SRR(Pars = list(alpha = ab$alpha, beta = ab$beta), Model = Model, SD = 0.4))
    hH  <- Simulate(omH, silent = TRUE)
    hAB <- Simulate(omAB, silent = TRUE)
    expect_equal(hAB@SBiomass, hH@SBiomass)
    expect_equal(hAB@Number[[1]], hH@Number[[1]])
  }
})

test_that("time-varying alpha-beta with a recruitment lag follows the alpha-beta curve", {
  skip_on_cran()
  Years <- 2000:2029
  alpha <- array(c(0.5, 0.5, 0.3), c(3, length(Years)), list(Sim = 1:3, Year = Years))
  alpha[, as.character(2012:2029)] <- alpha[, as.character(2012:2029)] * 0.5
  om <- .ABOM(SRR(Pars = list(alpha = alpha, beta = c(1e-3, 2e-3)), SD = 0.3), MinAge = 1)
  hist <- Simulate(om, silent = TRUE)
  AB   <- hist@OM@Stock[[1]]@SRR@Misc$AlphaBeta
  Rec  <- apply(hist@Number[[1]][, 1, , , drop = FALSE], c(1, 3), sum)
  SP   <- hist@SProduction[, 1, ]
  dev  <- hist@OM@Stock[[1]]@SRR@RecDevHist
  ys <- as.character(2000:2023); yr <- as.character(2001:2024)
  expected <- AB$alpha[, ys] * SP[, ys] / (1 + AB$beta[, ys] * SP[, ys]) * dev[, yr]
  expect_equal(unname(Rec[, yr]), unname(expected))
})

test_that("Simulate runs with at-age schedules supplied in each accepted form and no Length", {
  skip_on_cran()
  ages <- 0:20
  w <- 0.008 * (120 * (1 - exp(-0.2 * (ages + 0.1))))^3
  m <- 1 / (1 + exp(-log(19) * (ages - 4) / 2))
  forms <- list(
    vector = function(x) x,
    SimAge = function(x) array(x, c(1, 21), list(Sim = 1, Age = ages)),
    LaterYear = function(x) array(x, c(1, 21, 1), list(Sim = 1, Age = ages, Year = 2024)))
  ref <- NULL
  for (f in forms) {
    stk <- Stock(Name = "s", Ages = Ages(MaxAge = 20),
                 Weight = Weight(MeanAtAge = f(w)), Maturity = Maturity(MeanAtAge = f(m)),
                 NaturalMortality = NaturalMortality(Pars = list(M = 0.2)),
                 SRR = SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4),
                 Depletion = Depletion(Final = 0.4))
    om <- OM(Stock = stk,
             Fleet = Fleet(Name = "F", Effort = .ABEffort(),
                           Selectivity = Selectivity(Pars = list(SA50 = 4, SA50_95 = 2))),
             nSim = 3, CurrentYear = 2024, nYear = 25, pYear = 5)
    hist <- Simulate(om, silent = TRUE)
    expect_s4_class(hist, "hist")
    if (is.null(ref)) ref <- hist@SBiomass else expect_equal(hist@SBiomass, ref)
  }
})
