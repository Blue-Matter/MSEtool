# Deterministic OM with constant effort and Depletion@Initial
.FishedInitOM <- function(OM, Initial = 0.4, Reference = "B0", nSim = 3, SD = 0) {
  OM@nSim <- nSim
  for (st in seq_along(OM@Stock)) {
    OM@Stock[[st]]@SRR@SD <- SD
    OM@Stock[[st]]@Depletion <- Depletion(Initial = Initial, Reference = Reference)
  }
  OM <- Populate(OM, silent = TRUE)
  for (st in seq_along(OM@Stock)) {
    for (fl in seq_along(OM@Fleet[[st]])) {
      Effort <- ExtendSims(OM@Fleet[[st]][[fl]]@Effort@Effort, nSim)
      Effort[] <- 1
      OM@Fleet[[st]][[fl]]@Effort@Effort <- Effort
    }
  }
  OM
}

.EffortScale <- function(Hist, st = 1) {
  as.numeric(ExtendSims(Hist@OM@Stock[[st]]@Depletion@Misc$InitialEffortScale, nSim(Hist)))
}

# historical fishing at the effort scale solved for the initial equilibrium
.AtInitialF <- function(OM, Hist) {
  for (st in seq_along(OM@Stock)) {
    Scale <- .EffortScale(Hist, st)
    for (fl in seq_along(OM@Fleet[[st]])) {
      q <- ExtendSims(Hist@OM@Fleet[[st]][[fl]]@Catchability@Efficiency, nSim(Hist))
      OM@Fleet[[st]][[fl]]@Catchability@Efficiency <- q * Scale
    }
  }
  OM
}

.InitialDepletion <- function(Hist, st = 1) {
  Ref <- Hist@OM@Stock[[st]]@Depletion@Reference
  B  <- if (Ref == "B0") Hist@Biomass else Hist@SBiomass
  B0 <- if (Ref == "B0") Hist@Unfished@Equilibrium@Biomass else Hist@Unfished@Equilibrium@SBiomass
  nSim <- nSim(Hist)
  unname(ExtendSims(B, nSim)[, st, 1] / ExtendSims(B0, nSim)[, st, 1])
}

# max relative change over years 2:nYear vs year 1, matched by season
.Transient <- function(Hist, st = 1, nYear = 8) {
  nSeason <- Hist@OM@Seasons
  Y1 <- seq_len(nSeason)
  N1 <- Hist@Number[[st]][, , Y1, , drop = FALSE]
  Use <- N1 > 1e-8 * max(N1)
  Landings <- apply(Hist@Landings[, st, , , drop = FALSE], c(1, 3), sum)
  Rel <- \(a, b) { r <- abs(a / b - 1); max(r[is.finite(r)], 0) }
  max(vapply(2:nYear, \(y) {
    Yy <- Y1 + (y - 1) * nSeason
    Ny <- Hist@Number[[st]][, , Yy, , drop = FALSE]
    max(Rel(Ny[Use], N1[Use]),
        Rel(Hist@SBiomass[, st, Yy], Hist@SBiomass[, st, Y1]),
        Rel(Landings[, Yy], Landings[, Y1]))
  }, numeric(1)))
}

# uniformly scaled unfished age structure, as from `Depletion@Misc$InitialAtAge`
.UniformInitial <- function(OM) {
  for (st in seq_along(OM@Stock)) {
    Ages <- OM@Stock[[st]]@Ages@Classes[-1]
    OM@Stock[[st]]@Depletion@Misc$InitialAtAge <- array(1, c(1, length(Ages)),
                                                        list(Sim = 1, Age = Ages))
  }
  OM
}

.ExpectFlatStart <- function(OM) {
  Hist <- Simulate(OM, silent = TRUE)
  for (st in seq_along(OM@Stock)) {
    Target <- rep_len(as.numeric(OM@Stock[[st]]@Depletion@Initial), nSim(OM))
    Active <- Target < 1
    expect_equal(.InitialDepletion(Hist, st)[Active], Target[Active], tolerance = 1e-8)
  }

  AtF <- .AtInitialF(OM, Hist)
  Flat <- Simulate(AtF, silent = TRUE)
  Uniform <- Simulate(.UniformInitial(AtF), silent = TRUE)
  for (st in seq_along(OM@Stock)) {
    Active <- rep_len(as.numeric(OM@Stock[[st]]@Depletion@Initial), nSim(OM)) < 1
    expect_equal(.EffortScale(Flat, st)[Active], rep(1, sum(Active)), tolerance = 1e-6)
    expect_lt(.Transient(Flat, st), 1e-4)
    expect_gt(.Transient(Uniform, st), 0.1)
  }
  invisible(Hist)
}

test_that("annual multi-area OM starts from a fished equilibrium (B0 and SB0)", {
  skip_on_cran()
  .ExpectFlatStart(.FishedInitOM(SingleStockOM, Initial = c(0.3, 1, 0.5)))
  .ExpectFlatStart(.FishedInitOM(SingleStockOM, Reference = "SB0"))
})

test_that("seasonal multi-area OM with a plus group starts from a fished equilibrium", {
  skip_on_cran()
  OM <- SeasonalSpatialOM
  expect_true(OM@Stock[[1]]@Ages@PlusGroup)
  expect_gt(nArea(OM), 1)
  .ExpectFlatStart(.FishedInitOM(OM, Reference = "SB0", nSim = 2))
})

test_that("multi-stock OM starts each stock from a fished equilibrium", {
  skip_on_cran()
  OM <- .FishedInitOM(ComplexOM, Reference = "SB0")
  OM@Stock[[2]]@Depletion@Initial <- array(c(0.25, 0.5, 0.6), 3, list(Sim = 1:3))
  .ExpectFlatStart(OM)
})

test_that("Depletion@Initial is matched with stochastic recruitment deviations", {
  skip_on_cran()
  for (Reference in c("B0", "SB0")) {
    OM <- .FishedInitOM(SingleStockOM, Initial = c(0.2, 0.6), Reference = Reference, SD = 0.6)
    Hist <- Simulate(OM, silent = TRUE)
    expect_equal(.InitialDepletion(Hist), as.numeric(OM@Stock[[1]]@Depletion@Initial),
                 tolerance = 1e-8)
  }
})

test_that("multi-fleet initial equilibrium uses the optimized relative catchability", {
  skip_on_cran()
  OM <- TwoFleetOM
  OM@Stock[[1]]@Spatial <- Spatial()
  OM <- .FishedInitOM(OM, Reference = "SB0")
  OM@Stock[[1]]@Depletion@Final <- array(0.3, 3, list(Sim = 1:3))
  OM@CatchFrac <- list(matrix(c(0.7, 0.3), 1, 2))

  Hist <- Simulate(OM, silent = TRUE)
  nYear <- length(Years(Hist, "H"))
  expect_equal(.InitialDepletion(Hist), rep(0.4, 3), tolerance = 1e-8)
  SB0 <- ExtendSims(Hist@Unfished@Equilibrium@SBiomass, 3)[, 1, nYear]
  expect_equal(unname(Hist@SBiomass[, 1, nYear] / SB0), rep(0.3, 3), tolerance = 1e-3)
  Landings <- Hist@Landings[, 1, nYear, ]
  expect_equal(unname(Landings[, 1] / rowSums(Landings)), rep(0.7, 3), tolerance = 1e-3)

  # same initial conditions when the optimized catchability is supplied directly
  Fixed <- OM
  Fixed@Stock[[1]]@Depletion@Final <- NULL
  for (fl in 1:2)
    Fixed@Fleet[[1]][[fl]]@Catchability@Efficiency <- Hist@OM@Fleet[[1]][[fl]]@Catchability@Efficiency
  FixedHist <- Simulate(Fixed, silent = TRUE)
  expect_equal(FixedHist@Number[[1]][, -1, 1, ], Hist@Number[[1]][, -1, 1, ], tolerance = 1e-6)

  .ExpectFlatStart(Fixed)
})

test_that("InitialAtAge takes precedence and is scaled to Depletion@Initial", {
  skip_on_cran()
  OM <- .FishedInitOM(SingleStockOM, Reference = "SB0", SD = 0.6)
  Ages <- OM@Stock[[1]]@Ages@Classes[-1]
  OM@Stock[[1]]@Depletion@Misc$InitialAtAge <- array(exp(-0.2 * Ages), c(1, length(Ages)),
                                                     list(Sim = 1, Age = Ages))
  Hist <- Simulate(OM, silent = TRUE)
  expect_null(Hist@OM@Stock[[1]]@Depletion@Misc$InitialEffortScale)
  expect_equal(.InitialDepletion(Hist), rep(0.4, 3), tolerance = 1e-8)

  # numbers-at-age proportional to unfished x deviations x InitialAtAge
  Unfished <- CalcUnfished_Dynamic(OM, silent = TRUE)
  Ratio <- apply(Hist@Number[[1]][, -1, 1, ], 1:2, sum) / apply(Unfished@Number[[1]][, -1, 1, ], 1:2, sum)
  Ratio <- sweep(Ratio, 2, exp(-0.2 * Ages), "/")
  expect_equal(unname(Ratio / Ratio[, 1]), matrix(1, 3, length(Ages)), tolerance = 1e-8)
})

test_that("the fishing pattern falls back to the first year with effort", {
  skip_on_cran()
  OM <- SingleStockOM
  OM@nSim <- 2
  OM@Stock[[1]]@Depletion <- Depletion(Initial = 0.4, Reference = "B0")
  OM <- Populate(OM, silent = TRUE)
  expect_true(all(ExtendSims(OM@Fleet[[1]][[1]]@Effort@Effort, 2)[, 1] == 0))
  Hist <- Simulate(OM, silent = TRUE)
  expect_equal(.InitialDepletion(Hist), rep(0.4, 2), tolerance = 1e-8)
  expect_true(any(grepl("No fishing effort in the first historical year", unlist(Hist@Log))))
})
