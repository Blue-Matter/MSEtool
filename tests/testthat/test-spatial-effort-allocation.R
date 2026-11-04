# Spatial effort allocation: D[f,a] ~ w[a] * u[f,a]^lambda, u = b * h(F) / w

.SetEffortSlot <- function(om, slotname, value) {
  for (st in seq_along(om@Fleet))
    for (fl in seq_along(om@Fleet[[st]]))
      slot(om@Fleet[[st]][[fl]]@Effort, slotname) <- value
  om
}

.AreaApicalF <- function(hist, stock = 1, fleet = 1) {
  apply(hist@FInteractArea[[stock]][, , , fleet, , drop = FALSE], c(1, 3, 5), max)
}

# largest deviation of the engine's Distribution from the allocation equation, single-stock Density OM;
# each time step is re-run from the inputs the engine sees
.AllocationError <- function(hist, lambda, Steps = c(5, 12, 20)) {
  hp <- .PrepHistMisc(hist, "Historical")
  M  <- hp@Misc
  Si <- \(x, s) min(s, dim(x)[1])
  Yrs <- Years(hist@OM, "H")
  err <- 0
  for (yi in Steps) {
    hy <- hp
    hy@Distribution[, as.character(Yrs[yi]), , ] <- NA
    out <- .CalcFisheryDynamics(hy, Years = Yrs[yi], DoCalcRecruitment = 0, DoCalcNumberNext = 0,
                                DoCalcSpawnProduction = 0, DoCalcTransition = 0)
    for (s in seq_len(nSim(hist))) {
      w <- M$RelSize[Si(M$RelSize, s), ]
      D <- matrix(out@Distribution[s, yi, , ], nrow = nFleet(hist))
      Ftot <- 0
      for (f in seq_len(nFleet(hist)))
        Ftot <- Ftot + M$Catchability[Si(M$Catchability, s), 1, yi, f] *
          out@Effort[Si(out@Effort, s), yi, f] * D[f, ] / w
      Ftot <- pmin(Ftot, M$maxF)
      h <- ifelse(Ftot > 1e-6, -expm1(-Ftot) / Ftot, 1)
      for (f in seq_len(nFleet(hist))) {
        b <- vapply(seq_along(w), \(a) sum(hp@Number[[1]][s, , yi, a] *
          M$SelAgeList[[1]][Si(M$SelAgeList[[1]], s), , yi, f, a] *
          M$RetAgeList[[1]][Si(M$RetAgeList[[1]], s), , yi, f, a] *
          M$WeightFleetRetainedList[[1]][Si(M$WeightFleetRetainedList[[1]], s), , yi, f]), numeric(1))
        target <- w * (b * h / w)^lambda
        err <- max(err, abs(target / sum(target) - D[f, ]))
      }
    }
  }
  err
}

test_that("PopulateEffort() defaults Targeting to 1 and ReleaseValue to 0, and validates both", {
  Eff <- PopulateEffort(Effort(Effort = array(1, c(1, 3), dimnames = list(Sim = 1, Year = 1:3))),
                        HistYears = 1:3, nArea = 2, nSim = 4, seed = 1)
  expect_equal(c(Eff@Targeting), 1)
  expect_equal(c(Eff@ReleaseValue), 0)

  Eff <- PopulateEffort(Effort(Effort = array(1, c(1, 3), dimnames = list(Sim = 1, Year = 1:3)),
                               Targeting = c(0.5, 2), ReleaseValue = 1),
                        HistYears = 1:3, nArea = 2, nSim = 4, seed = 1)
  expect_equal(dim(Eff@Targeting), c(4, 1))
  expect_true(all(Eff@Targeting >= 0.5 & Eff@Targeting <= 2))
  expect_equal(c(Eff@ReleaseValue), 1)

  expect_error(PopulateEffort(Effort(Effort = array(1, c(1, 3), dimnames = list(Sim = 1, Year = 1:3)),
                                     ReleaseValue = -1),
                              HistYears = 1:3, nArea = 2, nSim = 4, seed = 1),
               "non-negative")
})

test_that("a stock with equal density everywhere is fished at the same rate in every area", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 2
  om@Stock[[1]]@Spatial@RelativeSize <- "EqualDensity"
  for (lambda in c(0, 1, 3)) {
    hist <- Simulate(.SetEffortSlot(om, "Targeting", lambda), silent = TRUE)
    Fa <- .AreaApicalF(hist)
    expect_equal(Fa[, -1, 1], Fa[, -1, 2], tolerance = 1e-8)
    RS <- hist@OM@Stock[[1]]@Spatial@RelativeSize
    expect_equal(unname(hist@Distribution[, -1, 1, 1]),
                 unname(matrix(RS[, 1], nrow(RS), dim(Fa)[2] - 1)), tolerance = 1e-8)
  }
})

test_that("Biomass mode with Targeting 0 gives each area an equal share of effort", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 2
  om <- .SetEffortSlot(om, "Mode", "Biomass")
  hist <- Simulate(.SetEffortSlot(om, "Targeting", 0), silent = TRUE)
  expect_equal(range(hist@Distribution[, -1, 1, ]), c(0.5, 0.5))
})

test_that("Distribution solves the allocation equation with depletion from all fleets in the time step", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 2
  for (lambda in c(1, 3)) {
    hist <- Simulate(.SetEffortSlot(om, "Targeting", lambda), silent = TRUE)
    expect_lt(.AllocationError(hist, lambda), 1e-9)
  }

  two <- TwoFleetOM
  two@nSim <- 2
  hist <- Simulate(two, silent = TRUE)
  expect_lt(.AllocationError(hist, 1), 1e-9)
})

test_that("ReleaseValue sets how strongly released catch attracts effort", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 2
  hist <- Simulate(om, silent = TRUE)
  Year <- Years(hist@OM, "H")[10]

  Dist <- function(ReleaseValue, Release2 = TRUE) {
    hp <- .PrepHistMisc(hist, "Historical")
    if (Release2) hp@Misc$RetAgeList[[1]][, , , 1, 2] <- 0
    hp@Misc$ReleaseValue[] <- ReleaseValue
    hp@Distribution[, as.character(Year), , ] <- NA
    .CalcFisheryDynamics(hp, Years = Year)@Distribution[, as.character(Year), 1, ]
  }

  # everything released in area 2: no effort there unless released catch has value
  expect_equal(unname(Dist(0)[, 2]), c(0, 0))
  Half <- Dist(0.5)[, 2]
  Full <- Dist(1)[, 2]
  expect_true(all(Half > 0 & Half < Full))
  # same selected and retained weights here, so full release value matches full retention
  expect_equal(Full, Dist(0, Release2 = FALSE)[, 2], tolerance = 1e-8)
})

test_that("spatial effort settings must agree across stocks for a fleet", {
  skip_on_cran()
  om <- MultiStockOM
  om@nSim <- 2
  om@Fleet[[2]][[1]]@Effort@Mode <- "Biomass"
  expect_error(Simulate(om, silent = TRUE), "differs between stocks")
})

test_that("legacy Spat_targ maps to Targeting and released catch attracts effort", {
  fleet <- ConvertFleet(Generic_Fleet, silent = TRUE)
  expect_equal(c(fleet@Effort@Targeting), Generic_Fleet@Spat_targ[1])
  expect_equal(fleet@Effort@ReleaseValue, 1)

  Fl <- Generic_Fleet
  Fl@Spat_targ <- c(0.5, 2)
  expect_equal(ConvertFleet(Fl, silent = TRUE)@Effort@Targeting, c(0.5, 2))
})
