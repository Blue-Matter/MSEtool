# Projection-ready hist object, as .ProjectHist() builds it
.InPlaceProj <- function(OM, nSim = 3) {
  Hist <- Simulate(OM, nSim = nSim, silent = TRUE)
  YearsHist <- Years(Hist@OM, 'Historical')
  Proj <- .PrepHistMisc(Hist)
  Proj <- .ExtendHist(Proj, Years = c(YearsHist, Years(Hist@OM, 'Projection')), silent = TRUE)
  .CalcFisheryDynamics(Proj, Years = utils::tail(YearsHist, 1))
}

.SlotDigests <- function(x)
  vapply(setdiff(methods::slotNames('timeseries'), 'Misc'),
         \(sl) digest::digest(methods::slot(x, sl)), character(1))

.ProbeCatch <- function(Slice, Year, TSIndex, Effort) {
  nFleet <- nFleet(Slice)
  Slice  <- .WriteStateToProj(Slice, 1, TSIndex, Effort = Effort)
  .CalcFleetCatch(Slice, 1, TSIndex, Year,
                  list(rep('Removals', nFleet)), list(rep('Biomass', nFleet)))
}

test_that("default engine calls leave every other reference to the input unchanged", {
  skip_on_cran()
  Proj  <- .InPlaceProj(TwoFleetOM)
  Other <- Proj
  Before <- .SlotDigests(Other)
  Year1 <- Years(Proj@OM, 'Projection')[1]
  Out <- .CalcFisheryDynamics(Proj, Years = Year1)
  expect_identical(.SlotDigests(Other), Before)
  expect_false(identical(Out@Landings, Other@Landings))
})

test_that("InPlace calls on an owned object leave other references unchanged", {
  skip_on_cran()
  Proj  <- .InPlaceProj(TwoFleetOM)
  Other <- Proj
  Before <- .SlotDigests(Other)
  Owned <- .OwnDynamicsSlots(Proj)
  Year1 <- Years(Proj@OM, 'Projection')[1]
  Out <- .CalcFisheryDynamics(Owned, Years = Year1, InPlace = TRUE)
  expect_identical(.SlotDigests(Other), Before)
  expect_identical(Out@Landings, .CalcFisheryDynamics(Proj, Years = Year1)@Landings)
})

test_that("a single-sim slice does not share arrays with the object it came from", {
  skip_on_cran()
  Proj  <- .InPlaceProj(SingleStockOM, nSim = 1)
  Before <- .SlotDigests(Proj)
  Year1 <- Years(Proj@OM, 'Projection')[1]
  TSIndex <- match(Year1, Years(Proj@OM))
  Slice <- .SliceSim(Proj, 1, .DynamicsProbeSlots)
  invisible(.ProbeCatch(Slice, Year1, TSIndex, Effort = 2))
  expect_identical(.SlotDigests(Proj), Before)
})

test_that("repeated probes on one slice match probes on a fresh slice", {
  skip_on_cran()
  for (MinAge in c(0, 1)) {
    OM <- SingleStockOM
    if (MinAge > 0) {
      a <- OM@Stock[[1]]@Ages
      a@MinAge <- MinAge
      a@Classes <- a@Classes[a@Classes >= MinAge]
      OM@Stock[[1]]@Ages <- a
    }
    Proj  <- .InPlaceProj(OM)
    Year1 <- Years(Proj@OM, 'Projection')[1]
    TSIndex <- match(Year1, Years(Proj@OM))
    Slice <- .SliceSim(Proj, 2, .DynamicsProbeSlots)
    invisible(.ProbeCatch(Slice, Year1, TSIndex, Effort = 3))
    Reused <- .ProbeCatch(Slice, Year1, TSIndex, Effort = 0.5)
    Fresh  <- .ProbeCatch(.SliceSim(Proj, 2, .DynamicsProbeSlots), Year1, TSIndex, Effort = 0.5)
    expect_identical(Reused, Fresh, label = paste('MinAge', MinAge))
  }
})

test_that("Project() leaves the hist object unchanged", {
  skip_on_cran()
  Hist <- Simulate(TwoFleetOM, nSim = 3, silent = TRUE)
  Before <- digest::digest(Hist)
  invisible(Project(Hist, MPs = c('CurrentEffort', 'CurrentCatch'), silent = TRUE))
  expect_identical(digest::digest(Hist), Before)
})
