test_that("CatchObs accepts valid parameters", {
  co <- CatchObs(CV = 0.2, Bias = 1.1, Units = "Biomass")
  expect_s4_class(co, "catchobs")
  expect_equal(CV(co), 0.2)
  expect_equal(Bias(co), 1.1)
})

test_that("CatchObs rejects invalid CV/Bias/Units", {
  expect_error(CatchObs(CV = -0.1))
  expect_error(CatchObs(Bias = 0))
  expect_error(CatchObs(Bias = -1))
  expect_error(CatchObs(Units = "Weight"))
})

test_that("EffortObs rejects invalid CV/Bias", {
  expect_error(EffortObs(CV = -0.1))
  expect_error(EffortObs(Bias = 0))
})

test_that("IndicesObs accepts valid Units and rejects invalid ones", {
  io <- IndicesObs(CV = 0.3, Units = "Recruitment")
  expect_s4_class(io, "indicesobs")
  expect_error(IndicesObs(Units = "Weight"))
  expect_error(IndicesObs(TruncSD = -1))
})

test_that("CompObs rejects Theta outside (0, 1]", {
  expect_error(CompObs(Theta = 0))
  expect_error(CompObs(Theta = 1.5))
  expect_silent(CompObs(Theta = 1))
})

test_that("CompObs rejects non-positive SampleSize/ESS", {
  expect_error(CompObs(SampleSize = -10))
  expect_error(CompObs(ESS = 0))
})

test_that("CompObs accessors round-trip via the generic .AccessSlot pattern", {
  co <- CompObs(SampleSize = 200, ESS = 50, Theta = 0.5)
  expect_equal(SampleSize(co), 200)
  expect_equal(ESS(co), 50)
  expect_equal(Theta(co), 0.5)

  SampleSize(co) <- 300
  expect_equal(SampleSize(co), 300)
})

test_that("Theta() remains backward compatible with effort-class objects", {
  ef <- Effort()
  expect_null(Theta(ef))
  Theta(ef) <- 0.8
  expect_equal(Theta(ef), 0.8)
})

test_that("Obs() attaches sub-objects to a complete obs object", {
  obs <- Obs(Landings = CatchObs(CV = 0.2), CPUE = IndicesObs(CV = 0.3))
  expect_s4_class(obs, "obs")
  expect_equal(CV(Landings(obs)), 0.2)
  expect_equal(CV(CPUE(obs)), 0.3)
})
