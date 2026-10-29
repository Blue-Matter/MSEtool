Mov2 <- array(c(0.9, 0.2, 0.1, 0.8), dim = c(1, 2, 2, 1, 1),
              dimnames = list(Sim = 1, FromArea = 1:2, ToArea = 1:2, Age = 1, Year = 1))

test_that("nArea is 1 for empty objects", {
  expect_equal(nArea(OM()), 1)
  expect_equal(nArea(Stock()), 1)
  expect_equal(nArea(Spatial()), 1)
  expect_equal(nArea(OM(Stock = list())), 1)
})

test_that("nArea reads every Spatial slot", {
  expect_equal(nArea(Spatial(Movement = Mov2)), 2)
  expect_equal(nArea(Spatial(Movement = unname(Mov2))), 2)
  expect_equal(nArea(Spatial(UnfishedDist = 0.6)), 2)
  expect_equal(nArea(Spatial(UnfishedDist = c(0.4, 0.6))), 2)
  expect_equal(nArea(Spatial(UnfishedDist = array(1/3, c(1, 3, 1, 1)))), 3)
  expect_equal(nArea(Spatial(ProbStaying = c(0.8, 0.9, 0.7))), 3)
  expect_equal(nArea(Spatial(RelativeSize = c(0.2, 0.3, 0.5))), 3)
  expect_equal(nArea(Spatial(RelativeSize = "EqualDensity")), 1)

  st <- Stock()
  Spatial(st) <- Spatial(Movement = Mov2)
  expect_equal(nArea(st), 2)
  expect_equal(nArea(OM(Stock = st)), 2)
  expect_equal(nArea(OM(Stock = list(Stock(), st)), st = 2), 2)
  expect_error(nArea(OM(Stock = list(st)), st = 2), "not found")
})

test_that("nArea accepts the two-area ProbStaying convention", {
  sp <- Spatial(UnfishedDist = 0.6, ProbStaying = 0.9)
  expect_equal(nArea(sp), 2)
  expect_equal(nArea(Populate(sp, silent = TRUE)), 2)
  expect_equal(nArea(Spatial(UnfishedDist = array(1/3, c(1, 3, 1, 1)), ProbStaying = 0.5)), 3)
  expect_equal(nArea(Spatial(UnfishedDist = array(1/3, c(1, 3, 1, 1)), ProbStaying = c(0.4, 0.6))), 3)
})

test_that("nArea errors on inconsistent Area dimensions", {
  sp <- Populate(Spatial(), silent = TRUE)
  expect_equal(nArea(sp), 1)
  sp@Movement <- Mov2
  expect_error(nArea(sp), "Inconsistent number of areas")
  expect_error(Populate(sp, silent = TRUE), "Inconsistent number of areas")

  expect_error(nArea(Spatial(Movement = Mov2, RelativeSize = array(1/3, c(1, 3)))),
               "Inconsistent number of areas")
  expect_error(nArea(Spatial(Movement = array(0.5, c(1, 2, 3)))), "ToArea")
  expect_error(nArea(Spatial(Movement = matrix(0.5, 2, 2))), "Incorrect dimensions")
  expect_error(nArea(Spatial(UnfishedDist = c(0.2, 0.3, 0.5))), "length 1 or length 2")
})
