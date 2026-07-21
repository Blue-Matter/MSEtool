test_that("BevertonHolt passes through S0/R0 at unfished equilibrium", {
  # By construction, R(S0) == R0 for any valid steepness
  S0 <- 1000; R0 <- 500; h <- 0.7
  expect_equal(BevertonHolt(S0, S0, R0, h), R0, tolerance = 1e-6)
})

test_that("BevertonHolt recruitment increases with spawning biomass", {
  S0 <- 1000; R0 <- 500; h <- 0.7
  r_low  <- BevertonHolt(200, S0, R0, h)
  r_high <- BevertonHolt(800, S0, R0, h)
  expect_lt(r_low, r_high)
  expect_lt(r_high, R0)
})

test_that("Ricker passes through S0/R0 at unfished equilibrium", {
  S0 <- 1000; R0 <- 500; hR <- 0.6
  expect_equal(Ricker(S0, S0, R0, hR), R0, tolerance = 1e-6)
})

test_that("HockeyStick saturates at R0 beyond the hinge point", {
  S0 <- 1000; R0 <- 500; Shinge <- 0.5
  expect_equal(HockeyStick(S0, S0, R0, Shinge), R0, tolerance = 1e-6)
  expect_equal(HockeyStick(2 * S0, S0, R0, Shinge), R0, tolerance = 1e-6)
})

test_that("HockeyStick is linear below the hinge point", {
  S0 <- 1000; R0 <- 500; Shinge <- 0.5
  r_quarter <- HockeyStick(0.25 * S0, S0, R0, Shinge)
  r_half    <- HockeyStick(0.5 * S0, S0, R0, Shinge)
  expect_equal(r_quarter / r_half, 0.5, tolerance = 1e-6)
})

test_that("HockeyStick rejects Shinge outside (0, 1]", {
  expect_error(HockeyStick(500, 1000, 500, 0))
  expect_error(HockeyStick(500, 1000, 500, 1.5))
})

test_that("BevertonHolt_RelRec recovers SPR=1 -> RelRec=1 at unfished conditions", {
  out <- BevertonHolt_RelRec(Pars = list(h = 0.7), SPR = 1)
  expect_equal(out, 1, tolerance = 1e-6)
})

test_that("Ricker_RelRec recovers SPR=1 -> RelRec=1 at unfished conditions", {
  out <- Ricker_RelRec(Pars = list(hR = 0.6), SPR = 1)
  expect_equal(out, 1, tolerance = 1e-6)
})

test_that("RelRec functions are floored at zero", {
  expect_equal(BevertonHolt_RelRec(Pars = list(h = 0.7), SPR = 0), 0)
  expect_gte(Ricker_RelRec(Pars = list(hR = 0.6), SPR = 1e-6), 0)
})

test_that(".CheckSArg rejects non-numeric or NA spawning biomass", {
  expect_error(BevertonHolt("a", 1000, 500, 0.7))
  expect_error(BevertonHolt(NA_real_, 1000, 500, 0.7))
})
