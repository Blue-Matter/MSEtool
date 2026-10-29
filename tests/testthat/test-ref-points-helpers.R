test_that(".Interp1 finds y at x == target by linear interpolation", {
  x <- c(0, 1, 2, 3)
  y <- c(0, 10, 20, 30)
  expect_equal(MSEtool:::.Interp1(x, y, target = 1.5), 15, tolerance = 1e-6)
})

test_that(".Interp1 returns NA outside the interpolation range by default", {
  x <- c(0, 1, 2)
  y <- c(0, 10, 20)
  expect_true(is.na(MSEtool:::.Interp1(x, y, target = 5)))
})

test_that(".Interp1 supports rule=2 (constant extrapolation)", {
  x <- c(0, 1, 2)
  y <- c(0, 10, 20)
  expect_equal(MSEtool:::.Interp1(x, y, target = 5, rule = 2), 20)
})

test_that(".FindF01 finds the F where slope is 10% of the initial slope", {
  # A YPR curve with a known, exact 10%-of-initial-slope crossing
  Fgrid <- seq(0, 10, by = 0.01)
  # dYPR/dF starts at 1 and decays linearly to 0 at F=10 -> YPR = F - F^2/20
  ypr <- Fgrid - Fgrid^2 / 20
  f01 <- MSEtool:::.FindF01(ypr, Fgrid)
  # slope(F) = 1 - F/10; want slope = 0.1 -> F = 9
  expect_equal(f01, 9, tolerance = 0.05)
})

test_that(".FindFmax returns the grid argmax", {
  Fgrid <- seq(0, 5, by = 0.1)
  ypr <- -(Fgrid - 2)^2 + 10  # peaks at F=2
  expect_equal(MSEtool:::.FindFmax(ypr, Fgrid), 2, tolerance = 0.05)
})

test_that(".FindFmax returns NA when no finite values are present", {
  Fgrid <- seq(0, 5, by = 0.1)
  expect_true(is.na(MSEtool:::.FindFmax(rep(NA_real_, length(Fgrid)), Fgrid)))
})

test_that(".ApplyOverF reshapes an [.., F] array and applies FUN row-wise", {
  arr <- array(c(3, 1, 5), dim = c(1, 1, 3),
              dimnames = list(Sim = 1, Stock = 1, F = c("0", "1", "2")))
  # for a single (Sim,Stock) slice, FUN should receive the length-3 F-vector
  out <- MSEtool:::.ApplyOverF(arr, Fgrid = c(0, 1, 2), FUN = function(y, Fgrid) max(y))
  expect_equal(as.numeric(out), 5)
})
