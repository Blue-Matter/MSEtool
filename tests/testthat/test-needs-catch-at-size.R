# Coverage for .NeedsCatchAtSize() and SimControl(CalcCatchAtSize = ...):
# auto-detecting (per stock) whether anything downstream actually needs
# Hist@LandingsAtSize/DiscardsAtSize, with a tri-state override. See
# .NeedsCatchAtSize() in R/calc-catch-at-size.R.

skip_on_cran()

test_that("SimControl() defaults CalcCatchAtSize to NA (auto-detect)", {
  expect_true(is.na(SimControl()$CalcCatchAtSize))
})

test_that(".NeedsCatchAtSize() override forces every stock on or off", {
  data(MultiStockOM, envir = environment())
  om <- MSEtool:::.StartUp(MultiStockOM, nSim = NULL, silent = TRUE)

  expect_equal(.NeedsCatchAtSize(om, SimControl(CalcCatchAtSize = TRUE)),
              rep(TRUE, nStock(om)))
  expect_equal(.NeedsCatchAtSize(om, SimControl(CalcCatchAtSize = FALSE)),
              rep(FALSE, nStock(om)))
})

test_that(".NeedsCatchAtSize() auto-detects per stock from Obs configuration", {
  # MultiStockOM has a size-composition observation model configured for
  # one stock but not the other -- a real mixed case, not a synthetic one.
  data(MultiStockOM, envir = environment())
  om <- MSEtool:::.StartUp(MultiStockOM, nSim = NULL, silent = TRUE)

  needed <- .NeedsCatchAtSize(om, SimControl())
  expect_length(needed, nStock(om))
  expect_true(any(needed))
  expect_true(any(!needed))

  # Matches the underlying no_obs check .GenHistDataSizeComp() uses itself
  complexes <- Complexes(om)
  for (i in seq_along(complexes)) {
    no_obs <- all(vapply(c('LandingsAtSize', 'DiscardsAtSize'), \(type) {
      all(vapply(om@Obs[[i]], \(o) isNewObject(slot(o, type)), logical(1)))
    }, logical(1)))
    expect_equal(all(!needed[complexes[[i]]]), no_obs)
  }
})

test_that(".NeedsCatchAtSize() is FALSE for every stock when no complex needs it", {
  data(SingleStockOM, envir = environment())
  om <- MSEtool:::.StartUp(SingleStockOM, nSim = NULL, silent = TRUE)
  expect_equal(.NeedsCatchAtSize(om, SimControl()), FALSE)
})

test_that("Simulate() auto-skips catch-at-size and stores the decision on Hist@OM@Control", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  expect_identical(hist@OM@Control$CalcCatchAtSizeNeeded, FALSE)
  expect_true(all(hist@LandingsAtSize[[1]][[1]] == 0))
  expect_true(all(hist@DiscardsAtSize[[1]][[1]] == 0))
})

test_that("SimControl(CalcCatchAtSize = TRUE) forces real catch-at-size even with no Obs/data", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE, control = SimControl(CalcCatchAtSize = TRUE))

  expect_identical(hist@OM@Control$CalcCatchAtSizeNeeded, TRUE)
  expect_true(sum(hist@LandingsAtSize[[1]][[1]]) > 0)
})

test_that(".CalcCatchAtSize() `needed = NULL` still computes every stock (backward compatible)", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE, control = SimControl(CalcCatchAtSize = FALSE))
  expect_true(all(hist@LandingsAtSize[[1]][[1]] == 0))

  HistYears <- Years(hist, 'H')
  recomputed <- .CalcCatchAtSize(hist, Years = HistYears) # needed defaults to NULL -> all TRUE
  expect_true(sum(recomputed@LandingsAtSize[[1]][[1]]) > 0)
})

test_that("Project() reuses the Simulate()-resolved decision without re-triggering it", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE, control = SimControl(CalcCatchAtSize = TRUE))
  mse  <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)

  expect_s4_class(mse, "mse")
  expect_true(sum(mse@LandingsAtSize[[1]][[1]]) > 0)
})

test_that("SimControl(CalcCatchAtSizeCpp = FALSE) falls back to the R implementation and matches C++", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1

  set.seed(1)
  hist_r <- Simulate(om, silent = TRUE,
                     control = SimControl(CalcCatchAtSize = TRUE, CalcCatchAtSizeCpp = FALSE))
  set.seed(1)
  hist_cpp <- Simulate(om, silent = TRUE,
                       control = SimControl(CalcCatchAtSize = TRUE, CalcCatchAtSizeCpp = TRUE))

  expect_identical(hist_r@OM@Control$CalcCatchAtSizeCpp, FALSE)
  expect_identical(hist_cpp@OM@Control$CalcCatchAtSizeCpp, TRUE)
  expect_equal(hist_r@LandingsAtSize[[1]][[1]], hist_cpp@LandingsAtSize[[1]][[1]])
  expect_equal(hist_r@DiscardsAtSize[[1]][[1]], hist_cpp@DiscardsAtSize[[1]][[1]])
  expect_true(sum(hist_r@LandingsAtSize[[1]][[1]]) > 0)
})
