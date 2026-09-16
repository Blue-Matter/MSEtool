# Regression coverage for .CalcCatchAtSize()/.CalcCatchAtSizeFleet()
# (R/calc-catch-at-size.R, R/calc-conditioned-key.R).
#
# .CalcCatchAtSizeFleet() has a fast path (per-(Year,Area) contraction,
# batched over Sim, never materialising a full Sim x Age x Class x Year x
# Area array) and a generic fallback (.CatchAtSizeFleetGeneric(), the
# original full-array-broadcast implementation) used whenever the fast
# path's Sim/Year/Area alignment assumptions don't hold. The two must always
# agree -- that agreement, not a hand re-derivation of the maths, is what is
# tested at the unit level below.

skip_on_cran()

.make_key <- function(nSim, nAge, nClass, nYear, nArea, seed = 1) {
  set.seed(seed)
  arr <- array(runif(nSim * nAge * nClass * nYear), dim = c(nSim, nAge, nClass, nYear))
  # rows (age classes) sum to 1 over Class, like a real ALK/AWK
  arr <- aperm(apply(arr, c(1, 2, 4), \(x) x / sum(x)), c(2, 3, 1, 4))
  dimnames(arr) <- list(Sim = seq_len(nSim), Age = seq_len(nAge),
                        Class = seq_len(nClass), Year = seq_len(nYear))
  AddDimension(arr, 'Area')
}

.make_selectivity <- function(nSim, nClass, nYear, nArea, seed = 2) {
  set.seed(seed)
  arr <- array(runif(nSim * nClass * nYear * nArea), dim = c(nSim, nClass, nYear, nArea))
  dimnames(arr) <- list(Sim = seq_len(nSim), Class = seq_len(nClass),
                        Year = seq_len(nYear), Area = seq_len(nArea))
  arr
}

.make_N <- function(nSim, nAge, nYear, nArea, seed = 3) {
  set.seed(seed)
  arr <- array(runif(nSim * nAge * nYear * nArea, 0, 100), dim = c(nSim, nAge, nYear, nArea))
  dimnames(arr) <- list(Sim = seq_len(nSim), Age = seq_len(nAge),
                        Year = seq_len(nYear), Area = seq_len(nArea))
  arr
}

.expect_fast_matches_generic <- function(key, selectivity, sel_mode, landings_N, discards_N,
                                         tol = 1e-9) {
  # useCpp = TRUE (default): the C++ fast path
  fast    <- .CalcCatchAtSizeFleet(key, selectivity, sel_mode, landings_N, discards_N)
  generic <- .CatchAtSizeFleetGeneric(key, selectivity, sel_mode, landings_N, discards_N)

  expect_equal(dim(fast$LAS), dim(generic$LAS))
  expect_equal(dim(fast$DAS), dim(generic$DAS))
  expect_equal(as.numeric(fast$LAS), as.numeric(generic$LAS), tolerance = tol)
  expect_equal(as.numeric(fast$DAS), as.numeric(generic$DAS), tolerance = tol)

  # useCpp = FALSE: the SimControl(CalcCatchAtSizeCpp = FALSE) fallback --
  # .CatchAtSizeFleetFastR(), the pure-R fast path, NOT .CatchAtSizeFleetGeneric()
  # (dropping all the way back to the slow pre-optimisation path would defeat
  # the point of the fallback). Must independently match both.
  forced_r <- .CalcCatchAtSizeFleet(key, selectivity, sel_mode, landings_N, discards_N,
                                    useCpp = FALSE)
  expect_equal(dim(forced_r$LAS), dim(generic$LAS))
  expect_equal(as.numeric(forced_r$LAS), as.numeric(generic$LAS), tolerance = tol)
  expect_equal(as.numeric(forced_r$DAS), as.numeric(generic$DAS), tolerance = tol)
  expect_equal(as.numeric(forced_r$LAS), as.numeric(fast$LAS), tolerance = tol)
  expect_equal(as.numeric(forced_r$DAS), as.numeric(fast$DAS), tolerance = tol)
}

test_that(".CalcCatchAtSizeFleet() fast path matches the generic fallback -- key and N share Sim", {
  nSim <- 6; nAge <- 8; nClass <- 10; nYear <- 5; nArea <- 2
  key <- .make_key(nSim, nAge, nClass, nYear, nArea)
  sel <- .make_selectivity(nSim, nClass, nYear, nArea)
  landings_N <- .make_N(nSim, nAge, nYear, nArea, seed = 10)
  discards_N <- .make_N(nSim, nAge, nYear, nArea, seed = 11)

  .expect_fast_matches_generic(key, sel, "length", landings_N, discards_N)
  .expect_fast_matches_generic(key, sel, "age",    landings_N, discards_N)
})

test_that(".CalcCatchAtSizeFleet() fast path matches the generic fallback -- key Sim-broadcast (Sim=1)", {
  nSim <- 6; nAge <- 8; nClass <- 10; nYear <- 5; nArea <- 2
  key <- .make_key(1, nAge, nClass, nYear, nArea)   # ALK constant across sims
  sel <- .make_selectivity(nSim, nClass, nYear, nArea)
  landings_N <- .make_N(nSim, nAge, nYear, nArea, seed = 20)
  discards_N <- .make_N(nSim, nAge, nYear, nArea, seed = 21)

  .expect_fast_matches_generic(key, sel, "length", landings_N, discards_N)
  .expect_fast_matches_generic(key, sel, "age",    landings_N, discards_N)
})

test_that(".CalcCatchAtSizeFleet() fast path matches the generic fallback -- nSim = 1", {
  key <- .make_key(1, 5, 6, 3, 1)
  sel <- .make_selectivity(1, 6, 3, 1)
  landings_N <- .make_N(1, 5, 3, 1, seed = 30)
  discards_N <- .make_N(1, 5, 3, 1, seed = 31)

  .expect_fast_matches_generic(key, sel, "length", landings_N, discards_N)
  .expect_fast_matches_generic(key, sel, "age",    landings_N, discards_N)
})

test_that(".CalcCatchAtSizeFleet() conserves total numbers-at-age when projected to size", {
  # Each age row of the (possibly selectivity-weighted) key sums to 1 over
  # Class, so summing the fleet's landings/discards-at-size over Class must
  # reproduce the total numbers dying from landings/discards at age.
  nSim <- 4; nAge <- 7; nClass <- 9; nYear <- 4; nArea <- 3
  key <- .make_key(nSim, nAge, nClass, nYear, nArea)
  sel <- .make_selectivity(nSim, nClass, nYear, nArea)
  landings_N <- .make_N(nSim, nAge, nYear, nArea, seed = 40)
  discards_N <- .make_N(nSim, nAge, nYear, nArea, seed = 41)

  res <- .CalcCatchAtSizeFleet(key, sel, "length", landings_N, discards_N)

  expect_equal(sum(res$LAS), sum(landings_N), tolerance = 1e-8)
  expect_equal(sum(res$DAS), sum(discards_N), tolerance = 1e-8)
})

test_that(".CalcCatchAtSize() runs end-to-end and produces finite, non-negative output", {
  for (om_name in c("SingleStockOM", "MultiStockOM", "SeasonalSpatialOM")) {
    data(list = om_name, envir = environment())
    om <- get(om_name, envir = environment())
    om@nSim <- 2
    set.seed(1)
    hist <- Simulate(om, silent = TRUE)

    for (st in seq_along(hist@LandingsAtSize)) {
      for (fl in seq_along(hist@LandingsAtSize[[st]])) {
        las <- hist@LandingsAtSize[[st]][[fl]]
        das <- hist@DiscardsAtSize[[st]][[fl]]
        expect_false(anyNA(las), info = om_name)
        expect_false(anyNA(das), info = om_name)
        expect_true(all(is.finite(las)), info = om_name)
        expect_true(all(las >= 0), info = om_name)
        expect_true(all(das >= 0), info = om_name)
      }
    }
  }
})
