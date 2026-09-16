# Regression coverage for .CalcCatchAtSize()/.CalcCatchAtSizeFleet()
# (R/calc-catch-at-size.R, R/calc-conditioned-key.R).
#
# .CalcCatchAtSizeFleet() has two implementations of the same fast
# per-(Year,Area) contraction, batched over Sim -- .CatchAtSizeFleetCpp() and
# .CatchAtSizeFleetFastR() (the SimControl(CalcCatchAtSizeCpp = FALSE)
# fallback) -- selected via `useCpp`. Sim/Year/Area alignment between
# `key`/`selectivity` and `landings_N`/`discards_N` is an invariant enforced
# by every caller in the package (see .CalcCatchAtSize()'s .SubsetYear()/
# ExtendAreas() calls), not a runtime dispatch condition: if it's ever
# violated, .CalcCatchAtSizeFleet() aborts rather than silently falling back
# to a slower implementation (there used to be a third, generic
# implementation for this; it was removed once the alignment invariant was
# fixed to hold in the one real case that broke it -- see git history).

skip_on_cran()

.make_key <- function(nSim, nAge, nClass, nYear, nArea, seed = 1) {
  set.seed(seed)
  arr <- array(runif(nSim * nAge * nClass * nYear), dim = c(nSim, nAge, nClass, nYear))
  # rows (age classes) sum to 1 over Class, like a real ALK/AWK
  arr <- aperm(apply(arr, c(1, 2, 4), \(x) x / sum(x)), c(2, 3, 1, 4))
  dimnames(arr) <- list(Sim = seq_len(nSim), Age = seq_len(nAge),
                        Class = seq_len(nClass), Year = seq_len(nYear))
  # ExtendAreas(), not bare AddDimension() -- matches how key_area_default is
  # actually built in .CalcCatchAtSize() (broadcast across areas, since the
  # ALK itself doesn't vary by area). A bare Area=1 here previously left
  # these tests always hitting the (now-removed) generic path regardless of
  # `useCpp`, since the fast paths require an exact Area match, not a
  # broadcast -- this masked the fast path never actually being exercised at
  # nArea > 1 until the abort added in .CalcCatchAtSizeFleet() caught it.
  ExtendAreas(AddDimension(arr, 'Area'), Areas = seq_len(nArea))
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

# Deliberately naive, maximally-explicit reference implementation -- loops
# over every (sim, year, area, age) with no vectorisation tricks. Kept only
# in tests (not shipped) as an independent correctness anchor for the two
# production implementations, now that the generic broadcast implementation
# that used to serve this role has been removed from the package.
.naive_reference <- function(key, selectivity, sel_mode, landings_N, discards_N) {
  d_key <- dim(key); d_N <- dim(landings_N)
  nSim <- d_N[1]; nAge <- d_key[2]; nClass <- d_key[3]
  nYear <- d_N[3]; nArea <- d_N[4]
  key_nSim <- d_key[1]
  sel_nSim <- if (sel_mode == "length") dim(selectivity)[1] else 1L

  dn <- list(Sim = dimnames(landings_N)$Sim, Class = as.numeric(dimnames(key)$Class),
            Year = dimnames(landings_N)$Year, Area = dimnames(landings_N)$Area)
  LAS <- array(0, dim = c(nSim, nClass, nYear, nArea), dimnames = dn)
  DAS <- array(0, dim = c(nSim, nClass, nYear, nArea), dimnames = dn)

  key_nArea <- d_key[5]

  for (s in seq_len(nSim)) {
    ks <- if (key_nSim == 1) 1 else s
    ss <- if (sel_mode == "length" && sel_nSim > 1) s else 1
    for (y in seq_len(nYear)) {
      for (a in seq_len(nArea)) {
        ka <- if (key_nArea == 1) 1 else a
        for (age in seq_len(nAge)) {
          if (sel_mode == "length") {
            w <- key[ks, age, , y, ka] * selectivity[ss, , y, a]
            denom <- sum(w)
            if (denom == 0) denom <- .Machine$double.eps
            cond <- w / denom
            cond[!is.finite(cond)] <- 0
          } else {
            cond <- key[ks, age, , y, ka]
          }
          LAS[s, , y, a] <- LAS[s, , y, a] + cond * landings_N[s, age, y, a]
          DAS[s, , y, a] <- DAS[s, , y, a] + cond * discards_N[s, age, y, a]
        }
      }
    }
  }
  list(LAS = LAS, DAS = DAS)
}

.expect_both_match_naive <- function(key, selectivity, sel_mode, landings_N, discards_N,
                                     tol = 1e-9) {
  ref <- .naive_reference(key, selectivity, sel_mode, landings_N, discards_N)

  cpp <- .CalcCatchAtSizeFleet(key, selectivity, sel_mode, landings_N, discards_N, useCpp = TRUE)
  r   <- .CalcCatchAtSizeFleet(key, selectivity, sel_mode, landings_N, discards_N, useCpp = FALSE)

  expect_equal(dim(cpp$LAS), dim(ref$LAS))
  expect_equal(as.numeric(cpp$LAS), as.numeric(ref$LAS), tolerance = tol)
  expect_equal(as.numeric(cpp$DAS), as.numeric(ref$DAS), tolerance = tol)
  expect_equal(as.numeric(r$LAS), as.numeric(ref$LAS), tolerance = tol)
  expect_equal(as.numeric(r$DAS), as.numeric(ref$DAS), tolerance = tol)
}

test_that(".CalcCatchAtSizeFleet() (C++ and R) match a naive reference -- key and N share Sim", {
  nSim <- 6; nAge <- 8; nClass <- 10; nYear <- 5; nArea <- 2
  key <- .make_key(nSim, nAge, nClass, nYear, nArea)
  sel <- .make_selectivity(nSim, nClass, nYear, nArea)
  landings_N <- .make_N(nSim, nAge, nYear, nArea, seed = 10)
  discards_N <- .make_N(nSim, nAge, nYear, nArea, seed = 11)

  .expect_both_match_naive(key, sel, "length", landings_N, discards_N)
  .expect_both_match_naive(key, sel, "age",    landings_N, discards_N)
})

test_that(".CalcCatchAtSizeFleet() (C++ and R) match a naive reference -- key Sim-broadcast (Sim=1)", {
  nSim <- 6; nAge <- 8; nClass <- 10; nYear <- 5; nArea <- 2
  key <- .make_key(1, nAge, nClass, nYear, nArea)   # ALK constant across sims
  sel <- .make_selectivity(nSim, nClass, nYear, nArea)
  landings_N <- .make_N(nSim, nAge, nYear, nArea, seed = 20)
  discards_N <- .make_N(nSim, nAge, nYear, nArea, seed = 21)

  .expect_both_match_naive(key, sel, "length", landings_N, discards_N)
  .expect_both_match_naive(key, sel, "age",    landings_N, discards_N)
})

test_that(".CalcCatchAtSizeFleet() (C++ and R) match a naive reference -- nSim = 1", {
  key <- .make_key(1, 5, 6, 3, 1)
  sel <- .make_selectivity(1, 6, 3, 1)
  landings_N <- .make_N(1, 5, 3, 1, seed = 30)
  discards_N <- .make_N(1, 5, 3, 1, seed = 31)

  .expect_both_match_naive(key, sel, "length", landings_N, discards_N)
  .expect_both_match_naive(key, sel, "age",    landings_N, discards_N)
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

test_that(".CalcCatchAtSizeFleet() aborts when key/N Year or Area are misaligned", {
  nSim <- 3; nAge <- 4; nClass <- 5; nArea <- 2
  key <- .make_key(nSim, nAge, nClass, nYear = 5, nArea)          # Year = 1:5
  sel <- .make_selectivity(nSim, nClass, nYear = 5, nArea)
  landings_N <- .make_N(nSim, nAge, nYear = 3, nArea)             # Year = 1:3 -- misaligned
  discards_N <- .make_N(nSim, nAge, nYear = 3, nArea)

  expect_error(
    .CalcCatchAtSizeFleet(key, sel, "length", landings_N, discards_N),
    class = "rlang_error"
  )
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

test_that(".CalcCatchAtSize() does not error when a fleet's selectivity is on a different size-class grid than the stock's ALK", {
  # A fleet is allowed to define selectivity-at-size on its own class grid,
  # different from the stock's ALK/AWK (see compdata-class) -- .CalcCatchAtSize()
  # detects this (`recalc_key`) and recomputes the key via CalcAgeSizeKey() on
  # the fleet's classes. That recalculated key must itself be .SubsetYear()'d
  # and ExtendAreas()'d to align with landings_N/discards_N -- without that,
  # this would now abort instead of silently falling back to a slower path.
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE, control = SimControl(CalcCatchAtSize = TRUE))
  HistYears <- Years(hist, 'H')

  ask_classes <- as.numeric(dimnames(hist@OM@Stock[[1]]@Length@ALK)$Class)
  sel <- hist@OM@Fleet[[1]][[1]]@Selectivity@MeanAtLength
  new_classes <- ask_classes[seq(2, length(ask_classes), by = 2)]
  sel2 <- sel[, seq_len(length(new_classes)), , , drop = FALSE]
  dimnames(sel2)$Class <- as.character(new_classes)
  hist@OM@Fleet[[1]][[1]]@Selectivity@MeanAtLength <- sel2

  out_cpp <- .CalcCatchAtSize(hist, Years = HistYears, useCpp = TRUE)
  out_r   <- .CalcCatchAtSize(hist, Years = HistYears, useCpp = FALSE)

  las_cpp <- out_cpp@LandingsAtSize[[1]][[1]]
  las_r   <- out_r@LandingsAtSize[[1]][[1]]
  expect_false(anyNA(las_cpp))
  expect_true(all(is.finite(las_cpp)))
  expect_true(all(las_cpp >= 0))
  expect_equal(las_cpp, las_r, tolerance = 1e-8)
})
