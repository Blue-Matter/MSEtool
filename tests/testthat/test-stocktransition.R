# stocktransition class + Herm() constructor (R/class-stocktransition.R,
# R/constructor-stocktransition.R). Engine consumption lands in a later
# goal - these only check construction, slot values, and om@Herm typing.

test_that("Herm() builds a stocktransition object from stock indices", {
  Frac <- array(c(1, 1, 1, 0.5, 0, 0), dim = c(1, 6))
  h <- Herm(From = 1, To = 2, Frac = Frac)

  expect_s4_class(h, "stocktransition")
  expect_equal(h@From, 1)
  expect_equal(h@To, 2)
  expect_equal(h@Frac, Frac)
  expect_equal(h@Misc, list())
})

test_that("Herm() builds a stocktransition object from stock names", {
  h <- Herm(From = "Female", To = "Male")

  expect_s4_class(h, "stocktransition")
  expect_equal(h@From, "Female")
  expect_equal(h@To, "Male")
  expect_null(h@Frac)
})

test_that("Herm() rejects From == To and missing arguments", {
  expect_error(Herm(From = 1, To = 1))
  expect_error(Herm(From = "A", To = "A"))
  expect_error(Herm(From = 1))
})

test_that("Herm() rejects Frac values outside [0, 1]", {
  expect_error(Herm(From = 1, To = 2, Frac = array(1.5, dim = c(1, 5))))
  expect_error(Herm(From = 1, To = 2, Frac = array(-0.1, dim = c(1, 5))))
})

test_that("om@Herm accepts a list of stocktransition objects", {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  sn <- StockNames(om)

  h <- Herm(From = sn[1], To = sn[2], Frac = array(1, dim = c(1, 5)))
  om@Herm <- list(h)

  expect_no_error(methods::validObject(om))
  expect_true(is.list(om@Herm))
  expect_s4_class(om@Herm[[1]], "stocktransition")
})

test_that("Herm() acts as a pass-through accessor on om objects", {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  sn <- StockNames(om)

  expect_null(Herm(om))

  h <- Herm(From = sn[1], To = sn[2])
  om@Herm <- list(h)
  out <- Herm(om)

  expect_true(is.list(out))
  expect_s4_class(out[[1]], "stocktransition")
})

# .HermHazardRate() (R/utils-herm.R) - direct port of legacy hrate()
# (R/zz_popdyn_MICE.R:723-738), adapted to Frac's "remaining" convention
# (starts near 1, decreases toward 0) vs legacy's "transitioned" convention
# (starts at 0, ends at 1): legacy_frac == 1 - Frac, so legacy's m1frac ==
# Frac directly, and the hazard ratio is identical either way.

.legacy_hrate <- function(frac) {
  m1frac <- 1 - frac
  ind1 <- seq_len(length(frac) - 1)
  ind2 <- ind1 + 1
  hrate <- rep(0, length(frac))
  hrate[ind2] <- 1 - (m1frac[ind2] / m1frac[ind1])
  hrate[is.na(hrate)] <- 1
  hrate[hrate < 0] <- 0
  hrate
}

test_that(".HermHazardRate() matches legacy hrate() on a hand-computed curve", {
  OurFrac <- c(1, 0.8, 0.5, 0.2, 0)
  legacy_frac <- 1 - OurFrac
  expected <- .legacy_hrate(legacy_frac)

  # hand-computed directly too: hazard[1]=0;
  # hazard[a] = clamp(1 - Frac[a]/Frac[a-1], 0, 1), 1 when Frac[a-1]<=0
  hand <- c(0, 1 - 0.8/1, 1 - 0.5/0.8, 1 - 0.2/0.5, 1)
  expect_equal(expected, hand, tolerance = 1e-8)

  Frac <- array(OurFrac, dim = c(1, 5), dimnames = list(Sim = 1, Age = 1:5))
  hazard <- .HermHazardRate(Frac)

  expect_equal(as.numeric(hazard[1, ]), expected, tolerance = 1e-8)
  expect_equal(as.numeric(hazard[1, 1]), 0)
})

test_that(".HermHazardRate() broadcasts across a Year dimension when given [sim, age, year]", {
  OurFrac <- c(1, 0.8, 0.5, 0.2, 0)
  Frac3 <- array(OurFrac, dim = c(1, 5, 3),
                 dimnames = list(Sim = 1, Age = 1:5, Year = 2001:2003))
  hazard <- .HermHazardRate(Frac3)

  expect_equal(unname(dim(hazard)), c(1L, 5L, 3L))
  expect_equal(as.numeric(hazard[1, , 1]), as.numeric(hazard[1, , 3]))
})

test_that(".HermHazardRate() returns NULL for NULL input", {
  expect_null(.HermHazardRate(NULL))
})

# .PrepHistMisc()'s Herm block. R/utils-startup.R's `stop('Herm not done
# yet!')` gate blocks Simulate()/PopulateOM() when om@Herm is set, so build
# Hist with Herm unset first, then attach Herm directly to hist@OM before
# calling .PrepHistMisc() in isolation - this does not touch or bypass that
# gate (it is not called on this path).

test_that(".PrepHistMisc() builds TransitionHazard/To/FromStock/Flag for a Herm pair", {
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1
  sn <- StockNames(om)

  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  nAge1 <- nAge(hist@OM@Stock[[1]])
  Frac <- array(1, dim = c(1, nAge1))
  Frac[1, 8:nAge1] <- seq(1, 0, length.out = nAge1 - 7)

  hist@OM@Herm <- list(Herm(From = sn[1], To = sn[2], Frac = Frac))

  hist2 <- .PrepHistMisc(hist)

  expect_equal(hist2@Misc$TransitionFlag, 1)
  expect_equal(hist2@Misc$TransitionToStock, 2L)
  expect_equal(hist2@Misc$TransitionFromStock, 1L)
  expect_equal(length(hist2@Misc$TransitionHazard), 1)

  nyears <- length(Years(hist2, 'H'))
  expect_equal(unname(dim(hist2@Misc$TransitionHazard[[1]])), c(1L, nAge1, nyears))

  expected_hazard <- .HermHazardRate(Frac)
  expect_equal(
    as.numeric(hist2@Misc$TransitionHazard[[1]][1, , 1]),
    as.numeric(expected_hazard[1, ])
  )
  # broadcast forward-filled across all years
  expect_equal(
    as.numeric(hist2@Misc$TransitionHazard[[1]][1, , 1]),
    as.numeric(hist2@Misc$TransitionHazard[[1]][1, , nyears])
  )

  expect_no_error(.CheckHistMisc(hist2, 'Historical'))
})

test_that(".PrepHistMisc() sets TransitionFlag = 0 and empty entries with no Herm", {
  skip_on_cran()
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1

  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  hist2 <- .PrepHistMisc(hist)

  expect_equal(hist2@Misc$TransitionFlag, 0)
  expect_equal(length(hist2@Misc$TransitionHazard), 0)
  expect_equal(length(hist2@Misc$TransitionToStock), 0)
  expect_equal(length(hist2@Misc$TransitionFromStock), 0)

  expect_no_error(.CheckHistMisc(hist2, 'Historical'))
})
