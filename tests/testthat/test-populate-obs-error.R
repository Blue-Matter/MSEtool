# .PopulateObsError() (R/populate-obs-helpers.R), shared by
# PopulateEffortObs()/PopulateCatchObs()/PopulateIndexObs(). PopulateObs()
# ends every call with ReduceDims(Obs, IncYear = TRUE), which can collapse an
# Error array's Year dimension down to only the years where its value
# actually changed (see .UniqueYears()/ReduceDims() in R/utils-unique-years.R
# and R/reduce.R). Re-populating that object later (e.g. PopulateOM(force =
# TRUE) on an already-populated OM) must re-expand such a reduced array
# rather than reject it as malformed - it's losslessly reconstructible via
# forward-fill, exactly what ExtendYears() already does elsewhere.

test_that("a ReduceDims-collapsed Error array is re-expanded by forward-fill, not rejected", {
  Years <- 2001:2010
  full  <- c(0.9, 0.95, 1.0, 1.1, 1.1, 0.8, 0.85, 1.2, 1.05, 0.97)
  # column 5 duplicates column 4 -> ReduceDims(IncYear=TRUE) would drop it
  reduced <- full[-5]
  reduced_years <- Years[-5]

  Index <- IndicesObs(CV = 0.2)
  Index@Error <- matrix(reduced, nrow = 1,
                        dimnames = list(Sim = 1, Year = reduced_years))

  out <- MSEtool:::.PopulateObsError(Index, nSim = 1, Years = Years)

  expect_equal(unname(dim(out)), c(1, 10))
  expect_equal(as.numeric(dimnames(out)$Year), Years)
  expect_equal(as.numeric(out[1, ]), full, tolerance = 1e-8)
})

test_that("a reduced Error array with years outside `Years` still errors", {
  Years <- 2001:2010
  Index <- IndicesObs(CV = 0.2)
  # 9 columns, but year 1999 isn't in the target `Years` grid at all -- not a
  # ReduceDims artifact of this Years grid, genuinely malformed
  Index@Error <- matrix(rep(1, 9), nrow = 1,
                        dimnames = list(Sim = 1, Year = c(1999, 2003:2010)))

  expect_error(MSEtool:::.PopulateObsError(Index, nSim = 1, Years = Years),
              "wrong number of columns")
})

test_that("PopulateOM(force = TRUE) does not error on a ReduceDims-collapsed Obs Error array", {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1
  om1 <- PopulateOM(om, silent = TRUE)

  # manually collapse one Obs Error array the same way ReduceDims would,
  # deterministically (rather than relying on a coincidental RNG tie)
  e <- om1@Obs[[1]][[1]]@Survey@Error
  years <- as.numeric(dimnames(e)$Year)
  drop_col <- 5L
  e[1, drop_col] <- e[1, drop_col - 1L]  # force a tie ReduceDims would collapse
  om1@Obs[[1]][[1]]@Survey@Error <- e[, -drop_col, drop = FALSE]

  # PopulateObs() re-collapses via ReduceDims() at the end of every populate,
  # so an exact forced tie is expected to come back out reduced again - the
  # point of this test is that the round-trip doesn't error, not the count.
  expect_no_error(om2 <- PopulateOM(om1, silent = TRUE, force = TRUE))
  expect_true(unname(ncol(om2@Obs[[1]][[1]]@Survey@Error)) <= length(years))
})

test_that("re-populating with force = TRUE does not re-apply AC to an already-transformed Error", {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 1
  om1 <- PopulateOM(om, silent = TRUE)

  expect_true(any(om1@Obs[[1]][[1]]@Survey@AC != 0))

  e1 <- om1@Obs[[1]][[1]]@Survey@Error
  om2 <- PopulateOM(om1, silent = TRUE, force = TRUE)
  e2 <- om2@Obs[[1]][[1]]@Survey@Error
  om3 <- PopulateOM(om2, silent = TRUE, force = TRUE)
  e3 <- om3@Obs[[1]][[1]]@Survey@Error

  expect_equal(as.numeric(e2), as.numeric(e1), tolerance = 1e-8)
  expect_equal(as.numeric(e3), as.numeric(e1), tolerance = 1e-8)
})
