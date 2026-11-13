# Herm plus group: a plus-group hazard strictly between 0 and 1 moves fish between
# stocks within the plus group every year, and Simulate() warns when that matters.

skip_on_cran()

.HermPlusGroupOM <- function(LastFrac) {
  data(HermOM, envir = environment())
  om <- HermOM
  om@nSim <- 1
  h <- om@Herm
  if (inherits(h, "stocktransition")) h <- list(h)
  Frac <- h[[1]]@Frac
  n <- dim(Frac)[2]
  Frac[, seq_len(n - 1)] <- matrix(seq(1, 0.5, length.out = n - 1), nrow(Frac), n - 1, byrow = TRUE)
  if (!is.null(LastFrac)) Frac[, n] <- LastFrac
  h[[1]]@Frac <- Frac
  om@Herm <- h
  om
}

.SimulateCatch <- function(om) {
  Warnings <- character(0)
  hist <- withCallingHandlers(Simulate(om, silent = TRUE), warning = function(w) {
    Warnings <<- c(Warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(hist = hist, PlusWarning = any(grepl("plus group", Warnings)))
}

.PlusGroupN <- function(hist, st) {
  N <- hist@Number[[st]]
  apply(N[, dim(N)[2], , , drop = FALSE], 3, sum)
}

test_that("a plus-group hazard between 0 and 1 with a large share in From warns", {
  Partial <- .SimulateCatch(.HermPlusGroupOM(0.4))
  expect_true(Partial$PlusWarning)

  NoTransition <- .SimulateCatch(.HermPlusGroupOM(0.5))
  expect_false(NoTransition$PlusWarning)
  expect_true(all(.PlusGroupN(Partial$hist, 1) < .PlusGroupN(NoTransition$hist, 1)))
})

test_that("a plus-group Frac of 0 empties From's plus group without a warning", {
  Complete <- .SimulateCatch(.HermPlusGroupOM(0))
  expect_false(Complete$PlusWarning)
  expect_true(all(.PlusGroupN(Complete$hist, 1) < 1e-8 * .PlusGroupN(Complete$hist, 2)))
})

test_that("no warning when almost none of From reaches the plus group", {
  data(HermOM, envir = environment())
  om <- HermOM
  om@nSim <- 1
  expect_false(.SimulateCatch(om)$PlusWarning)
})
