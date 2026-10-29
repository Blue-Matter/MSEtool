# TwoFleetOM has zero effort for every fleet in the first historical year;
# sim 2 is also zeroed in a later year so the fallback is per (sim, year).

.MaxSelBySimYear <- function(fleet)
  apply(fleet@Selectivity@MeanAtAge, c('Sim', 'Year'), max)

test_that("CombineFleets() keeps at-age curves defined in years where every fleet has zero F", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 4
  om <- Populate(om, silent = TRUE)
  zeroYear <- dimnames(om@Fleet[[1]][[1]]@Effort@Effort)$Year[5]
  for (fl in 1:2) om@Fleet[[1]][[fl]]@Effort@Effort[2, zeroYear] <- 0

  src <- om@Fleet[[1]]
  expect_true(all(purrr::map_lgl(src, \(f) all(f@Effort@Effort[, 1] == 0))))

  omc <- CombineFleets(om, silent = TRUE)
  fleet <- omc@Fleet[[1]][[1]]

  expect_true(all(.MaxSelBySimYear(fleet) == 1))
  expect_true(all(apply(fleet@Retention@MeanAtAge, c('Sim', 'Year'), max) > 0))
  expect_true(all(WeightFleetSelected(fleet) > 0))
  expect_true(all(WeightFleetRetained(fleet) > 0))
  expect_equal(unname(fleet@Effort@Effort[2, zeroYear]), 0)

  q <- purrr::map_dbl(src, \(f) f@Catchability@Efficiency[2, zeroYear])
  qSel <- q[1] * src[[1]]@Selectivity@MeanAtAge[2, , 1, 1] +
    q[2] * src[[2]]@Selectivity@MeanAtAge[2, , 1, 1]
  expect_equal(fleet@Selectivity@MeanAtAge[2, , zeroYear, 1], qSel / max(qSel))

  set.seed(1)
  hist <- Simulate(omc, silent = TRUE)
  expect_true(all(.MaxSelBySimYear(hist@OM@Fleet[[1]][[1]]) > 0))
  expect_no_error(
    Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  )
})

test_that(".FillZeroWeights() fills only the (sim, year) cells where weights sum to zero", {
  dn <- list(Sim = 1:2, Year = 2001:2002)
  w1 <- array(c(0, 1, 0, 2), c(2, 2), dn)
  w2 <- array(c(0, 3, 0, 0), c(2, 2), dn)
  fill1 <- array(5, c(2, 2), dn)
  fill2 <- array(7, c(2, 2), dn)

  out <- .FillZeroWeights(list(w1, w2), list(fill1, fill2))
  expect_equal(out[[1]], array(c(5, 1, 5, 2), c(2, 2), dn))
  expect_equal(out[[2]], array(c(7, 3, 7, 0), c(2, 2), dn))

  eq <- .FillZeroWeights(list(w1, w2))
  expect_equal(eq[[1]], array(c(1, 1, 1, 2), c(2, 2), dn))
  expect_equal(eq[[2]], array(c(1, 3, 1, 0), c(2, 2), dn))
})
