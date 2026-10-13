skip_on_cran()

.ComplexOM <- function(Complexes, Depletion = FALSE) {
  om <- MultiStockOM
  om@nSim <- 3
  om@Complexes <- Complexes
  om@Imp <- om@Imp[seq_along(Complexes)]
  om@Obs <- om@Obs[seq_along(Complexes)]
  if (!Depletion)
    for (st in 1:2) om@Stock[[st]]@Depletion@Final <- NULL
  om <- .StartUp(om, silent = TRUE)
  # stock 2 is caught mostly by fleet 2, so the stocks' fleet splits differ
  om@Fleet[[2]][[2]]@Catchability@Efficiency <- om@Fleet[[2]][[2]]@Catchability@Efficiency * 10
  om
}

.RemovalsShare <- function(Hist, Stocks, Years) {
  Rem <- Hist@Landings[, Stocks, Years, , drop = FALSE] + Hist@Discards[, Stocks, Years, , drop = FALSE]
  Rem <- apply(Rem, c(1, 4), sum)
  matrix(Rem / rowSums(Rem), nrow(Rem))
}

test_that("the default TAC allocation of each complex uses its own stocks' removals", {
  Hist <- Simulate(.ComplexOM(list(Butterfish = 2, Albacore = 1)), silent = TRUE, Reduce = FALSE)
  nYear <- length(Years(Hist, 'H'))
  Last5 <- (nYear - 4):nYear

  for (cx in 1:2) {
    Stock <- Hist@OM@Complexes[[cx]]
    expect_equal(unname(Hist@OM@FleetAllocation[[cx]]), .RemovalsShare(Hist, Stock, Last5),
                 label = names(Hist@OM@Complexes)[cx])
  }
  expect_false(isTRUE(all.equal(Hist@OM@FleetAllocation[[1]], Hist@OM@FleetAllocation[[2]],
                                check.attributes = FALSE)))
})

test_that("the default TAC allocation of a multi-stock complex combines all of its stocks", {
  Hist <- Simulate(.ComplexOM(list(Both = 1:2)), silent = TRUE, Reduce = FALSE)
  nYear <- length(Years(Hist, 'H'))
  expect_equal(unname(Hist@OM@FleetAllocation[[1]]), .RemovalsShare(Hist, 1:2, (nYear - 4):nYear))

  om <- .ComplexOM(list(Both = 1:2), Depletion = TRUE)
  om@CatchFrac <- list(matrix(c(0.8, 0.2), 1, 2), matrix(c(0.2, 0.8), 1, 2))
  Hist <- Simulate(om, silent = TRUE, Reduce = FALSE)

  Final <- apply(Hist@Landings[, , nYear, , drop = FALSE] + Hist@Discards[, , nYear, , drop = FALSE], c(1, 2), sum)
  Weights <- Final / rowSums(Final)
  Expected <- Weights[, 1] %o% c(0.8, 0.2) + Weights[, 2] %o% c(0.2, 0.8)
  expect_equal(unname(Hist@OM@FleetAllocation[[1]]), unname(Expected))
})
