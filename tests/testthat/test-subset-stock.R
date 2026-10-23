# Subset(Stocks = ) on om/hist/mse objects keeps whole complexes and re-indexes
# OM@Complexes to the retained stocks.

test_that("numeric and character Stocks give the same consistent single-stock Hist", {
  skip_on_cran()
  om <- MultiStockOM
  om@nSim <- 3
  pYear(om) <- 3
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  Names <- StockNames(hist)

  for (st in 1:2) {
    h <- Subset(hist, Stocks = st)
    expect_identical(h, Subset(hist, Stocks = Names[st]))

    expect_equal(StockNames(h), Names[st])
    expect_equal(h@OM@Complexes, stats::setNames(list(1L), Names[st]))
    expect_equal(names(h@OM@Fleet), Names[st])
    expect_equal(names(h@OM@Imp), Names[st])
    expect_true(all(purrr::map_lgl(h@Data, \(d) identical(names(d), Names[st]))))
    expect_equal(dimnames(h@OM@StockTargeting@Targeting)$Stock, Names[st])
    expect_equal(dimnames(h@SBiomass)$Stock, Names[st])

    mse <- Project(h, MPs = c("CurrentEffort", "CurrentCatch"), silent = TRUE)
    expect_length(mse@Log$error, 0)
    expect_true(all(is.finite(mse@SBiomass)))
  }
})

test_that("Subset(Stocks = ) on an mse keeps the retained stock's results and advice", {
  skip_on_cran()
  om <- MultiStockOM
  om@nSim <- 3
  pYear(om) <- 3
  set.seed(1)
  mse <- Project(Simulate(om, silent = TRUE), MPs = "CurrentCatch", silent = TRUE)
  Butterfish <- StockNames(mse)[2]

  m2 <- Subset(mse, Stocks = 2)
  expect_equal(m2@OM@Complexes, stats::setNames(list(1L), Butterfish))
  expect_equal(m2@SBiomass, mse@SBiomass[, Butterfish, , , drop = FALSE])
  expect_equal(names(m2@Misc$Advice$CurrentCatch[[1]][[1]]), Butterfish)
})

test_that("Subset(Stocks = ) rejects unknown stocks and split complexes or links", {
  hist_stocks <- StockNames(MultiStockOM)
  expect_error(Subset(MultiStockOM, Stocks = 3), "out of range")
  expect_error(Subset(MultiStockOM, Stocks = "Nope"), "not found")

  om <- ComplexOM
  om@Complexes <- list(StockComplex = 1:2)
  expect_error(Subset(om, Stocks = 1), "whole complexes")

  om <- HermOM
  om@Complexes <- list(Female = 1, Male = 2)
  expect_error(Subset(om, Stocks = 1), "Herm")

  om <- MultiStockOM
  om@Complexes <- stats::setNames(list(1, 2), hist_stocks)
  om@Stock[[2]]@SRR@SPFrom <- hist_stocks[1]
  expect_error(Subset(om, Stocks = 2), "SPFrom")
  expect_equal(StockNames(Subset(om, Stocks = 1)), hist_stocks[1])
})
