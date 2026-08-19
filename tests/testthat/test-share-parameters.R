# .ValidateSPFrom() (R/populate-om.R) runs inside PopulateOM() right after
# .UpdateSPFrom(). Fixtures built by extending the packaged MultiStockOM
# (2 stocks) with a duplicated third stock/fleet/obs/imp entry where a 2+-hop
# chain needs to be exercised.

.make_multistock_om <- function(nStock = 2) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 2

  if (nStock == 2) return(om)

  stC <- om@Stock[[1]]
  Name(stC) <- "Stock C"
  om@Stock <- list(om@Stock[[1]], om@Stock[[2]], stC)
  om@Fleet <- c(om@Fleet, stats::setNames(list(om@Fleet[[1]]), "Stock C"))
  om@Obs   <- c(om@Obs,   stats::setNames(list(om@Obs[[1]]), "Stock C"))
  om@Imp   <- c(om@Imp,   stats::setNames(list(om@Imp[[1]]), "Stock C"))
  om
}

test_that("one-hop SPFrom validates and PopulateOM() succeeds", {
  om <- .make_multistock_om()
  sn <- StockNames(om)

  om@Stock[[2]]@SRR@SPFrom <- sn[1]
  out <- PopulateOM(om, silent = TRUE)
  expect_s4_class(out, "om")
})

test_that("self-reference SPFrom is a no-op", {
  om <- .make_multistock_om()
  sn <- StockNames(om)

  # explicit self-reference
  om1 <- om
  om1@Stock[[2]]@SRR@SPFrom <- sn[2]
  expect_no_error(PopulateOM(om1, silent = TRUE))

  # SPFrom unset entirely
  om2 <- om
  om2@Stock[[2]]@SRR@SPFrom <- NULL
  expect_no_error(PopulateOM(om2, silent = TRUE))
})

test_that("a 2-stock cycle aborts PopulateOM()", {
  om <- .make_multistock_om()
  sn <- StockNames(om)

  om@Stock[[1]]@SRR@SPFrom <- sn[2]
  om@Stock[[2]]@SRR@SPFrom <- sn[1]

  expect_error(PopulateOM(om, silent = TRUE))
})

test_that("a 2+-hop chain aborts PopulateOM()", {
  om <- .make_multistock_om(nStock = 3)
  sn <- StockNames(om)

  om@Stock[[2]]@SRR@SPFrom <- sn[3]  # B -> C
  om@Stock[[3]]@SRR@SPFrom <- sn[1]  # C -> A (B -> C -> A is a 2-hop chain)

  expect_error(PopulateOM(om, silent = TRUE))
})
