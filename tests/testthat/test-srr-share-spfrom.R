skip_on_cran()

# Stock 2 is a copy of stock 1 (same ages and SRR model) with its own h, SD,
# and R0, recruiting from stock 1's spawning production
.share_om <- function(SPFrom = NULL, ShareSPFrom = TRUE, nSim = 3) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- nSim
  Male <- om@Stock[[1]]
  Male@Name <- "Male"
  Male@SRR@Pars$h <- 0.4
  Male@SRR@SD <- 0.8
  Male@SRR@R0 <- 200
  Male@SRR@SPFrom <- if (is.null(SPFrom)) StockNames(om)[1] else SPFrom
  Male@SRR@ShareSPFrom <- ShareSPFrom
  om@Stock[[2]] <- Male
  names(om@Stock)[2] <- "Male"
  om
}

test_that("a single-source SPFrom stock shares the source's SRR parameters and deviations", {
  om  <- PopulateOM(.share_om(), silent = TRUE)
  Src <- om@Stock[[1]]@SRR
  Dep <- om@Stock[[2]]@SRR
  for (sl in c("Pars", "SD", "AC", "RecDevInit", "RecDevHist", "RecDevProj"))
    expect_identical(slot(Dep, sl), slot(Src, sl), info = sl)
  expect_false(identical(Dep@R0, Src@R0))
})

test_that("ShareSPFrom = FALSE keeps an independent SRR and deviations", {
  om  <- PopulateOM(.share_om(ShareSPFrom = FALSE), silent = TRUE)
  Src <- om@Stock[[1]]@SRR
  Dep <- om@Stock[[2]]@SRR
  expect_false(identical(Dep@Pars, Src@Pars))
  expect_false(identical(Dep@RecDevHist, Src@RecDevHist))
  expect_false(identical(Dep@RecDevProj, Src@RecDevProj))
})

test_that("a user-supplied RecDevHist is kept while the other deviations are shared", {
  om <- .share_om()
  nHist <- length(Years(om, 'Historical'))
  Supplied <- matrix(exp(seq(-0.5, 0.5, length.out = nHist)), nrow = om@nSim, ncol = nHist,
                     byrow = TRUE)
  om@Stock[[2]]@SRR@RecDevHist <- Supplied
  om  <- PopulateOM(om, silent = TRUE)
  Src <- om@Stock[[1]]@SRR
  Dep <- om@Stock[[2]]@SRR
  expect_equal(unname(Dep@RecDevHist), unname(Supplied))
  expect_identical(Dep@RecDevProj, Src@RecDevProj)
  expect_identical(Dep@Pars, Src@Pars)
})

test_that("weighted SPFrom blends are not shared", {
  om  <- .share_om(SPFrom = c("Example Albacore Stock" = 0.5, Male = 0.5))
  om  <- PopulateOM(om, silent = TRUE)
  expect_false(identical(om@Stock[[2]]@SRR@Pars, om@Stock[[1]]@SRR@Pars))
})

test_that("sharing aborts for stocks with different age classes", {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- 2
  om@Stock[[2]]@SRR@SPFrom <- StockNames(om)[1]
  expect_error(PopulateOM(om, silent = TRUE), "different .*Ages@Classes")
  om@Stock[[2]]@SRR@ShareSPFrom <- FALSE
  expect_s4_class(PopulateOM(om, silent = TRUE), "om")
})

test_that("re-populating a shared OM leaves it unchanged", {
  om1 <- PopulateOM(.share_om(), silent = TRUE)
  om2 <- PopulateOM(om1, silent = TRUE)
  expect_identical(om2@Stock[[2]]@SRR@RecDevHist, om1@Stock[[2]]@SRR@RecDevHist)
  expect_identical(om2@Stock[[2]]@SRR@Pars, om1@Stock[[2]]@SRR@Pars)
})

test_that("a sharing dependent's relative recruitment equals its source's every year", {
  om <- .share_om()
  om@Complexes <- list(Both = 1:2)
  om@Imp <- om@Imp[1]
  om@Obs <- om@Obs[1]
  hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
  R <- purrr::map(hist@Number, \(N) apply(N[, 1, , , drop = FALSE], c(1, 3), sum))
  R0 <- purrr::map_dbl(hist@OM@Stock, \(s) s@SRR@R0[1, 1])
  expect_equal(R[[2]] / R0[2], R[[1]] / R0[1], tolerance = 1e-8)
})
