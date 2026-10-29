.RelParsFixture <- function(nSim = 3) {
  data(SingleStockOM, envir = environment())
  om    <- SingleStockOM
  stock <- om@Stock[[1]]
  Years <- Years(om)
  Ages  <- stock@Ages
  Length   <- PopulateLength(stock@Length, Ages, Years, nSim, silent = TRUE)
  Maturity <- PopulateMaturity(stock@Maturity, Ages, Length, NULL, Years, nSim)
  list(om = om, Ages = Ages, Length = Length, Maturity = Maturity, Years = Years,
       nSim = nSim, L50 = as.numeric(.FindL50(Maturity)))
}

.PopRet <- function(fx, object, nSim = fx$nSim, force = FALSE)
  PopulateRetention(object, fx$Ages, fx$Length, NULL, fx$Maturity, NULL, nSim = nSim,
                    Years = fx$Years, silent = TRUE, force = force)

.PopSel <- function(fx, object, nSim = fx$nSim)
  PopulateSelectivity(object, fx$Ages, fx$Length, NULL, fx$Maturity, nSim = nSim,
                      Years = fx$Years, silent = TRUE)

test_that("relative at-length retention equals the absolute specification", {
  fx  <- .RelParsFixture()
  L50 <- fx$L50
  expect_gt(diff(range(L50)), 0)

  rel <- .PopRet(fx, Retention(Pars = list(RL50 = 1, RL50_95 = 0.2, MaxRet = 1), isRel = TRUE))
  abs <- .PopRet(fx, Retention(Pars = list(RL50 = L50, RL50_95 = 0.2 * L50, MaxRet = 1)))
  expect_false(rel@isRel)
  expect_equal(rel@MeanAtAge, abs@MeanAtAge)

  rel <- .PopRet(fx, Retention(Pars = list(RL = 1.1), isRel = TRUE))
  abs <- .PopRet(fx, Retention(Pars = list(RL = 1.1 * L50)))
  expect_equal(rel@MeanAtAge, abs@MeanAtAge)

  rel <- .PopRet(fx, Retention(Pars = list(LR5 = 0.8, LFR = 1.1, Rmaxlen = 1), isRel = TRUE))
  abs <- .PopRet(fx, Retention(Pars = list(LR5 = 0.8 * L50, LFR = 1.1 * L50, Rmaxlen = 1)))
  expect_equal(rel@MeanAtAge, abs@MeanAtAge)
})

test_that("relative at-length selectivity equals the absolute specification", {
  fx  <- .RelParsFixture()
  L50 <- fx$L50

  rel <- .PopSel(fx, Selectivity(Pars = list(SL50 = 1, SL50_95 = 0.2), isRel = TRUE))
  abs <- .PopSel(fx, Selectivity(Pars = list(SL50 = L50, SL50_95 = 0.2 * L50)))
  expect_false(rel@isRel)
  expect_equal(rel@MeanAtAge, abs@MeanAtAge)

  rel <- .PopSel(fx, Selectivity(Pars = list(SL = 0.9), isRel = TRUE))
  abs <- .PopSel(fx, Selectivity(Pars = list(SL = 0.9 * L50)))
  expect_equal(rel@MeanAtAge, abs@MeanAtAge)

  rel <- .PopSel(fx, Selectivity(Pars = list(L5 = 0.8, LFS = 1.1, Vmaxlen = 1), isRel = TRUE))
  abs <- .PopSel(fx, Selectivity(Pars = list(L5 = 0.8 * L50, LFS = 1.1 * L50, Vmaxlen = 1)))
  expect_equal(rel@MeanAtAge, abs@MeanAtAge)
})

test_that("re-populating a relative schedule does not rescale it", {
  fx <- .RelParsFixture()
  ret <- .PopRet(fx, Retention(Pars = list(LR5 = 0.8, LFR = 1.1, Rmaxlen = 1), isRel = TRUE))
  expect_equal(.PopRet(fx, ret, force = TRUE)@MeanAtAge, ret@MeanAtAge)
  expect_equal(.PopRet(fx, ret, nSim = 2)@Pars$LFR, ret@Pars$LFR)

  sel <- .PopSel(fx, Selectivity(Pars = list(SL50 = 1, SL50_95 = 0.2), isRel = TRUE))
  expect_equal(.PopSel(fx, sel, nSim = 2)@Pars$SL50, sel@Pars$SL50)
})

test_that("isRel with an at-age model is an informative error", {
  fx <- .RelParsFixture()
  expect_error(.PopRet(fx, Retention(Pars = list(RA50 = 5, RA50_95 = 2, MaxRet = 1), isRel = TRUE)),
               "at-length Retention model")
  expect_error(.PopSel(fx, Selectivity(Pars = list(SA50 = 3, SA50_95 = 1), isRel = TRUE)),
               "at-length Selectivity model")
})

test_that("an OM with relative at-length retention can be simulated again from Hist@OM", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 3
  om@Fleet[[1]][[1]]@Retention <- Retention(Pars = list(RL50 = 1, RL50_95 = 0.2, MaxRet = 1),
                                            isRel = TRUE)
  om@Fleet[[1]][[1]]@DiscardMortality <- DiscardMortality(MeanAtAge = rep(0.5, nAge(om@Stock[[1]])))
  hist <- Simulate(om, silent = TRUE)
  expect_true(all(apply(hist@Discards, 1, sum) > 0))
  expect_false(hist@OM@Fleet[[1]][[1]]@Retention@isRel)

  hist2 <- suppressMessages(Simulate(hist@OM, silent = TRUE))
  expect_equal(hist2@Discards, hist@Discards)
})
