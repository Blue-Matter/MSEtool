.CparsHist <- function(cpars, OM = .LegacyOM(proyears = 6)) {
  OM@cpars <- cpars
  Simulate(OM, silent = TRUE)
}

test_that("stock cpars override the sampled stock parameters", {
  skip_on_cran()
  OM <- .LegacyOM(proyears = 6)
  SP <- Simulate(OM, silent = TRUE)@SampPars$Stock
  nAge <- OM@maxage + 1
  allyears <- OM@nyears + OM@proyears

  H <- .CparsHist(list(Perr_y = matrix(1, OM@nsim, OM@maxage + allyears)))
  expect_true(all(H@SampPars$Stock$Perr_y == 1))

  H <- .CparsHist(list(Len_age = SP$Len_age, LatASD = SP$LatASD * 2))
  expect_equal(H@SampPars$Stock$Len_age, SP$Len_age)
  expect_equal(H@SampPars$Stock$LatASD, SP$LatASD * 2)

  H <- .CparsHist(list(Wt_age = SP$Wt_age * 1.1))
  expect_equal(H@SampPars$Stock$Wt_age, SP$Wt_age * 1.1)

  H <- .CparsHist(list(M_ageArray = array(0.3, c(OM@nsim, nAge, allyears))))
  expect_true(all(H@SampPars$Stock$M_ageArray == 0.3))

  H <- .CparsHist(list(Fdisc = rep(0.5, OM@nsim)))
  expect_true(all(H@SampPars$Stock$Fdisc == 0.5))
})

test_that("fleet cpars override selectivity, retention and fishing mortality", {
  skip_on_cran()
  OM <- .LegacyOM(proyears = 6)
  FP <- Simulate(OM, silent = TRUE)@SampPars$Fleet
  nAge <- OM@maxage + 1
  allyears <- OM@nyears + OM@proyears

  H <- .CparsHist(list(V = FP$V))
  expect_equal(H@SampPars$Fleet$V, FP$V)

  H <- .CparsHist(list(SLarray = FP$SLarray))
  expect_equal(H@SampPars$Fleet$SLarray, FP$SLarray)
  expect_equal(H@SampPars$Fleet$V, FP$V, tolerance = 1e-6)

  H <- .CparsHist(list(SLarray = FP$SLarray, V = FP$V))
  expect_equal(H@SampPars$Fleet$V, FP$V)

  H <- .CparsHist(list(retA = array(1, c(OM@nsim, nAge, allyears))))
  expect_true(all(H@SampPars$Fleet$retA == 1))

  H <- .CparsHist(list(retL = array(1, dim(FP$retL))))
  expect_true(all(H@SampPars$Fleet$retL == 1))

  H <- .CparsHist(list(Find = FP$Find, qs = FP$qs))
  expect_equal(H@SampPars$Fleet$Find, FP$Find)
  expect_equal(H@SampPars$Fleet$qs, FP$qs)
})

test_that("cpars MPA closes an area to fishing", {
  skip_on_cran()
  OM <- .LegacyOM(proyears = 6)
  allyears <- OM@nyears + OM@proyears
  H <- .CparsHist(list(MPA = matrix(rep(c(0, 1), each = allyears), allyears, 2)))
  expect_true(all(H@TSdata$Removals[, , 1] < 1e-8))
  expect_true(all(H@TSdata$Removals[, -1, 2] > 0))
})

test_that("invalid cpars dimensions are rejected", {
  skip_on_cran()
  expect_error(.CparsHist(list(MPA = matrix(c(1, 1, 0, 1), nrow = 1))), "MPA")
})

test_that("cpars SRR supplies a custom stock-recruit function", {
  skip_on_cran()
  OM <- .LegacyOM(proyears = 6)
  OM@Perr <- OM@AC <- c(0, 0)
  SRR <- list(SRRfun = function(SB, SRRpars) SRRpars$Rconst * SB / SB,
              SRRpars = data.frame(Rconst = rep(500, OM@nsim)))
  H <- .CparsHist(list(SRR = SRR), OM)
  expect_true(all(H@SampPars$Stock$SRrel == 3))
  Rec <- apply(H@AtAge$Number[, 1, -1, ], c(1, 2), sum)
  expect_equal(Rec, matrix(500, OM@nsim, OM@nyears - 1), ignore_attr = TRUE, tolerance = 1e-6)

  bad <- list(SRRfun = function(SB, x) SB, SRRpars = data.frame(x = rep(1, OM@nsim)))
  expect_error(.CparsHist(list(SRR = bad), OM), "SRRfun")
})
