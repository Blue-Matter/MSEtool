.RecDevOM <- function(nSim = 40, pYear = 100, AC = 0.3, Correlated = NULL) {
  OM <- MultiStockOM
  OM@nSim <- nSim
  OM@pYear <- pYear
  for (st in 1:2) OM@Stock[[st]]@SRR@AC <- AC
  OM@Control$CorrelatedRecDevs <- Correlated
  OM
}

.LogDevs <- function(OM, slot = "RecDevProj")
  lapply(OM@Stock, \(s) log(methods::slot(s@SRR, slot)))

.SimCor <- function(L) vapply(seq_len(nrow(L[[1]])), \(i) stats::cor(L[[1]][i, ], L[[2]][i, ]), numeric(1))

# correlated log deviations with marginal SD `SD` and cross-stock correlation `rho`
.CorrelatedHist <- function(nSim, nYear, SD, rho, Seed = 1) {
  set.seed(Seed)
  Z1 <- matrix(stats::rnorm(nSim * nYear), nSim)
  Z2 <- rho * Z1 + sqrt(1 - rho^2) * matrix(stats::rnorm(nSim * nYear), nSim)
  lapply(list(Z1, Z2), \(z) exp(z * SD))
}

test_that("projection recruitment deviation SD matches SRR@SD", {
  P <- PopulateOM(.RecDevOM(), silent = TRUE)
  for (st in 1:2) {
    SRR <- P@Stock[[st]]@SRR
    Target <- as.numeric(SRR@SD)
    Ratio <- apply(log(SRR@RecDevProj), 1, stats::sd) / Target
    expect_equal(mean(Ratio), 1, tolerance = 0.06)
    expect_true(all(Ratio > 0.65 & Ratio < 1.35))
  }
})

test_that("generated historical deviations give independent projection deviations by default", {
  OM <- .RecDevOM()
  P <- PopulateOM(OM, silent = TRUE)
  Cor <- .SimCor(.LogDevs(P))
  expect_lt(abs(mean(Cor)), 0.06)
  expect_lt(stats::sd(Cor), 0.16)

  OM@Control$CorrelatedRecDevs <- FALSE
  PF <- PopulateOM(OM, silent = TRUE)
  for (st in 1:2)
    expect_identical(P@Stock[[st]]@SRR@RecDevProj, PF@Stock[[st]]@SRR@RecDevProj)
})

test_that("supplied correlated historical deviations are reproduced with SRR@SD", {
  OM <- .RecDevOM(AC = 0)
  Hist <- .CorrelatedHist(OM@nSim, OM@nYear, SD = 0.6, rho = 0.8)
  for (st in 1:2) {
    OM@Stock[[st]]@SRR@RecDevHist <- Hist[[st]]
    OM@Stock[[st]]@SRR@SD <- 0.3
  }
  P <- PopulateOM(OM, silent = TRUE)

  HistCor <- .SimCor(.LogDevs(P, "RecDevHist"))
  ProjCor <- .SimCor(.LogDevs(P))
  expect_gt(mean(ProjCor), 0.6)
  expect_lt(abs(mean(ProjCor) - mean(HistCor)), 0.06)
  expect_gt(stats::cor(HistCor, ProjCor), 0.5)

  for (st in 1:2) {
    SD <- apply(log(P@Stock[[st]]@SRR@RecDevProj), 1, stats::sd)
    expect_equal(mean(SD), 0.3, tolerance = 0.06)
  }

  OM@Control$CorrelatedRecDevs <- FALSE
  PF <- PopulateOM(OM, silent = TRUE)
  ProjCorF <- .SimCor(.LogDevs(PF))
  expect_lt(abs(mean(ProjCorF)), 0.06)
  expect_lt(stats::sd(ProjCorF), 0.16)
})

test_that("CorrelatedRecDevs = TRUE imposes the historical correlation of generated deviations", {
  P <- PopulateOM(.RecDevOM(AC = 0, Correlated = TRUE), silent = TRUE)
  HistCor <- .SimCor(.LogDevs(P, "RecDevHist"))
  ProjCor <- .SimCor(.LogDevs(P))
  expect_gt(stats::cor(HistCor, ProjCor), 0.5)
  for (st in 1:2) {
    SRR <- P@Stock[[st]]@SRR
    Ratio <- apply(log(SRR@RecDevProj), 1, stats::sd) / as.numeric(SRR@SD)
    expect_equal(mean(Ratio), 1, tolerance = 0.06)
  }
})

test_that("stocks with identical historical deviations share projection deviations", {
  OM <- .RecDevOM(nSim = 6, pYear = 30)
  Hist <- .CorrelatedHist(OM@nSim, OM@nYear, SD = 0.5, rho = 0)[[1]]
  for (st in 1:2) {
    OM@Stock[[st]]@SRR@RecDevHist <- Hist
    OM@Stock[[st]]@SRR@SD <- 0.5
  }
  for (Correlated in list(NULL, TRUE, FALSE)) {
    OM@Control$CorrelatedRecDevs <- Correlated
    P <- PopulateOM(OM, silent = TRUE)
    expect_identical(unname(P@Stock[[1]]@SRR@RecDevProj), unname(P@Stock[[2]]@SRR@RecDevProj))
    expect_gt(min(apply(log(P@Stock[[1]]@SRR@RecDevProj), 1, stats::sd)), 0.2)
  }
})

test_that("projection deviations are reproducible and don't depend on nSim", {
  OM <- .RecDevOM(nSim = 3, pYear = 20, Correlated = TRUE)
  P3 <- PopulateOM(OM, silent = TRUE)
  OM@nSim <- 6
  P6 <- PopulateOM(OM, silent = TRUE)
  for (st in 1:2)
    expect_equal(unname(P3@Stock[[st]]@SRR@RecDevProj),
                 unname(P6@Stock[[st]]@SRR@RecDevProj[1:3, ]))
})

test_that("invalid CorrelatedRecDevs is an error", {
  OM <- .RecDevOM(nSim = 2, pYear = 5, Correlated = "yes")
  expect_error(PopulateOM(OM, silent = TRUE), "CorrelatedRecDevs")
})
