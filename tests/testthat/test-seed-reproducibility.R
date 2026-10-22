.SimArrays <- function(x, nSim, path = "", out = list()) {
  if (isS4(x)) {
    for (s in methods::slotNames(x)) out <- .SimArrays(methods::slot(x, s), nSim, paste0(path, "@", s), out)
    return(out)
  }
  if (is.list(x) && !is.data.frame(x)) {
    nms <- names(x) %||% as.character(seq_along(x))
    for (i in seq_along(x)) out <- .SimArrays(x[[i]], nSim, paste0(path, "$", nms[i]), out)
    return(out)
  }
  if (is.numeric(x) && NROW(x) == nSim && "Sim" %in% names(dimnames(x))[1])
    out[[path]] <- matrix(x, nSim)
  out
}

# every array varying by Sim in `Small` equals the first rows of the same array in `Large`
.ExpectFirstSims <- function(Small, Large, nSmall, nLarge) {
  A <- .SimArrays(Small, nSmall)
  B <- .SimArrays(Large, nLarge)
  Varies <- names(A)[vapply(A, \(a) any(apply(a, 2, \(z) length(unique(z)) > 1)), logical(1))]
  expect_gt(length(Varies), 0)
  for (nm in intersect(Varies, names(B)))
    expect_equal(unname(A[[nm]]), unname(B[[nm]][seq_len(nSmall), , drop = FALSE]),
                 info = nm)
}

.MakeOM <- function(OM, n, Seed = 101) {
  nSim(OM) <- n
  OM@Seed <- Seed
  OM
}

test_that("simulation i is the same for any nSim (single stock)", {
  skip_on_cran()
  P1 <- Populate(.MakeOM(SingleStockOM, 4), silent = TRUE)
  P2 <- Populate(.MakeOM(SingleStockOM, 9), silent = TRUE)
  .ExpectFirstSims(P1, P2, 4, 9)

  H1 <- Simulate(P1, silent = TRUE)
  H2 <- Simulate(P2, silent = TRUE)
  .ExpectFirstSims(H1, H2, 4, 9)
  for (i in seq_len(4)) expect_equal(H1@Data[[i]], H2@Data[[i]], info = paste("Data sim", i))

  M1 <- Project(H1, MPs = c("CurrentCatch", "IndexRate"), silent = TRUE)
  M2 <- Project(H2, MPs = c("CurrentCatch", "IndexRate"), silent = TRUE)
  .ExpectFirstSims(M1, M2, 4, 9)
})

test_that("simulation i is the same for any nSim (multi-stock)", {
  skip_on_cran()
  P1 <- Populate(.MakeOM(MultiStockOM, 3), silent = TRUE)
  P2 <- Populate(.MakeOM(MultiStockOM, 7), silent = TRUE)
  .ExpectFirstSims(P1, P2, 3, 7)
  .ExpectFirstSims(Simulate(P1, silent = TRUE), Simulate(P2, silent = TRUE), 3, 7)
})

test_that("default length and weight classes don't depend on nSim", {
  skip_on_cran()
  P1 <- Populate(.MakeOM(SingleStockOM, 3), silent = TRUE)
  P2 <- Populate(.MakeOM(SingleStockOM, 20), silent = TRUE)
  expect_identical(P1@Stock[[1]]@Length@Classes, P2@Stock[[1]]@Length@Classes)

  Linf <- SingleStockOM@Stock[[1]]@Length@Pars$Linf
  expect_gte(.MaxClassBound(P1@Stock[[1]]@Length@Classes), max(Linf))

  Weight <- SingleStockOM@Stock[[1]]@Weight
  Weight@CVatAge <- 0.1
  Pop <- function(n) {
    L <- PopulateLength(SingleStockOM@Stock[[1]]@Length, Ages = SingleStockOM@Stock[[1]]@Ages,
                        Years = 2001:2010, nSim = n, seed = 1, silent = TRUE)
    PopulateWeight(Weight, Ages = SingleStockOM@Stock[[1]]@Ages, Length = L,
                   Years = 2001:2010, nSim = n, AWK = TRUE, seed = 2, silent = TRUE)
  }
  W1 <- Pop(3)
  W2 <- Pop(20)
  expect_gt(length(W1@Classes), 0)
  expect_identical(W1@Classes, W2@Classes)
})

test_that("Seed controls every generated value and reproduces exactly", {
  skip_on_cran()
  PA  <- Populate(.MakeOM(SingleStockOM, 5, Seed = 1), silent = TRUE)
  PA2 <- Populate(.MakeOM(SingleStockOM, 5, Seed = 1), silent = TRUE)
  PB  <- Populate(.MakeOM(SingleStockOM, 5, Seed = 2), silent = TRUE)

  A  <- .SimArrays(PA, 5)
  A2 <- .SimArrays(PA2, 5)
  B  <- .SimArrays(PB, 5)
  Varies <- names(A)[vapply(A, \(a) any(apply(a, 2, \(z) length(unique(z)) > 1)), logical(1))]
  for (nm in Varies) {
    expect_identical(A[[nm]], A2[[nm]], info = nm)
    expect_false(isTRUE(all.equal(A[[nm]], B[[nm]])), info = nm)
  }
})

test_that("components don't share random draws", {
  skip_on_cran()
  P <- Populate(.MakeOM(SingleStockOM, 48), silent = TRUE)
  Linf <- as.numeric(P@Stock[[1]]@Length@Pars$Linf)
  L5   <- as.numeric(P@Fleet[[1]][[1]]@Selectivity@Pars$L5) /
    as.numeric(P@Stock[[1]]@Maturity@Pars$L50)
  expect_lt(abs(stats::cor(Linf, L5)), 0.9)

  Bias <- as.numeric(P@Obs[[1]][[1]]@Landings@Bias)
  expect_lt(abs(stats::cor(Linf, Bias, method = "spearman")), 0.9)
})

test_that("Populate and Simulate leave the session random number stream unchanged", {
  skip_on_cran()
  set.seed(42)
  P <- Populate(.MakeOM(SingleStockOM, 3), silent = TRUE)
  H <- Simulate(P, silent = TRUE)
  x <- stats::runif(3)
  set.seed(42)
  expect_identical(x, stats::runif(3))
})

test_that("changing Seed after population warns", {
  skip_on_cran()
  P <- Populate(.MakeOM(SingleStockOM, 3), silent = TRUE)
  expect_warning(Seed(P) <- 5, "after the OM was populated")
  expect_warning(Populate(P, silent = TRUE), "after the OM was populated")

  O <- .MakeOM(SingleStockOM, 3)
  expect_no_warning(Seed(O) <- 5)
  expect_no_warning(Populate(O, silent = TRUE))
})

test_that("GenRecDevs and GenResiduals with Seed don't depend on nSim", {
  a <- Ages(10)
  Dev <- function(n, Seed) GenRecDevs(SD = 0.6, AC = 0.3, Ages = a, HistYears = 2000:2020,
                                      ProjYears = 2021:2040, nSim = n, Seed = Seed)
  D1 <- Dev(3, 7); D2 <- Dev(8, 7)
  for (nm in names(D1)) expect_equal(unname(D1[[nm]]), unname(D2[[nm]][1:3, ]))
  expect_false(isTRUE(all.equal(Dev(3, 7)$RecDevProj, Dev(3, 8)$RecDevProj)))

  set.seed(1); S1 <- GenRecDevs(SD = 0.6, Ages = a, HistYears = 2000:2020, ProjYears = 2021:2040, nSim = 3)
  set.seed(1); S2 <- GenRecDevs(SD = 0.6, Ages = a, HistYears = 2000:2020, ProjYears = 2021:2040, nSim = 3)
  expect_identical(S1, S2)

  R1 <- GenResiduals(SD = rep(0.3, 3), AC = rep(0.5, 3), Years = 2021:2040, Seed = 3)
  R2 <- GenResiduals(SD = rep(0.3, 6), AC = rep(0.5, 6), Years = 2021:2040, Seed = 3)
  expect_equal(unname(R1), unname(R2[1:3, ]))
})

test_that("ImportSS uses Seed for its generated values", {
  skip_on_cran()
  SSDir <- file.path("../../..", "WCNPOSWO-2023", "Final Base-case") |>
    testthat::test_path()
  skip_if_not(file.exists(file.path(SSDir, "Report.sso")))

  Import <- function(n, Seed)
    suppressMessages(ImportSS(SSDir, nSim = n, pYear = 5, silent = TRUE, Seed = Seed))
  OM1 <- Import(2, 1)
  OM2 <- Import(4, 1)
  OM3 <- Import(2, 2)
  expect_equal(OM1@Seed, 1)
  Proj <- \(OM) OM@Stock[[1]]@SRR@RecDevProj
  expect_equal(unname(Proj(OM1)), unname(Proj(OM2)[1:2, ]))
  expect_false(isTRUE(all.equal(Proj(OM1), Proj(OM3))))
})
