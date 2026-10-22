skip_on_cran()

.herm_om <- function(R0Male = NULL, Seasons = 1, nYear = 20, LongRun = FALSE) {
  data(HermOM, envir = environment())
  om <- HermOM
  om@nSim    <- 2
  om@nYear   <- nYear
  om@Seasons <- Seasons
  if (!is.null(R0Male)) om@Stock[[2]]@SRR@R0 <- R0Male
  if (LongRun) {
    for (s in 1:2) {
      om@Stock[[s]]@SRR@SD <- 0
      om@Stock[[s]]@Spatial <- new("spatial")
      om@Stock[[s]]@Depletion@Final <- if (s == 1) 0.4 else numeric()
      for (f in seq_along(om@Fleet[[s]]))
        om@Fleet[[s]][[f]]@Effort@Effort <- data.frame(Year = c(0, 1), Lower = c(1, 1),
                                                       Upper = c(1, 1), CV = c(0, 0))
    }
  }
  om
}

# Stock x (all other dims) matrix of a reference point array
.herm_by_stock <- function(arr) {
  arr <- .Aperm(arr, c('Stock', setdiff(names(dimnames(arr)), 'Stock')))
  matrix(arr, nrow = dim(arr)[1])
}

# Equilibrium at the apical F reproducing the female stock's terminal-year recruitment
.herm_compare <- function(hist) {
  yrs    <- as.numeric(dimnames(hist@SProduction)$Year)
  lastCY <- max(floor(yrs))
  idx    <- which(floor(yrs) == lastCY)
  R0F    <- sum(hist@OM@Stock[[1]]@SRR@R0[1, floor(as.numeric(colnames(hist@OM@Stock[[1]]@SRR@R0))) == lastCY])
  RRobs  <- apply(hist@Number[[1]][, 1, idx, , drop = FALSE], 1, sum) / R0F
  SBobs  <- apply(hist@SBiomass[, , idx, drop = FALSE], 1:2, mean)
  Bobs   <- apply(hist@Biomass[, , idx, drop = FALSE], 1:2, mean)
  eq <- CalcEquilibrium(hist, apicalF = exp(seq(log(1e-3), log(2), length.out = 200)),
                        Years = lastCY)
  purrr::map(seq_along(RRobs), \(s) {
    rr <- eq@RelRecruits[s, 1, 1, ]
    ok <- rr > 1e-6
    At <- \(q) purrr::map_dbl(seq_len(dim(q)[2]), \(st)
      stats::splinefun(rr[ok], q[s, st, 1, ok])(RRobs[s]))
    list(SBeq = At(eq@SBiomass), SBobs = SBobs[s, ], Beq = At(eq@Biomass), Bobs = Bobs[s, ])
  })
}

test_that("Herm fished per-recruit reproduces the unfished equilibrium at F = 0", {
  hist <- Simulate(.herm_om(), silent = TRUE, control = SimControl(MSYRefs = FALSE))
  yr <- as.numeric(utils::tail(dimnames(hist@SProduction)$Year, 1))
  pr <- CalcPerRecruit(hist, apicalF = 1e-10, Years = yr)
  expect_equal(as.numeric(pr@SPR), rep(1, length(pr@SPR)), tolerance = 1e-6)
  eq <- CalcEquilibrium(hist, apicalF = 1e-10, Years = yr)
  ue <- hist@Unfished@Equilibrium
  expect_equal(as.numeric(drop(eq@SBiomass)), as.numeric(ue@SBiomass[, , as.character(yr)]), tolerance = 1e-6)
  expect_equal(as.numeric(drop(eq@Biomass)), as.numeric(ue@Biomass[, , as.character(yr)]), tolerance = 1e-6)
})

test_that("Herm equilibrium matches long-run dynamics (all males from sex change)", {
  hist <- Simulate(.herm_om(R0Male = 0, nYear = 150, LongRun = TRUE), silent = TRUE,
                   control = SimControl(MSYRefs = FALSE))
  for (res in .herm_compare(hist)) {
    expect_equal(unname(res$SBeq), unname(res$SBobs), tolerance = 1e-4)
    expect_equal(unname(res$Beq), unname(res$Bobs), tolerance = 1e-4)
    expect_gt(res$SBeq[2], 0)
  }
})

test_that("Herm equilibrium matches long-run dynamics (with primary males)", {
  hist <- Simulate(.herm_om(R0Male = 100, nYear = 150, LongRun = TRUE), silent = TRUE,
                   control = SimControl(MSYRefs = FALSE))
  for (res in .herm_compare(hist)) {
    expect_equal(unname(res$SBeq), unname(res$SBobs), tolerance = 1e-4)
    expect_equal(unname(res$Beq), unname(res$Bobs), tolerance = 1e-4)
  }
})

test_that("seasonal Herm equilibrium matches long-run dynamics", {
  hist <- Simulate(.herm_om(R0Male = 0, Seasons = 4, nYear = 100, LongRun = TRUE), silent = TRUE,
                   control = SimControl(MSYRefs = FALSE))
  for (res in .herm_compare(hist)) {
    expect_equal(unname(res$SBeq), unname(res$SBobs), tolerance = 2e-4)
    expect_equal(unname(res$Beq), unname(res$Bobs), tolerance = 2e-4)
  }
})

test_that("Herm yield-per-recruit is lifetime yield including catch after sex change", {
  hist <- Simulate(.herm_om(R0Male = 0), silent = TRUE, control = SimControl(MSYRefs = FALSE))
  yr <- as.numeric(utils::tail(dimnames(hist@SProduction)$Year, 1))
  Fg <- c(0.02, 0.08, 0.15)
  pr <- CalcPerRecruit(hist, apicalF = Fg, Years = yr)
  Lifetime <- MSEtool:::.AddHermLifetime(pr@Removals, pr@Misc$HermCross, 'Removals')
  eq <- CalcEquilibrium(hist, apicalF = Fg, Years = yr)
  PerFemaleRecruit <- apply(eq@Removals, c(1, 3, 4), sum)[, 1, ] / eq@Recruits[, 1, 1, ]
  expect_equal(unname(Lifetime[, 1, 1, ]), unname(PerFemaleRecruit), tolerance = 1e-8)
  expect_true(all(Lifetime[, 1, 1, ] > pr@Removals[, 1, 1, ]))
})

test_that("a dependent stock's crash and replacement reference points are its source's", {
  hist <- Simulate(.herm_om(R0Male = 0), silent = TRUE)
  ref  <- hist@Reference
  for (sl in c('FCrash', 'SPRcrash', 'FMed', 'FSPR')) {
    arr <- .herm_by_stock(slot(ref, sl))
    expect_identical(arr[2, ], arr[1, ], info = sl)
  }
  expect_true(all(ref@FCrash > 0, na.rm = TRUE))
})

test_that("crash reference points are NA for a weighted SPFrom blend", {
  hist <- Simulate(.herm_om(), silent = TRUE, control = SimControl(MSYRefs = FALSE))
  Blend <- c(0.5, 0.5) |> stats::setNames(StockNames(hist))
  hist@OM@Stock[[2]]@SRR@SPFrom <- Blend
  Fcrash <- MSEtool:::.SpawningSourceRefPoint(hist@Reference@FCrash, hist@OM) |>
    .herm_by_stock()
  expect_true(all(is.na(Fcrash[2, ])))
  expect_false(anyNA(Fcrash[1, ]))
})

test_that("CalcMSYCpp uses the R implementation for Herm OMs", {
  hist <- Simulate(.herm_om(R0Male = 0), silent = TRUE, control = SimControl(MSYRefs = FALSE))
  yr <- as.numeric(utils::tail(dimnames(hist@SProduction)$Year, 1))
  msy_r   <- CalcMSY(hist, Years = yr, silent = TRUE)
  msy_cpp <- MSEtool:::CalcMSYCpp(hist, Years = yr, silent = TRUE)
  expect_equal(msy_r, msy_cpp)
  expect_true(all(msy_r@SBMSY > 0))
})

test_that("Herm-linked stocks in different complexes warn", {
  hist <- Simulate(.herm_om(), silent = TRUE, control = SimControl(MSYRefs = FALSE))
  rlang::reset_warning_verbosity("MSEtool_herm_cross_complex")
  expect_warning(MSEtool:::.HermForStocks(hist@OM, StockNames(hist)[1]), "different complexes")
})
