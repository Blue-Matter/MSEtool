# Deterministic legacy OM: point parameter values, no process or effort error
.DeterministicLegacyOM <- function() {
  OM <- MSEtool::testOM
  OM@nsim <- 2
  OM@proyears <- 5
  OM@interval <- 1
  OM@qinc <- OM@qcv <- c(0, 0)
  for (s in slotNames(OM)) {
    v <- slot(OM, s)
    if (is.numeric(v) && length(v) == 2 && !s %in% c("EffYears", "EffLower", "EffUpper"))
      slot(OM, s) <- rep(mean(v), 2)
  }
  OM@Perr <- OM@AC <- OM@Esd <- OM@Msd <- OM@Linfsd <- OM@Ksd <- c(0, 0)
  OM@EffLower <- OM@EffUpper <- (OM@EffLower + OM@EffUpper) / 2
  OM
}

.ParityCache <- new.env()
.ParityPair <- function(MatCpars = FALSE) {
  key <- paste0("pair", MatCpars)
  if (is.null(.ParityCache[[key]])) {
    OM <- .DeterministicLegacyOM()
    if (MatCpars)
      OM@cpars$Mat_age <- suppressMessages(Simulate(OM, silent = TRUE))@SampPars$Stock$Mat_age
    Hist <- suppressMessages(Simulate(OM, silent = TRUE))
    om <- Convert(OM, silent = TRUE)
    .ParityCache[[key]] <- list(OM = OM, Hist = Hist, om = om, hist = Simulate(om, silent = TRUE))
  }
  .ParityCache[[key]]
}

test_that("legacy effort control points map to legacy historical years", {
  expect_equal(.LegacyEffortYears(c(0, 0.3, 0.6, 1), 50), c(1, 16, 31, 50))
  expect_equal(.LegacyEffortYears(1:10, 10), 1:10)
})

test_that("Vmaxlen at the maximum length class keeps selectivity at Linf", {
  lfs <- 80; linf <- 128; maxlen <- 146
  v <- .VmaxLenOpt(lfs, 0.5, linf, maxlen)
  sel <- DoubleNormal(c(linf, maxlen), L5 = 25, LFS = lfs, Vmaxlen = v)
  expect_equal(sel[1], 0.5, tolerance = 1e-8)
  expect_equal(.VmaxLenOpt(lfs, 1, linf, maxlen), 1)
})

test_that("converted OM reproduces legacy maturity-at-age and unfished spawning biomass", {
  skip_on_cran()
  P <- .ParityPair()
  SP <- P$Hist@SampPars$Stock
  Mat <- P$hist@OM@Stock[[1]]@Maturity@MeanAtAge[1, , 1]
  # legacy forces age-0 maturity to zero
  expect_equal(Mat[-1], SP$Mat_age[1, -1, 1], ignore_attr = TRUE, tolerance = 1e-6)
  expect_equal(as.numeric(P$hist@Unfished@Equilibrium@SBiomass[1, 1, 1]), SP$SSB0[1], tolerance = 1e-6)
})

test_that("converted OM reproduces the legacy historical effort path", {
  skip_on_cran()
  P <- .ParityPair()
  Find <- P$Hist@SampPars$Fleet$Find[1, ]
  Eff <- P$hist@OM@Fleet[[1]][[1]]@Effort@Effort[1, ]
  expect_equal(as.numeric(Eff), Find / Find[length(Find)], tolerance = 1e-8)
})

test_that("converted OM reproduces legacy relative selectivity", {
  skip_on_cran()
  for (MatCpars in c(FALSE, TRUE)) {
    P <- .ParityPair(MatCpars)
    FP <- P$Hist@SampPars$Fleet
    Sel <- P$hist@OM@Fleet[[1]][[1]]@Selectivity
    expect_equal(as.numeric(Sel@Pars$L5[1, 1]), FP$L5_y[1, 1], tolerance = 1e-6)
    expect_equal(as.numeric(Sel@Pars$LFS[1, 1]), FP$LFS_y[1, 1], tolerance = 1e-6)
    v4 <- Sel@MeanAtAge[1, , 1, 1]
    legacy <- FP$V_real[1, , 1]
    expect_equal(as.numeric(v4 / max(v4)), legacy / max(legacy), tolerance = 0.01)
  }
})

test_that("converted OM reproduces legacy spawning biomass with legacy spatial effort", {
  skip_on_cran()
  P <- .ParityPair()
  om <- P$om
  Years <- dimnames(om@Fleet[[1]][[1]]@Effort@Effort)$Year
  Size1 <- P$OM@Size_area_1[1]
  # legacy allocates effort in proportion to vulnerable biomass; equal density here
  om@Fleet[[1]][[1]]@Effort@Distribution <- array(rep(c(Size1, 1 - Size1), each = length(Years)),
                                                  c(1, length(Years), 2),
                                                  dimnames = list(Sim = 1, Year = Years, Area = 1:2))
  hist <- Simulate(om, silent = TRUE)
  legacySB <- rowSums(P$Hist@TSdata$SBiomass, dims = 2)[1, ]
  v4SB <- as.numeric(hist@SBiomass[1, 1, seq_len(P$OM@nyears)])
  expect_equal(v4SB, legacySB, ignore_attr = TRUE, tolerance = 2e-3)
})
