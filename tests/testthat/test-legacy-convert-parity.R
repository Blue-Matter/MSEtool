# Deterministic legacy OM: point parameter values, no process or effort error,
# and maturity-at-age given in cpars so both engines use the same schedule
.ParityOM <- function() {
  OM <- .LegacyOM(nsim = 2, proyears = 5)
  for (s in slotNames(OM)) {
    v <- slot(OM, s)
    if (is.numeric(v) && length(v) == 2 && !s %in% c("EffYears", "EffLower", "EffUpper"))
      slot(OM, s) <- rep(mean(v), 2)
  }
  OM@Perr <- OM@AC <- OM@Esd <- OM@Msd <- OM@Linfsd <- OM@Ksd <- c(0, 0)
  OM@EffLower <- OM@EffUpper <- (OM@EffLower + OM@EffUpper) / 2
  OM@cpars$Mat_age <- Simulate(OM, silent = TRUE)@SampPars$Stock$Mat_age
  OM
}

test_that("converted OM reproduces legacy unfished and terminal spawning biomass", {
  skip_on_cran()
  OM <- .ParityOM()
  Hist <- Simulate(OM, silent = TRUE)
  hist <- Simulate(Convert(OM, silent = TRUE), silent = TRUE)

  # identical simulations are reduced to one in the converted OM
  expect_equal(as.numeric(hist@Unfished@Equilibrium@SBiomass[1, 1, 1]),
               Hist@SampPars$Stock$SSB0[1], tolerance = 1e-6)

  legacySB <- rowSums(Hist@TSdata$SBiomass, dims = 2)[1, ]
  v4SB <- hist@SBiomass[1, 1, seq_len(OM@nyears)]
  expect_equal(as.numeric(v4SB[1]), legacySB[1], tolerance = 1e-6)
  expect_equal(as.numeric(v4SB[OM@nyears]), legacySB[OM@nyears], tolerance = 1e-3)
})

test_that("converted OM uses the legacy at-age schedules", {
  skip_on_cran()
  OM <- .ParityOM()
  SP <- Simulate(OM, silent = TRUE)@SampPars$Stock
  Stock <- Simulate(Convert(OM, silent = TRUE), silent = TRUE)@OM@Stock[[1]]
  expect_equal(Stock@NaturalMortality@MeanAtAge[1, , 1], SP$M_ageArray[1, , 1], ignore_attr = TRUE, tolerance = 1e-6)
  expect_equal(Stock@Length@MeanAtAge[1, , 1], SP$Len_age[1, , 1], ignore_attr = TRUE, tolerance = 1e-6)
  expect_equal(Stock@Weight@MeanAtAge[1, , 1], SP$Wt_age[1, , 1], ignore_attr = TRUE, tolerance = 1e-6)
  expect_equal(Stock@Maturity@MeanAtAge[1, , 1], SP$Mat_age[1, , 1], ignore_attr = TRUE, tolerance = 1e-6)
})
