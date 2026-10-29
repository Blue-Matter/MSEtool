test_that("Seasonal per-recruit equilibrium with a plus group matches the deterministic dynamics", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@Stock[[1]]@Spatial <- Spatial()
  om@Stock[[1]]@SRR@SD <- 0
  om@nSim <- 2
  om@pYear <- 25
  expect_true(om@Stock[[1]]@Ages@PlusGroup)

  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)

  nSeason  <- om@Seasons
  LastProj <- tail(seq_len(dim(mse@SProduction)[3]), nSeason)
  LastHist <- tail(seq_len(dim(hist@SProduction)[3]), nSeason)
  SP0 <- ExtendSims(hist@Unfished@Equilibrium@SProduction, 2)
  R0  <- ExtendSims(hist@OM@Stock[[1]]@SRR@R0, 2)
  FTot <- SumOverFleet(mse@FDead)

  for (sim in 1:2) {
    inputs <- .PrepPerRecruitInputs(Subset(hist@OM@Stock, Sims = sim),
                                    Subset(hist@OM@Fleet, Sims = sim),
                                    Subset(Array2List(hist@Reference@SPR0), Sims = sim),
                                    max(floor(Years(hist@OM, "Historical"))))
    Eq <- function(lf) .OptCalcRefMSYSims(lf, inputs, StockNames(hist@OM), "Removals", option = 2)
    FAnnual <- sum(FTot[sim, 1, LastProj, 1])
    E <- Eq(uniroot(\(lf) as.numeric(Eq(lf)@FMSY) - FAnnual, c(log(1e-4), log(5)))$root)

    Removals <- sum(ExtendSims(mse@Landings, 2)[sim, 1, LastProj, , ]) +
      sum(ExtendSims(mse@Discards, 2)[sim, 1, LastProj, , ])
    expect_equal(Removals, sum(E@MSYLandings) + sum(E@MSYDiscards), tolerance = 0.005)

    SPRDyn <- (sum(mse@SProduction[sim, 1, LastProj, 1]) /
                 sum(mse@Number[[1]][sim, 1, LastProj, 1, 1])) /
      (sum(SP0[sim, 1, LastHist]) / sum(R0[sim, LastHist]))
    expect_equal(as.numeric(E@SPRMSY), SPRDyn, tolerance = 0.005)

    SB <- SB_SBMSY(mse, df = FALSE)[sim, 1, om@pYear, 1] * ExtendSims(hist@Reference@MSY@SBMSY, 2)[sim, 1, 1]
    expect_equal(SB, as.numeric(E@SBMSY), tolerance = 0.005)
  }
})
