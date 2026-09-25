# PopulateCompObs() (R/populate-obs-comp.R): a CompObs with `ESS` but no
# `SampleSize` uses the populated `ESS` as `SampleSize`.

test_that("PopulateCompObs() defaults SampleSize to ESS", {
  set.seed(1)
  comp <- PopulateCompObs(CompObs(ESS = c(50, 100)), nSim = 4,
                          HistYears = 2001:2010, ProjYears = 2011:2015,
                          Bins = 0:10, BinName = "Age")

  expect_equal(unname(dim(comp@SampleSize)), c(4, 15))
  expect_identical(comp@SampleSize, comp@ESS)
  expect_true(all(comp@ESS >= 50 & comp@ESS <= 100))
})

test_that("PopulateCompObs() leaves a CompObs with only Theta ungenerated", {
  comp <- PopulateCompObs(CompObs(Theta = 0.5), nSim = 2,
                          HistYears = 2001:2010, ProjYears = 2011:2015,
                          Bins = 0:10, BinName = "Age")
  expect_null(comp@SampleSize)
  expect_null(comp@ESS)
})

test_that("Simulate() generates LandingsAtAge/LandingsAtSize from ESS-only CompObs", {
  skip_on_cran()

  MyObs <- Obs('ESS only')
  Landings(MyObs)       <- CatchObs(CV = c(0.1, 0.2), Bias = c(0.8, 1))
  LandingsAtAge(MyObs)  <- CompObs(ESS = c(50, 100))
  LandingsAtSize(MyObs) <- CompObs(ESS = c(50, 100))
  om <- OM(Name = 'ESS only', nSim = 4, nYear = 25, pYear = 10,
           Stock = ButterfishExStock, Fleet = AsympExFleet, Obs = MyObs,
           Imp = FullComplianceImp)

  set.seed(1)
  hist <- expect_no_error(Simulate(om, silent = TRUE))

  for (type in c('LandingsAtAge', 'LandingsAtSize')) {
    comp <- slot(hist@OM@Obs[[1]][[1]], type)
    expect_identical(comp@SampleSize, comp@ESS)

    for (x in seq_len(nSim(hist))) {
      val    <- slot(hist@Data[[x]][[1]], type)@Value
      totals <- apply(val, 1, sum, na.rm = TRUE)
      totals <- totals[totals > 0]
      expect_gt(length(totals), 0)
      expect_true(all(totals == round(comp@SampleSize[x, names(totals)])), info = type)
    }
  }
})
