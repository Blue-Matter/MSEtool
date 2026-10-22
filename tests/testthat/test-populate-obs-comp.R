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

# Re-populating ReduceDims() output (e.g. AdjustSeasonalFecundity() or a
# repeated Populate()) must reconstruct the populated arrays by Year name.
.RoundTripComp <- function(Comp, nSim = 3, HistYears = 2001:2010, ProjYears = 2011:2013,
                           Bins = 0:10) {
  first <- PopulateCompObs(Comp, nSim, HistYears, ProjYears, Bins, BinName = "Age")
  red   <- ReduceDims(first, IncYear = TRUE)
  list(first = first, reduced = red,
       again = PopulateCompObs(red, nSim, HistYears, ProjYears, Bins, BinName = "Age"))
}

test_that("PopulateCompObs() re-populates a reduced Shift", {
  Years <- 2001:2013
  cp <- array(rep(c(0.2, -0.3), c(5, 8)), dim = c(1, 13, 11),
              dimnames = list(Sim = 1, Year = Years, Age = 0:10))
  set.seed(1)
  simvar <- array(rnorm(33), dim = c(3, 1, 11),
                  dimnames = list(Sim = 1:3, Year = 2001, Age = 0:10))
  cases <- list(scalar = list(0.1, c(1, 1, 11)),
                binvec = list(seq(-0.5, 0.5, length.out = 11), c(1, 1, 11)),
                changepoint = list(cp, c(1, 2, 11)),
                simvar = list(simvar, c(3, 1, 11)))
  for (nm in names(cases)) {
    rt <- .RoundTripComp(CompObs(SampleSize = 100, ESS = 40, Shift = cases[[nm]][[1]]))
    expect_equal(unname(dim(rt$reduced@Shift)), cases[[nm]][[2]], label = nm)
    expect_identical(rt$again@Shift, rt$first@Shift, label = nm)
  }
})

test_that("PopulateCompObs() maps a named Shift by dimname, not position", {
  shift <- aperm(array(1:33 / 10, dim = c(3, 11, 1),
                       dimnames = list(Sim = 1:3, Age = 0:10, Year = 2001)), c(2, 1, 3))
  comp <- PopulateCompObs(CompObs(SampleSize = 100, Shift = shift), nSim = 3,
                          HistYears = 2001:2010, ProjYears = 2011:2013,
                          Bins = 0:10, BinName = "Age")
  expect_equal(unname(dim(comp@Shift)), c(3, 13, 11))
  expect_equal(comp@Shift["2", "2012", "4"], shift["4", "2", "2001"])
})

test_that("PopulateCompObs() keeps change-point ESS years when re-populated", {
  ess <- array(rep(c(40, 80), c(5, 8)), dim = c(1, 13),
               dimnames = list(Sim = 1, Year = 2001:2013))
  rt <- .RoundTripComp(CompObs(SampleSize = 100, ESS = ess))
  expect_equal(dimnames(rt$reduced@ESS)$Year, c("2001", "2006"))
  expect_identical(rt$again@ESS, rt$first@ESS)
  expect_equal(unname(rt$again@ESS[, "2003"]), rep(40, 3))
})

test_that("Populate() then Simulate() runs with a composition Shift", {
  skip_on_cran()

  obs <- CommercialFleetObs
  obs@LandingsAtAge <- CompObs(SampleSize = 100, ESS = 40, Shift = 0.1)

  om <- OM(nSim = 3, nYear = 10, pYear = 3, Stock = ButterfishExStock,
           Fleet = AsympExFleet, Obs = obs, Imp = FullComplianceImp)
  expect_no_error(Simulate(Populate(om, silent = TRUE), silent = TRUE))

  stock <- SeasonalSpatialExStock
  stock@Spatial <- Spatial()
  om <- OM(nSim = 3, nYear = 10, pYear = 3, Seasons = 12, Stock = stock,
           Fleet = DomeExFleet, Obs = obs, Imp = FullComplianceImp)
  pop <- expect_no_error(Populate(om, silent = TRUE))
  comp <- pop@Obs[[1]][[1]]@LandingsAtAge
  Ages <- as.numeric(dimnames(comp@Shift)$Age)
  full <- PopulateCompObs(comp, 3, Years(pop, "Historical"), Years(pop, "Projection"),
                          Bins = Ages, BinName = "Age")
  expect_equal(unname(dim(full@Shift)), c(3, 156, length(Ages)))
  expect_true(all(full@Shift == 0.1))
})

test_that("PopulateCompObs() repeats a user seasonal pattern and steps reduced arrays", {
  Years <- 2001 + (0:23) / 4
  HistYears <- Years[1:16]
  ProjYears <- Years[17:24]
  pat <- c(10, 60, 60, 10)
  ess <- array(rep(pat, 4), c(1, 16), dimnames = list(Sim = 1, Year = HistYears))
  shift <- array(rep(pat / 100, 4), c(1, 16, 11),
                 dimnames = list(Sim = 1, Year = HistYears, Age = 0:10))

  comp <- PopulateCompObs(CompObs(SampleSize = 100, ESS = ess, Shift = shift), nSim = 2,
                          HistYears = HistYears, ProjYears = ProjYears,
                          Bins = 0:10, BinName = "Age")
  expect_equal(unname(comp@ESS[2, ]), rep(pat, 6))
  expect_equal(unname(comp@Shift[2, , 5]), rep(pat, 6) / 100)

  again <- PopulateCompObs(ReduceDims(comp, IncYear = TRUE), nSim = 2,
                           HistYears = HistYears, ProjYears = ProjYears,
                           Bins = 0:10, BinName = "Age")
  expect_identical(again@ESS, comp@ESS)
  expect_identical(again@Shift, comp@Shift)

  # shorter than a year: no full cycle to repeat
  short <- array(c(10, 20, 30), c(1, 3), dimnames = list(Sim = 1, Year = Years[1:3]))
  comp <- PopulateCompObs(CompObs(SampleSize = 100, ESS = short), nSim = 2,
                          HistYears = HistYears, ProjYears = ProjYears,
                          Bins = 0:10, BinName = "Age")
  expect_equal(unname(comp@ESS[1, ]), c(10, 20, rep(30, 22)))

  # reduced to a consecutive run of steps, then constant: re-populates flat
  vary <- array(c(seq(5, 80, by = 5), rep(80, 8)), c(1, 24), dimnames = list(Sim = 1, Year = Years))
  comp <- PopulateCompObs(CompObs(SampleSize = 100, ESS = vary), nSim = 2,
                          HistYears = HistYears, ProjYears = ProjYears,
                          Bins = 0:10, BinName = "Age")
  red <- ReduceDims(comp, IncYear = TRUE)
  expect_equal(dimnames(red@ESS)$Year, as.character(HistYears))
  again <- PopulateCompObs(red, nSim = 2, HistYears = HistYears, ProjYears = ProjYears,
                           Bins = 0:10, BinName = "Age")
  expect_equal(unname(again@ESS[1, ]), unname(vary[1, ]))
})
