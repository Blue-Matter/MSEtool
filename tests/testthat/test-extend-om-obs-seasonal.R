test_that("Simulate() and Project() fill mid-year Obs change points as steps", {
  skip_on_cran()
  stock <- SeasonalSpatialExStock
  stock@Spatial <- Spatial()
  mk <- function(obs) OM(nSim = 3, nYear = 10, pYear = 3, Seasons = 12, Stock = stock,
                         Fleet = DomeExFleet, Obs = obs, Imp = FullComplianceImp)
  AllYears <- Years(mk(CommercialFleetObs))
  nAll <- length(AllYears)
  ess <- array(c(rep(30, 39), rep(60, nAll - 39)), c(1, nAll),
               list(Sim = 1, Year = AllYears))
  obs <- CommercialFleetObs
  obs@LandingsAtAge <- CompObs(SampleSize = 100, ESS = ess)
  om <- Populate(mk(obs), silent = TRUE)
  Pop <- ExtendYears(om@Obs[[1]][[1]]@LandingsAtAge@ESS, AllYears,
                     maintain_seasonal_pattern = FALSE)

  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  ESS <- hist@OM@Obs[[1]][[1]]@LandingsAtAge@ESS
  expect_equal(ESS, Pop[, dimnames(ESS)$Year, drop = FALSE])

  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  expect_equal(mse@OM@Obs[[1]][[1]]@LandingsAtAge@ESS, Pop)
})

test_that(".ExtendOM() restores reduced seasonal Obs arrays exactly", {
  stock <- SeasonalSpatialExStock
  stock@Spatial <- Spatial()
  om <- Populate(OM(nSim = 3, nYear = 10, pYear = 3, Seasons = 12, Stock = stock,
                    Fleet = DomeExFleet, Obs = CommercialFleetObs,
                    Imp = FullComplianceImp), silent = TRUE)
  AllYears <- Years(om)
  err <- om@Obs[[1]][[1]]@Landings@Error
  err[, 1:13] <- 1
  om@Obs[[1]][[1]]@Landings@Error <- ReduceDims(err, IncYear = TRUE)
  expect_lt(ncol(om@Obs[[1]][[1]]@Landings@Error), length(AllYears))

  out <- .ExtendOM(om, Years = AllYears, silent = TRUE)
  Ext <- out@Obs[[1]][[1]]@Landings@Error
  expect_equal(dimnames(Ext), dimnames(err))
  expect_equal(c(Ext), c(err))
})
