test_that("CollapseSeasons() collapses populated Obs and Imp to annual resolution", {
  skip_on_cran()
  stock <- SeasonalSpatialExStock
  stock@Spatial <- Spatial()
  obs <- CommercialFleetObs
  obs@LandingsAtAge <- CompObs(SampleSize = c(50, 100), ESS = 40)
  imp <- FullComplianceImp
  imp@TAC <- ImpSlot(Mean = 1, SD = 0.1, Compliance = c(0.3, 0.8))
  om <- OM(nSim = 3, nYear = 10, pYear = 3, Seasons = 12, Stock = stock,
           Fleet = DomeExFleet, Obs = obs, Imp = imp)
  om <- Populate(om, silent = TRUE)

  omc <- CollapseSeasons(om, silent = TRUE)
  AllYears <- as.character(Years(omc))
  yr <- AllYears[4]

  src <- om@Obs[[1]][[1]]
  obsc <- omc@Obs[[1]][[1]]
  expect_equal(dimnames(obsc@Landings@Error)$Year, AllYears)
  expect_equal(obsc@Landings@Error[, yr], src@Landings@Error[, yr])
  expect_equal(dimnames(obsc@CPUE@Error)$Year, AllYears)
  expect_equal(obsc@CPUE@Years, Years(omc, "H"))

  impc <- omc@Imp[[1]][[1]]
  expect_equal(dimnames(impc@TAC@Error)$Year, AllYears)
  expect_equal(dimnames(impc@TAC@Compliance)$Year, AllYears)
  expect_equal(impc@TAC@Error[, yr], om@Imp[[1]][[1]]@TAC@Error[, yr])

  set.seed(1)
  hist <- Simulate(omc, silent = TRUE)
  expect_equal(unname(hist@OM@Obs[[1]][[1]]@Landings@Error), unname(obsc@Landings@Error))
  expect_no_error(
    Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  )
})

test_that(".CollapsePickYear() and .CollapsePickAge() keep the first season of each year and integer ages", {
  dn <- list(Sim = 1:2, Year = c(2001, 2001.5, 2002, 2002.5), Age = c(0, 0.5, 1, 1.5, 2))
  arr <- array(seq_len(40), c(2, 4, 5), dn)

  out <- arr |> .CollapsePickYear() |> .CollapsePickAge()
  expect_equal(dimnames(out), list(Sim = c("1", "2"), Year = c("2001", "2002"), Age = c("0", "1", "2")))
  expect_equal(unname(out), unname(arr[, c(1, 3), c(1, 3, 5), drop = FALSE]))

  reduced <- array(1, c(2, 1), list(Sim = 1:2, Year = 2001.5))
  expect_equal(dimnames(.CollapsePickYear(reduced))$Year, "2001")
})
