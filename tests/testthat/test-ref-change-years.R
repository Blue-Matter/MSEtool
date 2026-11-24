# Change-point years of the inputs to the unfished values and MSY reference
# points: unfished values stored at the change points (historical and
# projection), MSY at the last historical year with a logged assumption.

.TimeVaryingMOM <- function(MYears = c(2007, 2015, 2035), MVals = c(0.2, 0.3, 0.25)) {
  om <- SingleStockOM
  om@nSim <- 2
  pYear(om) <- 15
  om <- suppressMessages(Populate(om, silent = TRUE))
  Ages <- dimnames(om@Stock[[1]]@NaturalMortality@MeanAtAge)$Age
  om@Stock[[1]]@NaturalMortality@MeanAtAge <- array(
    rep(MVals, each = length(Ages)), dim = c(1, length(Ages), length(MYears)),
    dimnames = list(Sim = 1, Age = Ages, Year = MYears))
  om@Stock[[1]]@NaturalMortality@Pars <- list()
  om
}

test_that(".CollapseYearsText collapses consecutive years and long lists", {
  expect_equal(.CollapseYearsText(c(2000, 2001, 2002, 2005, 2010:2012)), "2000-2002, 2005, 2010-2012")
  expect_equal(.CollapseYearsText(2007), "2007")
  long <- c(1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004)
  expect_equal(.CollapseYearsText(long), "1990, 1992, 1994, 1996, ..., 2004")
})

test_that("RefChangeYears() finds historical and projection change points", {
  skip_on_cran()
  om <- .TimeVaryingMOM()
  cy <- RefChangeYears(om)
  expect_equal(as.numeric(cy), c(2007, 2015, 2035))
  expect_equal(attr(cy, "Parameters")[["2035"]], "natural mortality")
  expect_equal(as.numeric(RefChangeYears(om, "Unfished")), c(2007, 2015, 2035))
  expect_equal(as.numeric(RefChangeYears(om, Period = "Historical")), c(2007, 2015))
  expect_equal(as.numeric(RefChangeYears(om, Period = "Projection")), c(2027, 2035))

  # time-invariant single-fleet OM: the first year only
  expect_equal(as.numeric(RefChangeYears(SingleStockOM)), min(Years(SingleStockOM, "H")))
})

test_that("unfished values are stored at the change points and filled by the accessors", {
  skip_on_cran()
  om <- .TimeVaryingMOM()
  H <- Simulate(om, silent = TRUE)
  Stored <- H@Unfished@Equilibrium@SBiomass
  expect_equal(dimnames(Stored)$Year, c("2007", "2015", "2035"))
  expect_equal(names(H@Unfished@Equilibrium@Number[[1]][1, 1, , 1]), c("2007", "2015", "2035"))

  Filled <- SB0(H)
  AllYears <- c(Years(H, "H"), Years(H, "P"))
  expect_equal(as.numeric(dimnames(Filled)$Year), AllYears)
  expect_equal(Filled[, , "2014"], Stored[, , "2007"])
  expect_equal(Filled[, , "2034"], Stored[, , "2015"])
  expect_equal(Filled[, , "2041"], Stored[, , "2035"])
  expect_identical(SB0(H, Fill = FALSE), ReduceDims(Stored))

  # each stored year is the equilibrium of that year's parameters
  for (yr in c(2007, 2015, 2035)) {
    om1 <- om
    Ages <- dimnames(om@Stock[[1]]@NaturalMortality@MeanAtAge)$Age
    om1@Stock[[1]]@NaturalMortality@MeanAtAge <- om@Stock[[1]]@NaturalMortality@MeanAtAge[, , as.character(yr), drop = FALSE]
    dimnames(om1@Stock[[1]]@NaturalMortality@MeanAtAge)$Year <- "2007"
    Eq <- CalcUnfished_Equilibrium(om1, silent = TRUE)
    expect_equal(as.numeric(Eq@SBiomass[, , 1]), as.numeric(Stored[, , as.character(yr)]))
  }

  # SB/SB0 uses the unfished value of each projection year
  mse <- Project(H, MPs = "CurrentEffort", silent = TRUE)
  sb <- SB_SB0(mse, Reduce = FALSE)
  SB <- mse@SBiomass[1, 1, "2036", 1]
  expect_equal(sb$Value[sb$Sim == 1 & sb$Year == 2036], SB / Stored[1, 1, "2035"], ignore_attr = TRUE)
})

test_that("MSY change points are logged with the call to calculate them", {
  skip_on_cran()
  om <- .TimeVaryingMOM()
  H <- Simulate(om, silent = TRUE)
  Log <- Filter(\(e) identical(e$name, "MSYChangeYears"), H@Log$assumption)
  expect_length(Log, 1)
  expect_match(Log[[1]]$message, "natural mortality")
  expect_match(Log[[1]]$message, "2007, 2015, 2035", fixed = TRUE)
  expect_match(Log[[1]]$message, "RefChangeYears(OM)", fixed = TRUE)
  expect_match(Log[[1]]$message, "CalcMSY(Hist, Years = RefChangeYears(Hist))", fixed = TRUE)

  # the suggested calls give MSY for every change-point year
  MSY <- CalcMSY(H, Years = RefChangeYears(H), silent = TRUE)
  expect_equal(dimnames(MSY@FMSY)$Year, c("2007", "2015", "2035"))
  expect_true(all(is.finite(MSY@FMSY)))

  om@Control$RefYears <- RefChangeYears(om)
  H2 <- Simulate(om, silent = TRUE)
  expect_equal(H2@Reference@MSY@FMSY, MSY@FMSY, ignore_attr = TRUE)
  expect_false(any(vapply(H2@Log$assumption, \(e) identical(e$name, "MSYChangeYears"), logical(1))))

  # nothing to log for a time-invariant single-fleet OM
  om3 <- SingleStockOM
  om3@nSim <- 2
  H3 <- Simulate(om3, silent = TRUE)
  expect_false(any(vapply(H3@Log$assumption, \(e) identical(e$name, "MSYChangeYears"), logical(1))))
})

test_that("MSY in a year without fishing effort uses the nearest year with effort", {
  skip_on_cran()
  om <- SingleStockOM
  om@nSim <- 2
  H <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
  First <- min(Years(H, "H"))
  Eff <- H@Effort
  expect_equal(sum(abind::asub(Eff, as.character(First), match("Year", names(dimnames(Eff))))), 0)
  MSY1 <- CalcMSY(H, Years = First, silent = TRUE)
  MSY2 <- CalcMSY(H, Years = First + 1, silent = TRUE)
  expect_lt(max(MSY1@FMSY), H@OM@maxF * 0.99)
  expect_equal(as.numeric(MSY1@FMSY), as.numeric(MSY2@FMSY))
})

test_that(".AlignDenomYears step-fills stored change-point years", {
  den <- array(c(10, 20, 30), dim = c(1, 1, 3),
               dimnames = list(Sim = 1, Stock = "A", Year = c(2000, 2005, 2010)))
  out <- .AlignDenomYears(den, as.character(1998:2012))
  expect_equal(as.numeric(out), c(10, 10, 10, 10, 10, 10, 10, 20, 20, 20, 20, 20, 30, 30, 30))
  expect_true(all(is.na(.AlignDenomYears(den, "2003", fill = FALSE))))
})

test_that(".CompareAbsError uses the distance outside the simulation range", {
  expect_equal(.CompareAbsError(OM = 12, Ref = 10, Lower = NA, Upper = NA), 0.2)
  expect_equal(.CompareAbsError(OM = 12, Ref = 10, Lower = 8, Upper = 15), 0)
  expect_equal(.CompareAbsError(OM = 12, Ref = 10, Lower = 11, Upper = 15), 0.1)
  # simulations identical: the compared simulation
  expect_equal(.CompareAbsError(OM = 12, Ref = 10, Lower = 12, Upper = 12), 0.2)
})
