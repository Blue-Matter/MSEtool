# Regression coverage for .GenProjDataSizeComp()
# (R/generate-data-proj-sizecomp.R): true catch-at-size for a fleet must only
# be aggregated from `Proj@LandingsAtSize`/`Proj@DiscardsAtSize` lazily, when
# it is actually going to be used. Previously this was computed unconditionally
# for every fleet up front, which crashed once a stock's true catch-at-size
# was unavailable (e.g. `SimControl(CalcCatchAtSize = FALSE)`) while real
# historical composition data and a configured Obs model meant the function
# didn't exit early. See generate-data-proj-sizecomp.R for details.

skip_on_cran()

.BuildProjWithUnavailableTrueCatchAtSize <- function() {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE, control = SimControl(CalcCatchAtSize = FALSE))

  HistYears <- Years(hist, 'H')
  ProjYears <- Years(hist, 'P')
  YearsAll  <- c(HistYears, ProjYears)
  FleetNms  <- FleetNames(hist)
  nSim      <- nSim(hist)

  Classes <- as.numeric(dimnames(hist@OM@Stock[[1]]@Length@ALK)$Class)
  nBin    <- length(Classes)

  # Real historical composition data (e.g. conditioned from supplied
  # `OM@Data`), independent of the (skipped) true catch-at-size calculation --
  # this is what stops `.GenProjDataSizeComp()`'s "no composition data"
  # early exit from firing.
  val <- array(0, dim = c(length(HistYears), 1, nBin),
              dimnames = list(Year = HistYears, Fleet = FleetNms, Class = seq_len(nBin)))
  val[, 1, max(1, round(nBin / 2))] <- 50
  compdata <- CompData(Name = FleetNms, Value = val, Classes = list(Classes), Units = "cm")
  for (x in seq_len(nSim))
    hist@Data[[x]][[1]]@LandingsAtSize <- compdata

  # A configured Obs model with a real SampleSize routes the fleet into the
  # stochastic (non-OM-value) branch, which is where true catch-at-size is
  # aggregated.
  ss <- matrix(50, nSim, length(YearsAll),
              dimnames = list(Sim = seq_len(nSim), Year = YearsAll))
  hist@OM@Obs[[1]][[1]]@LandingsAtSize <- CompObs(SampleSize = ss)

  Proj <- MSEtool:::.ExtendHist(hist, Years = YearsAll, silent = TRUE)

  # Simulate a stock whose true catch-at-size was never computed (what
  # `CalcCatchAtSize = FALSE` should eventually produce for that stock,
  # instead of a zero-filled array).
  Proj@LandingsAtSize[1] <- list(NULL)

  list(Proj = Proj, YearsAll = YearsAll, DataYear = ProjYears[1], nSim = nSim)
}

test_that(".GenProjDataSizeComp() does not error when true catch-at-size is unavailable for a stock", {
  built <- .BuildProjWithUnavailableTrueCatchAtSize()

  result <- MSEtool:::.GenProjDataSizeComp(built$Proj, built$DataYear, built$YearsAll,
                                           i = 1, stocks = 1, nSim = built$nSim,
                                           type = 'LandingsAtSize')

  expect_length(result, built$nSim)
  for (cd in result) {
    expect_true(built$DataYear %in% dimnames(cd@Value)[[1]])
    # No true data available to simulate from -- the new year is left NA,
    # the same as when no Obs is configured for a fleet.
    expect_true(all(is.na(cd@Value[as.character(built$DataYear), , ])))
  }
})
