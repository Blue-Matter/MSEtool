# Single-area monthly OM with zero effort in every season of the 3rd year for
# all sims, and of the 6th year for sim 2 only, so the fallback is per (sim, year).
.SeasonalZeroEffortOM <- function() {
  stock <- SeasonalSpatialExStock
  stock@Spatial <- Spatial()
  om <- OM(nSim = 3, nYear = 10, pYear = 3, Seasons = 12, Stock = stock,
           Fleet = DomeExFleet, Obs = CommercialFleetObs, Imp = FullComplianceImp)
  om <- Populate(om, silent = TRUE)
  eff <- om@Fleet[[1]][[1]]@Effort@Effort
  yr  <- floor(as.numeric(dimnames(eff)$Year))
  eff[, yr == unique(yr)[3]] <- 0
  eff[2, yr == unique(yr)[6]] <- 0
  om@Fleet[[1]][[1]]@Effort@Effort <- eff
  om
}

test_that("CollapseSeasons() keeps at-age curves defined in years where every season has zero F", {
  skip_on_cran()
  om <- .SeasonalZeroEffortOM()
  src <- om@Fleet[[1]][[1]]
  HistYears <- unique(floor(as.numeric(dimnames(src@Effort@Effort)$Year)))
  zeroYear  <- as.character(HistYears[6])

  omc <- CollapseSeasons(om, silent = TRUE)
  fleet <- omc@Fleet[[1]][[1]]

  expect_true(all(apply(fleet@Selectivity@MeanAtAge, c('Sim', 'Year'), max) == 1))
  expect_true(all(apply(fleet@Retention@MeanAtAge, c('Sim', 'Year'), max) > 0))
  expect_true(all(fleet@WeightFleetSelected > 0))
  expect_true(all(fleet@WeightFleetRetained > 0))
  expect_equal(unname(fleet@Effort@Effort[, as.character(HistYears[3])]), c(0, 0, 0))
  expect_equal(unname(fleet@Effort@Effort[2, zeroYear]), 0)

  AgeMap <- .CollapseSeasonsAgeMap(om@Stock[[1]]@Ages, om@Seasons)
  qYear  <- floor(as.numeric(dimnames(src@Catchability@Efficiency)$Year)) == HistYears[6]
  q      <- src@Catchability@Efficiency[2, qYear]
  sel    <- src@Selectivity@MeanAtAge[2, , 1, 1]
  qSel   <- apply(AgeMap$PathIdx, 2, \(path) sum(q * sel[path]))
  expect_equal(unname(fleet@Selectivity@MeanAtAge[2, , zeroYear, 1]), qSel / max(qSel))

  set.seed(1)
  hist <- Simulate(omc, silent = TRUE)
  expect_true(all(apply(hist@OM@Fleet[[1]][[1]]@Selectivity@MeanAtAge, c('Sim', 'Year'), max) > 0))
  expect_no_error(
    Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  )
})

test_that(".FillZeroYearWeights() fills only the (sim, year) blocks where weights sum to zero", {
  IndexMap <- list(Seasons = 2, SeasonalYears = c(2001, 2001.5, 2002, 2002.5),
                   AnnualYears = 2001:2002, YearBlock = c(1, 1, 2, 2))
  dn <- list(Sim = 1:2, Year = IndexMap$SeasonalYears)
  w    <- array(c(0, 3, 0, 0, 1, 0, 2, 0), c(2, 4), dn)
  fill <- array(5, c(2, 4), dn)

  expect_equal(.FillZeroYearWeights(w, IndexMap, fill),
               array(c(5, 3, 5, 0, 1, 5, 2, 5), c(2, 4), dn))
  expect_equal(.FillZeroYearWeights(w, IndexMap),
               array(c(1, 3, 1, 0, 1, 1, 2, 1), c(2, 4), dn))
})
