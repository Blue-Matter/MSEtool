RoundTrip <- function(OM, MPs = c('CurrentCatch', 'CurrentEffort'), nSim = 3) {
  nSim(OM) <- nSim
  Hist <- Simulate(OM, silent = TRUE)
  MSE  <- Project(Hist, MPs = MPs, silent = TRUE)
  Hist2 <- Hist(MSE)
  list(Hist = Hist, MSE = MSE, Hist2 = Hist2,
       MSE2 = Project(Hist2, MPs = MPs, silent = TRUE))
}

ExpectSameProjection <- function(rt) {
  expect_s4_class(rt$Hist2, 'hist')
  for (fn in c('SBiomass', 'Landings', 'Removals', 'Discards'))
    expect_equal(get(fn)(rt$MSE2)$Value, get(fn)(rt$MSE)$Value, label = fn)
  expect_equal(rt$MSE2@Effort, rt$MSE@Effort)
  expect_equal(rt$MSE2@PPD, rt$MSE@PPD)
  expect_equal(rt$Hist2@Data, rt$Hist@Data)
}

test_that("Project(Hist(MSE)) reproduces Project(Hist) for a two-fleet OM", {
  skip_on_cran()
  ExpectSameProjection(RoundTrip(TwoFleetOM))
})

test_that("Project(Hist(MSE)) reproduces Project(Hist) for a stock-complex OM", {
  skip_on_cran()
  ExpectSameProjection(RoundTrip(ComplexOM))
})

test_that("Project(Hist(MSE)) reproduces Project(Hist) for a seasonal spatial OM", {
  skip_on_cran()
  ExpectSameProjection(RoundTrip(SeasonalSpatialOM))
})

test_that("Hist(MSE) extracts the historical time-series stored in the MSE", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  MSE  <- Project(Hist, MPs = 'CurrentCatch', silent = TRUE)
  Hist2 <- Hist(MSE)

  for (fn in c('SBiomass', 'Landings')) {
    mse_hist <- get(fn)(MSE, Extend = TRUE)
    mse_hist <- mse_hist[mse_hist$Period == 'Historical', ]
    expect_equal(get(fn)(Hist2, Extend = TRUE)$Value, mse_hist$Value, label = fn)
  }
  expect_length(Hist2@Misc, 0)
  mp_entries <- purrr::keep(unlist(Hist2@Log, recursive = FALSE),
                            \(e) .IsLogEntry(e) && !is.null(e$mp))
  expect_length(mp_entries, 0)
})

test_that("Project() does not extend RecDevHist over the projection years", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  MSE  <- Project(Hist, MPs = 'CurrentCatch', silent = TRUE)
  expect_equal(MSE@OM@Stock[[1]]@SRR@RecDevHist, Hist@OM@Stock[[1]]@SRR@RecDevHist)
})

test_that(".PrepHistMisc() gives the same RecDevs when RecDevHist spans the projection years", {
  skip_on_cran()
  OM <- SingleStockOM
  nSim(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)

  Extended <- Hist
  Extended@OM@Stock[[1]]@SRR@RecDevHist <- Extend(
    Hist@OM@Stock[[1]]@SRR@RecDevHist,
    Years = c(Years(Hist, 'Historical'), Years(Hist, 'Projection')))
  expect_gt(ncol(Extended@OM@Stock[[1]]@SRR@RecDevHist), length(Years(Hist, 'Historical')))
  expect_equal(.PrepHistMisc(Extended)@Misc$RecDevs, .PrepHistMisc(Hist)@Misc$RecDevs)
})

test_that("Hist() with no argument returns an empty hist object", {
  expect_s4_class(Hist(), 'hist')
})
