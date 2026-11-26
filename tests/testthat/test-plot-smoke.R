skip_on_cran()

.SmokeOMs <- c('SingleStockOM', 'TwoFleetOM', 'MultiStockOM', 'ComplexOM', 'HermOM',
               'SeasonalSpatialOM')

.SmokeOM <- function(Name, n = 2) {
  OM <- get(Name)
  nSim(OM)  <- n
  pYear(OM) <- 3
  OM
}

.SmokeHist <- local({
  cache <- list()
  function(Name) {
    if (is.null(cache[[Name]]))
      cache[[Name]] <<- Simulate(.SmokeOM(Name), silent = TRUE)
    cache[[Name]]
  }
})

.IsPlot <- function(p) inherits(p, c('ggplot', 'patchwork'))

test_that("plot(om) returns a patchwork for every example OM", {
  for (nm in .SmokeOMs) {
    p <- suppressMessages(plot(.SmokeOM(nm)))
    expect_s3_class(p, 'patchwork')
  }
})

test_that("plot(om) adds Herm and Spatial panels only when present", {
  nPanels <- \(p) length(p$patches$plots) + 1
  expect_gt(nPanels(suppressMessages(plot(.SmokeOM('HermOM')))),
            nPanels(suppressMessages(plot(.SmokeOM('SingleStockOM')))))
})

test_that("hist plots render for every example OM", {
  for (nm in .SmokeOMs) {
    Hist <- .SmokeHist(nm)
    for (f in list(PlotDynamics, PlotRecruits, PlotCatch, PlotF, PlotEffort,
                   PlotSelectivityCurve, PlotLengthCurve, PlotSRRCurve, PlotRecDevs))
      expect_true(.IsPlot(suppressMessages(f(Hist))), info = nm)
  }
})

test_that("byStock defaults to a line per stock and accepts TRUE and 'sum'", {
  Hist <- .SmokeHist('MultiStockOM')
  p <- PlotBiomass(Hist)
  expect_s3_class(p$facet, 'FacetNull')
  expect_identical(ggplot2::get_labs(p)$colour, 'Stock')
  expect_false(inherits(PlotBiomass(Hist, byStock = TRUE)$facet, 'FacetNull'))
  expect_true(.IsPlot(PlotBiomass(Hist, byStock = 'sum')))
  expect_identical(ggplot2::get_labs(PlotLengthCurve(Hist))$colour, 'Stock')
  expect_match(rlang::as_label(PlotRecDevs(Hist)$mapping$colour), 'Stock')
})

test_that("selectivity overlays retention and maturity by linetype", {
  OM <- .SmokeOM('HermOM')
  p  <- PlotSelectivityCurve(OM, IncMaturity = TRUE, IncRetention = TRUE)
  lt <- unique(unlist(purrr::map(seq_along(p$layers), \(i) ggplot2::layer_data(p, i)$linetype)))
  expect_true(all(c('solid', 'dashed', 'dotted') %in% lt))
  expect_identical(ggplot2::get_labs(p)$colour, 'Fleet')
  expect_false(inherits(p$facet, 'FacetNull'))
  expect_true(.IsPlot(PlotSelectivityCurve(OM, IncMaturity = TRUE, byFleet = TRUE, Sim = 1)))
  expect_true(.IsPlot(PlotSelectivityCurve(.SmokeOM('SingleStockOM'), IncMaturity = TRUE, x = 'Length')))
  expect_true(.IsPlot(PlotSelectivity(.SmokeOM('SingleStockOM'), IncMaturity = TRUE)))
})

test_that("PlotCatch matches PlotRemovals", {
  Hist <- .SmokeHist('TwoFleetOM')
  expect_equal(ggplot2::layer_data(PlotCatch(Hist)), ggplot2::layer_data(PlotRemovals(Hist)))
})

test_that("PlotHerm plots transitions and skips OMs without them", {
  expect_true(.IsPlot(PlotHerm(.SmokeOM('HermOM'))))
  expect_true(.IsPlot(PlotHerm(.SmokeOM('HermOM'), x = 'Length')))
  expect_message(p <- PlotHerm(.SmokeOM('SingleStockOM')), 'No stock transitions')
  expect_null(p)
})

test_that("PlotData selects a simulation and complex, with a message", {
  Hist <- .SmokeHist('MultiStockOM')
  expect_message(PlotData(Hist), 'Sim')
  expect_true(.IsPlot(PlotData(Hist, Sim = 2, Complex = 2)))
  expect_true(.IsPlot(suppressMessages(PlotLandings(Hist@Data))))
  expect_error(PlotData(Hist, Sim = 1, Complex = 'none'), 'Complex')

  OM <- .SmokeOM('MultiStockOM')
  OM@Data <- Hist@Data[[1]]
  expect_message(PlotData(OM), 'Complex')
  expect_true(.IsPlot(PlotData(OM, Complex = names(Hist@Data[[1]])[2])))
})

test_that("plot() works for stock and fleet objects", {
  OM <- .SmokeOM('SingleStockOM')
  expect_s3_class(suppressMessages(plot(OM@Stock[[1]])), 'PlotStockList')
  expect_s3_class(suppressMessages(plot(OM@Fleet[[1]][[1]])), 'PlotFleetList')
})

test_that("mse plots and Slick performance plots render", {
  Hist <- .SmokeHist('SingleStockOM')
  MSE  <- suppressMessages(Project(Hist, MPs = ExampleMPs(), parallel = FALSE, silent = TRUE))
  expect_true(.IsPlot(plot(MSE)))
  expect_true(.IsPlot(PlotCatch(MSE)))
  expect_true(.IsPlot(PlotRecruits(MSE)))

  skip_if_not_installed('Slick')
  slick <- suppressWarnings(MSE2Slick(MSE))
  expect_true(.IsPlot(suppressWarnings(PlotKobe(MSE))))
  expect_true(.IsPlot(PlotTradeoff(slick, xPI = 'Yield', yPI = 'Status')))
  expect_true(.IsPlot(suppressWarnings(PlotPMBoxplot(slick))))
  expect_true(.IsPlot(suppressWarnings(PlotPMTimeseries(slick, PI = 'F_FMSY'))))
  expect_error(PlotPMTimeseries(slick, PI = 'none'), 'Unknown')
})
