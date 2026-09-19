# `byStock = 'sum'` with `relative != 'none'` (R/plot-hist.R) sums the
# numerator and denominator arrays across stocks separately before dividing
# (via `.ExtractRelative(..., sumStock = TRUE)`, R/extract-relative.R),
# instead of summing per-stock ratios (which is not meaningful) or falling
# back to faceting.

skip_on_cran()

.build_multistock_hist <- function(nSim = 3) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- nSim
  Simulate(om, silent = TRUE)
}

.make_complex_om <- function(nSim = 2) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim <- nSim
  sn <- StockNames(om)
  Name(om@Stock[[2]]) <- paste("Female", sn[2])
  om@Complexes <- list(Complex1 = 1:2)
  # Obs/Imp are keyed by complex, not stock -- reset to defaults so they get
  # re-populated for the single merged complex instead of the (invalid)
  # per-stock structure inherited from MultiStockOM.
  om@Obs <- list()
  om@Imp <- list()
  om
}

.npanels <- function(p) nrow(ggplot2::ggplot_build(p)$layout$layout)

## ---- Unit tests: .SubsetStockDim ----

test_that(".SubsetStockDim subsets the named Stock dimension, preserving other dims", {
  arr <- array(1:24, dim = c(Sim = 2, Stock = 3, Year = 4),
               dimnames = list(Sim = 1:2, Stock = c('A', 'B', 'C'), Year = 2020:2023))
  sub <- MSEtool:::.SubsetStockDim(arr, c('A', 'C'))
  expect_equal(dim(sub), c(Sim = 2, Stock = 2, Year = 4))
  expect_equal(dimnames(sub)$Stock, c('A', 'C'))
  expect_equal(sub[, 'A', ], arr[, 'A', ])
})

test_that(".SubsetStockDim is a no-op when stockNames is NULL or there is no Stock dimension", {
  arr <- array(1:8, dim = c(Sim = 2, Year = 4), dimnames = list(Sim = 1:2, Year = 2020:2023))
  expect_identical(MSEtool:::.SubsetStockDim(arr, NULL), arr)
  expect_identical(MSEtool:::.SubsetStockDim(arr, 'A'), arr)
})

## ---- .ExtractRelative(sumStock = TRUE): sum-then-divide, not sum-of-ratios ----

test_that(".ExtractRelative(sumStock = TRUE) aggregates B/B0 across stocks", {
  Hist <- .build_multistock_hist()
  finalYear <- max(as.numeric(dimnames(Hist@Biomass)$Year))

  Bsum <- Biomass(Hist, df = TRUE) |>
    dplyr::filter(.data$Year == finalYear) |>
    dplyr::group_by(.data$Sim) |>
    dplyr::summarise(B = sum(.data$Value), .groups = 'drop')
  B0arr <- Hist@Unfished@Equilibrium@Biomass
  B0sum <- apply(B0arr[, , dim(B0arr)[3]], 1, sum)
  manualDep <- Bsum$B / B0sum[Bsum$Sim]

  out <- MSEtool:::.ExtractRelative(Hist, num_slot = 'Biomass', denom_slot = 'Biomass',
                                     ref = 'Unfished', var_name = 'B_B0', type = 'Equilibrium',
                                     sumStock = TRUE)
  outFinal <- out[out$Year == finalYear, ]

  expect_true(all(outFinal$Stock == 'Total'))
  expect_equal(sort(outFinal$Value), sort(unname(manualDep)), tolerance = 1e-8)
})

test_that(".ExtractRelative(sumStock = TRUE) aggregates B/BMSY across stocks", {
  Hist <- .build_multistock_hist()
  Hist@Reference@MSY <- CalcMSY(Hist, silent = TRUE)
  finalYear <- max(as.numeric(dimnames(Hist@Biomass)$Year))

  Bsum <- Biomass(Hist, df = TRUE) |>
    dplyr::filter(.data$Year == finalYear) |>
    dplyr::group_by(.data$Sim) |>
    dplyr::summarise(B = sum(.data$Value), .groups = 'drop')
  BMSYarr <- Hist@Reference@MSY@BMSY
  BMSYsum <- apply(BMSYarr[, , dim(BMSYarr)[3]], 1, sum)
  manualDep <- Bsum$B / BMSYsum[Bsum$Sim]

  out <- MSEtool:::.ExtractRelative(Hist, num_slot = 'Biomass', denom_slot = 'BMSY',
                                     ref = 'MSY', var_name = 'B_BMSY', sumStock = TRUE)
  outFinal <- out[out$Year == finalYear, ]

  expect_equal(sort(outFinal$Value), sort(unname(manualDep)), tolerance = 1e-8)
})

test_that(".ExtractRelative(sumStock = TRUE, stockNames = ...) restricts the sum to the selected stocks", {
  Hist <- .build_multistock_hist()
  sn <- StockNames(Hist@OM)
  finalYear <- max(as.numeric(dimnames(Hist@Biomass)$Year))

  Bdf <- Biomass(Hist, df = TRUE) |>
    dplyr::filter(.data$Year == finalYear, .data$Stock == sn[1])
  B0arr <- Hist@Unfished@Equilibrium@Biomass
  manualDep <- Bdf$Value / B0arr[Bdf$Sim, sn[1], dim(B0arr)[3]]

  out <- MSEtool:::.ExtractRelative(Hist, num_slot = 'Biomass', denom_slot = 'Biomass',
                                     ref = 'Unfished', var_name = 'B_B0', type = 'Equilibrium',
                                     stockNames = sn[1], sumStock = TRUE)
  outFinal <- out[out$Year == finalYear, ]

  expect_equal(sort(outFinal$Value), sort(unname(manualDep)), tolerance = 1e-8)
})

## ---- PlotBiomass(): byStock = 'sum' aggregate line ----

test_that("PlotBiomass(byStock = 'sum', relative = 'B0'/'BMSY') plots a single aggregate line, not facets", {
  Hist <- .build_multistock_hist()
  Hist@Reference@MSY <- CalcMSY(Hist, silent = TRUE)

  p1 <- PlotBiomass(Hist, byStock = 'sum', relative = 'B0')
  expect_s3_class(p1, 'ggplot')
  expect_equal(.npanels(p1), 1)

  p2 <- PlotBiomass(Hist, byStock = 'sum', relative = 'BMSY')
  expect_s3_class(p2, 'ggplot')
  expect_equal(.npanels(p2), 1)
})

test_that("PlotBiomass byStock = FALSE/TRUE with relative are unaffected by the sum fix", {
  Hist <- .build_multistock_hist()

  p_false <- PlotBiomass(Hist, byStock = FALSE, relative = 'B0')
  expect_s3_class(p_false, 'ggplot')
  expect_equal(.npanels(p_false), 1)  # colored on one panel, not faceted

  p_true <- PlotBiomass(Hist, byStock = TRUE, relative = 'B0')
  expect_s3_class(p_true, 'ggplot')
  expect_equal(.npanels(p_true), 2)   # faceted, one panel per stock
})

## ---- .PlotSpawning() / PlotSBiomass(): byFemale-aware aggregate ----

test_that("PlotSBiomass byStock='sum', relative='B0', byFemale=TRUE sums only the identified female stock", {
  om <- .make_complex_om()
  Hist <- Simulate(om, silent = TRUE)

  female <- MSEtool:::.FemaleStockNames(Hist@OM)
  expect_false(female$ambiguous)
  expect_length(female$stocks, 1)

  finalYear <- max(as.numeric(dimnames(Hist@SBiomass)$Year))
  SBdf <- SBiomass(Hist, df = TRUE) |>
    dplyr::filter(.data$Year == finalYear, .data$Stock %in% female$stocks)
  SB0arr <- Hist@Unfished@Equilibrium@SBiomass
  manualDep <- SBdf$Value / SB0arr[SBdf$Sim, female$stocks, dim(SB0arr)[3]]

  p <- PlotSBiomass(Hist, byStock = 'sum', relative = 'B0', byFemale = TRUE)
  expect_s3_class(p, 'ggplot')
  expect_equal(.npanels(p), 1)

  out <- MSEtool:::.ExtractRelative(Hist, num_slot = 'SBiomass', denom_slot = 'SBiomass',
                                     ref = 'Unfished', var_name = 'SB_SB0', type = 'Equilibrium',
                                     stockNames = female$stocks, sumStock = TRUE)
  outFinal <- out[out$Year == finalYear, ]

  expect_equal(sort(outFinal$Value), sort(unname(manualDep)), tolerance = 1e-8)
  expect_true(all(outFinal$Stock == 'Total'))
})

test_that("PlotSProduction byStock='sum', relative='BMSY', byFemale=TRUE runs and sums only the female stock", {
  om <- .make_complex_om()
  Hist <- Simulate(om, silent = TRUE)
  Hist@Reference@MSY <- CalcMSY(Hist, silent = TRUE)

  female <- MSEtool:::.FemaleStockNames(Hist@OM)

  p <- PlotSProduction(Hist, byStock = 'sum', relative = 'BMSY', byFemale = TRUE)
  expect_s3_class(p, 'ggplot')
  expect_equal(.npanels(p), 1)

  finalYear <- max(as.numeric(dimnames(Hist@SProduction)$Year))
  SPdf <- SProduction(Hist, df = TRUE) |>
    dplyr::filter(.data$Year == finalYear, .data$Stock %in% female$stocks)
  SPMSYarr <- Hist@Reference@MSY@SPMSY
  manualDep <- SPdf$Value / SPMSYarr[SPdf$Sim, female$stocks, dim(SPMSYarr)[3]]

  out <- MSEtool:::.ExtractRelative(Hist, num_slot = 'SProduction', denom_slot = 'SPMSY',
                                     ref = 'MSY', var_name = 'SP_SPMSY',
                                     stockNames = female$stocks, sumStock = TRUE)
  outFinal <- out[out$Year == finalYear, ]

  expect_equal(sort(outFinal$Value), sort(unname(manualDep)), tolerance = 1e-8)
})
