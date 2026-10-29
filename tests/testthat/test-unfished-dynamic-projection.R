skip_on_cran()

.ProjYearNames <- function(object) as.character(Years(object@OM, 'Projection'))

.DynamicYears <- function(object) dimnames(object@Unfished@Dynamic@SBiomass)[['Year']]

# Dynamic SB0 recovered from SB_SB0(type = 'Dynamic') in each MP's projection years
.ImpliedDynamicSB0 <- function(MSE) {
  ratio <- SB_SB0(MSE, type = 'Dynamic', df = FALSE)
  ExtendSims(MSE@SBiomass, nSim(MSE)) / ratio
}

test_that("MSE@Unfished@Dynamic covers all historical and projection years", {
  om <- SingleStockOM
  om@nSim <- 4
  hist <- Simulate(om, silent = TRUE)
  mse  <- Project(hist, MPs = c('CurrentEffort', 'FMSYref'), silent = TRUE)

  AllYears <- as.character(c(Years(om, 'Historical'), Years(om, 'Projection')))
  expect_identical(.DynamicYears(hist), AllYears)
  expect_identical(.DynamicYears(mse), AllYears)
  for (sl in c('Biomass', 'SBiomass', 'SProduction'))
    expect_true(all(is.finite(slot(mse@Unfished@Dynamic, sl)) & slot(mse@Unfished@Dynamic, sl) > 0))
  expect_identical(dimnames(mse@Unfished@Dynamic@Number[[1]])[['Year']], AllYears)

  SB0Dyn <- SB0(mse, type = 'Dynamic', Reduce = FALSE)[, , .ProjYearNames(mse), drop = FALSE]
  expect_true(all(apply(SB0Dyn, 1, \(x) length(unique(signif(x, 8)))) > 1))

  # relative extractors divide by the projection-year dynamic SB0, identical across MPs
  Implied <- .ImpliedDynamicSB0(mse)
  for (mp in dimnames(Implied)[['MP']])
    expect_equal(unname(Implied[, , , mp]), unname(SB0Dyn[, , , drop = TRUE]), tolerance = 1e-10)

  df <- SB_SB0(mse, type = 'Dynamic')
  expect_false(anyNA(df$Value))
})

test_that("deterministic recruitment and constant biology give dynamic SB0 equal to equilibrium SB0", {
  om <- SingleStockOM
  om@nSim <- 3
  om@Stock[[1]]@SRR@SD <- 0
  om@Stock[[1]]@Spatial <- new("spatial")
  hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))

  ProjYears <- .ProjYearNames(hist)
  Dyn <- SB0(hist, type = 'Dynamic', Reduce = FALSE)
  Eq  <- SB0(hist, type = 'Equilibrium', Reduce = FALSE)
  expect_true(all(ProjYears %in% dimnames(Dyn)[['Year']]))
  EqLast <- Eq[, , dim(Eq)[3], drop = FALSE]
  for (yr in ProjYears)
    expect_equal(unname(Dyn[, , yr]), unname(EqLast[, , 1]), tolerance = 1e-4)
})

test_that("projection dynamic SB0 follows RecDevProj and is identical across MPs", {
  om <- SingleStockOM
  om@nSim <- 4
  hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
  ProjYears <- .ProjYearNames(hist)
  HistYears <- as.character(Years(hist@OM, 'Historical'))

  Base <- CalcUnfished_Dynamic(hist, silent = TRUE)
  expect_equal(Base@SBiomass, hist@Unfished@Dynamic@SBiomass, tolerance = 1e-10)

  Up <- hist
  Up@OM@Stock[[1]]@SRR@RecDevProj <- Up@OM@Stock[[1]]@SRR@RecDevProj * 1.5
  UpDyn <- CalcUnfished_Dynamic(Up, silent = TRUE)

  expect_equal(UpDyn@SBiomass[, , HistYears], Base@SBiomass[, , HistYears], tolerance = 1e-10)
  Later <- utils::tail(ProjYears, 10)
  expect_true(all(UpDyn@SBiomass[, , Later] > Base@SBiomass[, , Later] * 1.2))

  mse <- Project(hist, MPs = c('CurrentEffort', 'FMSYref'), silent = TRUE)
  expect_equal(mse@Unfished@Dynamic@SBiomass, Base@SBiomass, tolerance = 1e-10)
})

test_that("single-simulation shortcut matches the per-simulation calculation", {
  om <- SingleStockOM
  om@nSim <- 4
  hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
  same <- .SubsetSim(hist, rep(1, 4), nSim = 4)
  RecDevProj <- ExtendSims(same@OM@Stock[[1]]@SRR@RecDevProj, 4)
  RecDevProj[] <- RecDevProj * rep(c(0.6, 0.9, 1.2, 1.6), times = ncol(RecDevProj))
  same@OM@Stock[[1]]@SRR@RecDevProj <- RecDevProj
  expect_true(.IdenticalSims(same@OM, ignore = 'RecDevProj'))

  Short <- CalcUnfished_Dynamic(same, IdenticalHist = TRUE, silent = TRUE)
  Full  <- CalcUnfished_Dynamic(same, IdenticalHist = FALSE, silent = TRUE)
  for (sl in c('Biomass', 'SBiomass', 'SProduction'))
    expect_equal(ExtendSims(slot(Short, sl), 4), ExtendSims(slot(Full, sl), 4), tolerance = 1e-10)
  expect_equal(unname(dim(Full@SBiomass)[1]), 4L)
})

test_that("dynamic unfished covers projection years for seasonal and multi-stock OMs", {
  for (name in c('SeasonalSpatialOM', 'MultiStockOM', 'HermOM')) {
    om <- get(name)
    om@nSim <- 2
    hist <- Simulate(om, silent = TRUE, control = SimControl(MSYRefs = FALSE))
    mse  <- Project(hist, MPs = 'CurrentEffort', silent = TRUE)

    expect_true(all(.ProjYearNames(mse) %in% .DynamicYears(mse)), info = name)
    expect_true(all(is.finite(mse@Unfished@Dynamic@SBiomass)), info = name)
    expect_true(all(mse@Unfished@Dynamic@SBiomass > 0), info = name)
    expect_equal(dimnames(mse@Unfished@Dynamic@SBiomass)[['Stock']], StockNames(om), info = name)

    df <- SB_SB0(mse, type = 'Dynamic')
    expect_false(anyNA(df$Value), info = name)
    expect_true(all(df$Value[df$Period == 'Projection'] > 0), info = name)
  }
})

test_that("Year is collapsed only when every time step is identical", {
  Yrs <- as.character(2020:2024)
  Const <- array(5, dim = c(2, 1, 5), dimnames = list(Sim = 1:2, Stock = "S", Year = Yrs))
  expect_identical(dimnames(.CollapseConstantYears(Const))$Year, "2020")

  Vary <- Const
  Vary[, , 3:5] <- 6
  expect_identical(.CollapseConstantYears(Vary), Vary)

  Num <- list(S = array(1, dim = c(1, 2, 5, 1),
                        dimnames = list(Sim = 1, Age = 0:1, Year = Yrs, Area = 1)))
  expect_identical(dimnames(.CollapseConstantYears(Num)$S)$Year, "2020")
})
