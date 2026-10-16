test_that("named lookup renames stocks and fleets, including array dimnames", {
  OM <- Rename(MultiStockOM,
               Stocks = c(Alb = 'Example Albacore Stock'),
               Fleets = list(Dome = 'DomeExFleet'))
  expect_equal(StockNames(OM), c('Alb', 'Example Butterfish Stock'))
  expect_equal(FleetNames(OM), c('AsympExFleet', 'Dome'))
  expect_equal(names(OM@Fleet), c('Alb', 'Example Butterfish Stock'))
  expect_equal(names(OM@Fleet[[2]]), c('AsympExFleet', 'Dome'))
})

test_that("unnamed vector renames positionally", {
  OM <- Rename(MultiStockOM, Stocks = c('A', 'B'), Fleets = c('F1', 'F2'))
  expect_equal(StockNames(OM), c('A', 'B'))
  expect_equal(FleetNames(OM), c('F1', 'F2'))
  expect_equal(names(OM@Fleet[[1]]), c('F1', 'F2'))
})

test_that("unnamed vector can swap names", {
  OM <- Rename(MultiStockOM, Fleets = rev(FleetNames(MultiStockOM)))
  expect_equal(FleetNames(OM), rev(FleetNames(MultiStockOM)))
})

test_that("single-stock, single-fleet OM accepts a length-1 unnamed vector", {
  OM <- Rename(SingleStockOM, Stocks = 'S', Fleets = 'F')
  expect_equal(StockNames(OM), 'S')
  expect_equal(FleetNames(OM), 'F')
})

test_that("invalid lookups error", {
  OM <- MultiStockOM
  expect_error(Rename(OM, Stocks = 'A'), 'must have length 2')
  expect_error(Rename(OM, Fleets = c('F1', 'F2', 'F3')), 'must have length 2')
  expect_error(Rename(OM, Stocks = c('A', 'A')), 'Duplicated new names')
  expect_error(Rename(OM, Stocks = c('A', NA)), 'empty')
  expect_error(Rename(OM, Stocks = c('A', '')), 'empty')
  expect_error(Rename(OM, Stocks = c(A = 'Example Albacore Stock', 'Example Butterfish Stock')), 'empty')
  expect_error(Rename(OM, Stocks = c(A = 'Example Albacore Stock', B = 'Example Albacore Stock')), 'Duplicated old names')
  expect_error(Rename(OM, Stocks = c(A = 'Not A Stock')), 'not found')
  expect_error(Rename(OM, Stocks = c('Example Butterfish Stock' = 'Example Albacore Stock')), 'duplicate names')
  expect_error(Rename(OM, Stocks = list(A = c('Example Albacore Stock', 'x'))), 'single name')
  expect_error(Rename(OM, Stocks = list(A = 1)), 'must be character')
  expect_error(Rename(OM, Stocks = 1:2), 'must be a character vector')
  expect_error(Rename(OM, Stocks = character(0)), 'empty')
})
