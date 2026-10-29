# OM replacement functions (`Seed<-`, `Control<-`, ...) on om/hist/mse
# objects: .AssignOMSlot() in R/constructor-om.R.

MakeHist <- function() {
  h <- new('hist')
  h@OM <- SingleStockOM
  h
}

MakeMSE <- function() {
  m <- new('mse')
  m@OM <- SingleStockOM
  m
}

test_that(".AssignSlot() errors rather than returning NULL for a missing slot", {
  expect_error(.AssignSlot(MakeHist(), 1, 'Seed'), 'not found')
  expect_error(.AssignSlot(list(), 1, 'Seed'), 'not found')
})

test_that("setters on om objects are unchanged", {
  OM <- SingleStockOM
  Seed(OM) <- 7
  Seasons(OM) <- 1
  Author(OM) <- 'A'
  Control(OM)$MSYType <- 'Landings'
  expect_equal(Seed(OM), 7)
  expect_equal(Author(OM), 'A')
  expect_equal(Control(OM)$MSYType, 'Landings')
})

test_that("metadata setters work on hist and mse objects", {
  Hist <- MakeHist()
  Author(Hist) <- 'A'
  expect_s4_class(Hist, 'hist')
  expect_equal(Hist@OM@Author, 'A')

  MSE <- MakeMSE()
  Source(MSE) <- 'S'
  expect_s4_class(MSE, 'mse')
  expect_equal(MSE@OM@Source, 'S')
})

test_that("projection setters work on hist but error on mse", {
  Hist <- MakeHist()
  Interval(Hist) <- 3
  Control(Hist)$DataOM <- 'Biomass'
  DataLag(Hist) <- 2
  expect_s4_class(Hist, 'hist')
  expect_equal(Interval(Hist), 3)
  expect_equal(Control(Hist)$DataOM, 'Biomass')
  expect_equal(DataLag(Hist), 2)

  MSE <- MakeMSE()
  expect_error(Interval(MSE) <- 3, 'Project')
  expect_error(Control(MSE)$DataOM <- TRUE, 'Project')
  expect_s4_class(MSE, 'mse')
})

test_that("structural setters error on hist and mse, leaving the object intact", {
  Hist <- MakeHist()
  for (fn in c('Seed<-', 'Seasons<-', 'nYear<-', 'maxF<-', 'Complexes<-')) {
    expect_error(do.call(fn, list(Hist, value = 1)), 'Simulate')
  }
  expect_error(Seed(Hist) <- 5, 'Simulate')
  expect_s4_class(Hist, 'hist')
  expect_equal(Seed(Hist), Seed(SingleStockOM))

  MSE <- MakeMSE()
  expect_error(Seed(MSE) <- 5, 'Simulate')
})
