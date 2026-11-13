test_that("stock components are returned from every object that holds them", {
  om <- MultiStockOM
  expect_s3_class(Length(om), 'StockList')
  expect_named(Length(om), StockNames(om))
  expect_identical(Length(om@Stock), Length(om))
  expect_identical(Length(om@Stock[[1]]), om@Stock[[1]]@Length)

  d <- Data()
  expect_identical(Length(d), d@LifeHistory@Length)
  expect_identical(Length(d@LifeHistory), d@LifeHistory@Length)
  expect_identical(Ages(LifeHistoryObs()), LifeHistoryObs()@Ages)
})

test_that("stock component replacement requires a list named by stock for multi-stock OMs", {
  om1 <- SingleStockOM
  len <- Length(om1)[[1]]
  len@Misc <- list(Tag = 1)
  Length(om1) <- len
  expect_identical(Length(om1)[[1]]@Misc$Tag, 1)

  om <- MultiStockOM
  expect_error(Length(om) <- Length(om)[[1]], "named by stock")
  v <- unclass(Length(om))
  expect_error(Length(om) <- unname(v), "named by stock")

  v <- rev(v)
  v[[1]]@Misc <- list(Tag = 2)
  Length(om) <- v
  expect_identical(Length(om)[[names(v)[1]]]@Misc$Tag, 2)
  expect_identical(Length(om)[[names(v)[2]]], Length(MultiStockOM)[[names(v)[2]]])

  d <- Data()
  Length(d) <- Length(Pars = list(Linf = 100, K = 0.2, t0 = 0))
  expect_identical(Length(d)@Pars$Linf, 100)
})

test_that("fleet component replacement matches values by stock and fleet name", {
  om <- TwoFleetOM
  sel <- Selectivity(om)
  expect_s3_class(sel, 'StockFleetList')
  expect_error(Selectivity(om) <- sel[[1]][[1]], "named by fleet")

  flat <- unclass(sel[[1]])
  flat[[2]]@Misc <- list(Tag = 3)
  Selectivity(om) <- rev(flat)
  expect_identical(Selectivity(om)[[1]][[names(flat)[2]]]@Misc$Tag, 3)

  Selectivity(om) <- sel
  expect_identical(Selectivity(om), sel)

  oms <- MultiStockOM
  expect_error(Selectivity(oms) <- unclass(Selectivity(oms)[[1]]), "named by stock")
  Selectivity(oms) <- Selectivity(oms)
  expect_identical(Selectivity(oms), Selectivity(MultiStockOM))

  om1 <- SingleStockOM
  Selectivity(om1) <- Selectivity(om1)[[1]][[1]]
  expect_identical(Selectivity(om1), Selectivity(SingleStockOM))
})

test_that("components of simulated objects can be read but not replaced", {
  hist <- Simulate(SingleStockOM, nSim = 2, silent = TRUE)
  expect_identical(Length(hist), Length(hist@OM))
  expect_identical(Selectivity(hist), Selectivity(hist@OM))
  expect_identical(Imp(hist), hist@OM@Imp)
  expect_identical(Fleet(hist), hist@OM@Fleet)
  expect_identical(Stock(hist), hist@OM@Stock)
  expect_error(Length(hist) <- Length(hist)[[1]], "cannot be changed")
  expect_error(Selectivity(hist) <- Selectivity(hist), "cannot be changed")
})

test_that("StockTargeting() constructs or returns the OM slot", {
  om <- MultiStockOM
  expect_identical(StockTargeting(om), om@StockTargeting)
  st <- StockTargeting(Mean = array(1, c(1, 1, 1)))
  expect_s4_class(st, 'stocktargeting')
  StockTargeting(om) <- st
  expect_identical(StockTargeting(om), st)
  expect_identical(Covariance(st), NULL)
})

test_that("slot accessors exist for the remaining component slots", {
  ret <- Retention()
  expect_identical(isRel(ret), ret@isRel)
  isAtLength(ret) <- FALSE
  expect_false(isAtLength(ret))
  a <- Advice()
  Closure(a) <- 1
  expect_identical(Closure(a), 1)
})
