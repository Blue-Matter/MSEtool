test_that("nStock and nFleet are 0 for empty objects", {
  expect_equal(nStock(OM()), 0)
  expect_equal(nFleet(OM()), 0)
  expect_equal(FleetNames(OM()), character(0))
  expect_equal(StockNames(OM()), character(0))
})

test_that("nStock and nFleet accept stock, fleet, and list objects", {
  expect_equal(nStock(Stock(Name = "S")), 1)
  expect_equal(nStock(list(Stock(Name = "A"), Stock(Name = "B"))), 2)
  expect_equal(nStock(SingleStockOM), 1)
  expect_equal(nStock(MultiStockOM), 2)

  expect_equal(nFleet(Fleet(Name = "F")), 1)
  expect_equal(nFleet(list(Fleet(Name = "F1"), Fleet(Name = "F2"))), 2)
  expect_equal(nFleet(TwoFleetOM), 2)
  expect_equal(nFleet(MultiStockOM), 2)
})

test_that("nFleet and FleetNames handle a flat OM@Fleet list", {
  om <- OM(Stock = Stock(Name = "S"), Fleet = list(F1 = Fleet(Name = "F1"), F2 = Fleet(Name = "F2")))
  om@Fleet <- list(Fleet(Name = "F1"), Fleet(Name = "F2"))
  expect_equal(nFleet(om), 2)
  expect_equal(FleetNames(om), c("F1", "F2"))

  om@Fleet <- Fleet(Name = "F1")
  expect_equal(nFleet(om), 1)
  expect_equal(FleetNames(om), "F1")
})

test_that("StockNames and FleetNames accept stock and fleet objects", {
  expect_equal(StockNames(Stock(Name = "S")), "S")
  expect_equal(StockNames(list(A = Stock(Name = "S"))), "A")
  expect_equal(StockNames(list(Stock(Name = "S"))), "S")
  expect_equal(FleetNames(Fleet(Name = "F")), "F")
  expect_equal(FleetNames(list(Fleet(Name = "F1"), Fleet(Name = "F2"))), c("F1", "F2"))
  expect_equal(FleetNames(MultiStockOM@Fleet), FleetNames(MultiStockOM))
  expect_error(FleetNames(1), "must be class")
  expect_error(nStock(1), "must be class")
})

test_that("FleetNames, nFleet, and nStock work for data objects", {
  Hist <- Simulate(TwoFleetOM, silent = TRUE)
  Data <- Hist@Data[[1]][[1]]

  expect_equal(FleetNames(Data), FleetNames(TwoFleetOM))
  expect_equal(nFleet(Data), 2)
  expect_equal(nStock(Data), 1)
  expect_equal(FleetNames(Hist@Data[[1]]), FleetNames(TwoFleetOM))
  expect_equal(nStock(Hist@Data[[1]]), 1)
  expect_equal(FleetNames(Hist), FleetNames(TwoFleetOM))
  expect_equal(nFleet(Hist), 2)

  Data@Landings@Name <- NULL
  Data@Discards@Name <- NULL
  Data@LandingsAtAge@Name <- NULL
  expect_equal(FleetNames(Data), FleetNames(TwoFleetOM))
  dimnames(Data@Landings@Value) <- NULL
  expect_true("Fleet 1" %in% FleetNames(Data))
})

test_that("FleetNames IncSurvey adds index names from data", {
  d <- Data()
  d@Landings@Name <- "F1"
  d@Landings@Value <- array(1, c(2, 1))
  d@Survey@Name <- "Acoustic"
  d@Survey@Value <- array(1, c(2, 1))
  expect_equal(FleetNames(d), "F1")
  expect_equal(FleetNames(d, IncSurvey = TRUE), c("F1", "Acoustic"))
  expect_equal(nFleet(d), 1)
})

test_that("nAge and AgeClasses work for om, hist, stock, and data objects", {
  expect_equal(nAge(SingleStockOM), 21)
  expect_equal(nAge(Ages(MaxAge = 5)), 6)
  expect_type(nAge(MultiStockOM), "list")
  expect_length(nAge(MultiStockOM), 2)
  expect_equal(nAge(MultiStockOM, st = 2), nAge(MultiStockOM@Stock[[2]]))
  expect_equal(nAge(MultiStockOM, st = StockNames(MultiStockOM)[2]), nAge(MultiStockOM, st = 2))
  expect_error(nAge(MultiStockOM, st = 3), "not found")
  expect_equal(AgeClasses(SingleStockOM), SingleStockOM@Stock[[1]]@Ages@Classes)

  Hist <- Simulate(MultiStockOM, silent = TRUE)
  Data <- Hist@Data[[1]][[1]]
  expect_equal(nAge(Data), nAge(MultiStockOM, st = 1))
  expect_equal(AgeClasses(Data), AgeClasses(MultiStockOM, st = 1))
  expect_equal(nAge(Hist@Data[[1]])[[1]], nAge(MultiStockOM)[[1]])
  expect_length(nAge(Hist@Data[[1]]), 2)

  Data@LandingsAtAge <- CompData()
  Data@DiscardsAtAge <- CompData()
  expect_null(AgeClasses(Data))
  Data@LifeHistory@Ages <- Ages(MaxAge = 3)
  expect_equal(nAge(Data), 4)
})

test_that("nLength and LengthClasses work for stock and data objects", {
  stock <- SingleStockOM@Stock[[1]]
  stock@Length@Classes <- seq(0, 100, by = 5)
  expect_equal(nLength(stock), 21)
  expect_equal(LengthClasses(stock), seq(0, 100, by = 5))
  expect_equal(nLength(stock@Length), 21)

  d <- Data()
  expect_equal(nLength(d), 0)
  d@LifeHistory@Length@Classes <- c(10, 20, 30)
  expect_equal(nLength(d), 3)
  d@LandingsAtSize@Name <- c("F1", "F2")
  d@LandingsAtSize@Classes <- list(F1 = c(1, 2), F2 = c(1, 2))
  expect_equal(LengthClasses(d), c(1, 2))
  d@LandingsAtSize@Classes <- list(F1 = c(1, 2), F2 = c(1, 2, 3))
  expect_equal(nLength(d), c(F1 = 2L, F2 = 3L))
  expect_type(LengthClasses(d), "list")
})
