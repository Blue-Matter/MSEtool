# Closures with fixed area splits, fleet-specific closures, and closures for some stocks only

test_that("closed shares in a fixed area split move to the fleet's open cells", {
  Dist <- array(c(0.4, NA, 0, 0.6, NA, 1), c(1, 3, 2))
  Closed <- array(c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), c(1, 3, 2))
  out <- .MaskDistributionRows(Dist, Closed)
  expect_equal(out[1, 1, ], c(1, 0))
  expect_true(all(is.na(out[1, 2, ])))
  expect_true(all(is.na(out[1, 3, ])))

  expect_identical(.MaskDistributionRows(Dist, Closed & FALSE), Dist)
})

test_that("a cell is closed to a fleet only if every stock it catches is closed there", {
  Closure <- array(1, c(1, 2, 1, 2, 2), dimnames = list(Sim = 1, Stock = 1:2, Year = 2000, Fleet = 1:2, Area = 1:2))
  q <- array(1, c(1, 2, 1, 2), dimnames = list(Sim = 1, Stock = 1:2, Year = 2000, Fleet = 1:2))
  Closure[1, 1, 1, , 2] <- 0
  expect_false(any(.ClosedFleetAreas(Closure, q, 1, 2000)))
  q[1, 2, 1, 2] <- 0
  expect_equal(.ClosedFleetAreas(Closure, q, 1, 2000)[1, , 2], c(FALSE, TRUE))
  Closure[1, 2, 1, , 2] <- 0
  expect_equal(.ClosedFleetAreas(Closure, q, 1, 2000)[1, , 2], c(TRUE, TRUE))
})

skip_on_cran()

.CDOM <- function() {
  OM <- TwoFleetOM
  nSim(OM)  <- 2
  pYear(OM) <- 3
  OM@maxF   <- 10
  OM
}
.CDHist <- Simulate(.CDOM(), silent = TRUE)
.CDPY   <- as.character(Years(.CDHist, 'P'))

test_that("fleet x area effort advice puts no effort in a closed area and keeps the fleet total", {
  Split <- matrix(c(0.3, 0.3, 0.7, 0.7), 2, 2)
  Open   <- function(Data) Advice(Effort = Split)
  Closed <- function(Data) { a <- Advice(Effort = Split); a@Closure <- c(1, 0); a }
  class(Open) <- class(Closed) <- 'mp'
  MSE <- Project(.CDHist, MPs = list(Open = Open, Closed = Closed), parallel = FALSE, silent = TRUE)
  expect_length(MSE@Log$error, 0)
  FA <- MSE@FInteractArea[[1]]
  expect_true(all(FA[, , .CDPY, , '2', 'Closed'] == 0))
  expect_true(any(FA[, , .CDPY, , '2', 'Open'] > 0))
  expect_equal(MSE@Distribution[, .CDPY, , '1', 'Closed'], MSE@Distribution[, .CDPY, , '1', 'Closed'] * 0 + 1)
  expect_equal(MSE@Effort[, .CDPY, , 'Closed'], MSE@Effort[, .CDPY, , 'Open'])
})

test_that("fleet-specific closure advice is stored for each fleet", {
  Proj <- .PrepHistMisc(.CDHist)
  Proj <- .ExtendHist(Proj, Years = c(Years(.CDHist, 'H'), Years(.CDHist, 'P')), silent = TRUE)
  A <- Advice()
  A@Closure <- rbind(c(1, 0), c(1, 1))
  A <- .CheckAdviceClosure(A, Proj, FleetNames(.CDHist), 1:2)
  PY <- Years(.CDHist, 'P')
  Proj <- .UpdateClosureSim(Proj, 1, PY[1], PY, list(A), list(NULL), FleetNames(.CDHist),
                            StockNames(.CDHist@OM), Proj@OM@Complexes, 1:2, 2)
  for (fl in 1:2) {
    expect_equal(unname(Proj@OM@Fleet[[1]][[fl]]@Closure[1, .CDPY, ]),
                 unname(Proj@Misc$Closure[1, 1, .CDPY, fl, ]))
  }
  expect_true(all(Proj@Misc$Closure[1, 1, .CDPY, 1, 2] == 0))
  expect_true(all(Proj@Misc$Closure[1, 1, .CDPY, 2, 2] == 1))
})

test_that("a historical area split into a closed area is moved to the open area", {
  OM <- .CDOM()
  Yr <- Years(OM, 'H')[1]
  OM@Fleet[[1]][[1]]@Effort@Distribution <- array(c(0.4, 0.6), c(1, 1, 2), dimnames = list(Sim = 1, Year = Yr, Area = 1:2))
  OM@Fleet[[1]][[1]]@Closure <- array(c(1, 0), c(1, 1, 2), dimnames = list(Sim = 1, Year = Yr, Area = 1:2))
  expect_no_warning(Hist <- Simulate(OM, silent = TRUE))
  expect_true(any(grepl("closed to the fleet", vapply(Hist@Log$warning, \(e) e$message, character(1)))))
  expect_true(all(Hist@Distribution[, , 1, 2] == 0))
  expect_true(all(Hist@Distribution[, , 1, 1] == 1))
  expect_true(all(Hist@FInteractArea[[1]][, , , 1, 2] == 0))
})

test_that("a closure for one stock releases that stock, and a closure for every stock stops fishing", {
  OM <- MultiStockOM
  nSim(OM)  <- 2
  pYear(OM) <- 3
  Hist <- Simulate(OM, silent = TRUE)
  PY <- as.character(Years(Hist, 'P'))
  One  <- function(DataList) { a <- lapply(DataList, \(d) Advice(Effort = 1)); a[[1]]@Closure <- c(1, 0); a }
  Both <- function(DataList) lapply(DataList, \(d) Advice(Effort = 1, Closure = c(1, 0)))
  class(One) <- class(Both) <- 'mmp'
  MSE <- Project(Hist, MPs = list(One = One, Both = Both), parallel = FALSE, silent = TRUE)
  expect_length(MSE@Log$error, 0)

  Area2 <- \(x, st, mp) x[[st]][, , PY, , '2', mp]
  expect_true(any(Area2(MSE@FInteractArea, 1, 'One') > 0))
  expect_true(all(Area2(MSE@FRetainArea, 1, 'One') == 0))
  expect_true(any(Area2(MSE@FRetainArea, 2, 'One') > 0))
  expect_true(any(MSE@FRetainArea[[1]][, , PY, , '1', 'One'] > 0))

  for (st in 1:2) {
    expect_true(all(Area2(MSE@FInteractArea, st, 'Both') == 0))
  }
  expect_true(all(MSE@Distribution[, PY, , '2', 'Both'] == 0))
})
