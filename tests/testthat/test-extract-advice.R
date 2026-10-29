# TACs() reads MSE@PPD, which is indexed per complex, not per stock.

.RunTACsMSE <- function(om) {
  om@nSim <- 3
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
}

.ExpectTACs <- function(mse) {
  tac <- TACs(mse)
  expect_s3_class(tac, "data.frame")
  expect_gt(nrow(tac), 0)
  expect_identical(names(tac)[1:5], c("Sim", "Stock", "MP", "Year", "Period"))
  expect_setequal(unique(tac$Stock), names(mse@OM@Complexes))
  expect_setequal(unique(tac$Sim), seq_len(nSim(mse)))
  expect_setequal(unique(tac$MP), names(PPD(mse)))
  expect_true(all(tac$Variable == "TAC"))
  expect_true(all(is.finite(tac$Value)))

  aavy <- PM_AAVY(mse, Type = "TAC")
  expect_true(any(is.finite(aavy@Stat)))
  invisible(tac)
}

test_that("TACs() works for a single-stock OM", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  mse <- .RunTACsMSE(SingleStockOM)
  tac <- .ExpectTACs(mse)
  expect_identical(unique(tac$Stock), StockNames(mse))
})

test_that("TACs() labels rows by complex for a multi-stock complex OM", {
  skip_on_cran()
  data(ComplexOM, envir = environment())
  mse <- .RunTACsMSE(ComplexOM)
  tac <- .ExpectTACs(mse)
  expect_identical(unique(tac$Stock), "StockComplex")

  aavy <- PM_AAVY(mse, Type = "TAC", Stocks = StockNames(mse)[1])
  expect_true(any(is.finite(aavy@Stat)))
})

test_that("TACs() carries a TAC forward through years the MP leaves it unchanged", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 2
  pYear(om) <- 5
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  Odd <- function(Data) if (Data@Misc$AdviceYear %% 2 == 1) CurrentCatch(Data) else Advice()
  class(Odd) <- "mp"
  mse <- Project(hist, MPs = list(Odd = Odd), parallel = FALSE, silent = TRUE)
  expect_lt(nrow(mse@PPD$Odd[[1]][[1]]@Advice@TAC), length(Years(mse, "P")))

  tac <- TACs(mse)
  expect_setequal(tac$Year, Years(mse, "P"))
  expect_true(all(tac$Period == "Projection"))
  Even <- tac[tac$Year %% 2 == 0, ]
  Prev <- tac[match(paste(Even$Sim, Even$Fleet, Even$Year - 1), paste(tac$Sim, tac$Fleet, tac$Year)), ]
  expect_equal(Even$Value, Prev$Value)
  expect_identical(TAC(mse), tac)
})

test_that("TACs() returns an empty data frame when no MP sets a TAC", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)

  tac <- TACs(mse)
  expect_s3_class(tac, "data.frame")
  expect_equal(nrow(tac), 0)
  expect_identical(names(tac)[1:5], c("Sim", "Stock", "MP", "Year", "Period"))
  expect_no_error(PM_AAVY(mse, Type = "TAC"))
})
