test_that("runMP applies MPs to a legacy Data object", {
  skip_on_cran()
  Data <- MSEtool::SimulatedData
  out <- runMP(Data, MPs = c("FMSYref", "curEref", "NFref"), reps = 5, silent = TRUE)
  expect_s4_class(out, "Data")
  expect_equal(dim(out@TAC), c(3, 5, 1))
  expect_identical(out@MPs, c("FMSYref", "curEref", "NFref"))
  expect_true(all(out@TAC[3, , ] < 1e-8))
})

test_that("applyMP returns recommendations for each MP", {
  skip_on_cran()
  out <- applyMP(MSEtool::SimulatedData, MPs = "NFref", reps = 5, silent = TRUE)
  expect_length(out, 2)
  expect_s4_class(out[[2]], "Data")
  expect_true(all(out[[1]][[1]]$TAC < 1e-8))
})

test_that("Can lists the MPs that run on a Data object", {
  skip_on_cran()
  can <- Can(MSEtool::SimulatedData)
  expect_true(all(c("curEref", "FMSYref", "NFref") %in% can))
  expect_true(all(can %in% avail("MP", "MSEtool", msg = FALSE)))
})

test_that("Required lists the data slots an MP uses", {
  req <- Required("FMSYref")
  expect_true(is.data.frame(req) || is.matrix(req))
  expect_true("FMSYref" %in% req[, 1])
})

test_that("Lag_Data drops the most recent years", {
  Data <- MSEtool::SimulatedData
  lagged <- Lag_Data(Data, Data_Lag = 2)
  ny <- ncol(Data@Cat)
  expect_equal(dim(lagged@Cat), c(nrow(Data@Cat), ny - 2))
  expect_equal(lagged@Cat, Data@Cat[, 1:(ny - 2), drop = FALSE])
  expect_identical(Lag_Data(Data, Data_Lag = 0), Data)
})

test_that("joinData stacks simulations from several Data objects", {
  Data <- MSEtool::SimulatedData
  joined <- joinData(list(Data, Data))
  expect_equal(nrow(joined@Cat), 2 * nrow(Data@Cat))
  expect_equal(joined@Cat, rbind(Data@Cat, Data@Cat), ignore_attr = TRUE)
})

test_that("simulated Data track the operating model", {
  skip_on_cran()
  OM <- .LegacyOM()
  Hist <- .LegacyHist()
  Data <- Hist@Data
  expect_equal(length(Data@Year), OM@nyears)
  expect_equal(dim(Data@Ind), c(OM@nsim, OM@nyears))
  expect_equal(Data@LHYear, Data@Year[OM@nyears])
  # simulated catch is a noisy observation of the true removals
  trueCat <- apply(Hist@TSdata$Landings, c(1, 2), sum)
  ratio <- Data@Cat / trueCat
  expect_true(all(is.finite(ratio[, -1])))
  expect_true(abs(log(mean(ratio[, -1]))) < 1)
})

test_that("ConvertData converts a legacy Data object", {
  skip_on_cran()
  d <- ConvertData(MSEtool::SimulatedData, silent = TRUE)
  expect_s4_class(d, "data")
})
