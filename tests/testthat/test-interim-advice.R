
.InterimRows <- function(Mean, CV = NA_real_, Max = NA_real_)
  data.frame(Mean = Mean, CV = CV, Max = Max)

test_that("interim multiplier is deterministic when CV is NA/0 or all Mean are 0", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 10
  expect_equal(.SampleInterimMultiplier(om, 2025, "S", "TAC", .InterimRows(100), 1), 1)
  expect_equal(.SampleInterimMultiplier(om, 2025, "S", "TAC", .InterimRows(100, 0), 1), 1)
  expect_equal(.SampleInterimMultiplier(om, 2025, "S", "TAC", .InterimRows(c(0, 0), 0.3), 1), 1)
})

test_that("interim multiplier has mean 1 and the requested CV", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 20000
  rows <- .InterimRows(c(600, 400), 0.2)
  m <- .SampleInterimMultiplier(om, 2025, "S", "TAC", rows, seq_len(om@nSim))
  expect_equal(mean(m), 1, tolerance = 0.01)
  expect_equal(sd(m) / mean(m), 0.2, tolerance = 0.03)
})

test_that("interim multiplier is truncated at the smallest Max / Mean", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 5000
  rows <- .InterimRows(c(600, 400, 0), 0.3, c(720, 500, NA))
  m <- .SampleInterimMultiplier(om, 2025, "S", "TAC", rows, seq_len(om@nSim))
  expect_true(all(m <= 1.2))
  expect_true(mean(m) < 1)
})

test_that("interim multiplier seeds differ across years, stocks, and types", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 5
  rows <- .InterimRows(100, 0.3)
  draw <- function(y, s, t) .SampleInterimMultiplier(om, y, s, t, rows, seq_len(om@nSim))
  expect_false(isTRUE(all.equal(draw(2024, "S", "TAC"), draw(2042, "S", "TAC"))))
  expect_false(isTRUE(all.equal(draw(2025, "S1", "TAC"), draw(2024, "S2", "TAC"))))
  expect_false(isTRUE(all.equal(draw(2025, "S", "TAC"), draw(2025, "S", "Effort"))))
  expect_equal(draw(2025, "S", "TAC"), draw(2025, "S", "TAC"))
})

test_that("InterimAdvice validation rejects SD, negative CV, mixed CV, and Max < Mean", {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  yr <- CurrentYear(om) + 1
  MPStartYear(om) <- yr + 1
  set_ia <- function(df) { om@InterimAdvice <- df; methods::validObject(om) }

  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, SD = 10)), "use `CV`")
  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, CV = -0.1)), "CV")
  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Fleet = c("A", "B"),
                                 Mean = c(100, 50), CV = c(0.1, 0.2))), "identical")
  expect_error(set_ia(data.frame(Year = yr, Type = "TAC", Mean = 100, Max = 90)), "Max")

  expect_no_error(set_ia(data.frame(Year = yr, Type = "TAC", Fleet = c("A", "B", "C"),
                                Mean = c(100, 50, 0), CV = c(0.2, 0.2, NA), Max = c(100, NA, NA))))
})
