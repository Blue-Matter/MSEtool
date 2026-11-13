# OM() returns the OM slot of an enclosing object; OM<- replaces it except on hist/mse.

test_that("OM() still constructs an om from no or character arguments", {
  expect_s4_class(OM(), "om")
  om <- OM("My OM", nSim = 3)
  expect_s4_class(om, "om")
  expect_identical(om@Name, "My OM")
})

test_that("OM() and OM<- work on any object with an OM slot", {
  methods::setClass("omholder", slots = c(OM = "om"), where = environment())
  holder <- methods::new("omholder", OM = OM("A"))
  expect_identical(OM(holder)@Name, "A")
  expect_identical(lapply(OM(list(a = holder, b = holder)), Name), list(a = "A", b = "A"))

  OM(holder) <- OM("B")
  expect_identical(OM(holder)@Name, "B")
  expect_error(OM(holder) <- "B", "must be class")
  expect_error(OM(list(holder, 1)), "No slot")
})

test_that("OM() extracts the om from a hist, and OM<- refuses hist and mse", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  hist <- Simulate(om, silent = TRUE)
  expect_s4_class(OM(hist), "om")
  expect_identical(OM(hist), hist@OM)
  expect_error(OM(hist) <- OM(hist), "cannot be replaced")

  mse <- methods::new("mse")
  expect_error(OM(mse) <- om, "cannot be replaced")
})
