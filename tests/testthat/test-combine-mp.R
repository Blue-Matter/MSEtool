skip_on_cran()

.CombineMPCache <- new.env()
.CombineMPFixture <- function() {
  if (is.null(.CombineMPCache$x)) {
    OM <- SingleStockOM
    nSim(OM) <- 2
    pYear(OM) <- 3
    Hist <- Simulate(OM, silent = TRUE)
    .CombineMPCache$x <- list(
      Hist = Hist,
      Both = Project(Hist, MPs = c("CurrentCatch", "AverageCatch"), silent = TRUE),
      A    = Project(Hist, MPs = "CurrentCatch", silent = TRUE),
      B    = Project(Hist, MPs = "AverageCatch", silent = TRUE)
    )
  }
  .CombineMPCache$x
}

test_that("CombineMP() of single-MP projections reproduces projecting the MPs together", {
  X <- .CombineMPFixture()
  C <- CombineMP(list(X$A, X$B), silent = TRUE)

  expect_s4_class(C, "mse")
  expect_equal(names(MPs(C)), c("CurrentCatch", "AverageCatch"))
  expect_identical(C@OM, X$Both@OM)
  expect_equal(C@Misc, X$Both@Misc)
  for (sl in methods::slotNames("timeseries"))
    expect_equal(methods::slot(C, sl), methods::slot(X$Both, sl), info = sl)
  expect_equal(names(dimnames(C@Landings)), names(dimnames(X$A@Landings)))
  expect_equal(dimnames(C@Landings)$MP, c("CurrentCatch", "AverageCatch"))
})

test_that("CombineMP() keeps each MP's own posterior predictive data", {
  X <- .CombineMPFixture()
  C <- CombineMP(list(X$A, X$B), silent = TRUE)
  expect_equal(names(C@PPD), c("CurrentCatch", "AverageCatch"))
  expect_equal(PPD(C), PPD(X$Both))
  expect_equal(Years(PPD(C)$AverageCatch[[1]][[1]]), Years(PPD(X$B)$AverageCatch[[1]][[1]]))
})

test_that("CombineMP() returns a single mse unchanged", {
  X <- .CombineMPFixture()
  expect_identical(CombineMP(list(X$A), silent = TRUE), X$A)
})

test_that("CombineMP() rejects objects that do not share the same OM", {
  X <- .CombineMPFixture()
  expect_error(CombineMP(list(X$A, X$Hist), silent = TRUE), "mse")
  expect_error(CombineMP(X$A, silent = TRUE))

  Fewer <- Subset(X$B, Sims = 1)
  expect_error(CombineMP(list(X$A, Fewer), silent = TRUE), "number of simulations")

  Other <- X$B
  Other@OM@Name <- paste(Other@OM@Name, "(changed)")
  expect_error(CombineMP(list(X$A, Other), silent = TRUE), "same operating model")
})

test_that("CombineMP() keeps the log entries of every object, shared entries once", {
  X <- .CombineMPFixture()
  C <- CombineMP(list(X$A, X$B), silent = TRUE)
  Msg <- \(Log) vapply(Log, \(e) e$message, character(1))
  for (type in union(names(X$A@Log), names(X$B@Log))) {
    Expected <- unique(c(X$A@Log[[type]], X$B@Log[[type]]))
    expect_setequal(Msg(C@Log[[type]]), Msg(Expected))
    expect_length(C@Log[[type]], length(Expected))
  }
  Shared <- intersect(Msg(X$A@Log$assumption), Msg(X$B@Log$assumption))
  expect_gt(length(Shared), 0)
  expect_equal(sum(Msg(C@Log$assumption) %in% Shared), length(Shared))
})

test_that("CombineMP() rejects MP names repeated across objects", {
  X <- .CombineMPFixture()
  expect_error(CombineMP(list(X$A, X$A), silent = TRUE), "CurrentCatch")
  expect_error(CombineMP(list(X$Both, X$B), silent = TRUE), "unique")
})
