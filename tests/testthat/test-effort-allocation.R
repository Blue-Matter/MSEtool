# `EffortAllocation` splits a scalar, absolute Effort recommendation
# (`EffType = "Abs"`) across fleets, the same way `FleetAllocation` splits a
# scalar TAC - see `.CheckEffortAllocation()` in R/check-effort-allocation.R
# and the application in `.UpdateEffortSim()` in R/update-effort.R. It must
# not affect scalar `Rel` Effort, fleet-length Effort vectors, fleet x area
# Effort matrices, or single-fleet OMs. `EFactor` is a deprecated alias,
# kept only so objects that already set it keep working.

test_that(".CheckEffortAllocation() defaults to relative effort over the last 5 historical years", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  EA <- hist@OM@EffortAllocation[[1]]
  expect_equal(unname(dim(EA)), c(2, 2))
  expect_equal(unname(rowSums(EA)), c(1, 1), tolerance = 1e-8)

  HistYears <- Years(hist, 'H')
  last_5 <- utils::tail(seq_len(length(HistYears)), 5)
  eff <- hist@Effort[, last_5, , drop = FALSE]
  eff <- apply(eff, c('Sim', 'Fleet'), sum)
  expected <- eff / apply(eff, 'Sim', sum)
  expect_equal(unname(EA), unname(matrix(expected, 2, 2)), tolerance = 1e-8)
})

test_that(".CheckEffortAllocation() validates shape, positivity, and row sums", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  bad_dim <- hist
  bad_dim@OM@EffortAllocation <- list(matrix(0.5, 2, 3))  # wrong nFleet
  expect_error(.CheckEffortAllocation(bad_dim))

  bad_negative <- hist
  bad_negative@OM@EffortAllocation <- list(matrix(c(-0.5, 1.5), 1, 2))
  expect_error(.CheckEffortAllocation(bad_negative))

  bad_sum <- hist
  bad_sum@OM@EffortAllocation <- list(matrix(c(0.5, 0.6), 1, 2))  # sums to 1.1
  expect_error(.CheckEffortAllocation(bad_sum))

  bad_length <- hist
  bad_length@OM@EffortAllocation <- list(matrix(0.5, 1, 2), matrix(0.5, 1, 2))  # too many complexes
  expect_error(.CheckEffortAllocation(bad_length))

  ok <- hist
  ok@OM@EffortAllocation <- list(matrix(c(0.3, 0.7), 1, 2, dimnames = list(Sim = 1, Fleet = FleetNames(om))))
  ok <- .CheckEffortAllocation(ok)
  expect_equal(unname(ok@OM@EffortAllocation[[1]]), matrix(c(0.3, 0.7), 1, 2))
})

test_that(".CheckEffortAllocation() falls back to the deprecated EFactor slot", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)

  hist@OM@EffortAllocation <- list()
  hist@OM@EFactor <- stats::setNames(
    list(matrix(c(0.4, 0.6), 1, 2)), names(hist@OM@Complexes)
  )
  hist <- .CheckEffortAllocation(hist)

  expect_equal(unname(hist@OM@EffortAllocation[[1]]), matrix(c(0.4, 0.6), 1, 2))
})

test_that("a scalar absolute Effort recommendation is split by EffortAllocation", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 1
  pYear(om) <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  hist@OM@EffortAllocation <- stats::setNames(
    list(matrix(c(0.3, 0.7), 1, 2)), names(hist@OM@Complexes)
  )

  scalarAbsMP <- function(Data) Advice(Effort = 2, EffType = "Abs")
  class(scalarAbsMP) <- "mp"
  assign("scalarAbsMP", scalarAbsMP, envir = globalenv())
  on.exit(rm("scalarAbsMP", envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = "scalarAbsMP", parallel = FALSE, silent = TRUE)

  eff <- mse@Effort[1, 1, , 1]
  expect_equal(unname(eff), c(2 * 0.3, 2 * 0.7), tolerance = 1e-6)
})

test_that("a scalar relative Effort recommendation ignores EffortAllocation and preserves fleet shares", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 1
  pYear(om) <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  # a lopsided EffortAllocation that must NOT be applied to `Rel` effort
  hist@OM@EffortAllocation <- stats::setNames(
    list(matrix(c(0.99, 0.01), 1, 2)), names(hist@OM@Complexes)
  )

  scalarRelMP <- function(Data) Advice(Effort = 1.5, EffType = "Rel")
  class(scalarRelMP) <- "mp"
  assign("scalarRelMP", scalarRelMP, envir = globalenv())
  on.exit(rm("scalarRelMP", envir = globalenv()), add = TRUE)

  HistYears <- Years(hist, 'H')
  lastHistEffort <- hist@Effort[1, length(HistYears), ]

  mse <- Project(hist, MPs = "scalarRelMP", parallel = FALSE, silent = TRUE)
  eff <- mse@Effort[1, 1, , 1]

  expect_equal(unname(eff), unname(1.5 * lastHistEffort), tolerance = 1e-6)
})

test_that("EffortAllocation is not required and not applied for single-fleet OMs", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 1
  pYear(om) <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  hist@OM@EffortAllocation <- list()  # deliberately unset

  scalarAbsMP <- function(Data) Advice(Effort = 2, EffType = "Abs")
  class(scalarAbsMP) <- "mp"
  assign("scalarAbsMP", scalarAbsMP, envir = globalenv())
  on.exit(rm("scalarAbsMP", envir = globalenv()), add = TRUE)

  mse <- Project(hist, MPs = "scalarAbsMP", parallel = FALSE, silent = TRUE)
  expect_equal(unname(mse@Effort[1, 1, , 1]), 2, tolerance = 1e-6)
})
