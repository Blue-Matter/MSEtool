test_that("fleet-specific TAC and Effort are matched by fleet name", {
  FleetNames <- c("LL", "PS")
  Areas <- c("1", "2")

  Advice <- .CheckAdviceTAC(Advice(TAC = c(PS = 5, LL = 10)), NULL, FleetNames, Areas)
  expect_equal(as.numeric(Advice@TAC), c(10, 5))
  expect_equal(dimnames(Advice@TAC)$Fleet, FleetNames)

  Advice <- .CheckAdviceTAC(Advice(TAC = c(10, 5)), NULL, FleetNames, Areas)
  expect_equal(as.numeric(Advice@TAC), c(10, 5))

  Advice <- .CheckAdviceTAC(Advice(TAC = c(`Fleet 1` = 10, `Fleet 2` = 5)), NULL, FleetNames, Areas)
  expect_equal(as.numeric(Advice@TAC), c(10, 5))

  expect_error(.CheckAdviceTAC(Advice(TAC = c(PS = 5, LX = 10)), NULL, FleetNames, Areas),
               "must match the fleet names")

  Advice <- .CheckAdviceEffort(Advice(Effort = c(PS = 0.5, LL = 1)), NULL, FleetNames, Areas, 1)
  expect_equal(as.numeric(Advice@Effort), c(1, 0.5))

  Eff <- matrix(1:4, 2, 2, dimnames = list(c("PS", "LL"), Areas))
  Advice <- .CheckAdviceEffort(Advice(Effort = Eff), NULL, FleetNames, Areas, 1)
  expect_equal(unname(Advice@Effort[, 1]), c(2, 1))
  expect_equal(dimnames(Advice@Effort)$Fleet, FleetNames)

  Advice <- .CheckAdviceTAC(Advice(TAC = c(Stock = 10)), NULL, "LL", Areas)
  expect_equal(as.numeric(Advice@TAC), 10)
})
