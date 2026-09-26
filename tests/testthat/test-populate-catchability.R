# qInc/qCV must be applied to projection Efficiency once, however often the fleet is re-populated

ProjEfficiency <- function(om, pYear) {
  q <- om@Fleet[[1]][[1]]@Catchability@Efficiency
  q[, tail(colnames(q), pYear), drop = FALSE]
}

test_that("re-populating a Catchability object does not re-apply qInc or qCV", {
  C0 <- Catchability(Efficiency = 0.5, qInc = c(-1, 3), qCV = 0.2)
  set.seed(1)
  Pop <- PopulateCatchability(C0, nSim = 3, HistYears = 2001:2010,
                              ProjYears = 2011:2015)
  expect_length(Pop@qInc, 3)
  expect_equal(Pop@qCV, 0.2)

  set.seed(99)
  RePop <- PopulateCatchability(Pop, nSim = 3, HistYears = 2001:2010,
                                ProjYears = 2011:2015)
  expect_identical(RePop@Efficiency, Pop@Efficiency)
  expect_identical(RePop@qInc, Pop@qInc)
  expect_identical(RePop@qCV, Pop@qCV)
})

test_that("editing a populated fleet and re-populating leaves projection Efficiency unchanged", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 5
  om@Fleet[[1]][[1]]@Catchability@qInc <- 2
  om@Fleet[[1]][[1]]@Catchability@qCV <- 0.2

  om <- Populate(om, silent = TRUE)
  q1 <- ProjEfficiency(om, 5)

  om@Fleet[[1]][[1]]@Effort@Effort[, 1] <- 1e-6
  om <- Populate(om, silent = TRUE)
  expect_identical(ProjEfficiency(om, 5), q1)

  om <- Populate(om, silent = TRUE, force = TRUE)
  expect_identical(ProjEfficiency(om, 5), q1)
})

test_that("changing qInc after populating replaces the trend and keeps the qCV deviations", {
  skip_on_cran()
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 5
  om@Fleet[[1]][[1]]@Catchability@qInc <- 2
  om@Fleet[[1]][[1]]@Catchability@qCV <- 0.2
  om <- Populate(om, silent = TRUE)

  q <- om@Fleet[[1]][[1]]@Catchability@Efficiency
  LastHist <- q[, ncol(q) - 5]
  qCVMult <- om@Fleet[[1]][[1]]@Catchability@Misc$qAdjust$qCV$Multiplier

  om@Fleet[[1]][[1]]@Catchability@qInc <- 3
  om <- Populate(om, silent = TRUE)
  expected <- LastHist * outer(rep(1, 3), 1.03^(1:5)) * qCVMult
  expect_equal(unname(ProjEfficiency(om, 5)), unname(expected))

  om@Fleet[[1]][[1]]@Catchability@qInc <- 0
  om <- Populate(om, silent = TRUE)
  expect_null(om@Fleet[[1]][[1]]@Catchability@qInc)
  expect_equal(unname(ProjEfficiency(om, 5)), unname(LastHist * qCVMult))
})
