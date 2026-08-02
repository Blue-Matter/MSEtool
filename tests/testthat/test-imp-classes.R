test_that("EmptyObject works on imp objects without erroring", {
  # Regression test: imp@Name was previously typed as non-nullable
  # "character", but isNewObject() unconditionally does
  # `object@Name <- NULL` for any class with a Name slot, which crashed
  # for a strictly-typed character slot.
  expect_true(EmptyObject(Imp()))
  expect_false(EmptyObject(Imp(TAC = ImpSlot(Mean = 0.9))))
})

test_that("Imp()/ImpSlot() construct and attach correctly", {
  imp <- Imp(Name = "test", TAC = ImpSlot(Compliance = 1))
  expect_s4_class(imp, "imp")
  expect_equal(imp@Name, "test")
  expect_equal(imp@TAC@Compliance, 1)
  expect_s4_class(imp@Effort, "impslot")
})

test_that("PopulateImpSlot treats a scalar Mean as a literal fixed value, not a CV", {
  # Mean is documented as the literal mean implemented fraction (e.g. 0.9 =
  # 90% average compliance) -- unlike CatchObs's Bias, a scalar must not be
  # treated as the CV of stochastic noise around 1.
  sl <- ImpSlot(Mean = 0.9, SD = 0.05)
  out <- PopulateImpSlot(sl, nSim = 4, Years = 2020:2024)
  expect_equal(as.numeric(out@Mean), rep(0.9, 4))
  expect_equal(dim(out@Error), c(Sim = 4L, Year = 5L))
  expect_equal(mean(as.numeric(out@Error)), 0.9, tolerance = 0.05)
})

test_that("PopulateImpSlot leaves an empty ImpSlot untouched", {
  sl <- ImpSlot()
  out <- PopulateImpSlot(sl, nSim = 4, Years = 2020:2024)
  expect_length(out@Mean, 0)
  expect_length(out@Error, 0)
})

test_that("PopulateImpSlot respects a directly-supplied Error array", {
  err <- array(1.5, dim = c(2, 3), dimnames = list(Sim = 1:2, Year = 2020:2022))
  sl <- ImpSlot(Mean = 0.9, Error = err)
  out <- PopulateImpSlot(sl, nSim = 2, Years = 2020:2022)
  expect_equal(out@Error, err)
})

test_that(".CombineEffortByCompliance interpolates between the first and last binding TAC", {
  # one fleet, two complexes: complex 1 binds at effort 0.4, complex 2 at 1.0
  EbyC <- matrix(c(0.4, 1.0), nrow = 2, ncol = 1)

  # full compliance -> stop at the first TAC reached
  expect_equal(MSEtool:::.CombineEffortByCompliance(EbyC, matrix(1, 1, 2)), 0.4)
  # no compliance -> fish on until the last TAC is reached
  expect_equal(MSEtool:::.CombineEffortByCompliance(EbyC, matrix(0, 1, 2)), 1.0)
  # partial compliance sits between the two
  half <- MSEtool:::.CombineEffortByCompliance(EbyC, matrix(0.5, 1, 2))
  expect_gt(half, 0.4)
  expect_lt(half, 1.0)
})

test_that(".CombineEffortByCompliance treats an unset Compliance as a hard cap", {
  EbyC <- matrix(c(0.4, 1.0), nrow = 2, ncol = 1)
  Compliance <- matrix(NA_real_, nrow = 1, ncol = 2)
  expect_equal(MSEtool:::.CombineEffortByCompliance(EbyC, Compliance), 0.4)
})

test_that(".CombineEffortByCompliance ignores complexes with no TAC", {
  # complex 1 has no TAC (NA row); only complex 2 constrains the fleet
  EbyC <- matrix(c(NA_real_, 0.7), nrow = 2, ncol = 1)
  expect_equal(MSEtool:::.CombineEffortByCompliance(EbyC, matrix(1, 1, 2)), 0.7)

  # no complex constrains it at all -> NA, left for the caller to fill in
  none <- matrix(NA_real_, nrow = 2, ncol = 1)
  expect_true(is.na(MSEtool:::.CombineEffortByCompliance(none, matrix(1, 1, 2))))
})

test_that("PopulateImpSlot expands a scalar Compliance to [Sim x Year], constant across sims/years", {
  sl <- ImpSlot(Compliance = 0.7)
  out <- PopulateImpSlot(sl, nSim = 3, Years = 2020:2022)
  expect_equal(dim(out@Compliance), c(Sim = 3L, Year = 3L))
  expect_true(all(out@Compliance == 0.7))
})

test_that("PopulateImpSlot samples a length-2 Compliance once per sim, constant across years", {
  sl <- ImpSlot(Compliance = c(0.2, 0.8))
  out <- PopulateImpSlot(sl, nSim = 4, Years = 2020:2023)
  expect_equal(dim(out@Compliance), c(Sim = 4L, Year = 4L))
  # constant across years within a sim
  expect_true(all(apply(out@Compliance, 1, function(x) length(unique(x))) == 1))
  # varies across sims (not all identical)
  expect_gt(length(unique(out@Compliance[, 1])), 1)
  expect_true(all(out@Compliance >= 0.2 & out@Compliance <= 0.8))
})

test_that("PopulateImpSlot forward-fills a breakpoint Compliance array across Years", {
  arr <- array(c(0.5, 0.9), dim = c(1, 2), dimnames = list(Sim = 1, Year = c(2020, 2022)))
  sl <- ImpSlot(Compliance = arr)
  out <- PopulateImpSlot(sl, nSim = 2, Years = 2020:2023)
  expect_equal(dim(out@Compliance), c(Sim = 2L, Year = 4L))
  expect_equal(as.numeric(out@Compliance["1", c("2020", "2021")]), c(0.5, 0.5))
  expect_equal(as.numeric(out@Compliance["1", c("2022", "2023")]), c(0.9, 0.9))
  # broadcast to sim 2
  expect_equal(out@Compliance["2", ], out@Compliance["1", ])
})

test_that("PopulateImpSlot leaves Compliance untouched (empty) when unset", {
  sl <- ImpSlot(Mean = 0.9)
  out <- PopulateImpSlot(sl, nSim = 3, Years = 2020:2022)
  expect_length(out@Compliance, 0)
})

test_that(".ResolveComplianceMatrix looks up the per-sim/year Compliance value", {
  om <- methods::new("om")
  sl <- ImpSlot(Compliance = c(0.2, 0.8))
  sl <- PopulateImpSlot(sl, nSim = 3, Years = 2020:2022)
  imp_set   <- Imp(TAC = sl)
  imp_unset <- Imp()

  om@Imp <- list(
    ComplexA = list(FleetA = imp_set),
    ComplexB = list(FleetA = imp_unset)
  )
  hist_obj <- methods::new("hist", OM = om)

  for (s in seq_len(3)) {
    cm <- MSEtool:::.ResolveComplianceMatrix(
      hist_obj, FleetNames = "FleetA", ComplexNames = c("ComplexA", "ComplexB"),
      sim = s, Year = 2021
    )
    expect_equal(cm["FleetA", "ComplexA"], sl@Compliance[s, "2021"], ignore_attr = TRUE)
    expect_true(is.na(cm["FleetA", "ComplexB"]))
  }
})
