# Slot accessors return the slot when given a data object, a list of them, or a data sub-object.

test_that("data slot accessors return the slot of a data object", {
  d <- new("data")
  for (fn in c("Effort", "Advice", "Landings", "Discards", "CPUE", "Survey",
               "LandingsAtAge", "LifeHistory", "Exploitation", "Reference", "YearLH", "Misc"))
    expect_identical(get(fn)(d), slot(d, fn), info = fn)
  expect_identical(Effort(d@Advice), d@Advice@Effort)
  expect_identical(TAC(d@Advice), d@Advice@TAC)
})

test_that("data slot accessors map over a list of data objects", {
  d <- new("data")
  L <- list(A = d, B = d)
  for (fn in c("Effort", "Advice", "Landings", "Discards", "CPUE", "Survey", "LandingsAtAge",
               "LifeHistory", "Exploitation", "Reference", "YearLH", "Agency", "Latitude", "Misc"))
    expect_identical(get(fn)(L), lapply(L, slot, fn), info = fn)
})

test_that("life-history and exploitation accessors return the slot of data sub-objects", {
  lh <- new("lifehistorydata")
  for (fn in c("Ages", "Length", "Weight", "NaturalMortality", "Maturity",
               "Fecundity", "SRR", "Spatial", "Depletion")) {
    expect_identical(get(fn)(lh), slot(lh, fn), info = fn)
    expect_identical(get(fn)(list(S1 = lh)), list(S1 = slot(lh, fn)), info = fn)
  }
  ex <- new("exploitationdata")
  for (fn in c("Selectivity", "Retention", "DiscardMortality")) {
    expect_identical(get(fn)(ex), slot(ex, fn), info = fn)
    expect_identical(get(fn)(list(S1 = list(F1 = ex))), list(S1 = list(F1 = slot(ex, fn))), info = fn)
  }
})
