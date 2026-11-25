test_that("spawn timing is derived from Spawn_month when r4ss leaves it NA", {
  expect_equal(.GetSSSpawnTimeFrac(list(Spawn_timing_in_season = 0.25, Spawn_month = 9)), 0.25)
  expect_equal(.GetSSSpawnTimeFrac(list(Spawn_timing_in_season = NA, Spawn_month = 9, nseasons = 1, spawnseas = 1)), 8 / 12)
  expect_equal(.GetSSSpawnTimeFrac(list(Spawn_timing_in_season = NA, Spawn_month = 5, nseasons = 4, spawnseas = 2)), 1 / 3)
  expect_equal(.GetSSSpawnTimeFrac(list(Spawn_timing_in_season = NA, Spawn_month = 1, nseasons = 1, spawnseas = 1)), 0)
})

test_that("age classes start at the settlement age", {
  natage <- data.frame(Yr = 2000, `0` = 0, `1` = 10, `2` = 5, check.names = FALSE)
  rd <- \(Age) list(recruit_dist = data.frame(Age = Age))
  expect_equal(.GetSSAgeClasses(list(natage = natage, recruitment_dist = rd(1))), c(1, 2))
  expect_equal(.GetSSAgeClasses(list(natage = natage, recruitment_dist = rd(0))), c(0, 1, 2))
  expect_equal(.GetSSAgeClasses(list(natage = natage)), c(0, 1, 2))
})

test_that("empirical weight-at-age is read by fleet and carried forward to years without a row", {
  wtatage <- data.frame(year = c(2000, 2002, 2000), seas = 1, sex = 1, fleet = c(0, 0, 1),
                        `1` = c(1, 2, 5), `2` = c(3, 4, 6), check.names = FALSE)
  replist <- list(wtatage = wtatage, wtatage_switch = TRUE)
  YearsList <- list(YearsHist = 1999:2003, Seasons = 1)
  Ages <- Ages(MaxAge = 2, MinAge = 1, Units = "year")

  W <- .GetSSWtAtAge(1, replist, YearsList, 0, Ages)
  expect_equal(dimnames(W), list(Age = c("1", "2"), Year = as.character(1999:2003)))
  expect_equal(unname(W["1", ]), c(1, 1, 1, 2, 2))
  expect_equal(unname(.GetSSWtAtAge(1, replist, YearsList, 1, Ages)["2", ]), rep(6, 5))
  expect_null(.GetSSWtAtAge(1, replist, YearsList, -2, Ages))
  expect_null(.GetSSWtAtAge(1, list(wtatage = wtatage, wtatage_switch = FALSE), YearsList, 0, Ages))
})

test_that("bias-adjustment breakpoints are read when r4ss shifts the columns", {
  rec <- data.frame(Yr = 1980:2030, biasadjuster = 0.8)
  Shifted <- data.frame(last_yr_early = "breakpoints_for_bias_adjustment_ramp:",
                        first_yr_full = "1985", last_yr_full = "1990",
                        first_yr_recent = "2026", max_bias_adj = "2014")
  out <- .GetSSPlateauYears(list(recruit = rec, breakpoints_for_bias_adjustment_ramp = Shifted), 1985:2026)
  expect_equal(out$plateau, 1990:2026)
  expect_length(out$trailing_unsupported, 0)
})
