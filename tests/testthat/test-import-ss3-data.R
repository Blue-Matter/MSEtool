MockSSCatchReplist <- function() {
  Catch <- expand.grid(Yr = 2000:2002, Fleet = 1:2)
  Catch$Fleet_Name <- c('F1_Biomass', 'F2_Number')[Catch$Fleet]
  Catch$Area     <- 1
  Catch$Seas     <- 1
  Catch$ret_num  <- c(1, 2, 3, 10, 20, 30)
  Catch$kill_num <- Catch$ret_num * 1.1
  Catch$ret_bio  <- c(100, 200, 300, 500, 1000, 1500)
  Catch$kill_bio <- Catch$ret_bio * 1.2
  list(catch = Catch, catch_units = c(1, 2), IsFishFleet = c(TRUE, TRUE),
       nseasons = 1, startyr = 2000, endyr = 2002)
}

test_that(".ImportSSDataCatch keeps the SS3 catch units of each fleet by default", {
  replist <- MockSSCatchReplist()
  Out <- .ImportSSDataCatch(replist)
  Catch <- replist$catch

  expect_equal(unname(Out$Landings@Units), c('Biomass', 'Number'))
  expect_equal(unname(Out$Discards@Units), c('Biomass', 'Number'))
  expect_equal(unname(Out$Landings@Value[, 'F1_Biomass']), Catch$ret_bio[Catch$Fleet == 1])
  expect_equal(unname(Out$Landings@Value[, 'F2_Number']), Catch$ret_num[Catch$Fleet == 2])
  expect_equal(unname(Out$Discards@Value[, 'F2_Number']),
               with(Catch[Catch$Fleet == 2, ], kill_num - ret_num))
})

test_that(".ImportSSDataCatch imports every fleet in biomass with CatchUnits = 'Biomass'", {
  replist <- MockSSCatchReplist()
  Out <- .ImportSSDataCatch(replist, CatchUnits = 'Biomass')
  Catch <- replist$catch

  expect_equal(unname(Out$Landings@Units), c('Biomass', 'Biomass'))
  expect_equal(unname(Out$Discards@Units), c('Biomass', 'Biomass'))
  expect_equal(unname(Out$Landings@Value[, 'F1_Biomass']), Catch$ret_bio[Catch$Fleet == 1])
  expect_equal(unname(Out$Landings@Value[, 'F2_Number']), Catch$ret_bio[Catch$Fleet == 2])
  expect_equal(unname(Out$Discards@Value[, 'F2_Number']),
               with(Catch[Catch$Fleet == 2, ], kill_bio - ret_bio))
})

test_that("ImportSSData rejects unknown CatchUnits", {
  expect_error(ImportSSData(list(), CatchUnits = 'Numbers'), "should be one of")
})
