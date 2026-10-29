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

test_that("SS3 survey selectivity is assigned to the survey's Obs by name when fleets are interleaved", {
  # SS3 fleets: F1 fishery, S1 survey, F2 fishery, S2 survey with composition data only, S3 survey
  replist <- list(FleetNames = c("F1", "S1", "F2", "S2", "S3"),
                  IsFishFleet = c(TRUE, FALSE, TRUE, FALSE, FALSE))
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@Data <- list(Data(Survey = IndicesData(Name = c("S3", "S1"))))
  om@Obs <- list(MakeNamedList(c("F1", "F2", "S3", "S1"), new("obs")))

  local_mocked_bindings(
    .GetSSYears = function(...) list(),
    .SS2Selectivity = function(st, fl, ...) methods::new("selectivity", MeanAtAge = array(fl))
  )
  out <- .ProcessSurveyObsSelectivity(om, list(replist))
  expect_equal(as.numeric(out@Obs[[1]][["S1"]]@Survey@Selectivity[[1]]), 2)
  expect_equal(as.numeric(out@Obs[[1]][["S3"]]@Survey@Selectivity[[1]]), 5)
  expect_equal(names(out@Obs[[1]][["S1"]]@Survey@Selectivity), StockNames(om))
  expect_null(out@Obs[[1]][["F2"]]@Survey@Selectivity)
  expect_equal(names(out@Obs[[1]]), c("F1", "F2", "S3", "S1"))
})

test_that("SS3 spawning-production and recruitment surveys use SBiomass and flat selectivity", {
  replist <- MockSSCatchReplist()
  replist$IsFishFleet <- c(TRUE, FALSE, FALSE, FALSE)
  replist$FleetNames <- c("F1", "S_SSB", "S_Rec", "S_Bio")
  replist$survey_units <- c(1, 30, 33, 1)
  replist$cpue <- data.frame(Fleet = rep(2:4, each = 3), Fleet_name = rep(replist$FleetNames[2:4], each = 3),
                             Yr = rep(2000:2002, 3), Seas = 1, Obs = 1:9, SE = 0.2)
  Survey <- .ImportSSDataIndex(replist, "Survey")
  expect_equal(Survey@Name, c("S_SSB", "S_Rec", "S_Bio"))
  expect_equal(unname(Survey@Units), c("Biomass", "Recruitment", "Biomass"))

  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@Data <- list(Data(Survey = Survey))
  om@Obs <- list(MakeNamedList(c("F1", Survey@Name), new("obs")))
  local_mocked_bindings(
    .GetSSYears = function(...) list(),
    .SS2Selectivity = function(st, fl, ...) methods::new("selectivity", MeanAtAge = array(fl))
  )
  out <- .ProcessSurveyObsSelectivity(om, list(replist))
  expect_equal(out@Obs[[1]][["S_SSB"]]@Survey@Selectivity, "SBiomass")
  expect_equal(out@Obs[[1]][["S_Rec"]]@Survey@Selectivity, "Biomass")
  expect_equal(as.numeric(out@Obs[[1]][["S_Bio"]]@Survey@Selectivity[[1]]), 4)
})

test_that("SS3 fishing fleets are matched by fleet number and take user FleetNames", {
  # SS3 fleets: F1 fishery, S1 survey, F2 fishery
  replist <- MockSSCatchReplist()
  replist$catch$Fleet[replist$catch$Fleet == 2] <- 3
  replist$IsFishFleet <- c(TRUE, FALSE, TRUE)
  replist$FleetNames <- c("F1_Biomass", "S1", "F2_Number")
  replist$catch_units <- c(1, 1, 2)
  replist$survey_units <- c(1, 1, 0)
  replist$cpue <- data.frame(Fleet = rep(c(3, 2), each = 3), Fleet_name = rep(c("F2_Number", "S1"), each = 3),
                             Yr = rep(2000:2002, 2), Seas = 1, Obs = 1:6, SE = 0.2)
  expect_equal(.SSFishFleets(replist)$SS, c(1, 3))

  Out <- .ImportSSDataCatch(replist)
  Catch <- replist$catch
  expect_equal(colnames(Out$Landings@Value), c("F1_Biomass", "F2_Number"))
  expect_equal(unname(Out$Landings@Value[, "F2_Number"]), Catch$ret_num[Catch$Fleet == 3])

  Out <- .ImportSSDataCatch(replist, FleetNames = c("Longline", "Purse seine"))
  expect_equal(Out$Landings@Name, c("Longline", "Purse seine"))
  expect_equal(unname(Out$Landings@Value[, "Purse seine"]), Catch$ret_num[Catch$Fleet == 3])

  CPUE <- .ImportSSDataIndex(replist, "CPUE", FleetNames = c("Longline", "Purse seine"))
  expect_equal(CPUE@Name, "Purse seine")
  expect_equal(unname(CPUE@Units), "Number")
  Survey <- .ImportSSDataIndex(replist, "Survey", FleetNames = c("Longline", "Purse seine"))
  expect_equal(Survey@Name, "S1")
  expect_equal(unname(Survey@Units), "Biomass")
})

test_that("SS3 index timing is taken from the observation month", {
  annual <- list(nseasons = 1, seasdurations = 1)
  expect_equal(.SSIndexTiming(data.frame(Month = 7, Seas = 1), annual, "x"), 0.5)
  expect_equal(.SSIndexTiming(data.frame(Month = 1, Seas = 1), annual, "x"), 0)
  quarterly <- list(nseasons = 4, seasdurations = rep(0.25, 4))
  expect_equal(.SSIndexTiming(data.frame(Month = c(4, 4), Seas = c(2, 2)), quarterly, "x"), 0)
  expect_equal(.SSIndexTiming(data.frame(Month = 5.5, Seas = 2), quarterly, "x"), 0.5)
  expect_equal(.SSIndexTiming(data.frame(Seas = 1), annual, "x"), 0)
  expect_message(t <- .SSIndexTiming(data.frame(Month = c(1, 4, 7), Seas = 1), annual, "x"), "median")
  expect_equal(t, 0.25)
})
