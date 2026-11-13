# ReadDataCSV() / WriteDataCSV(): long-format CSV round trips for data objects.

.ExampleCSV <- function() test_path("../../inst/extdata/ExampleData.csv")

.CSVSlots <- c("Landings", "Discards", "Effort", "CPUE", "Survey",
               "LandingsAtAge", "DiscardsAtAge", "LandingsAtSize", "DiscardsAtSize")

.WriteRows <- function(rows) {
  file <- tempfile(fileext = ".csv")
  utils::write.csv(rows, file, row.names = FALSE, na = "")
  file
}

.Row <- function(Slot, Name, Year = NA, Class = NA, Value = 1, CV = NA, Units = NA, Timing = NA, Season = NA)
  data.frame(Slot = Slot, Name = Name, Year = Year, Season = Season, Class = Class, Value = Value,
             CV = CV, Units = Units, Timing = Timing)

test_that("the example file survives read -> write -> read and write -> read -> write", {
  dat <- ReadDataCSV(.ExampleCSV(), silent = TRUE)
  f1 <- tempfile(fileext = ".csv")
  WriteDataCSV(dat, f1, silent = TRUE)
  dat2 <- ReadDataCSV(f1, silent = TRUE)
  for (sl in .CSVSlots) {
    expect_identical(Value(slot(dat2, sl)), Value(slot(dat, sl)), info = sl)
    if ("CV" %in% slotNames(slot(dat, sl)))
      expect_identical(CV(slot(dat2, sl)), CV(slot(dat, sl)), info = sl)
  }
  f2 <- tempfile(fileext = ".csv")
  WriteDataCSV(dat2, f2, silent = TRUE)
  expect_identical(readLines(f2), readLines(f1))
})

test_that("multiple fleets and indices, discards, effort and metadata are read", {
  dat <- ReadDataCSV(.ExampleCSV(), silent = TRUE)
  expect_identical(Name(Landings(dat)), c("Trawl", "Longline"))
  expect_identical(dim(Value(Landings(dat))), c(15L, 2L))
  expect_true(anyNA(Value(Discards(dat))))
  expect_identical(Name(Effort(dat)), c("Trawl", "Longline"))
  expect_identical(Name(CPUE(dat)), "Trawl CPUE")
  expect_identical(Timing(CPUE(dat)), 0.5)
  expect_identical(Name(Survey(dat)), "Acoustic survey")
  expect_identical(dim(Value(LandingsAtAge(dat))), c(15L, 2L, 11L))
  expect_identical(names(Classes(LandingsAtSize(dat))), c("Trawl", "Longline"))
  expect_identical(CommonName(dat), "Example fish")
  expect_identical(Species(dat), "Piscis exemplaris")
  expect_identical(Latitude(dat), -30)
  expect_identical(Years(dat), 2011:2025)
})

test_that("unlisted classes in an observed year and fleet are zero", {
  rows <- rbind(.Row("LandingsAtAge", "F1", 2000, 1, 5), .Row("LandingsAtAge", "F1", 2000, 3, 2),
                .Row("LandingsAtAge", "F1", 2001, 2, 4))
  dat <- ReadDataCSV(.WriteRows(rows), silent = TRUE)
  v <- Value(LandingsAtAge(dat))
  expect_equal(unname(v["2000", "F1", ]), c(5, 0, 2))
  expect_equal(unname(v["2001", "F1", ]), c(0, 4, 0))
})

test_that("invalid files give informative errors", {
  ok <- .Row("Landings", "F1", 2000)
  expect_error(ReadDataCSV(.WriteRows(.Row("Catch", "F1", 2000)), silent = TRUE), "Unknown")
  expect_error(ReadDataCSV(.WriteRows(rbind(ok, .Row("Metadata", "Colour", Value = "red"))), silent = TRUE),
               "Unknown.*Metadata")
  expect_error(ReadDataCSV(.WriteRows(.Row("Landings", "F1", 2000, Value = "lots")), silent = TRUE),
               "Non-numeric")
  expect_error(ReadDataCSV(.WriteRows(rbind(ok, .Row("Landings", "F1", NA))), silent = TRUE), "Year.*missing")
  expect_error(ReadDataCSV(.WriteRows(.Row("LandingsAtAge", "F1", 2000)), silent = TRUE), "Class.*missing")
  expect_error(ReadDataCSV(.WriteRows(rbind(ok, ok)), silent = TRUE), "Duplicated")
  expect_error(ReadDataCSV(.WriteRows(rbind(ok, .Row("Metadata", "FirstYear", Value = 2001),
                                            .Row("Metadata", "LastYear", Value = 2005))), silent = TRUE),
               "outside the historical years")
})

test_that("WriteDataCSV() rejects existing files", {
  dat <- ReadDataCSV(.ExampleCSV(), silent = TRUE)
  file <- tempfile(fileext = ".csv")
  WriteDataCSV(dat, file, silent = TRUE)
  expect_error(WriteDataCSV(dat, file, silent = TRUE), "exists")
})

# data from a simulated OM: annual or seasonal, with indices and age and size comps
.SimData <- function(Seasons) {
  stock <- if (Seasons == 1) ButterfishExStock else SeasonalSpatialExStock
  if (Seasons > 1) stock@Spatial <- Spatial()
  om <- OM(nSim = 1, nYear = 5, pYear = 2, Seasons = Seasons, Stock = stock, Fleet = DomeExFleet,
           Obs = DataRichObs, Imp = FullComplianceImp)
  Data(Simulate(om, silent = TRUE))[[1]][[1]]
}

.ExpectSameData <- function(new, old) {
  expect_equal(Years(new), Years(old))
  expect_equal(Seasons(new), Seasons(old))
  expect_equal(new@YearLH, old@YearLH)
  for (sl in .CSVSlots) {
    o <- slot(old, sl); n <- slot(new, sl)
    expect_equal(length(Value(n)), length(Value(o)), info = sl)
    if (!length(Value(o))) next
    expect_equal(unname(Value(n)), unname(Value(o)), info = sl)
    expect_equal(as.numeric(dimnames(Value(n))[[1]]), Years(old), info = sl)
    expect_identical(Name(n), Name(o), info = sl)
    expect_identical(Units(n), Units(o), info = sl)
    if ("CV" %in% slotNames(o)) expect_equal(unname(CV(n)), unname(CV(o)), info = sl)
    if ("Classes" %in% slotNames(o)) expect_equal(Classes(n), Classes(o), info = sl)
  }
}

test_that("data simulated from annual and seasonal OMs survive write -> read", {
  skip_on_cran()
  for (Seasons in c(1, 4)) {
    dat <- .SimData(Seasons)
    expect_true(length(Value(LandingsAtAge(dat))) > 0)
    file <- tempfile(fileext = ".csv")
    out <- WriteDataCSV(dat, file, silent = TRUE)
    expect_identical("Season" %in% names(out), Seasons > 1)
    dat2 <- ReadDataCSV(file, silent = TRUE)
    .ExpectSameData(dat2, dat)

    f2 <- tempfile(fileext = ".csv")
    WriteDataCSV(dat2, f2, silent = TRUE)
    expect_identical(readLines(f2), readLines(file))
  }
})

test_that("seasonal files are read by Year and Season or by decimal year", {
  Steps <- CalcYears(2, 0, 2001, Seasons = 4)
  rows <- rbind(.Row("Metadata", "Seasons", Value = 4),
                .Row("Landings", "F1", 2000, Value = 1, Season = 1),
                .Row("Landings", "F1", 2001, Value = 2, Season = 3),
                .Row("Landings", "F1", Steps[8], Value = 3),
                .Row("LandingsAtAge", "F1", 2000, Class = 0.25, Value = 4, Season = 2))
  dat <- ReadDataCSV(.WriteRows(rows), silent = TRUE)
  expect_equal(Years(dat), Steps)
  expect_equal(Seasons(dat), 4)
  expect_equal(dat@YearLH, 2001)
  expect_equal(unname(Value(Landings(dat))[, 1]), c(1, NA, NA, NA, NA, NA, 2, 3))
  expect_equal(unname(Value(LandingsAtAge(dat))[, 1, 1]), c(NA, 4, rep(NA, 6)))

  expect_error(ReadDataCSV(.WriteRows(rbind(rows, .Row("Landings", "F1", 2000, Season = 5))), silent = TRUE),
               "not a time step")
  expect_error(ReadDataCSV(.WriteRows(rbind(rows, .Row("Landings", "F1", 2000, Season = 1))), silent = TRUE),
               "Duplicated")
  expect_error(ReadDataCSV(.WriteRows(rows[-1, ]), silent = TRUE), "require")
})

test_that("ReadDataCSV uses one fleet order across catch, effort and composition slots", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeLines(c("Slot,Name,Year,Class,Value",
               "Landings,Trawl,2020,,10", "Landings,Longline,2020,,5", "Landings,Trawl,2021,,11",
               "Effort,Longline,2020,,1", "Effort,Trawl,2021,,2",
               "Discards,Trawl,2020,,0.5",
               "LandingsAtAge,Longline,2021,3,7",
               "CPUE,Index B,2020,,1", "CPUE,Index A,2021,,2"), f)
  d <- ReadDataCSV(f, silent = TRUE)
  for (sl in c("Landings", "Effort", "Discards"))
    expect_equal(dimnames(Value(slot(d, sl)))$Fleet, c("Trawl", "Longline"), info = sl)
  expect_equal(dimnames(Value(d@LandingsAtAge))$Fleet, c("Trawl", "Longline"))
  expect_true(all(is.na(Value(d@Discards)[, "Longline"])))
  expect_true(all(is.na(Value(d@LandingsAtAge)[, "Trawl", ])))
  expect_equal(Value(d@Effort)["2020", "Longline"], 1)
  expect_equal(dimnames(Value(d@CPUE))$Fleet, c("Index B", "Index A"))
})
