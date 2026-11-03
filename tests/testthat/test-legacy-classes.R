# Writes the scalar and range slots of a legacy object in the CSV layout read by new()
.WriteLegacyCSV <- function(obj, file) {
  rows <- character()
  for (sl in slotNames(obj)) {
    v <- slot(obj, sl)
    if ((is.numeric(v) || is.character(v)) && length(v) > 0 && is.null(dim(v)))
      rows <- c(rows, paste(c(sl, v), collapse = ","))
  }
  writeLines(rows, file)
}

test_that("Data objects are read from a CSV file", {
  Data <- suppressMessages(new("Data", system.file("Data/Cobia.csv", package = "MSEtool")))
  expect_s4_class(Data, "Data")
  expect_identical(Data@Name, "Cobia")
  expect_equal(ncol(Data@Cat), length(Data@Year))
  expect_equal(Data@Cat, MSEtool::Cobia@Cat, ignore_attr = TRUE)
  expect_equal(Data@Ind, MSEtool::Cobia@Ind, ignore_attr = TRUE)

  empty <- new("Data")
  expect_s4_class(empty, "Data")
  expect_equal(empty@CV_Cat, matrix(0.2), ignore_attr = TRUE)
})

test_that("Stock, Fleet, Obs and Imp objects round-trip through CSV", {
  file <- withr::local_tempfile(fileext = ".csv")
  for (obj in list(Albacore, Generic_Fleet, Generic_Obs, Perfect_Imp)) {
    .WriteLegacyCSV(obj, file)
    new_obj <- new(class(obj), file)
    expect_s4_class(new_obj, class(obj))
    for (sl in slotNames(obj)) {
      v <- slot(obj, sl)
      if (is.numeric(v) && length(v) > 0 && is.null(dim(v)))
        expect_equal(slot(new_obj, sl), v, ignore_attr = TRUE, info = paste(class(obj), sl))
    }
  }
  expect_error(new("Stock", "not_a_file.csv"), "File not found")
})

test_that("OM initialize combines Stock, Fleet, Obs and Imp", {
  OM <- new("OM", Albacore, Generic_Fleet, Generic_Obs, Perfect_Imp, nsim = 5)
  expect_equal(OM@nsim, 5)
  expect_equal(OM@maxage, Albacore@maxage)
  expect_length(OM@cpars, 0)

  OM2 <- new("OM", Albacore, Generic_Fleet, Generic_Obs)
  expect_equal(OM2@TACFrac, Perfect_Imp@TACFrac)
})

test_that("MOM initialize builds a multi-stock operating model", {
  MOM <- new("MOM", Stocks = list(Albacore, Butterfish),
             Fleets = list(list(Generic_Fleet), list(Generic_Fleet)),
             Obs = list(list(Generic_Obs), list(Generic_Obs)),
             Imps = list(list(Perfect_Imp), list(Perfect_Imp)),
             CatchFrac = list(matrix(1, 3, 1), matrix(1, 3, 1)),
             nsim = 3, proyears = 5, interval = 1)
  expect_s4_class(MOM, "MOM")
  expect_length(MOM@Stocks, 2)
  expect_equal(MOM@nsim, 3)
})

test_that("show methods print legacy objects", {
  skip_on_cran()
  for (obj in list(.LegacyOM(), .LegacyHist(), .LegacyMSE(), MSEtool::SimulatedData))
    expect_no_error(suppressMessages(capture.output(show(obj))))
  expect_output(show(PNOF(.LegacyMSE())), "Probability")
  expect_output(show(.LegacyRec(TAC = 1:3)), "TAC")
})

test_that("MPtype classifies MPs by management type", {
  out <- MPtype(c("FMSYref", "curEref", "NFref"))
  expect_s3_class(out, "data.frame")
  expect_identical(out$MP, c("FMSYref", "curEref", "NFref"))
  expect_true(all(out$Type == "Reference"))
  expect_identical(out$Recs, c("TAE", "TAE", "TAC"))
})

test_that("avail finds MPs and PMs", {
  expect_true(all(c("FMSYref", "curEref", "NFref") %in% avail("MP", "MSEtool", msg = FALSE)))
  expect_true(all(c("PNOF", "P10", "AAVY", "LTY") %in% avail("PM", "MSEtool", msg = FALSE)))
})

test_that("Dom returns all MPs when none is dominated", {
  skip_on_cran()
  dom <- Dom(.LegacyMSE(), "PNOF", "LTY")
  expect_identical(dom$MPs, sort(.LegacyMSE()@MPs))
  expect_equal(nrow(dom$DomMPs), 0)
})
