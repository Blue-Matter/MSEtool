# OM@Control$DataOM resolution (R/helpers-mp.R) and Data@Misc$DataOM
# population (.AddPopDyn(), R/calc-advice.R). See ?OMControl.

MakeMP <- function(DataOM = NULL) {
  f <- function(Data) Advice()
  class(f) <- 'mp'
  attr(f, 'DataOM') <- DataOM
  f
}

test_that(".ResolveDataOM() accepts TRUE, character, and named logical forms", {
  expect_null(.ResolveDataOM(NULL, 'MP', MakeMP()))
  expect_null(.ResolveDataOM(FALSE, 'MP', MakeMP()))
  expect_true(.ResolveDataOM(TRUE, 'MP', MakeMP()))
  expect_setequal(.ResolveDataOM(c('Biomass', 'Number'), 'MP', MakeMP()), c('Biomass', 'Number'))
  expect_setequal(.ResolveDataOM(list(Biomass = TRUE, Number = TRUE), 'MP', MakeMP()),
                  c('Biomass', 'Number'))
  expect_equal(.ResolveDataOM(list(Biomass = TRUE, Number = FALSE), 'MP', MakeMP()), 'Biomass')
  expect_equal(.ResolveDataOM(c(Biomass = TRUE), 'MP', MakeMP()), 'Biomass')
})

test_that(".ResolveDataOM() unions the OM-wide default with the MP attribute", {
  expect_equal(.ResolveDataOM(list(Reference = TRUE), 'myMP', MakeMP()), 'Reference')
  expect_setequal(.ResolveDataOM(list(Reference = TRUE), 'myMP', MakeMP('Biomass')),
                  c('Reference', 'Biomass'))
  expect_setequal(.ResolveDataOM('Biomass', 'refFMSY', refFMSY), c('Biomass', 'Reference'))
  expect_true(.ResolveDataOM(TRUE, 'refFMSY', refFMSY))
  expect_true(.ResolveDataOM('Biomass', 'MP', MakeMP(TRUE)))
  expect_equal(.ResolveDataOM(NULL, 'refFMSY', refFMSY), 'Reference')
})

test_that(".ResolveDataOM() per-MP entries replace the default and attribute", {
  Ctl <- list(Reference = TRUE, myMP = c('Biomass', 'Number'), MP2 = FALSE)
  expect_setequal(.ResolveDataOM(Ctl, 'myMP', MakeMP('Landings')), c('Biomass', 'Number'))
  expect_null(.ResolveDataOM(Ctl, 'MP2', refFMSY))
  expect_equal(.ResolveDataOM(Ctl, 'Other', MakeMP()), 'Reference')
  # historical data: OM-wide default only
  expect_equal(.ResolveDataOM(Ctl), 'Reference')
  expect_null(.ResolveDataOM(list(myMP = TRUE)))
})

test_that(".ResolveDataOM() rejects malformed values", {
  expect_error(.ResolveDataOM(list(TRUE, 'Biomass'), 'MP', MakeMP()), 'must be named')
  expect_error(.ResolveDataOM(1, 'MP', MakeMP()), 'Invalid')
})

test_that("unknown OM@Control names and DataOM slots are reported", {
  Msgs <- testthat::capture_messages(.CheckOMControl(list(Reference = TRUE)))
  expect_length(Msgs, 2)
  expect_match(Msgs[1], 'not a recognised')
  expect_match(Msgs[2], 'DataOM')
  expect_no_message(.CheckOMControl(list(MSYType = 'Landings', DataOM = list(Reference = TRUE))))
  expect_message(.CheckDataOM(c('Biomass', 'Biomas')), 'Biomas')
  expect_message(.CheckDataOM(list(Reference = TRUE, typoMP = TRUE), list(myMP = MakeMP())),
                 'typoMP')
  expect_message(.CheckDataOM(NULL, list(myMP = MakeMP('Bogus'))), 'Bogus')
  expect_no_message(.CheckDataOM(list(Reference = TRUE, myMP = TRUE), list(myMP = MakeMP())))
})

test_that(".DropYearsFrom() drops time steps at or after Year from nested arrays", {
  a <- array(1:12, c(2, 6), dimnames = list(Sim = 1:2, Year = 2020:2025))
  out <- .DropYearsFrom(list(A = a, B = 'x'), 2023)
  expect_equal(dimnames(out$A)$Year, as.character(2020:2022))
  expect_equal(out$B, 'x')
  expect_identical(.DropYearsFrom(a, 2030), a)
})

test_that("DataOM reaches MPs in projections, trimmed to past time steps", {
  skip_on_cran()

  OM <- SingleStockOM
  OM@nSim <- 3
  Control(OM)$DataOM <- list(Reference = TRUE)
  Hist <- Simulate(OM, silent = TRUE)

  # historical Data carries the OM-wide default only
  HistDataOM <- Hist@Data[[1]][[1]]@Misc$DataOM
  expect_s4_class(HistDataOM, 'hist')
  expect_true(length(HistDataOM@Reference@MSY@FMSY) > 0)
  expect_length(HistDataOM@Biomass, 0)

  myMP <- function(Data, tunepar = 1) {
    a <- refFMSY(Data)
    Effort(a) <- tunepar * Effort(a)
    a
  }
  class(myMP) <- 'mp'
  attr(myMP, 'Interval') <- 1

  # MPs are re-homed by .MakeSelfContained(), so record calls via globalenv
  Calls <- new.env()
  Calls$x <- list()
  assign('.DataOMCalls', Calls, envir = globalenv())
  on.exit(rm('.DataOMCalls', envir = globalenv()), add = TRUE)
  probe <- function(Data) {
    d <- Data@Misc$DataOM
    Calls <- get('.DataOMCalls', envir = globalenv())
    Calls$x[[length(Calls$x) + 1]] <- list(
      AdviceYear = as.numeric(Data@Misc$AdviceYear),
      Ref        = length(d@Reference@MSY@FMSY) > 0,
      BYears     = as.numeric(dimnames(d@Biomass)$Year),
      nSimB      = unname(dim(d@Biomass)[1]),
      Number     = length(d@Number)
    )
    Advice()
  }
  class(probe) <- 'mp'
  attr(probe, 'Interval') <- 5
  attr(probe, 'DataOM') <- 'Biomass'

  MSE <- Project(Hist, MPs = list(refFMSY = refFMSY, myMP = myMP, probe = probe),
                 silent = TRUE)

  # myMP gets Reference from the OM-wide default, so it matches refFMSY
  expect_equal(MSE@Effort[, , , 'myMP'], MSE@Effort[, , , 'refFMSY'])

  # probe: union of default + attribute, trimmed to before AdviceYear
  YearsProj <- Years(OM, 'Projection')
  YearsAll  <- c(Years(OM, 'Historical'), YearsProj)
  expect_length(Calls$x, 3 * length(seq(1, length(YearsProj), by = 5)))
  for (cl in Calls$x) {
    expect_true(cl$Ref)
    expect_equal(cl$nSimB, 1)
    expect_equal(max(cl$BYears), max(YearsAll[YearsAll < cl$AdviceYear]))
    expect_equal(cl$Number, 0)
  }
  AdviceYears <- vapply(Calls$x, `[[`, numeric(1), 'AdviceYear')
  expect_gt(length(unique(AdviceYears)), 1)

  # per-MP entry replaces the attribute and default
  Calls$x <- list()
  Control(Hist)$DataOM <- list(Reference = TRUE, probe = 'Number')
  expect_equal(Control(Hist)$DataOM$probe, 'Number')
  Project(Hist, MPs = list(probe = probe), silent = TRUE)
  expect_length(Calls$x, 3 * length(seq(1, length(YearsProj), by = 5)))
  for (cl in Calls$x) {
    expect_false(cl$Ref)
    expect_length(cl$BYears, 0)
    expect_gt(cl$Number, 0)
  }
})
