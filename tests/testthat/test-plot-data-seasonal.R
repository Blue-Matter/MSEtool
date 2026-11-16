# 4 seasons; 2020-2022 complete, 2023 only two of four seasons
.SeasonalPlotTestData <- function() {
  Seasons <- 4
  Years   <- c(rep(2020:2022, each = Seasons) + (0:3) / Seasons, 2023, 2023.25)
  nTS     <- length(Years)
  rn      <- list(Year = as.character(Years), Fleet = c('F1', 'F2'))

  Land <- array(c(seq_len(nTS), 10 * seq_len(nTS)), c(nTS, 2), rn)

  Season <- rep_len(1:4, nTS)
  Surv <- array(NA_real_, c(nTS, 1), list(Year = rn$Year, Fleet = 'S1'))
  Surv[, 1] <- Season

  Ages <- 0:2
  CAA  <- array(1, c(nTS, 2, length(Ages)),
                list(Year = rn$Year, Fleet = rn$Fleet, Age = Ages))

  Data(Years = Years, YearLH = 2023, Seasons = Seasons,
       Landings      = CatchData(Name = rn$Fleet, Value = Land),
       Survey        = IndicesData(Name = 'S1', Value = Surv),
       LandingsAtAge = CompData(Name = rn$Fleet, Value = CAA, Classes = Ages, Units = 'years'))
}

test_that("seasonal data are aggregated to calendar years by default, with a note", {
  D <- .SeasonalPlotTestData()

  expect_message(p <- PlotLandings(D), "aggregated to calendar years")
  expect_message(PlotLandings(D), "AggregateYear = FALSE")
  expect_message(PlotLandings(D), "Incomplete year 2023 omitted")

  expect_setequal(unique(p$data$Year), 2020:2022)
  # F1 landings are 1..nTS: 2020 = 1+2+3+4
  expect_equal(p$data$Value[p$data$Year == 2020 & p$data$Fleet == 'F1'], 10)
})

test_that("AggregateYear = FALSE plots seasonal time steps without a note", {
  D <- .SeasonalPlotTestData()
  expect_no_message(p <- PlotLandings(D, AggregateYear = FALSE))
  expect_equal(length(unique(p$data$Year)), length(D@Years))
})

test_that("AggregateYear = TRUE aggregates without a note", {
  D <- .SeasonalPlotTestData()
  expect_no_message(p <- PlotLandings(D, AggregateYear = TRUE))
  expect_setequal(unique(p$data$Year), 2020:2022)
})

test_that("indices are averaged over all seasons, or over Season", {
  D <- .SeasonalPlotTestData()

  p <- suppressMessages(PlotSurvey(D))
  expect_equal(unique(p$data$Value), mean(1:4))

  expect_message(p3 <- PlotSurvey(D, Season = 3), "averaged over season 3")
  expect_equal(unique(p3$data$Value), 3)

  p24 <- suppressMessages(PlotSurvey(D, Season = c(2, 4)))
  expect_equal(unique(p24$data$Value), 3)

  # unaggregated: Season filters the seasonal time steps
  pf <- PlotSurvey(D, Season = 2, AggregateYear = FALSE)
  expect_equal(unique(pf$data$Value), 2)
})

test_that("PlotData prints the seasonal note once and aggregates every panel", {
  D <- .SeasonalPlotTestData()
  msgs <- testthat::capture_messages(p <- PlotData(D))
  expect_equal(sum(grepl("aggregated to calendar years", msgs)), 1)
  expect_setequal(unique(p[[1]]$data$Year), 2020:2022)

  expect_no_message(PlotData(D, AggregateYear = FALSE))
})

test_that("composition plots sum seasonal counts to calendar years", {
  D <- .SeasonalPlotTestData()

  expect_message(p <- PlotLandingsAtAge(D), "aggregated to calendar years")
  expect_setequal(unique(p$data$Year), 2020:2022)
  expect_equal(unique(p$data$Proportion), 1 / 3)

  expect_no_message(ps <- PlotLandingsAtAge(D, AggregateYear = FALSE))
  expect_equal(length(unique(ps$data$Year)), length(D@Years))
})

test_that("no seasonal note for non-seasonal data or unpopulated slots", {
  D <- .SeasonalPlotTestData()
  # CPUE isn't populated: only the 'nothing to plot' message
  msgs <- testthat::capture_messages(PlotCPUE(D))
  expect_false(any(grepl("aggregated", msgs)))
  msgs <- testthat::capture_messages(PlotDiscardsAtAge(D))
  expect_false(any(grepl("aggregated", msgs)))

  A <- AnnualData(D)
  expect_no_message(PlotLandings(A))
})

test_that("invalid Season or AggregateYear is an error", {
  D <- .SeasonalPlotTestData()
  expect_error(PlotSurvey(D, Season = 5), "Season")
  expect_error(PlotLandings(D, AggregateYear = 'yes'), "AggregateYear")
})

# 3 catch fleets, 2 effort fleets, 2 CPUE indices (every season, Line 10x
# Trawl), 2 surveys each observed in a single season (Spring 2, Autumn 4)
.SeasonalMultiFleetData <- function(IndexDim = 'Fleet') {
  Seasons <- 4
  Years   <- rep(2020:2022, each = Seasons) + (0:3) / Seasons
  nTS <- length(Years); S <- rep_len(1:4, nTS); yr <- as.character(Years)
  dn  <- function(nms, dim = 'Fleet') stats::setNames(list(yr, nms), c('Year', dim))

  Land <- array(rep(c(1, 10, 100), each = nTS), c(nTS, 3), dn(c('Trawl', 'Line', 'Trap')))
  Eff  <- array(rep(c(2, 3), each = nTS), c(nTS, 2), dn(c('Trawl', 'Line')))
  CPUE <- array(c(S, 10 * S), c(nTS, 2), dn(c('Trawl', 'Line'), IndexDim))
  Surv <- array(NA_real_, c(nTS, 2), dn(c('Spring', 'Autumn'), IndexDim))
  Surv[S == 2, 1] <- 5
  Surv[S == 4, 2] <- 7

  Data(Years = Years, YearLH = 2022, Seasons = Seasons,
       Landings = CatchData(Name = c('Trawl', 'Line', 'Trap'), Value = Land, Units = 't'),
       Effort   = EffortData(Name = c('Trawl', 'Line'), Value = Eff),
       CPUE     = IndicesData(Name = c('Trawl', 'Line'), Value = CPUE, Units = 'kg/hr'),
       Survey   = IndicesData(Name = c('Spring', 'Autumn'), Value = Surv))
}

.ValueOf <- function(p, fleet) unique(p$data$Value[p$data$Fleet == fleet])

test_that("multi-fleet seasonal data aggregate per fleet and per index", {
  D <- .SeasonalMultiFleetData()
  p <- suppressMessages(PlotData(D))
  expect_equal(.ValueOf(p[[1]], 'Trap'), 400)    # landings summed
  expect_equal(.ValueOf(p[[2]], 'Line'), 12)     # effort summed
  expect_equal(.ValueOf(p[[3]], 'Line'), 25)     # CPUE averaged over seasons
  expect_equal(.ValueOf(p[[4]], 'Spring'), 5)
  expect_equal(.ValueOf(p[[4]], 'Autumn'), 7)
})

test_that("an index observed in one season uses that season, even with a global Season", {
  D <- .SeasonalMultiFleetData()

  msgs <- testthat::capture_messages(p <- PlotData(D, Season = 2))
  expect_true(any(grepl("single season use that season", msgs)))
  expect_true(any(grepl("Survey Autumn (season 4)", msgs, fixed = TRUE)))
  expect_false(any(grepl("No observations", msgs)))
  expect_equal(.ValueOf(p[[4]], 'Autumn'), 7)    # not blanked by Season = 2
  expect_equal(.ValueOf(p[[4]], 'Spring'), 5)
  expect_equal(.ValueOf(p[[3]], 'Trawl'), 2)     # multi-season index uses Season

  # unaggregated: each survey keeps its own season's time steps, one per year
  ps <- PlotSurvey(D, AggregateYear = FALSE, Season = 1)
  expect_equal(sort(ps$data$Year[ps$data$Fleet == 'Autumn']), 2020:2022 + 0.75)
  expect_equal(sort(ps$data$Year[ps$data$Fleet == 'Spring']), 2020:2022 + 0.25)
})

test_that("a season set for one index is respected, with a message if it's empty", {
  D <- .SeasonalMultiFleetData()

  msgs <- testthat::capture_messages(p <- PlotSurvey(D, Season = list(Autumn = 1)))
  expect_true(any(grepl("No observations in the selected season", msgs)))
  expect_true(any(grepl("Survey Autumn (observed in season 4)", msgs, fixed = TRUE)))
  expect_true(all(is.na(p$data$Value[p$data$Fleet == 'Autumn'])))
  expect_equal(.ValueOf(p, 'Spring'), 5)

  p2 <- suppressMessages(PlotData(D, Season = list(CPUE = list(Line = 3))))
  expect_equal(.ValueOf(p2[[3]], 'Line'), 30)
  expect_equal(.ValueOf(p2[[3]], 'Trawl'), 2.5)

  expect_error(PlotSurvey(D, Season = list(Winter = 1)), "not found")
  expect_error(PlotData(D, Season = list(Spring = 1)), "named")
})

test_that("byFleet = FALSE sums catch and effort but never indices", {
  D <- .SeasonalMultiFleetData()
  p <- suppressMessages(PlotData(D, byFleet = FALSE))
  expect_equal(.ValueOf(p[[1]], 'Total'), 444)
  expect_setequal(unique(as.character(p[[3]]$data$Fleet)), c('Trawl', 'Line'))
  expect_setequal(unique(as.character(p[[4]]$data$Fleet)), c('Spring', 'Autumn'))
  expect_equal(p[[4]]$labels$colour, 'Index')

  ps <- suppressMessages(PlotCPUE(D, byFleet = FALSE))
  expect_setequal(unique(as.character(ps$data$Fleet)), c('Trawl', 'Line'))
})

test_that("a fleet has the same color in every PlotData panel", {
  D <- .SeasonalMultiFleetData()
  p <- suppressMessages(PlotData(D))
  colorOf <- function(pp, fleet) {
    ld <- ggplot2::layer_data(pp, 1)
    unique(ld$colour[levels(pp$data$Fleet)[ld$group] == fleet])
  }
  expect_equal(colorOf(p[[1]], 'Line'), colorOf(p[[2]], 'Line'))
  expect_equal(colorOf(p[[1]], 'Line'), colorOf(p[[3]], 'Line'))
  expect_false(colorOf(p[[4]], 'Spring') %in% c(colorOf(p[[1]], 'Trawl'), colorOf(p[[1]], 'Line')))
})

test_that("indices with an `Index` dimension are drawn as separate series", {
  D <- .SeasonalMultiFleetData(IndexDim = 'Index')
  p <- suppressMessages(PlotSurvey(D))
  expect_setequal(unique(as.character(p$data$Fleet)), c('Spring', 'Autumn'))
  expect_equal(.ValueOf(p, 'Autumn'), 7)
})
