# Annual data: 2 catch fleets with CVs, 1 survey with a CV, age and size
# compositions
.PlotTestData <- function(LandCV = 0.2, SurvCV = 0.3) {
  Years <- 2001:2005
  n     <- length(Years)
  dn    <- list(Year = as.character(Years), Fleet = c('F1', 'F2'))
  Land  <- array(rep(c(1, 10), each = n), c(n, 2), dn)
  Disc  <- array(rep(c(2, 2), each = n), c(n, 2), dn)
  Surv  <- array(seq_len(n), c(n, 1), list(Year = dn$Year, Fleet = 'S1'))
  cv    <- function(x, v) if (!is.null(v)) array(v, dim(x), dimnames(x))

  Ages <- 0:2
  CAA  <- array(1, c(n, 2, 3), list(Year = dn$Year, Fleet = dn$Fleet, Age = Ages))
  Lens <- c(10, 20)
  CAL  <- array(c(rep(1, n * 2), rep(3, n * 2)), c(n, 2, 2),
                list(Year = dn$Year, Fleet = dn$Fleet, Class = 1:2))

  Data(Years = Years,
       Landings = CatchData(Name = dn$Fleet, Value = Land, CV = cv(Land, LandCV), Units = 't'),
       Discards = CatchData(Name = dn$Fleet, Value = Disc, CV = cv(Disc, LandCV), Units = 't'),
       Survey   = IndicesData(Name = 'S1', Value = Surv, CV = cv(Surv, SurvCV)),
       LandingsAtAge  = CompData(Name = dn$Fleet, Value = CAA, Classes = Ages, Units = 'years'),
       LandingsAtSize = CompData(Name = dn$Fleet, Value = CAL,
                                 Classes = list(F1 = Lens, F2 = Lens), Units = 'cm'))
}

.Bounds <- function(Value, CV) {
  sigma <- sqrt(log(1 + CV^2))
  c(Value * exp(-1.96 * sigma), Value * exp(1.96 * sigma))
}

.RibbonData <- function(p) {
  i <- which(purrr::map_lgl(p$layers, \(l) inherits(l$geom, 'GeomRibbon')))
  if (!length(i)) return(NULL)
  ggplot2::layer_data(p, i[1])
}

test_that("timeseries panels show a lognormal 95% band from the CV", {
  D <- .PlotTestData()
  p <- PlotSurvey(D)
  rd <- .RibbonData(p)
  expect_false(is.null(rd))
  expect_equal(c(rd$ymin[rd$x == 2003], rd$ymax[rd$x == 2003]), .Bounds(3, 0.3))

  expect_null(.RibbonData(PlotSurvey(D, IncCV = FALSE)))
  expect_null(.RibbonData(PlotSurvey(.PlotTestData(SurvCV = NULL))))
  # all-zero CVs draw no band
  expect_null(.RibbonData(PlotSurvey(.PlotTestData(SurvCV = 0))))
})

test_that("summed fleets and removals combine CVs assuming independence", {
  D <- .PlotTestData()

  p  <- PlotLandings(D, byFleet = FALSE)
  rd <- .RibbonData(p)
  cvTot <- sqrt((0.2 * 1)^2 + (0.2 * 10)^2) / 11
  expect_equal(c(rd$ymin[1], rd$ymax[1]), .Bounds(11, cvTot))

  p  <- PlotRemovals(D)
  rd <- .RibbonData(p)
  # F1 removals: landings 1 + discards 2
  cvF1 <- sqrt((0.2 * 1)^2 + (0.2 * 2)^2) / 3
  f1   <- rd[rd$group == 1, ]
  expect_equal(c(f1$ymin[1], f1$ymax[1]), .Bounds(3, cvF1))
})

test_that("an isolated observation gets an error bar, kept out of the legend", {
  D <- .PlotTestData()
  D@Survey@Value[c(2, 4), 1] <- NA
  p <- PlotSurvey(D)
  i <- which(purrr::map_lgl(p$layers, \(l) inherits(l$geom, 'GeomLinerange')))
  expect_length(i, 1)
  expect_false(p$layers[[i]]$show.legend)
  ld <- ggplot2::layer_data(p, i)
  expect_true(2003 %in% ld$x)
})

test_that("PlotData adds composition panels below the timeseries", {
  D <- .PlotTestData()
  p <- PlotData(D)
  expect_s3_class(p, 'patchwork')
  # nested: timeseries block, then the composition block
  comp <- p[[2]]
  expect_s3_class(comp, 'patchwork')
  titles <- purrr::map_chr(seq_along(comp), \(i) comp[[i]]$labels$title %||% NA_character_)
  expect_setequal(titles, c('Landings at Age', 'Landings at Size'))

  pt <- PlotData(D, IncComp = FALSE)
  expect_false(any(purrr::map_lgl(seq_along(pt), \(i) identical(pt[[i]]$labels$title, 'Landings at Age'))))
})

test_that("PlotData omits all-zero compositions and works with only compositions", {
  D <- .PlotTestData()
  D@LandingsAtSize@Value[] <- 0
  p <- PlotData(D)
  expect_equal(p[[2]]$labels$title, 'Landings at Age')

  Comp <- Data(Years = 2001:2005, LandingsAtAge = D@LandingsAtAge)
  pc <- PlotData(Comp)
  expect_equal(pc$labels$title, 'Landings at Age')
})

test_that("composition plots: units label, title, and empty or all-zero slots", {
  D <- .PlotTestData()
  p <- PlotLandingsAtAge(D)
  expect_equal(p$labels$y, 'Age (years)')
  expect_equal(p$labels$title, 'Landings at Age')
  expect_equal(PlotLandingsAtSize(D)$labels$y, 'Size Class (cm)')

  expect_message(r <- PlotDiscardsAtAge(D), "No .*DiscardsAtAge")
  expect_null(r)

  D@LandingsAtAge@Value[] <- 0
  expect_message(r <- PlotLandingsAtAge(D), "zero or missing")
  expect_null(r)
})

test_that("composition years with no sampled catch are dropped", {
  D <- .PlotTestData()
  D@LandingsAtAge@Value["2002", "F1", ] <- 0
  p <- PlotLandingsAtAge(D)
  expect_false(any(p$data$Year == 2002 & p$data$Fleet == 'F1'))
  expect_true(any(p$data$Year == 2002 & p$data$Fleet == 'F2'))
})

test_that("PlotData shows size compositions by fleet when grids differ and byFleet = FALSE", {
  D <- .PlotTestData()
  D@LandingsAtSize@Classes <- list(F1 = c(10, 20), F2 = c(15, 25))
  expect_error(PlotLandingsAtSize(D, byFleet = FALSE), "size-class grid")
  expect_message(p <- PlotData(D, byFleet = FALSE), "shown separately")
  size <- purrr::keep(seq_along(p[[2]]), \(i) identical(p[[2]][[i]]$labels$title, 'Landings at Size'))
  expect_setequal(unique(as.character(p[[2]][[size]]$data$Fleet)), c('F1', 'F2'))
})

test_that(".YearLabels labels decimal years by month", {
  expect_equal(.YearLabels(c(2020, 2020.25, 2020.5, 2020.75, NA)),
               c('2020', '2020-04', '2020-07', '2020-10', NA))
})
