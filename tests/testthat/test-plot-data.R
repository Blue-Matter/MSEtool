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

test_that("survey compositions are plotted, titled by a single survey, and added by PlotData", {
  D <- .PlotTestData()
  n <- length(D@Years)
  SAA <- array(c(rep(1, n), rep(2, n), rep(3, n)), c(n, 1, 3),
               list(Year = as.character(D@Years), Fleet = 'S1', Age = 0:2))
  D@SurveyAtAge <- CompData(Name = 'S1', Value = SAA, Classes = 0:2, Units = 'years')

  p <- PlotSurveyAtAge(D)
  expect_equal(p$labels$title, 'Survey at Age: S1')
  expect_equal(p$labels$y, 'Age (years)')
  expect_equal(p$data$Proportion[p$data$Year == 2001 & p$data$Age == 2], 0.5)
  expect_message(r <- PlotSurveyAtSize(D), "No .*SurveyAtSize")
  expect_null(r)
  expect_equal(PlotSurveyAtAge(list(Complex = D))$labels$title, 'Survey at Age: S1')

  comp   <- PlotData(D)[[2]]
  titles <- purrr::map_chr(seq_along(comp), \(i) comp[[i]]$labels$title %||% NA_character_)
  expect_true('Survey at Age: S1' %in% titles)
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

.StockTestData <- function() {
  Years <- 2001:2005
  n     <- length(Years)
  catch <- function(scale) {
    v <- array(scale * seq_len(n * 2), c(n, 2), list(Year = Years, Fleet = c('F1', 'F2')))
    CatchData(Name = c('F1', 'F2'), Value = v, CV = array(0.2, dim(v), dimnames(v)), Units = 't')
  }
  index <- function(scale) {
    v <- array(scale * seq_len(n), c(n, 1), list(Year = Years, Fleet = 'S1'))
    IndicesData(Name = 'S1', Value = v)
  }
  Data(Years = Years, Landings = list(Female = catch(2), Male = catch(1)),
       Survey = list(Female = index(2), Male = index(1)))
}

.LineData <- function(p)
  ggplot2::layer_data(p, which(purrr::map_lgl(p$layers, \(l) inherits(l$geom, 'GeomLine')))[1])

test_that("stock elements are drawn as a line per stock, faceted by fleet", {
  D <- .StockTestData()
  p <- PlotLandings(D)
  expect_match(rlang::as_label(p$mapping$colour), 'Stock')
  expect_s3_class(p$facet, 'FacetWrap')
  ld <- .LineData(p)
  expect_equal(nrow(ld), 5 * 2 * 2)
  expect_equal(max(ld$y), 2 * 10)

  summed <- .LineData(PlotLandings(D, byFleet = FALSE))
  expect_equal(max(summed$y), 2 * (5 + 10))

  expect_match(rlang::as_label(PlotLandings(D, byStock = TRUE)$mapping$colour), 'Fleet')
  combined <- PlotLandings(D, byStock = 'sum')
  expect_equal(max(.LineData(combined)$y), 3 * 10)
  expect_match(rlang::as_label(PlotSurvey(D)$mapping$colour), 'Stock')
  expect_s3_class(PlotData(D), 'patchwork')
  expect_true(inherits(PlotRemovals(D), 'ggplot'))
})

test_that("year axis breaks are whole years", {
  expect_equal(.YearBreaks(c(2001, 2010)), seq(2002, 2010, by = 2))
  expect_equal(.YearBreaks(c(1937, 2023)), seq(1940, 2020, by = 20))
  expect_true(all(.YearBreaks(c(2001, 2003.75)) %% 1 == 0))
  b <- ggplot2::ggplot_build(PlotLandings(.PlotTestData()))$layout$panel_params[[1]]$x$breaks
  expect_true(all(stats::na.omit(b) %% 1 == 0))
})

test_that("fleets with no non-zero values are not plotted", {
  D <- .PlotTestData()
  D@Landings@Value[, 'F2'] <- 0
  expect_equal(unique(as.character(.LineData(PlotLandings(D))$group)), '-1')
  D@Landings@Value[] <- 0
  expect_true(inherits(PlotLandings(D), 'ggplot'))
})

test_that("compositions of several stocks and fleets use a fleet x stock grid", {
  Years <- 2001:2003
  comp <- \(scale) CompData(Name = c('F1', 'F2'), Units = 'years', Classes = 0:2,
                            Value = array(scale, c(3, 2, 3), list(Year = Years, Fleet = c('F1', 'F2'), Age = 0:2)))
  D <- Data(Years = Years, LandingsAtAge = list(Female = comp(1), Male = comp(2)))
  p <- PlotLandingsAtAge(D)
  expect_s3_class(p$facet, 'FacetGrid')
  expect_equal(names(p$facet$params$rows), 'Fleet')
  expect_equal(names(p$facet$params$cols), 'Stock')
  expect_equal(.WrapStripLabel('Bottom_Trawl_Landings'), 'Bottom_Trawl_\nLandings')
})
