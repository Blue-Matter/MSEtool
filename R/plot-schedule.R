#' Plot Biological and Gear Schedules
#'
#' Line plots of an age-based schedule (selectivity, retention, discard
#' mortality, length, weight, maturity, natural mortality, fecundity) against
#' age, or (via `x = "Length"`) against length instead. By default, each
#' independent curve (the historical schedule, and each MP's projection
#' schedule for [mse-class] objects) is drawn for its own last available
#' year only; pass `Years` to see more.
#'
#' `PlotSelectivity()`, `PlotRetention()`, and `PlotDiscardMortality()` plot
#' fleet-level gear curves (`[Fleet()]`'s `Selectivity`/`Retention`/
#' `DiscardMortality` sub-objects). For the projection period of
#' [mse-class] objects, these reflect any change made by the MP via
#' `Advice()` (see [VBiomass()], which resolves the same effective curves).
#'
#' `PlotLength()`, `PlotWeight()`, `PlotMaturity()`, `PlotNaturalMortality()`,
#' and `PlotFecundity()` plot stock-level biological schedules. 
#'
#' @param object An [om-class], [hist-class], or [mse-class] object (`om`
#'   objects are populated via [PopulateOM()] if not already), or -- for
#'   `PlotLength()`/`PlotWeight()`/`PlotMaturity()`/`PlotNaturalMortality()`/
#'   `PlotFecundity()` only -- a [stock-class] object.
#' @param Sim Integer or `NULL` (default). Which simulation replicate to
#'   plot. `NULL` takes the median across all simulations, cell by cell, and
#'   adds a `probs` quantile ribbon behind it (unless `nSim == 1` or values 
#'   constant across simulations.
#' @param probs Numeric vector of length 2. Lower and upper quantiles of the
#'   across-simulation ribbon drawn when `Sim = NULL`. Default `c(0.05, 0.95)`.
#'   Ignored when `Sim` is a specific replicate.
#' @param byStock,byFleet One of `TRUE`, `FALSE`, or `NULL` (default,
#'   facets automatically when `object` has more than one stock/fleet).
#'   These plots never sum or average a curve across stocks/fleets -- that
#'   isn't a meaningful operation the way summing catch is -- so every
#'   stock/fleet is always distinguishable somehow. `TRUE`/`NULL` facets by
#'   `Stock`/`Fleet`. `FALSE` instead colors by that
#'   variable; 
#' @param Years Optional numeric vector, or `"all"`. Default `NULL`: each
#'   independent curve (`Historical`, and each MP's projection for
#'   `mse` objects) is trimmed to its own last full calendar year (every
#'   season-slice sharing that curve's most recent year, for a seasonal
#'   `Seasons > 1` OM; just the one latest year otherwise), colored and
#'   labelled by year in the legend. If the curve never changes value
#'   over its full year range, a single (arbitrary) year is
#'   drawn and the legend is dropped entirely. 
#'   `"all"` plots every available year. Both `NULL` and `"all"` further
#'   collapse consecutive years with an identical curve down
#'   to just the breakpoint years where the curve actually changed. 
#'   A numeric vector restricts
#'   to exactly those years with no compression, always with the legend
#'   shown.
#' @param units Logical or a character unit string. `TRUE` (default) labels
#'   the x-axis with the stock's `Ages@Units` (e.g. `"Age (year)"`) when it's
#'   set and agrees across every plotted stock, and additionally, for
#'   `PlotLength()`/`PlotWeight()`, labels the y-axis with `Length@Units`/
#'   `Weight@Units` (and appends the unit for `PlotNaturalMortality()`/
#'   `PlotFecundity()`). `FALSE` suppresses all unit labelling (axes fall
#'   back to plain `"Age"`/`"Length"`/etc.). For `PlotLength()`/
#'   `PlotWeight()` only, a character string (a length unit -- `"mm"`,
#'   `"cm"`, `"inch"`, `"m"` -- or mass unit -- `"g"`, `"kg"`, `"lb"`,
#'   `"t"`, etc.) both relabels the y-axis and rescales the plotted
#'   values into that unit; requesting a unit that can't be converted to is
#'   an error. Has no effect on `PlotMaturity()`/`PlotSelectivity()`/
#'   `PlotRetention()`/`PlotDiscardMortality()` beyond the x-axis label,
#'   since those are unitless proportions.
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param x `"Age"` (default) or `"Length"`. `"Length"` projects the curve
#'   through the stock's `[Length()]` age-length key (`@ALK`) onto a length
#'   axis instead -- e.g. `PlotWeight(x = "Length")` gives the weight-length
#'   relationship, `PlotMaturity(x = "Length")` gives maturity-at-length.
#'   Not meaningful for `PlotLength()` itself (length-at-length is a trivial
#'   identity), which errors if `x = "Length"` is requested. The length axis
#'   is always labelled in the stock's native `Length@Units`, independent of
#'   `units`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' PlotSelectivity(Hist)
#' PlotMaturity(Hist)
#'
#' MSE <- Project(Hist, ExampleMPs())
#' PlotRetention(MSE)
#' PlotWeight(MSE, Sim = 3)
#' }
#'
#' @name plot_schedule
#' @seealso [Fleet()], [Stock()], [VBiomass()]
NULL

#' @rdname plot_schedule
#' @export
PlotSelectivity <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL,
                            Years = NULL, units = TRUE, Stocks = NULL, x = c('Age', 'Length'),
                            probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotGearSchedule(object, 'Selectivity', Sim, byStock, byFleet, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotRetention <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL,
                          Years = NULL, units = TRUE, Stocks = NULL, x = c('Age', 'Length'),
                          probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotGearSchedule(object, 'Retention', Sim, byStock, byFleet, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotDiscardMortality <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL,
                                 Years = NULL, units = TRUE, Stocks = NULL, x = c('Age', 'Length'),
                                 probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotGearSchedule(object, 'DiscardMortality', Sim, byStock, byFleet, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotLength <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                       x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Length', Sim, byStock, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotWeight <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                       x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Weight', Sim, byStock, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotMaturity <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                         x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Maturity', Sim, byStock, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotNaturalMortality <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                                 x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'NaturalMortality', Sim, byStock, Years, units, Stocks, x, probs)
}

#' @rdname plot_schedule
#' @export
PlotFecundity <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                          x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Fecundity', Sim, byStock, Years, units, Stocks, x, probs)
}

# ---- internal helpers ----
.ResolveOM <- function(object) {
  if (inherits(object, 'om')) return(PopulateOM(object, silent = TRUE))
  object@OM
}

# Wraps a Stock in a minimal hist shell (OM@Stock only, using the Stock's
# own nYear/pYear/CurrentYear/Seasons set by PopulateStock()) so the
# existing hist/mse/om-oriented Plot*() machinery can be reused directly on
# a bare Stock -- no Fleet, no Simulate(). Self-contained: an unpopulated
# Stock is populated internally first, with the same illustrative defaults
# PlotStock() uses; an already-populated Stock is used as-is, with whatever
# nSim/Years it already has.
.StockToShellHist <- function(Stock) {
  if (is.null(Stock@Length@MeanAtAge))
    Stock <- PopulateStock(Stock, nYear = 20, pYear = 0, nSim = 5, silent = TRUE)

  ShellOM             <- methods::new('om')
  ShellOM@Stock       <- stats::setNames(list(Stock), Stock@Name)
  ShellOM@nYear       <- Stock@nYear
  ShellOM@pYear       <- Stock@pYear
  ShellOM@CurrentYear <- Stock@CurrentYear
  ShellOM@Seasons     <- Stock@Seasons
  ShellHist           <- methods::new('hist')
  ShellHist@OM        <- ShellOM
  ShellHist
}

.PlotGearSchedule <- function(object, what, Sim, byStock, byFleet, Years, units, Stocks, x = 'Age',
                                probs = c(0.05, 0.95)) {
  .CheckClass(object, c('hist', 'mse', 'om'), 'object')
  OM         <- .ResolveOM(object)
  isMSE      <- inherits(object, 'mse')
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  fleetNames <- FleetNames(OM)

  histYears <- Years(object, 'Historical')
  projYears <- if (isMSE) Years(object, 'Projection') else NULL

  histDF <- .ExtractGearSchedule(object, OM, what, stockNames, fleetNames, histYears, MPName = NULL, x = x) |>
    dplyr::mutate(MP = 'Historical')

  if (isMSE) {
    mpNames <- names(object@MPs)
    projDF <- purrr::map(mpNames, \(mp)
      .ExtractGearSchedule(object, OM, what, stockNames, fleetNames, projYears, MPName = mp, x = x) |>
        dplyr::mutate(MP = mp)
    ) |> dplyr::bind_rows()
    df <- dplyr::bind_rows(histDF, projDF)
  } else {
    df <- histDF
  }

  df <- df |> .FilterYears(Years)

  xlab <- .XAxisLabel(OM, stockNames, units, x)
  .BuildSchedulePlot(df, Sim = Sim, byStock = byStock, byFleet = byFleet, ylab = what, xlab = xlab,
                       defaultYears = is.null(Years), breakpointYears = is.null(Years) || identical(Years, 'all'),
                       probs = probs)
}

.ExtractGearSchedule <- function(object, OM, what, stockNames, fleetNames, Years, MPName, x = 'Age') {
  allStocks <- StockNames(OM)
  purrr::map(seq_along(allStocks), \(st) {
    if (!allStocks[st] %in% stockNames) return(NULL)
    purrr::map(seq_along(fleetNames), \(fl) {
      arr <- .EffectiveGearCurve(object, OM, what, st, fl, MPName, Years, x)
      arr <- arr[, , , 1, drop = FALSE] |> DropDimension('Area')
      Array2DF(arr) |>
        dplyr::mutate(Stock = allStocks[st], Fleet = fleetNames[fl])
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()
}

.PlotStockSchedule <- function(object, what, Sim, byStock, Years, units, Stocks, x = 'Age',
                                 probs = c(0.05, 0.95)) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  if (what == 'Length' && x == 'Length')
    cli::cli_abort('{.arg x = "Length"} is not meaningful for {.fn PlotLength}; use the default {.arg x = "Age"}.')
  OM         <- .ResolveOM(object)
  isMSE      <- inherits(object, 'mse')
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  allStocks  <- StockNames(OM)

  allYears <- Years(object, 'Historical')
  if (isMSE) allYears <- c(allYears, Years(object, 'Projection'))

  df <- purrr::map(seq_along(allStocks), \(st) {
    if (!allStocks[st] %in% stockNames) return(NULL)
    schedObj <- slot(OM@Stock[[st]], what)
    if (x == 'Length') {
      schedObj <- .MeanAtAge2MeanAtLength(schedObj, OM@Stock[[st]]@Length, replace = TRUE, Years = allYears)
      arr      <- schedObj@MeanAtLength |> .SubsetYear(allYears)
      names(dimnames(arr))[names(dimnames(arr)) == 'Class'] <- 'Age'
    } else {
      arr <- schedObj@MeanAtAge |> .SubsetYear(allYears)
    }
    Array2DF(arr) |> dplyr::mutate(Stock = allStocks[st])
  }) |> dplyr::bind_rows() |> .FilterYears(Years)

  ylab   <- what
  factor <- 1
  if (what == 'Length') {
    uinfo  <- .ResolveUnitInfo(.length_units_mm, .GetStockUnits(OM, 'Length', stockNames), 1, units, 'Length')
    ylab   <- .AppendUnits('Length', uinfo$label)
    factor <- uinfo$factor
  } else if (what == 'Weight') {
    uinfo  <- .ResolveUnitInfo(.mass_units_g, .GetStockUnits(OM, 'Weight', stockNames), 1, units, 'Weight')
    ylab   <- .AppendUnits('Weight', uinfo$label)
    factor <- uinfo$factor
  } else if (what == 'NaturalMortality' && !isFALSE(units)) {
    base_unit <- .GetStockUnits(OM, 'NaturalMortality', stockNames)
    if (!is.null(base_unit)) ylab <- paste0('NaturalMortality (per ', base_unit, ')')
  } else if (what == 'Fecundity' && !isFALSE(units)) {
    ylab <- .AppendUnits('Fecundity', .GetStockUnits(OM, 'Fecundity', stockNames))
  }
  df$Value <- df$Value * factor

  xlab <- .XAxisLabel(OM, stockNames, units, x)
  .BuildSchedulePlot(df, Sim = Sim, byStock = byStock, byFleet = FALSE, ylab = ylab, xlab = xlab,
                       defaultYears = is.null(Years), breakpointYears = is.null(Years) || identical(Years, 'all'),
                       probs = probs)
}

.XAxisLabel <- function(OM, stockNames, units, x) {
  if (x == 'Length') {
    if (isFALSE(units)) return('Length')
    return(.AppendUnits('Length', .GetStockUnits(OM, 'Length', stockNames)))
  }
  .AgeAxisLabel(OM, stockNames, units)
}


.AgeAxisLabel <- function(OM, stockNames, units) {
  if (isFALSE(units)) return('Age')
  .AppendUnits('Age', .GetStockUnits(OM, 'Ages', stockNames))
}

.SelectDefaultYears <- function(df) {
  seriesVars <- intersect(c('Stock', 'Fleet', 'MP'), colnames(df))
  keyVars    <- c(seriesVars, 'Age')

  varies <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keyVars))) |>
    dplyr::summarise(varies = length(unique(round(.data$Value, 4))) > 1, .groups = 'drop')

  if (length(seriesVars)) {
    varies <- varies |>
      dplyr::group_by(dplyr::across(dplyr::all_of(seriesVars))) |>
      dplyr::summarise(varies = any(.data$varies), .groups = 'drop')
  }

  if (any(df$Year %% 1 != 0)) {
    grouped <- if (length(seriesVars)) dplyr::group_by(df, dplyr::across(dplyr::all_of(seriesVars))) else df
    df <- grouped |>
      dplyr::filter(floor(.data$Year) == floor(max(.data$Year))) |>
      dplyr::ungroup()
  }

  list(df = df, showLegend = any(varies$varies))
}

.SelectBreakpointYears <- function(df) {
  seriesVars <- intersect(c('Stock', 'Fleet', 'MP'), colnames(df))

  breakpointYears <- function(sub) {
    years <- sort(unique(sub$Year))
    ages  <- sort(unique(sub$Age))
    if (length(years) <= 1) return(years)
    sub <- dplyr::arrange(sub, .data$Year, .data$Age)
    arr <- array(sub$Value, dim = c(length(ages), length(years)),
                 dimnames = list(Age = ages, Year = years))
    years[.UniqueYears(arr)]
  }

  if (length(seriesVars)) {
    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(seriesVars))) |>
      dplyr::group_modify(~ dplyr::filter(.x, .data$Year %in% breakpointYears(.x))) |>
      dplyr::ungroup()
  } else {
    dplyr::filter(df, .data$Year %in% breakpointYears(df))
  }
}

.CapRepresentativeYears <- function(df, maxPoints = 3) {
  seriesVars <- intersect(c('Stock', 'Fleet', 'MP'), colnames(df))

  pickYears <- function(years) {
    years <- sort(unique(years))
    if (any(years %% 1 != 0) || length(years) <= maxPoints)
      return(years)
    first          <- years[1]
    last           <- years[length(years)]
    mid_candidates <- setdiff(years, c(first, last))
    mid            <- mid_candidates[which.min(abs(mid_candidates - (first + last) / 2))]
    sort(c(first, mid, last))
  }

  if (length(seriesVars)) {
    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(seriesVars))) |>
      dplyr::group_modify(~ dplyr::filter(.x, .data$Year %in% pickYears(.x$Year))) |>
      dplyr::ungroup()
  } else {
    dplyr::filter(df, .data$Year %in% pickYears(df$Year))
  }
}

.DiscreteColorValues <- function(levels) {
  stats::setNames(.GgHuePal(length(levels)), levels)
}

.BuildSchedulePlot <- function(df, Sim, byStock, byFleet, ylab, xlab = 'Age', defaultYears = TRUE,
                                 breakpointYears = TRUE, probs = c(0.05, 0.95)) {
  hasSim <- 'Sim' %in% colnames(df)
  nSim   <- if (hasSim) length(unique(df$Sim)) else 1L

  if (hasSim && !is.null(Sim)) {
    if (!Sim %in% unique(df$Sim))
      cli::cli_abort("`Sim = {Sim}` not found; {length(unique(df$Sim))} simulation{?s} available.")
    df     <- dplyr::filter(df, .data$Sim == Sim)
    hasSim <- FALSE
  }

  medianDF <- if (hasSim) {
    groupVars <- setdiff(colnames(df), c('Sim', 'Value'))
    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groupVars))) |>
      dplyr::summarise(Value = stats::median(.data$Value, na.rm = TRUE), .groups = 'drop')
  } else df

  showLegend <- TRUE
  if (defaultYears && 'Year' %in% colnames(medianDF)) {
    sel        <- .SelectDefaultYears(medianDF)
    medianDF   <- sel$df
    showLegend <- sel$showLegend
  }

  if (breakpointYears && 'Year' %in% colnames(medianDF) && length(unique(medianDF$Year)) > 1) {
    medianDF <- .SelectBreakpointYears(medianDF)
    if (defaultYears)
      medianDF <- .CapRepresentativeYears(medianDF, maxPoints = 3)
  }

  showRibbon <- FALSE
  if (hasSim) {
    keepVars <- intersect(c('Stock', 'Fleet', 'MP', 'Year'), colnames(medianDF))
    keys     <- dplyr::distinct(medianDF, dplyr::across(dplyr::all_of(keepVars)))
    dfSub    <- dplyr::inner_join(df, keys, by = keepVars)

    groupVars <- setdiff(colnames(dfSub), c('Sim', 'Value'))
    summ <- dfSub |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groupVars))) |>
      dplyr::summarise(
        Lower = stats::quantile(.data$Value, min(probs), na.rm = TRUE),
        Upper = stats::quantile(.data$Value, max(probs), na.rm = TRUE),
        Value = stats::median(.data$Value, na.rm = TRUE),
        .groups = 'drop'
      )
    showRibbon <- nSim > 1 && any(round(summ$Upper - summ$Lower, 4) > 0)
    df <- if (showRibbon) summ else dplyr::select(summ, -"Lower", -"Upper")
  } else {
    df <- medianDF
  }

  allSeriesVars <- intersect(c('Stock', 'Fleet', 'MP'), colnames(df))
  groupVars     <- c('Year', allSeriesVars)
  df$.group     <- interaction(df[groupVars], drop = TRUE)

  facetVars <- allSeriesVars[purrr::map_lgl(allSeriesVars, \(v) {
    if (v == 'Stock' && isFALSE(byStock)) return(FALSE)
    if (v == 'Fleet' && isFALSE(byFleet)) return(FALSE)
    length(unique(df[[v]])) > 1
  })]
  colorCandidates <- intersect(c('Stock', 'Fleet'), setdiff(allSeriesVars, facetVars))
  colorCandidates <- colorCandidates[purrr::map_lgl(colorCandidates, \(v) length(unique(df[[v]])) > 1)]

  useYearColor <- length(colorCandidates) == 0
  colorVar     <- if (!useYearColor) colorCandidates[1] else NULL

  nYears         <- length(unique(df$Year))
  yearVaries     <- nYears > 1
  yearIsDiscrete <- nYears <= 6
  df$.Year <- if (yearIsDiscrete) factor(df$Year, levels = sort(unique(df$Year))) else df$Year

  useYearLinetype <- FALSE
  linetypeVar     <- NULL
  if (!useYearColor) {
    if (yearVaries) {
      useYearLinetype <- TRUE
      if (length(colorCandidates) > 1)
        facetVars <- union(facetVars, colorCandidates[2])
    } else if (length(colorCandidates) > 1) {
      linetypeVar <- colorCandidates[2]
    }
  }

  if (useYearColor) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Age, y = .data$Value,
                                          color = .data$.Year, group = .data$.group))
  } else {
    mapping <- ggplot2::aes(x = .data$Age, y = .data$Value, group = .data$.group,
                            color = .data[[colorVar]])
    if (useYearLinetype)
      mapping <- utils::modifyList(mapping, ggplot2::aes(linetype = .data$.Year))
    else if (!is.null(linetypeVar))
      mapping <- utils::modifyList(mapping, ggplot2::aes(linetype = .data[[linetypeVar]]))
    p <- ggplot2::ggplot(df, mapping)
  }

  if (showRibbon) {
    if (useYearColor) {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, group = .data$.group),
        fill = 'grey40', alpha = 0.25, color = NA)
    } else {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, group = .data$.group,
                     fill = .data[[colorVar]]),
        alpha = 0.25, color = NA)
    }
  }

  p <- p +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    # A non-zero lower expansion, not 0: a curve legitimately sitting at
    # its floor value (e.g. zero maturity at young ages, or the flat-zero
    # segment below a stock-recruit compensation threshold) would otherwise
    # land exactly on the panel border and become invisible against it.
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlab, y = ylab)

  if (useYearColor) {
    p <- p +
      (if (yearIsDiscrete)
        ggplot2::scale_color_manual(values = .DiscreteColorValues(levels(df$.Year)))
      else
        ggplot2::scale_color_viridis_c()) +
      ggplot2::labs(color = 'Year')
    if (!showLegend)
      p <- p + ggplot2::guides(color = 'none')
  } else {
    colorLevels <- sort(unique(df[[colorVar]]))
    p <- p + ggplot2::scale_color_manual(values = .DiscreteColorValues(colorLevels)) +
      ggplot2::labs(color = colorVar)
    if (showRibbon)
      p <- p + ggplot2::scale_fill_manual(values = .DiscreteColorValues(colorLevels), guide = 'none')
    if (useYearLinetype)
      p <- p + ggplot2::labs(linetype = 'Year')
    else if (!is.null(linetypeVar))
      p <- p + ggplot2::labs(linetype = linetypeVar)
  }

  if (length(facetVars)) {
    # Free scales when faceting by Stock: different stocks can have wildly
    # different biological scales (body size, recruitment, etc.), so a
    # shared axis range squashes the smaller-scale stock's curve flat.
    # Fleet/MP-only faceting keeps fixed scales, since those compare
    # naturally-comparable quantities (proportions, or curves for the same
    # stock) where a shared axis aids comparison rather than hurting it.
    freeScales <- if ('Stock' %in% facetVars) 'free' else 'fixed'
    if (all(c('Stock', 'Fleet') %in% facetVars)) {
      colVars <- setdiff(facetVars, 'Stock')
      p <- p + ggplot2::facet_grid(
        rows = ggplot2::vars(.data$Stock),
        cols = ggplot2::vars(!!!rlang::syms(colVars)),
        scales = freeScales
      )
    } else {
      p <- p + ggplot2::facet_wrap(facetVars, scales = freeScales)
    }
  }

  p
}
