#' Plot Observed Fishery Data
#'
#' Plotting functions for the observed (or simulated, with observation
#' error) fishery data in [data-class] objects. A [data-class] object holds
#' one realization of the data for one stock complex, so these functions
#' draw a single line per fleet or index.
#'
#' `object` can be a [data-class] object, an [om-class] object (its `Data`
#' slot, one [data-class] object per complex), a [hist-class] object (its
#' simulated `Data`, one [data-class] object per simulation and complex), or
#' a list of [data-class] objects named by complex, or a list of such lists
#' (one per simulation). When more than one simulation or complex is
#' available, the first is plotted, with a message; select another with
#' `Sim` and `Complex`.
#'
#' Fleets and indices with no non-zero values are not drawn.
#'
#' `PlotCPUE()` and `PlotSurvey()` plot the `CPUE` and `Survey` slots
#' respectively. `PlotLandings()`, `PlotDiscards()`, `PlotRemovals()`, and
#' `PlotEffort()` (see [plot_hist]) also accept a [data-class] object and are
#' documented here for that case.
#'
#' If the relevant slot is not populated (`NULL`, e.g. no survey configured
#' for this OM), the function prints an informational message and returns
#' `NULL` invisibly rather than plotting an empty panel. `PlotRemovals()`
#' plots `Landings` `+` `Discards`; it's omitted (with the same message)
#' only when *both* are unpopulated, and silently treats either as `0` when
#' just one is populated.
#'
#' For seasonal data (`object@Seasons > 1`), every function aggregates to
#' calendar years by default (catch and effort summed, indices averaged; see
#' `AggregateYear`) and prints a note pointing to `AggregateYear = FALSE`
#' and `Season` for plotting by season.
#'
#' Each timeseries panel shows the observation uncertainty from the slot's
#' `CV` (when populated and positive) as a shaded lognormal 95% interval,
#' `Value * exp(+/- 1.96 * sqrt(log(1 + CV^2)))`, or as an error bar for an
#' isolated observation. When fleets are summed (`byFleet = FALSE`), and
#' for `PlotRemovals()`, the CV of the total assumes independent errors.
#' Set `IncCV = FALSE` to omit it.
#'
#' `PlotData()` arranges whichever of `Landings`, `Discards`, `Effort`,
#' `CPUE`, and `Survey` are populated into a single figure with
#' [patchwork::wrap_plots()], silently omitting panels for slots that
#' aren't, without a message for missing slots. Below them (with `IncComp = TRUE`, the
#' default), it adds a bubble plot for each populated composition slot
#' (`LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize`;
#' see [plot_comp]), omitting any whose values are all zero or missing.
#' With `byFleet = FALSE`, size compositions whose fleets use different
#' size-class grids are shown by fleet instead of summed, with a message.
#' Fleet color legends are collected into a
#' single shared legend (`patchwork::plot_layout(guides = 'collect')`) only
#' when every panel with a color legend maps the same set of fleets;
#' otherwise each panel keeps its own legend, since a shared one would
#' misrepresent panels covering a different fleet set (e.g. `Landings` and
#' `Survey` are often reported by different fleets). If none of the five
#' slots are populated, it prints a message and returns `NULL`
#' invisibly instead of an empty figure. The `Discards` panel is also
#' omitted whenever it's populated but every value (across sims/years/
#' fleets) is zero, unless `showDiscards = TRUE`.
#'
#' @param object A [data-class], [om-class], or [hist-class] object, or a
#'   list of [data-class] objects. See Details.
#' @param byFleet Logical. Controls how multiple fleets (or indices) are
#'   displayed. Default (`NULL`) draws every fleet as its own colored line
#'   in a single panel. `TRUE` facets by fleet instead (one panel per
#'   fleet). `FALSE` sums across fleets and draws a single total line, for
#'   `Landings`/`Discards`/`Effort` only: `CPUE` and `Survey` indices have
#'   different scales and units and are never summed, so `FALSE` draws them
#'   the same as `NULL`. No effect when the slot has only one fleet.
#'   In `PlotData()`, a fleet or index keeps the same color in every panel.
#' @param AggregateYear Logical or `NULL`. For seasonal data
#'   (`object@Seasons > 1`), whether to aggregate to calendar years with
#'   [AnnualData()] before plotting: `Landings`, `Discards`, and `Effort`
#'   are summed over the seasons of each year, and `CPUE`/`Survey` are
#'   averaged over the seasons in `Season` (all seasons by default).
#'   Incomplete calendar years (e.g. a partially observed final year) are
#'   dropped, so a partial year isn't read as a low annual total. `NULL`
#'   (default) aggregates and prints a note on how to plot by season
#'   instead; `TRUE` aggregates without the note; `FALSE` plots every
#'   seasonal time step. Ignored for non-seasonal data.
#' @param Season For seasonal data (`object@Seasons > 1`), the season(s)
#'   (1 = the first timestep of each year) used for `CPUE`/`Survey`. When
#'   aggregating (see `AggregateYear`), the annual index is the mean over
#'   these seasons; with `AggregateYear = FALSE`, only time steps in these
#'   seasons are plotted. One of:
#'   - `NULL` (default): all seasons.
#'   - An integer vector, applied to every index.
#'   - For `PlotData()`, a list with elements `CPUE` and/or `Survey`, each
#'     an integer vector for every index in that slot, or a list with one
#'     element per index (in index order, or named by index; `NULL` for all
#'     seasons), e.g. `list(Survey = list(Spring = 2, Autumn = 4))`. For
#'     `PlotCPUE()`/`PlotSurvey()`, the per-index list is given directly,
#'     e.g. `list(Spring = 2, Autumn = 4)`.
#'
#'   An index observed in only one season always uses that season, unless a
#'   season was set for that index specifically: it can't be averaged over
#'   other seasons, and a `Season` meant for the other indices shouldn't
#'   blank it. A message lists any index left with no observations in its
#'   selected season(s). Ignored for non-seasonal data and by
#'   `PlotLandings()`/`PlotDiscards()`/`PlotRemovals()`/`PlotEffort()`,
#'   which are always summed when aggregated.
#' @param units Logical or a character unit string. `TRUE` (default) labels
#'   the y-axis with the slot's own `Units` (e.g. `object@CPUE@Units`,
#'   `"kg/trip"`) when it's set. `FALSE` suppresses the unit label. For
#'   `PlotLandings()`/`PlotDiscards()` only, `object@Landings@Units`/
#'   `object@Discards@Units` is a mass unit, so a character string (`"kg"`,
#'   `"t"`, `"lb"`, etc.) both relabels the axis *and* rescales the plotted
#'   values into that unit; requesting a character unit for `PlotCPUE()`/
#'   `PlotSurvey()` is an error, since those `Units` (e.g. `"kg/trip"`)
#'   aren't in a recognized conversion table. No effect on `PlotEffort()`,
#'   which has no unit concept for a single `data-class` object.
#' @param showDiscards Logical. `PlotData()` only. By default (`FALSE`),
#'   the `Discards` panel is omitted when it's populated but every value
#'   (across sims/years/fleets) is zero. Set `TRUE` to always show it.
#'
#' @param IncCV Logical. Draw the observation uncertainty from the slot's
#'   `CV` as a shaded band (see Details)? Default `TRUE`.
#' @param IncComp Logical. `PlotData()` only. Add bubble plots of the
#'   populated composition slots below the timeseries panels? Default
#'   `TRUE`.
#' @param Sim Integer, or `NULL` (default: the first). The simulation to
#'   plot when `object` holds data for more than one simulation.
#' @param byStock One of `FALSE` (default), `TRUE`, or `'sum'`. For a slot
#'   holding a list of objects named by the complex and/or its stocks (see
#'   [data-class]), `FALSE` draws each element as its own line colored by
#'   stock (or complex), with a panel per fleet or index; `TRUE` facets by
#'   stock and colors by fleet; `'sum'` combines the elements into one
#'   series per fleet or index. No effect on a slot holding one object.
#' @param Complex Integer or character, or `NULL` (default: the first). The
#'   complex (by position or name) to plot when `object` holds data for more
#'   than one complex.
#'
#' @return A `ggplot` object (a `patchwork` object for `PlotData()`), or
#'   `NULL` invisibly if the relevant slot isn't populated.
#'
#' @examples
#' Hist <- Simulate(SingleStockOM, nSim = 2)
#' dat  <- Hist@Data[[1]][[1]]
#'
#' PlotLandings(dat)
#' PlotDiscards(dat)
#' PlotEffort(dat)
#' PlotCPUE(dat)
#' PlotSurvey(dat)
#' PlotData(dat)
#'
#' PlotLandings(dat, byFleet = TRUE)
#' PlotLandings(dat, byFleet = FALSE)
#'
#' @name plot_data
#' @seealso [plot_hist], [plot_comp], [Landings()], [Discards()], [CPUE()], [Survey()]
#' @include class-data.R
NULL

#' @rdname plot_data
#' @export
PlotCPUE <- function(object, byFleet = NULL, Season = NULL, units = TRUE,
                     AggregateYear = NULL, IncCV = TRUE, Sim = NULL, Complex = NULL,
                     byStock = FALSE) {
  object <- .ResolvePlotData(object, Sim, Complex) |> UpdateObject()
  .PlotDataTs(object, 'CPUE', 'CPUE', byFleet = byFleet, AggregateYear = AggregateYear,
              Season = .SlotSeason(Season, 'CPUE'), units = units, IncCV = IncCV,
              byStock = byStock)
}

#' @rdname plot_data
#' @export
PlotSurvey <- function(object, byFleet = NULL, Season = NULL, units = TRUE,
                       AggregateYear = NULL, IncCV = TRUE, Sim = NULL, Complex = NULL,
                       byStock = FALSE) {
  object <- .ResolvePlotData(object, Sim, Complex) |> UpdateObject()
  .PlotDataTs(object, 'Survey', 'Survey', byFleet = byFleet, AggregateYear = AggregateYear,
              Season = .SlotSeason(Season, 'Survey'), units = units, IncCV = IncCV,
              byStock = byStock)
}

#' @rdname plot_data
#' @export
PlotData <- function(object, byFleet = NULL, AggregateYear = NULL, Season = NULL, units = TRUE,
                     showDiscards = FALSE, IncCV = TRUE, IncComp = TRUE,
                     Sim = NULL, Complex = NULL, byStock = FALSE) {
  object   <- .ResolvePlotData(object, Sim, Complex) |> UpdateObject()
  elements <- purrr::map(c(Landings = 'Landings', Discards = 'Discards', CPUE = 'CPUE', Survey = 'Survey'),
                         \(sl) names(.PlotStockElements(slot(object, sl), byStock))) |>
    purrr::keep(\(x) length(x) > 1)
  stockColors <- unique(unlist(elements))
  stockColors <- if (length(stockColors)) stats::setNames(.GgHuePal(length(stockColors)), stockColors)

  # aggregate once here so the seasonal notes print once, not once per panel;
  # panels then get the resolved per-index seasons. Stock elements are
  # aggregated one at a time by their panels.
  raw    <- object
  sp     <- .SeasonalPlotData(.CombineDataSeries(object), AggregateYear, Season)
  object <- sp$object
  byElement   <- length(elements) > 0
  panelObject <- if (byElement) raw else object
  panelAgg    <- byElement && !isFALSE(AggregateYear)
  panelSeason <- if (byElement) Season else sp$Season

  discardsVal      <- object@Discards@Value
  discardsAllZero  <- !is.null(discardsVal) &&
    any(!is.na(discardsVal)) && all(discardsVal == 0 | is.na(discardsVal))

  specs <- list(
    list(slot = 'Landings', ylab = 'Landings', units = units),
    list(slot = 'Discards', ylab = 'Discards', units = units,
         skip = discardsAllZero && !showDiscards),
    list(slot = 'Effort',   ylab = 'Effort'),
    list(slot = 'CPUE',     ylab = 'CPUE',     units = units),
    list(slot = 'Survey',   ylab = 'Survey',   units = units)
  )
  specs <- purrr::discard(specs, \(s) isTRUE(s$skip))

  # one color per fleet/index name across every panel, so a fleet keeps its
  # color even when panels cover different fleet sets
  allNames    <- unique(unlist(purrr::map(specs, \(s) .DataSeriesNames(object, s$slot))))
  fleetColors <- if (length(allNames)) stats::setNames(.GgHuePal(length(allNames)), allNames)

  panels <- purrr::map(specs, function(s) {
    .PlotDataTs(panelObject, s$slot, s$ylab, byFleet = byFleet, AggregateYear = panelAgg,
                 Season = panelSeason, units = s$units %||NA% FALSE, silent = TRUE,
                 colors = fleetColors, IncCV = IncCV, byStock = byStock,
                 stockColors = stockColors)
  })
  panels <- purrr::compact(panels)

  # composition bubble plots: populated, non-zero slots only (object is
  # already aggregated to calendar years when requested)
  compPanels <- list()
  if (isTRUE(IncComp)) {
    compSpecs  <- c(LandingsAtAge = 'Age', DiscardsAtAge = 'Age',
                    LandingsAtSize = 'Size Class', DiscardsAtSize = 'Size Class',
                    SurveyAtAge = 'Age', SurveyAtSize = 'Size Class')
    compPanels <- purrr::imap(compSpecs, \(ylab, what)
      .PlotDataCompBubble(object, what, ylab, byFleet = byFleet, AggregateYear = FALSE,
                          silent = TRUE, gridFallback = TRUE)) |>
      purrr::compact() |>
      unname()
  }

  if (!length(panels) && !length(compPanels)) {
    cli::cli_alert_info("No populated timeseries (Landings/Discards/Effort/CPUE/Survey) or composition data found in this `data` object.")
    return(invisible(NULL))
  }

  tsBlock <- NULL
  if (length(panels)) {
    legendFleets <- purrr::map(specs, \(s) {
      if (is.null(elements[[s$slot]])) .ColorLegendFleets(object, s$slot, byFleet)
      else if (isTRUE(byStock)) .DataSeriesNames(object, s$slot)
      else elements[[s$slot]]
    })
    legendFleets <- purrr::compact(legendFleets)
    collectGuides <- length(legendFleets) <= 1 ||
      all(purrr::map_lgl(legendFleets[-1], setequal, legendFleets[[1]]))

    tsBlock <- patchwork::wrap_plots(panels, ncol = 2)
    if (collectGuides)
      tsBlock <- tsBlock + patchwork::plot_layout(guides = 'collect')
  }
  if (!length(compPanels))
    return(tsBlock)

  # bubble plots faceted by fleet need the full width, and a height per facet row
  faceted   <- purrr::map_lgl(compPanels, \(p) !inherits(p$facet, 'FacetNull'))
  compCols  <- if (any(faceted) || length(compPanels) == 1) 1 else 2
  panelRows <- purrr::map_dbl(compPanels, .FacetRows)
  compBlock <- if (compCols == 1)
    patchwork::wrap_plots(compPanels, ncol = 1, heights = panelRows)
  else patchwork::wrap_plots(compPanels, ncol = compCols)
  if (is.null(tsBlock))
    return(compBlock)

  tsRows   <- ceiling(length(panels) / 2)
  compRows <- if (compCols == 1) sum(panelRows) else ceiling(length(compPanels) / compCols)
  patchwork::wrap_plots(tsBlock, compBlock, ncol = 1, heights = c(tsRows, compRows))
}

# relative height of a ggplot: 1 for one panel row, 0.5 for each further facet row
.FacetRows <- function(p) {
  if (inherits(p$facet, 'FacetNull'))
    return(1)
  1 + 0.5 * (max(ggplot2::ggplot_build(p)$layout$layout$ROW) - 1)
}

#' @param x A [data-class] object.
#' @param y Unused.
#' @param ... Additional arguments passed to `PlotData()`.
#' @rdname plot_data
#' @export
setMethod('plot', 'data', function(x, y, ...) {
  PlotData(x, ...)
})

# ---- internal helpers ----

# one `data` object from a data object, an om (`OM@Data`), a hist
# (`Hist@Data[[sim]][[complex]]`), or a list of them by complex or sim/complex
.ResolvePlotData <- function(object, Sim = NULL, Complex = NULL) {
  .CheckClass(object, c('data', 'om', 'hist', 'list'), 'object')
  if (inherits(object, c('om', 'hist')))
    object <- object@Data
  if (inherits(object, 'data'))
    return(object)
  if (!length(object))
    cli::cli_abort("No {.cls data} objects found in {.arg object}.", call = NULL)

  if (all(purrr::map_lgl(object, \(x) is.list(x) && !isS4(x)))) {
    object <- .SelectPlotDataElement(object, Sim, 'Sim', 'simulation')
  }
  object <- .SelectPlotDataElement(object, Complex, 'Complex', 'complex')
  .CheckClass(object, 'data', 'object')
  object
}

.SelectPlotDataElement <- function(x, ind, arg, label) {
  n <- length(x)
  if (n == 1)
    return(x[[1]])
  if (is.null(ind)) {
    first <- if (!is.null(names(x)) && nzchar(names(x)[1])) names(x)[1] else 1
    cli::cli_alert_info("Plotting data for {label} {.val {first}} of {n}; use {.arg {arg}} to choose another.")
    return(x[[1]])
  }
  valid <- length(ind) == 1 &&
    ((is.character(ind) && ind %in% names(x)) || (is.numeric(ind) && ind >= 1 && ind <= n))
  if (!valid)
    cli::cli_abort(c("x" = "{.arg {arg}} must be a single index (1 to {n}) or name.",
                     "i" = "Available: {.val {names(x) %||% seq_len(n)}}."), call = NULL)
  x[[ind]]
}

.IndexSlots <- c('CPUE', 'Survey')

.PlotDataTs <- function(object, slot_name, ylab, byFleet = NULL,
                         AggregateYear = NULL, Season = NULL, units = FALSE,
                         silent = FALSE, colors = NULL, IncCV = TRUE, byStock = FALSE,
                         stockColors = NULL) {
  isIndex  <- slot_name %in% .IndexSlots
  colorLab <- if (isIndex) 'Index' else 'Fleet'
  # indices are never summed: different indices have different scales/units
  if (isIndex && isFALSE(byFleet)) byFleet <- NULL

  elements <- .PlotStockElements(slot(object, slot_name), byStock)
  if (length(elements) > 1) {
    series <- purrr::imap(elements, \(el, nm) {
      obj <- object
      slot(obj, slot_name) <- el
      .DataTsSeries(obj, slot_name, ylab, AggregateYear, Season, units,
                    silent = silent || nm != names(elements)[1], IncCV = IncCV)
    })
    return(.BuildStockDataTsPlot(series, byFleet, byStock, stockColors, colorLab))
  }

  series <- .DataTsSeries(object, slot_name, ylab, AggregateYear, Season, units, silent, IncCV)
  if (is.null(series))
    return(invisible(NULL))
  .BuildDataTsPlot(series$value, series$ylab, object = series$object, byFleet = byFleet,
                   seasons = series$seasons, colors = colors, colorLab = colorLab,
                   cv_arr = series$cv)
}

# value and CV arrays of one slot, aggregated to calendar years when requested
.DataTsSeries <- function(object, slot_name, ylab, AggregateYear = NULL, Season = NULL,
                          units = FALSE, silent = FALSE, IncCV = TRUE) {
  object <- .CombineDataSeries(object)
  # skip the seasonal notes when there's nothing to plot
  sp <- .SeasonalPlotData(object, AggregateYear, Season,
                          silent  = silent || is.null(slot(object, slot_name)@Value),
                          sources = intersect(slot_name, .IndexSlots))
  object    <- sp$object
  value_arr <- slot(object, slot_name)@Value

  if (is.null(value_arr)) {
    if (!silent)
      cli::cli_alert_info("No {.field {slot_name}} data found in this `data` object; nothing to plot.")
    return(invisible(NULL))
  }

  if (!isFALSE(units)) {
    base_unit <- unique(slot(object, slot_name)@Units)
    if (length(base_unit) != 1 || base_unit %in% c('Biomass', 'Number', 'Recruitment'))
      base_unit <- NULL

    if (slot_name %in% c('Landings', 'Discards')) {
      uinfo     <- .ResolveUnitInfo(.mass_units_g, base_unit, 1, units, ylab)
      ylab      <- .AppendUnits(ylab, uinfo$label)
      value_arr <- value_arr * uinfo$factor
    } else {
      if (!isTRUE(units))
        cli::cli_abort("`units` only accepts `TRUE`/`FALSE` for {.field {slot_name}} (its `Units` aren't in a recognized conversion table).")
      ylab <- .AppendUnits(ylab, base_unit)
    }
  }

  list(value = value_arr, cv = if (isTRUE(IncCV)) slot(object, slot_name)@CV, ylab = ylab,
       seasons = sp$Season[[slot_name]], object = object) |>
    .KeepFleetsWithData()
}

# drops fleets/indices with no non-zero values, unless none has any
.KeepFleetsWithData <- function(series) {
  value <- series$value
  if (length(dim(value)) != 2)
    return(series)
  keep <- .HasSeriesData(value)
  if (!any(keep) || all(keep))
    return(series)
  series$value <- value[, keep, drop = FALSE]
  if (.SameDim(series$cv, value))
    series$cv <- series$cv[, keep, drop = FALSE]
  series
}

.HasSeriesData <- function(value) apply(value, 2, \(x) any(!is.na(x) & x != 0))

# non-empty elements of a stock-specific slot, unless stocks are summed
.PlotStockElements <- function(x, byStock) {
  if (!is.list(x) || identical(byStock, 'sum'))
    return(NULL)
  x[!vapply(x, EmptyObject, logical(1))]
}

.PlotDataRemovals <- function(object, byFleet = NULL, AggregateYear = NULL,
                               units = TRUE, silent = FALSE, IncCV = TRUE, byStock = FALSE,
                               stockColors = NULL) {
  L <- .PlotStockElements(object@Landings, byStock)
  D <- .PlotStockElements(object@Discards, byStock)
  elementNames <- union(names(L), names(D))
  if (length(elementNames) > 1) {
    series <- purrr::map(stats::setNames(nm = elementNames), \(nm) {
      obj <- object
      obj@Landings <- L[[nm]] %||% new('catchdata')
      obj@Discards <- D[[nm]] %||% new('catchdata')
      .DataRemovalsSeries(obj, AggregateYear, units, silent = silent || nm != elementNames[1], IncCV)
    })
    return(.BuildStockDataTsPlot(series, byFleet, byStock, stockColors, 'Fleet'))
  }

  series <- .DataRemovalsSeries(object, AggregateYear, units, silent, IncCV)
  if (is.null(series))
    return(invisible(NULL))
  .BuildDataTsPlot(series$value, series$ylab, object = series$object, byFleet = byFleet,
                   cv_arr = series$cv)
}

.DataRemovalsSeries <- function(object, AggregateYear = NULL, units = TRUE, silent = FALSE,
                                IncCV = TRUE) {
  object <- .CombineDataSeries(object)
  object <- .SeasonalPlotData(object, AggregateYear, silent = silent ||
                                (is.null(object@Landings@Value) && is.null(object@Discards@Value)),
                              sources = character(0))$object
  L <- object@Landings@Value
  D <- object@Discards@Value

  if (is.null(L) && is.null(D)) {
    if (!silent)
      cli::cli_alert_info("No {.field Landings} or {.field Discards} data found in this `data` object; nothing to plot.")
    return(invisible(NULL))
  }

  value_arr <- if (is.null(L)) D else if (is.null(D)) L else ArraySum(L, D)
  ylab <- 'Removals'

  # CV of Landings + Discards, assuming independent errors
  cv_arr <- NULL
  if (isTRUE(IncCV)) {
    sdSq <- function(x) if (.SameDim(x@CV, x@Value)) (x@CV * x@Value)^2
    VL <- if (!is.null(L)) sdSq(object@Landings)
    VD <- if (!is.null(D)) sdSq(object@Discards)
    ok <- (is.null(L) || !is.null(VL)) && (is.null(D) || !is.null(VD))
    if (ok) {
      Var    <- if (is.null(VL)) VD else if (is.null(VD)) VL else ArraySum(VL, VD)
      cv_arr <- sqrt(Var) / value_arr
    }
  }

  if (!isFALSE(units)) {
    landUnit  <- unique(object@Landings@Units)
    discUnit  <- unique(object@Discards@Units)
    base_unit <- if (is.null(L)) discUnit
                 else if (is.null(D)) landUnit
                 else if (length(landUnit) == 1 && identical(landUnit, discUnit)) landUnit
                 else NULL
    if (length(base_unit) != 1 || base_unit %in% c('Biomass', 'Number', 'Recruitment'))
      base_unit <- NULL

    uinfo     <- .ResolveUnitInfo(.mass_units_g, base_unit, 1, units, ylab)
    ylab      <- .AppendUnits(ylab, uinfo$label)
    value_arr <- value_arr * uinfo$factor
  }

  list(value = value_arr, cv = cv_arr, ylab = ylab, seasons = NULL, object = object) |>
    .KeepFleetsWithData()
}

# Handles seasonal data (`object@Seasons > 1`) for the data plots. Returns
# list(object, Season):
# - `Season` resolved per index for the index slots in `sources` (see
#   .ResolvePlotSeasons()), used to filter seasonal time steps when
#   `AggregateYear = FALSE`; NULL once aggregated.
# - `object` aggregated to calendar years with AnnualData() (flows summed,
#   indices averaged over their resolved seasons), unless
#   `AggregateYear = FALSE`.
# `AggregateYear = NULL` (the default everywhere) also prints a note on how
# to plot by season instead. Non-seasonal data is returned unchanged.
.SeasonalPlotData <- function(object, AggregateYear = NULL, Season = NULL, silent = FALSE,
                              sources = .IndexSlots) {
  if (!is.null(AggregateYear) && !(isTRUE(AggregateYear) || isFALSE(AggregateYear)))
    cli::cli_abort("{.arg AggregateYear} must be {.code TRUE}, {.code FALSE}, or {.code NULL}.")

  nSeason <- max(1L, as.integer(object@Seasons %||NA% 1))
  if (nSeason == 1L)
    return(list(object = object, Season = NULL))

  res <- .ResolvePlotSeasons(object, Season, nSeason, sources)

  if (!silent && length(res$Empty))
    cli::cli_inform(c(
      "!" = "No observations in the selected season(s) for {cli::qty(length(res$Empty))}{?this index/these indices}: {res$Empty}.",
      " " = "Change {.arg Season} to include the season(s) they were observed in."
    ))

  if (isFALSE(AggregateYear))
    return(list(object = object, Season = res$Seasons))

  out <- AnnualData(object, IndexSeasons = res$Seasons)

  if (is.null(AggregateYear) && !silent) {
    summary <- "catch, effort, and compositions summed"
    if (length(res$Seasons)) {
      seasonTxt <- if (is.null(Season)) 'all seasons'
                   else if (is.list(Season)) 'the selected seasons'
                   else paste0('season', if (length(Season) > 1) 's' else '', ' ',
                               paste(Season, collapse = ', '))
      summary <- paste0(summary, "; indices averaged over ", seasonTxt)
    }
    msg <- c("i" = paste0("Seasonal data (", nSeason, " seasons per year) aggregated to calendar years: ",
                          summary, "."))
    if (length(res$Auto))
      msg <- c(msg, " " = paste0("Indices observed in a single season use that season: ",
                                 paste(res$Auto, collapse = '; '), "."))
    msg <- c(msg, " " = if (length(res$Seasons))
      "Set {.code AggregateYear = FALSE} to plot each season, or {.code Season} to choose the season(s) used for the indices."
      else "Set {.code AggregateYear = FALSE} to plot each season.")
    dropped <- setdiff(unique(.CalendarYear(object@Years)), out@Years)
    if (length(dropped))
      msg <- c(msg, " " = paste0("Incomplete year", if (length(dropped) > 1) "s" else "",
                                 " ", paste(dropped, collapse = ", "), " omitted."))
    cli::cli_inform(msg)
  }
  list(object = out, Season = NULL)
}

# Resolves `Season` into one entry per index for each populated slot in
# `sources`: list(CPUE = list(<seasons or NULL>, ...), Survey = ...), each
# named by index. `Season` takes the same forms as AnnualData()'s
# `IndexSeasons`: NULL (all seasons), an integer vector for every index, or
# a list named CPUE/Survey whose elements are a vector for every index in
# that slot or a list with one element per index.
#
# An index observed in only one season uses that season, unless a season
# was set for that index specifically (per-index list form): it can't be
# averaged over other seasons, and a global `Season` shouldn't blank it.
# Returns the resolved seasons plus labels for the indices that were
# auto-selected (`Auto`) and that have no observations in their resolved
# seasons (`Empty`).
.ResolvePlotSeasons <- function(object, Season, nSeason, sources = .IndexSlots) {
  if (!is.null(Season) && !is.list(Season) && !is.numeric(Season))
    cli::cli_abort("{.arg Season} must be an integer vector or a list named {.val CPUE}/{.val Survey}.")
  if (is.list(Season) && (is.null(names(Season)) || !all(names(Season) %in% .IndexSlots)))
    cli::cli_abort("A list {.arg Season} must have elements named {.val CPUE} and/or {.val Survey}.")

  out <- list(); auto <- character(0); empty <- character(0)
  for (sl in sources) {
    Value <- slot(object, sl)@Value
    if (is.null(Value)) next
    nIndex     <- NCOL(Value)
    indexNames <- .DataSeriesNames(object, sl)
    perIndex   <- is.list(Season) && is.list(Season[[sl]])
    # a per-index list named by index: match by name (unnamed indices get NULL)
    if (perIndex && length(names(Season[[sl]])) && all(nzchar(names(Season[[sl]])))) {
      bad <- setdiff(names(Season[[sl]]), indexNames)
      if (length(bad))
        cli::cli_abort("{.arg Season${sl}} names {.val {bad}} not found; {sl} indices are {.val {indexNames}}.")
      Season[[sl]] <- purrr::map(indexNames, \(nm) Season[[sl]][[nm]])
    }
    seasons    <- .ResolveIndexSeasons(Season, sl, nIndex, nSeason)

    rowSeason <- .SeasonOfTimestep(.AnnualRowYears(Value, object@Years), nSeason)
    Mat       <- matrix(Value, nrow = dim(Value)[1])
    for (j in seq_len(nIndex)) {
      observed <- !is.na(Mat[, j]) & Mat[, j] >= 0
      obs      <- sort(unique(rowSeason[observed]))
      if (length(obs) == 1 && !(perIndex && !is.null(seasons[[j]]))) {
        seasons[j] <- list(obs)
        auto <- c(auto, sprintf('%s %s (season %d)', sl, indexNames[j], obs))
      }
      if (length(obs) && !is.null(seasons[[j]]) && !any(obs %in% seasons[[j]]))
        empty <- c(empty, sprintf('%s %s (observed in season%s %s)', sl, indexNames[j],
                                  if (length(obs) > 1) 's' else '', paste(obs, collapse = ', ')))
    }
    names(seasons) <- indexNames
    out[[sl]] <- seasons
  }
  list(Seasons = out, Auto = auto, Empty = empty)
}

# PlotCPUE()/PlotSurvey() take `Season` for one slot only: a vector, or a
# list with one element per index. Wraps a per-index list as
# list(<slot> = ...) for .ResolvePlotSeasons().
.SlotSeason <- function(Season, slot_name) {
  if (is.list(Season) && !(length(names(Season)) && all(names(Season) %in% .IndexSlots)))
    return(stats::setNames(list(Season), slot_name))
  Season
}

# fleet/index names of a timeseries slot, in column order
.DataSeriesNames <- function(object, slot_name) {
  Value <- slot(object, slot_name)@Value
  if (is.null(Value)) return(NULL)
  dimnames(Value)[[2]] %||% (slot(object, slot_name)@Name %||%
                               paste(slot_name, seq_len(NCOL(Value))))
}

.ColorLegendFleets <- function(object, slot_name, byFleet) {
  if (slot_name %in% .IndexSlots && isFALSE(byFleet)) byFleet <- NULL
  if (!is.null(byFleet)) return(NULL)
  fleets <- .DataSeriesNames(object, slot_name)
  value  <- slot(object, slot_name)@Value
  if (length(dim(value)) == 2 && any(.HasSeriesData(value)))
    fleets <- fleets[.HasSeriesData(value)]
  fleets <- unique(fleets)
  if (length(fleets) <= 1) return(NULL)
  fleets
}

# keeps, for each index (the `Fleet` column), only the seasonal time steps
# in its resolved seasons (a list named by index; NULL entries keep all)
.FilterSeasonData <- function(df, seasons, object) {
  if (is.null(seasons) || all(purrr::map_lgl(seasons, is.null)))
    return(df)

  nSeason <- max(1L, as.integer(object@Seasons %||NA% 1))
  if (nSeason == 1L)
    return(df)

  idx  <- if ('Fleet' %in% colnames(df)) match(as.character(df$Fleet), names(seasons))
          else rep(1L, nrow(df))
  s    <- .SeasonOfTimestep(df$Year, nSeason)
  keep <- purrr::map2_lgl(idx, s, \(i, si) is.na(i) || is.null(seasons[[i]]) || si %in% seasons[[i]])
  df[keep, , drop = FALSE]
}

.IsolatedRows <- function(df) {
  groupCol <- intersect(c('Stock', 'Fleet'), colnames(df))

  df <- df |> dplyr::arrange(dplyr::across(dplyr::any_of(c(groupCol, 'Year'))))
  if (length(groupCol))
    df <- df |> dplyr::group_by(dplyr::across(dplyr::all_of(groupCol)))

  df |>
    dplyr::filter(
      !is.na(.data$Value),
      dplyr::row_number() == 1 | is.na(dplyr::lag(.data$Value)),
      dplyr::row_number() == dplyr::n() | is.na(dplyr::lead(.data$Value))
    ) |>
    dplyr::ungroup()
}

# index arrays may name their second dimension e.g. `Index`; treat it as
# the series (`Fleet`) dimension regardless of name
.NormalizeSeriesDim <- function(arr) {
  dn <- dimnames(arr)
  if (length(dim(arr)) == 2 && !is.null(dn)) {
    dn[[2]] <- dn[[2]] %||% paste('Series', seq_len(ncol(arr)))
    names(dn) <- c(names(dn)[1] %||% 'Year', 'Fleet')
    if (!nzchar(names(dn)[1])) names(dn)[1] <- 'Year'
    dimnames(arr) <- dn
  }
  arr
}

# lognormal 95% interval from a CV
.CVBounds <- function(df) {
  hasBand <- !is.na(df$CV) & df$CV > 0 & !is.na(df$Value) & df$Value > 0
  sigma   <- sqrt(log(1 + df$CV^2))
  df$Lower <- ifelse(hasBand, df$Value * exp(-1.96 * sigma), NA_real_)
  df$Upper <- ifelse(hasBand, df$Value * exp( 1.96 * sigma), NA_real_)
  df
}

# long data frame of a value array, with a `CV` column when the CVs are usable
.DataTsDF <- function(value_arr, cv_arr = NULL, object = NULL, seasons = NULL) {
  value_arr <- .NormalizeSeriesDim(value_arr)
  df <- Array2DF(value_arr)

  hasCV <- !is.null(cv_arr) && .SameDim(cv_arr, value_arr) &&
    any(!is.na(cv_arr) & cv_arr > 0)
  if (hasCV) {
    dimnames(cv_arr) <- dimnames(value_arr)
    cvdf <- Array2DF(cv_arr) |> dplyr::rename(CV = 'Value')
    df   <- dplyr::left_join(df, cvdf, by = intersect(c('Year', 'Fleet'), colnames(df)))
  }

  if (!is.null(object))
    df <- .FilterSeasonData(df, seasons, object)
  df
}

.BuildDataTsPlot <- function(value_arr, ylab, object = NULL, byFleet = NULL,
                               seasons = NULL, colors = NULL, colorLab = 'Fleet',
                               cv_arr = NULL) {
  df    <- .DataTsDF(value_arr, cv_arr, object, seasons)
  hasCV <- 'CV' %in% colnames(df)

  multiFleet <- 'Fleet' %in% colnames(df) && length(unique(df$Fleet)) > 1
  mode <- if (!multiFleet) 'single'
          else if (isTRUE(byFleet))  'facet'
          else if (isFALSE(byFleet)) 'sum'
          else 'color'

  if (mode == 'sum') {
    if (hasCV) {
      # variance of the total, assuming independent errors among fleets
      df <- df |>
        dplyr::group_by(Year = .data$Year) |>
        dplyr::summarise(Var   = sum((.data$CV * .data$Value)^2),
                         Value = sum(.data$Value), .groups = 'drop') |>
        dplyr::mutate(CV = sqrt(.data$Var) / .data$Value, Fleet = 'Total') |>
        dplyr::select(-'Var')
    } else {
      df <- .SumOverFleet(df)
    }
  }

  if (hasCV) df <- .CVBounds(df)

  hasColor <- mode == 'color'
  if (hasColor) {
    fleetLevels <- levels(df$Fleet) %||% unique(as.character(df$Fleet))
    fleetValues <- if (!is.null(colors) && all(fleetLevels %in% names(colors)))
                     colors[fleetLevels]
                   else stats::setNames(.GgHuePal(length(fleetLevels)), fleetLevels)
  }

  mapping <- ggplot2::aes(x = .data$Year, y = .data$Value)
  if (hasColor)
    mapping <- utils::modifyList(mapping, ggplot2::aes(color = .data$Fleet))

  isolated <- .IsolatedRows(df)

  p <- ggplot2::ggplot(df, mapping)

  if (hasCV) {
    bandAes <- ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper)
    if (hasColor) {
      p <- p +
        ggplot2::geom_ribbon(utils::modifyList(bandAes, ggplot2::aes(fill = .data$Fleet)),
                             alpha = 0.2, colour = NA, na.rm = TRUE) +
        ggplot2::geom_linerange(data = isolated, bandAes, linewidth = 0.5, na.rm = TRUE,
                                show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = fleetValues, guide = 'none')
    } else {
      p <- p +
        ggplot2::geom_ribbon(bandAes, fill = 'grey50', alpha = 0.25, colour = NA, na.rm = TRUE) +
        ggplot2::geom_linerange(data = isolated, bandAes, linewidth = 0.5, na.rm = TRUE,
                                show.legend = FALSE)
    }
  }

  p <- p +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(data = isolated, size = 1.2, na.rm = TRUE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                breaks = .YearBreaks, labels = .YearLabels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab, color = if (hasColor) colorLab else NULL)

  if (hasColor)
    p <- p + ggplot2::scale_color_manual(values = fleetValues)

  if (mode == 'facet')
    p <- p + ggplot2::facet_wrap(~Fleet, scales = 'free_y')

  p
}

# one series per stock element: colored by stock and faceted by fleet, or
# faceted by stock and colored by fleet with `byStock = TRUE`
.BuildStockDataTsPlot <- function(series, byFleet, byStock, colors = NULL, colorLab = 'Fleet') {
  series <- purrr::compact(series)
  if (!length(series))
    return(invisible(NULL))
  ylab <- series[[1]]$ylab
  df <- purrr::imap(series, \(s, nm)
    .DataTsDF(s$value, s$cv, s$object, s$seasons) |> dplyr::mutate(Stock = nm)
  ) |> dplyr::bind_rows()
  df$Stock <- factor(df$Stock, levels = names(series))
  if (!'Fleet' %in% colnames(df))
    df$Fleet <- colorLab
  if (!'CV' %in% colnames(df))
    df$CV <- NA_real_

  if (isFALSE(byFleet) && length(unique(df$Fleet)) > 1) {
    df <- df |>
      dplyr::group_by(.data$Year, .data$Stock) |>
      dplyr::summarise(Var   = sum((.data$CV * .data$Value)^2),
                       Value = sum(.data$Value), .groups = 'drop') |>
      dplyr::mutate(CV = sqrt(.data$Var) / .data$Value, Fleet = 'Total') |>
      dplyr::select(-'Var')
  }
  hasCV <- any(!is.na(df$CV) & df$CV > 0)
  if (hasCV) df <- .CVBounds(df)

  facetStock <- isTRUE(byStock)
  multiFleet <- length(unique(df$Fleet)) > 1
  facetFleet <- multiFleet && if (facetStock) isTRUE(byFleet) else TRUE
  colorVar   <- if (facetStock) 'Fleet' else 'Stock'
  levels     <- if (colorVar == 'Stock') levels(df$Stock) else unique(as.character(df$Fleet))
  values     <- if (colorVar == 'Stock' && !is.null(colors) && all(levels %in% names(colors)))
                  colors[levels]
                else stats::setNames(.GgHuePal(length(levels)), levels)
  df$.group  <- interaction(df$Stock, df$Fleet, drop = TRUE)
  isolated   <- .IsolatedRows(df)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Year, y = .data$Value,
                                        color = .data[[colorVar]], group = .data$.group))
  if (hasCV)
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, fill = .data[[colorVar]]),
                           alpha = 0.2, colour = NA, na.rm = TRUE) +
      ggplot2::geom_linerange(data = isolated, ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper),
                              linewidth = 0.5, na.rm = TRUE, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = values, guide = 'none')

  p <- p +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(data = isolated, size = 1.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = values) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                breaks = .YearBreaks, labels = .YearLabels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab, color = if (colorVar == 'Stock') 'Stock' else colorLab)

  if (facetStock && facetFleet)
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data$Stock), cols = ggplot2::vars(.data$Fleet),
                                 scales = 'free_y')
  else if (facetStock)
    p <- p + ggplot2::facet_wrap(~Stock, scales = 'free_y')
  else if (facetFleet)
    p <- p + ggplot2::facet_wrap(~Fleet, scales = 'free_y')
  p
}
