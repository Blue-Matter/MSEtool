#' Plot Observed Fishery Data
#'
#' Plotting functions for [data-class] objects: the observed (or simulated,
#' with observation error) fishery timeseries, as opposed to the underlying
#' simulated dynamics plotted by [plot_hist]. A [data-class] object holds a
#' single simulation/stock realization (no `Sim` dimension), so these
#' functions draw a single line per fleet or index rather than a
#' median/ribbon summary.
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
#' `PlotData()` arranges whichever of `Landings`, `Discards`, `Effort`,
#' `CPUE`, and `Survey` are populated into a single figure with
#' [patchwork::wrap_plots()], silently omitting panels for slots that
#' aren't; unlike the individual `Plot*()` functions it does not print a
#' message for missing slots. Fleet color legends are collected into a
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
#' @param object A [data-class] object.
#' @param byFleet Logical. Controls how multiple fleets are displayed.
#'   Default (`NULL`) draws every fleet as its own colored line in a single
#'   panel. `TRUE` facets by fleet instead (one panel per fleet). `FALSE`
#'   sums across fleets and draws a single total line. No effect when the
#'   slot has only one fleet.
#' @param AggregateYear Logical. For seasonal data (`object@Seasons > 1`),
#'   sum `PlotLandings()`/`PlotDiscards()`/`PlotEffort()` sub-annual values
#'   into whole-year totals, since these are flow quantities. Default
#'   `FALSE`. Ignored for non-seasonal data and by `PlotCPUE()`/
#'   `PlotSurvey()` (see `Season`).
#' @param Season Integer. For seasonal data (`object@Seasons > 1`), restrict
#'   `PlotCPUE()`/`PlotSurvey()` to a single season (1 = the first timestep
#'   of each year), giving one snapshot per year instead of the full
#'   sub-annual series. Default `NULL` (no filtering). Ignored for
#'   non-seasonal data and by `PlotLandings()`/`PlotDiscards()`/
#'   `PlotEffort()` (see `AggregateYear`).
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
#' @return A `ggplot` object (a `patchwork` object for `PlotData()`), or
#'   `NULL` invisibly if the relevant slot isn't populated.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
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
#' }
#'
#' @name plot_data
#' @seealso [plot_hist], [Landings()], [Discards()], [CPUE()], [Survey()]
#' @include class-data.R
NULL

#' @rdname plot_data
#' @export
PlotCPUE <- function(object, byFleet = NULL, Season = NULL, units = TRUE) {
  .CheckClass(object, 'data', 'object')
  .PlotDataTs(object, 'CPUE', 'CPUE', byFleet = byFleet, Season = Season, units = units)
}

#' @rdname plot_data
#' @export
PlotSurvey <- function(object, byFleet = NULL, Season = NULL, units = TRUE) {
  .CheckClass(object, 'data', 'object')
  .PlotDataTs(object, 'Survey', 'Survey', byFleet = byFleet, Season = Season, units = units)
}

#' @rdname plot_data
#' @export
PlotData <- function(object, byFleet = NULL, AggregateYear = FALSE, Season = NULL, units = TRUE, showDiscards = FALSE) {
  .CheckClass(object, 'data', 'object')

  discardsVal      <- object@Discards@Value
  discardsAllZero  <- !is.null(discardsVal) &&
    any(!is.na(discardsVal)) && all(discardsVal == 0 | is.na(discardsVal))

  specs <- list(
    list(slot = 'Landings', ylab = 'Landings', AggregateYear = AggregateYear, units = units),
    list(slot = 'Discards', ylab = 'Discards', AggregateYear = AggregateYear, units = units,
         skip = discardsAllZero && !showDiscards),
    list(slot = 'Effort',   ylab = 'Effort',   AggregateYear = AggregateYear),
    list(slot = 'CPUE',     ylab = 'CPUE',     Season = Season, units = units),
    list(slot = 'Survey',   ylab = 'Survey',   Season = Season, units = units)
  )

  panels <- purrr::map(specs, function(s) {
    if (isTRUE(s$skip)) return(NULL)
    .PlotDataTs(object, s$slot, s$ylab, byFleet = byFleet,
                 AggregateYear = s$AggregateYear %||% FALSE,
                 Season = s$Season %||% NULL, units = s$units %||% FALSE, silent = TRUE)
  })
  panels <- purrr::compact(panels)

  if (!length(panels)) {
    cli::cli_alert_info("No populated timeseries data (Landings/Discards/Effort/CPUE/Survey) found in this `data` object.")
    return(invisible(NULL))
  }

  legendFleets <- purrr::map(specs, function(s) {
    if (isTRUE(s$skip)) return(NULL)
    .ColorLegendFleets(object, s$slot, byFleet)
  })
  legendFleets <- purrr::compact(legendFleets)
  collectGuides <- length(legendFleets) <= 1 ||
    all(purrr::map_lgl(legendFleets[-1], setequal, legendFleets[[1]]))

  p <- patchwork::wrap_plots(panels, ncol = 2)
  if (collectGuides)
    p <- p + patchwork::plot_layout(guides = 'collect')
  p
}

#' @rdname plot_data
#' @export
setMethod('plot', 'data', function(x, y, ...) {
  PlotData(x, ...)
})

# ---- internal helpers ----

.PlotDataTs <- function(object, slot_name, ylab, byFleet = NULL,
                         AggregateYear = FALSE, Season = NULL, units = FALSE,
                         silent = FALSE) {
  value_arr <- slot(object, slot_name)@Value

  if (is.null(value_arr)) {
    if (!silent)
      cli::cli_alert_info("No {.field {slot_name}} data found in this `data` object; nothing to plot.")
    return(invisible(NULL))
  }

  if (!isFALSE(units)) {.
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

  .BuildDataTsPlot(value_arr, ylab, object = object, byFleet = byFleet,
                      AggregateYear = AggregateYear, Season = Season)
}

.PlotDataRemovals <- function(object, byFleet = NULL, AggregateYear = FALSE,
                               units = TRUE, silent = FALSE) {
  L <- object@Landings@Value
  D <- object@Discards@Value

  if (is.null(L) && is.null(D)) {
    if (!silent)
      cli::cli_alert_info("No {.field Landings} or {.field Discards} data found in this `data` object; nothing to plot.")
    return(invisible(NULL))
  }

  value_arr <- if (is.null(L)) D else if (is.null(D)) L else ArraySum(L, D)
  ylab <- 'Removals'

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

  .BuildDataTsPlot(value_arr, ylab, object = object, byFleet = byFleet,
                    AggregateYear = AggregateYear, Season = NULL)
}

.ColorLegendFleets <- function(object, slot_name, byFleet) {
  if (!is.null(byFleet)) return(NULL)
  value_arr <- slot(object, slot_name)@Value
  if (is.null(value_arr)) return(NULL)
  dn <- dimnames(value_arr)
  if (is.null(dn) || !'Fleet' %in% names(dn)) return(NULL)
  fleets <- unique(dn$Fleet)
  if (length(fleets) <= 1) return(NULL)
  fleets
}

.FilterSeasonData <- function(df, Season, object) {
  if (is.null(Season))
    return(df)

  nSeason <- object@Seasons
  if (is.null(nSeason) || nSeason <= 1)
    return(df)

  all_years  <- object@Years
  season_idx <- ((seq_along(all_years) - 1) %% nSeason) + 1
  keep_years <- all_years[season_idx == Season]

  dplyr::filter(df, .data$Year %in% keep_years)
}

.AggregateYearData <- function(df, AggregateYear, object) {
  if (!isTRUE(AggregateYear))
    return(df)

  nSeason <- object@Seasons
  if (is.null(nSeason) || nSeason <= 1)
    return(df)

  df$Year <- floor(df$Year)

  cnames     <- colnames(df)
  group_vars <- cnames[!cnames %in% 'Value']
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(Value = sum(.data$Value), .groups = 'drop')
}

.IsolatedRows <- function(df) {
  groupCol <- if ('Fleet' %in% colnames(df)) 'Fleet' else NULL

  df <- df |> dplyr::arrange(dplyr::across(dplyr::any_of(c(groupCol, 'Year'))))
  if (!is.null(groupCol))
    df <- df |> dplyr::group_by(dplyr::across(dplyr::all_of(groupCol)))

  df |>
    dplyr::filter(
      !is.na(.data$Value),
      dplyr::row_number() == 1 | is.na(dplyr::lag(.data$Value)),
      dplyr::row_number() == dplyr::n() | is.na(dplyr::lead(.data$Value))
    ) |>
    dplyr::ungroup()
}

.BuildDataTsPlot <- function(value_arr, ylab, object = NULL, byFleet = NULL,
                               AggregateYear = FALSE, Season = NULL) {
  df <- Array2DF(value_arr)

  if (!is.null(object)) {
    df <- df |>
      .FilterSeasonData(Season, object) |>
      .AggregateYearData(AggregateYear, object)
  }

  multiFleet <- length(unique(df$Fleet)) > 1
  mode <- if (!multiFleet) 'single'
          else if (isTRUE(byFleet))  'facet'
          else if (isFALSE(byFleet)) 'sum'
          else 'color'

  if (mode == 'sum')
    df <- .SumOverFleet(df)

  hasColor <- mode == 'color'

  mapping <- ggplot2::aes(x = .data$Year, y = .data$Value)
  if (hasColor)
    mapping <- utils::modifyList(mapping, ggplot2::aes(color = .data$Fleet))

  isolated <- .IsolatedRows(df)

  p <- ggplot2::ggplot(df, mapping) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(data = isolated, size = 1.2, na.rm = TRUE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .YearLabels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab, color = if (hasColor) 'Fleet' else NULL)

  if (hasColor) {
    fleetValues <- stats::setNames(.GgHuePal(length(unique(df$Fleet))), levels(df$Fleet))
    p <- p + ggplot2::scale_color_manual(values = fleetValues)
  }

  if (mode == 'facet')
    p <- p + ggplot2::facet_wrap(~Fleet, scales = 'free_y')

  p
}
