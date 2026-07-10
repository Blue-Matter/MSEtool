#' Plot Observed Fishery Data
#'
#' Plotting functions for [data-class] objects: the observed (or simulated,
#' with observation error) fishery timeseries, as opposed to the underlying
#' simulated dynamics plotted by [plot_hist]. A [data-class] object holds a
#' single simulation/stock realization (no `Sim` dimension), so these
#' functions draw a single line per fleet or index rather than a
#' median/ribbon summary.
#'
#' `PlotEffort()`, `PlotCPUE()`, and `PlotSurvey()` plot the `Effort`, `CPUE`,
#' and `Survey` slots respectively. `PlotLandings()` and `PlotDiscards()`
#' (see [plot_hist]) also accept a [data-class] object and are documented
#' here for that case.
#'
#' If the relevant slot is not populated (`NULL`, e.g. no survey configured
#' for this OM), the function prints an informational message and returns
#' `NULL` invisibly rather than plotting an empty panel.
#'
#' `PlotData()` arranges whichever of `Landings`, `Discards`, `Effort`,
#' `CPUE`, and `Survey` are populated into a single figure with
#' [patchwork::wrap_plots()], silently omitting panels for slots that
#' aren't; unlike the individual `Plot*()` functions it does not print a
#' message for missing slots. If none of the five are populated, it prints
#' a message and returns `NULL` invisibly instead of an empty figure.
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
PlotEffort <- function(object, byFleet = NULL, AggregateYear = FALSE) {
  CheckClass(object, 'data', 'object')
  .plot_data_ts(object, 'Effort', 'Effort', byFleet = byFleet,
               AggregateYear = AggregateYear)
}

#' @rdname plot_data
#' @export
PlotCPUE <- function(object, byFleet = NULL, Season = NULL) {
  CheckClass(object, 'data', 'object')
  .plot_data_ts(object, 'CPUE', 'CPUE', byFleet = byFleet, Season = Season)
}

#' @rdname plot_data
#' @export
PlotSurvey <- function(object, byFleet = NULL, Season = NULL) {
  CheckClass(object, 'data', 'object')
  .plot_data_ts(object, 'Survey', 'Survey', byFleet = byFleet, Season = Season)
}

#' @rdname plot_data
#' @export
PlotData <- function(object, byFleet = NULL, AggregateYear = FALSE, Season = NULL) {
  CheckClass(object, 'data', 'object')

  specs <- list(
    list(slot = 'Landings', ylab = 'Landings', AggregateYear = AggregateYear),
    list(slot = 'Discards', ylab = 'Discards', AggregateYear = AggregateYear),
    list(slot = 'Effort',   ylab = 'Effort',   AggregateYear = AggregateYear),
    list(slot = 'CPUE',     ylab = 'CPUE',     Season = Season),
    list(slot = 'Survey',   ylab = 'Survey',   Season = Season)
  )

  panels <- purrr::map(specs, function(s)
    .plot_data_ts(object, s$slot, s$ylab, byFleet = byFleet,
                 AggregateYear = s$AggregateYear %||% FALSE,
                 Season = s$Season %||% NULL, silent = TRUE)
  )
  panels <- purrr::compact(panels)

  if (!length(panels)) {
    cli::cli_alert_info("No populated timeseries data (Landings/Discards/Effort/CPUE/Survey) found in this `data` object.")
    return(invisible(NULL))
  }

  patchwork::wrap_plots(panels, ncol = 2)
}

# ---- internal helpers ----

# Extract slot(object, slot_name)@Value and build a simple line plot, or
# skip (returning NULL invisibly) if it isn't populated. `silent` suppresses
# the "not populated" message, used when called from PlotData()'s composite
# so a handful of missing slots doesn't print a wall of messages.
.plot_data_ts <- function(object, slot_name, ylab, byFleet = NULL,
                         AggregateYear = FALSE, Season = NULL, silent = FALSE) {
  value_arr <- slot(object, slot_name)@Value

  if (is.null(value_arr)) {
    if (!silent)
      cli::cli_alert_info("No {.field {slot_name}} data found in this `data` object; nothing to plot.")
    return(invisible(NULL))
  }

  .build_data_ts_plot(value_arr, ylab, object = object, byFleet = byFleet,
                      AggregateYear = AggregateYear, Season = Season)
}

# Positional season filter for `data`-class objects, mirroring
# `.filter_season()` in plot-hist.R but reading `Seasons`/`Years` directly
# off the `data` object instead of via `object@OM@Seasons`.
.filter_season_data <- function(df, Season, object) {
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

# Sum sub-annual (seasonal) rows into whole-year totals for `data`-class flow
# variables (Landings/Discards/Effort), mirroring `.aggregate_year()` in
# plot-hist.R.
.aggregate_year_data <- function(df, AggregateYear, object) {
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

# Rows whose value has no non-NA neighbor (within its Fleet, if present) on
# either side -- `geom_line()` can't draw a segment through these, so they
# need an explicit point marker to stay visible. Runs of >= 2 consecutive
# non-NA values are left as line-only (no markers).
.isolated_rows <- function(df) {
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

.build_data_ts_plot <- function(value_arr, ylab, object = NULL, byFleet = NULL,
                               AggregateYear = FALSE, Season = NULL) {
  df <- Array2DF(value_arr)

  if (!is.null(object)) {
    df <- df |>
      .filter_season_data(Season, object) |>
      .aggregate_year_data(AggregateYear, object)
  }

  multiFleet <- length(unique(df$Fleet)) > 1
  mode <- if (!multiFleet) 'single'
          else if (isTRUE(byFleet))  'facet'
          else if (isFALSE(byFleet)) 'sum'
          else 'color'

  if (mode == 'sum')
    df <- .sum_over_fleet(df)

  hasColor <- mode == 'color'

  mapping <- ggplot2::aes(x = .data$Year, y = .data$Value)
  if (hasColor)
    mapping <- utils::modifyList(mapping, ggplot2::aes(color = .data$Fleet))

  isolated <- .isolated_rows(df)

  p <- ggplot2::ggplot(df, mapping) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(data = isolated, size = 1.2, na.rm = TRUE) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .year_labels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab, color = if (hasColor) 'Fleet' else NULL)

  # `Fleet` from Array2DF() is always an ordered factor, which makes ggplot2
  # fall back to its viridis-based ordinal scale unless overridden explicitly.
  if (hasColor) {
    fleetValues <- stats::setNames(.gg_hue_pal(length(unique(df$Fleet))), levels(df$Fleet))
    p <- p + ggplot2::scale_color_manual(values = fleetValues)
  }

  if (mode == 'facet')
    p <- p + ggplot2::facet_wrap(~Fleet, scales = 'free_y')

  p
}
