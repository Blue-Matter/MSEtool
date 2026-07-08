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
#' }
#'
#' @name plot_data
#' @seealso [plot_hist], [Landings()], [Discards()], [CPUE()], [Survey()]
#' @include class-data.R
NULL

#' @rdname plot_data
#' @export
PlotEffort <- function(object) {
  CheckClass(object, 'data', 'object')
  .plot_data_ts(object, 'Effort', 'Effort')
}

#' @rdname plot_data
#' @export
PlotCPUE <- function(object) {
  CheckClass(object, 'data', 'object')
  .plot_data_ts(object, 'CPUE', 'CPUE')
}

#' @rdname plot_data
#' @export
PlotSurvey <- function(object) {
  CheckClass(object, 'data', 'object')
  .plot_data_ts(object, 'Survey', 'Survey')
}

#' @rdname plot_data
#' @export
PlotData <- function(object) {
  CheckClass(object, 'data', 'object')

  specs <- list(
    list(slot = 'Landings', ylab = 'Landings'),
    list(slot = 'Discards', ylab = 'Discards'),
    list(slot = 'Effort',   ylab = 'Effort'),
    list(slot = 'CPUE',     ylab = 'CPUE'),
    list(slot = 'Survey',   ylab = 'Survey')
  )

  panels <- purrr::map(specs, function(s)
    .plot_data_ts(object, s$slot, s$ylab, silent = TRUE)
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
.plot_data_ts <- function(object, slot_name, ylab, silent = FALSE) {
  value_arr <- slot(object, slot_name)@Value

  if (is.null(value_arr)) {
    if (!silent)
      cli::cli_alert_info("No {.field {slot_name}} data found in this `data` object; nothing to plot.")
    return(invisible(NULL))
  }

  .build_data_ts_plot(value_arr, ylab)
}

.build_data_ts_plot <- function(value_arr, ylab) {
  df <- Array2DF(value_arr)

  nFleet <- length(unique(df$Fleet))
  hasFleet <- nFleet > 1

  mapping <- ggplot2::aes(x = .data$Year, y = .data$Value)
  if (hasFleet)
    mapping <- utils::modifyList(mapping, ggplot2::aes(color = .data$Fleet))

  p <- ggplot2::ggplot(df, mapping) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .year_labels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab, color = if (hasFleet) 'Fleet' else NULL)

  # `Fleet` from Array2DF() is always an ordered factor, which makes ggplot2
  # fall back to its viridis-based ordinal scale unless overridden explicitly.
  if (hasFleet) {
    fleetValues <- stats::setNames(.gg_hue_pal(nFleet), levels(df$Fleet))
    p <- p + ggplot2::scale_color_manual(values = fleetValues)
  }

  p
}
