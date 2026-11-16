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
#' For seasonal data (`object@Seasons > 1`), every function aggregates to
#' calendar years by default (catch and effort summed, indices averaged; see
#' `AggregateYear`) and prints a note pointing to `AggregateYear = FALSE`
#' and `Season` for plotting by season.
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
#' @seealso [plot_hist], [Landings()], [Discards()], [CPUE()], [Survey()]
#' @include class-data.R
NULL

#' @rdname plot_data
#' @export
PlotCPUE <- function(object, byFleet = NULL, Season = NULL, units = TRUE,
                     AggregateYear = NULL) {
  .CheckClass(object, 'data', 'object')
  .PlotDataTs(object, 'CPUE', 'CPUE', byFleet = byFleet, AggregateYear = AggregateYear,
              Season = .SlotSeason(Season, 'CPUE'), units = units)
}

#' @rdname plot_data
#' @export
PlotSurvey <- function(object, byFleet = NULL, Season = NULL, units = TRUE,
                       AggregateYear = NULL) {
  .CheckClass(object, 'data', 'object')
  .PlotDataTs(object, 'Survey', 'Survey', byFleet = byFleet, AggregateYear = AggregateYear,
              Season = .SlotSeason(Season, 'Survey'), units = units)
}

#' @rdname plot_data
#' @export
PlotData <- function(object, byFleet = NULL, AggregateYear = NULL, Season = NULL, units = TRUE, showDiscards = FALSE) {
  .CheckClass(object, 'data', 'object')

  # aggregate once here so the seasonal notes print once, not once per panel;
  # panels then get the resolved per-index seasons
  sp     <- .SeasonalPlotData(object, AggregateYear, Season)
  object <- sp$object

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
    .PlotDataTs(object, s$slot, s$ylab, byFleet = byFleet, AggregateYear = FALSE,
                 Season = sp$Season, units = s$units %||NA% FALSE, silent = TRUE,
                 colors = fleetColors)
  })
  panels <- purrr::compact(panels)

  if (!length(panels)) {
    cli::cli_alert_info("No populated timeseries data (Landings/Discards/Effort/CPUE/Survey) found in this `data` object.")
    return(invisible(NULL))
  }

  legendFleets <- purrr::map(specs, \(s) .ColorLegendFleets(object, s$slot, byFleet))
  legendFleets <- purrr::compact(legendFleets)
  collectGuides <- length(legendFleets) <= 1 ||
    all(purrr::map_lgl(legendFleets[-1], setequal, legendFleets[[1]]))

  p <- patchwork::wrap_plots(panels, ncol = 2)
  if (collectGuides)
    p <- p + patchwork::plot_layout(guides = 'collect')
  p
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

.IndexSlots <- c('CPUE', 'Survey')

.PlotDataTs <- function(object, slot_name, ylab, byFleet = NULL,
                         AggregateYear = NULL, Season = NULL, units = FALSE,
                         silent = FALSE, colors = NULL) {
  isIndex <- slot_name %in% .IndexSlots
  # indices are never summed: different indices have different scales/units
  if (isIndex && isFALSE(byFleet)) byFleet <- NULL

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

  .BuildDataTsPlot(value_arr, ylab, object = object, byFleet = byFleet,
                   seasons = sp$Season[[slot_name]], colors = colors,
                   colorLab = if (isIndex) 'Index' else 'Fleet')
}

.PlotDataRemovals <- function(object, byFleet = NULL, AggregateYear = NULL,
                               units = TRUE, silent = FALSE) {
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

  .BuildDataTsPlot(value_arr, ylab, object = object, byFleet = byFleet)
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
  fleets <- unique(.DataSeriesNames(object, slot_name))
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
                               seasons = NULL, colors = NULL, colorLab = 'Fleet') {
  # index arrays may name their second dimension e.g. `Index`; treat it as
  # the series dimension regardless of name
  dn <- dimnames(value_arr)
  if (length(dim(value_arr)) == 2 && !is.null(dn)) {
    dn[[2]] <- dn[[2]] %||% paste('Series', seq_len(ncol(value_arr)))
    names(dn) <- c(names(dn)[1] %||% 'Year', 'Fleet')
    if (!nzchar(names(dn)[1])) names(dn)[1] <- 'Year'
    dimnames(value_arr) <- dn
  }
  df <- Array2DF(value_arr)

  if (!is.null(object))
    df <- .FilterSeasonData(df, seasons, object)

  multiFleet <- 'Fleet' %in% colnames(df) && length(unique(df$Fleet)) > 1
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
    ggplot2::labs(x = 'Year', y = ylab, color = if (hasColor) colorLab else NULL)

  if (hasColor) {
    fleetLevels <- levels(df$Fleet) %||% unique(as.character(df$Fleet))
    fleetValues <- if (!is.null(colors) && all(fleetLevels %in% names(colors)))
                     colors[fleetLevels]
                   else stats::setNames(.GgHuePal(length(fleetLevels)), fleetLevels)
    p <- p + ggplot2::scale_color_manual(values = fleetValues)
  }

  if (mode == 'facet')
    p <- p + ggplot2::facet_wrap(~Fleet, scales = 'free_y')

  p
}
