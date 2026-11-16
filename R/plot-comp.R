#' Plot Age and Size Composition Data
#'
#' Bubble plots of catch-at-age or catch-at-size composition: `Year` on the
#' x-axis, `Age`/size `Class` on the y-axis, and bubble area proportional to
#' the proportion of that year's catch falling in that age/class. `PlotLandingsAtAge()`,
#' `PlotDiscardsAtAge()`, `PlotLandingsAtSize()`, and `PlotDiscardsAtSize()`
#' plot the [Landings()]/[Discards()] `byAge`/`bySize` composition data for a
#' [hist-class], [mse-class], or [data-class] object.
#'
#' @param object A [hist-class], [mse-class], or [data-class] object.
#'   `Sim`, `Years`, `IncHist`, and `Stocks` have no effect for [data-class]
#'   objects, which hold a single (stock-complex, simulation) realization.
#' @param byStock One of `TRUE`, `FALSE`, or `NULL` (default). `TRUE` facets
#'   by stock. `FALSE` sums counts across stocks before computing
#'   proportions; for `*AtSize()`, this raises an error if stocks don't all
#'   share the exact same size-class grid (see [compdata-class]). `NULL`
#'   facets automatically when `object` has more than one (selected) stock.
#'   No effect for [data-class] objects (single stock complex).
#' @param byFleet One of `TRUE`, `FALSE`, or `NULL` (default). `TRUE` facets
#'   by fleet. `FALSE` sums counts across fleets before computing
#'   proportions, via [Landings()]/[Discards()]; for `*AtSize()`, this
#'   raises an error if fleets don't all share the exact same size-class
#'   grid, since fleets are not required to (see [compdata-class]). `NULL`
#'   facets automatically when `object` has more than one fleet.
#' @param Sim Integer or `NULL` (default). For [hist-class]/[mse-class]
#'   objects, which simulation replicate to plot. `NULL` takes the median
#'   proportion across all simulations (computed per simulation, then
#'   summarised), rather than the median of raw counts.
#' @param Years Optional numeric vector. Subset the time series to these
#'   years before plotting.
#' @param IncHist Logical. For [mse-class] objects, include the historical
#'   period? Default `TRUE`. No effect for [hist-class] objects.
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param AggregateYear Logical or `NULL`. For a seasonal [data-class]
#'   object (`object@Seasons > 1`), whether to sum the composition counts
#'   over the seasons of each calendar year (via [AnnualData()]) before
#'   computing proportions. `NULL` (default) aggregates and prints a note on
#'   how to plot by season instead; `TRUE` aggregates without the note;
#'   `FALSE` plots every seasonal time step. No effect for non-seasonal data
#'   or for [hist-class]/[mse-class] objects.
#'
#' @details
#' Proportions are computed within each `Year` (and `Stock`/`Fleet`/`MP`
#' panel), i.e. each year's bubbles sum to 1 within a panel, making years
#' with very different total catch comparable and cohorts easier to track
#' visually. For [hist-class]/[mse-class] objects with `Sim = NULL`,
#' proportions are computed per simulation and then summarised with the
#' median across simulations, cell by cell -- not the proportion of median
#' counts.
#'
#' Panels are faceted by whichever of `Stock`, `Fleet`, and `MP` have more
#' than one level after filtering (`MP` always facets when [mse-class]
#' objects have more than one MP -- there is no color channel free for it,
#' since bubble size already encodes proportion).
#'
#' If the relevant `*AtAge`/`*AtSize` data is not populated for a
#' [data-class] object, or every value is zero or missing, the function
#' prints an informational message and returns `NULL` invisibly rather than
#' plotting an empty panel. Years (per fleet) with no catch sampled are
#' omitted. For a [data-class] object the plot is titled with the slot
#' (e.g. "Landings at Age"), and the y-axis label includes the slot's
#' `Units` (e.g. "Age (years)") when set.
#'
#' @return A `ggplot` object, or `NULL` invisibly if the relevant
#'   [data-class] slot isn't populated.
#'
#' @examples
#' Hist <- Simulate(SingleStockOM, nSim = 2, control = SimControl(CalcCatchAtSize = TRUE))
#' PlotLandingsAtAge(Hist)
#' PlotLandingsAtSize(Hist)
#' PlotLandingsAtSize(Hist, Sim = 2)
#'
#' MSE <- Project(Hist, ExampleMPs())
#' PlotDiscardsAtSize(MSE)
#'
#' dat <- Hist@Data[[1]][[1]]
#' PlotLandingsAtSize(dat)
#'
#' @name plot_comp
#' @seealso [Landings()], [Discards()], [compdata-class]
NULL

#' @rdname plot_comp
#' @export
PlotLandingsAtAge <- function(object, byStock = NULL, byFleet = NULL, Sim = NULL,
                              Years = NULL, IncHist = TRUE, Stocks = NULL,
                              AggregateYear = NULL) {
  .PlotCompBubble(object, 'LandingsAtAge', 'Age',
                    byStock, byFleet, Sim, Years, IncHist, Stocks, AggregateYear)
}

#' @rdname plot_comp
#' @export
PlotDiscardsAtAge <- function(object, byStock = NULL, byFleet = NULL, Sim = NULL,
                              Years = NULL, IncHist = TRUE, Stocks = NULL,
                              AggregateYear = NULL) {
  .PlotCompBubble(object, 'DiscardsAtAge', 'Age',
                    byStock, byFleet, Sim, Years, IncHist, Stocks, AggregateYear)
}

#' @rdname plot_comp
#' @export
PlotLandingsAtSize <- function(object, byStock = NULL, byFleet = NULL, Sim = NULL,
                               Years = NULL, IncHist = TRUE, Stocks = NULL,
                               AggregateYear = NULL) {
  .PlotCompBubble(object, 'LandingsAtSize', 'Size Class',
                    byStock, byFleet, Sim, Years, IncHist, Stocks, AggregateYear)
}

#' @rdname plot_comp
#' @export
PlotDiscardsAtSize <- function(object, byStock = NULL, byFleet = NULL, Sim = NULL,
                               Years = NULL, IncHist = TRUE, Stocks = NULL,
                               AggregateYear = NULL) {
  .PlotCompBubble(object, 'DiscardsAtSize', 'Size Class',
                    byStock, byFleet, Sim, Years, IncHist, Stocks, AggregateYear)
}

# ---- internal helpers ----

.PlotCompBubble <- function(object, what, ylab, byStock, byFleet, Sim,
                             Years, IncHist, Stocks, AggregateYear = NULL) {
  if (inherits(object, 'data'))
    return(.PlotDataCompBubble(object, what, ylab, byFleet, AggregateYear))

  .CheckClass(object, c('hist', 'mse'), 'object')
  isAtSize <- grepl('AtSize$', what)
  classCol <- if (isAtSize) 'Class' else 'Age'
  fn       <- if (grepl('^Landings', what)) 'Landings' else 'Discards'

  stockNames <- .ResolveStocks(object, Stocks)
  byStockRes <- .ResolveByStock(byStock, .NSelStock(object, stockNames))
  if (identical(byStockRes, 'sum')) byStockRes <- FALSE
  byFleetRes <- if (is.null(byFleet)) nFleet(object) > 1 else byFleet

  extractArgs <- list(object = object, df = TRUE, byFleet = byFleetRes)
  extractArgs[[if (isAtSize) 'bySize' else 'byAge']] <- TRUE
  df <- do.call(fn, extractArgs)

  df <- df |>
    .FilterStock(stockNames) |>
    .DropHistorical(IncHist) |>
    .FilterYears(Years)

  if (!byStockRes) {
    if (isAtSize) .CheckGridMatch(df, what, 'Stock')
    df <- .SumOverStock(df)
  }

  hasSim <- 'Sim' %in% colnames(df)
  if (!is.null(Sim))
    df <- .DFSubsetSim(df, Sim, keep_sim_name = TRUE)

  propGroupVars <- intersect(c('Sim', 'Stock', 'Fleet', 'MP', 'Year'), colnames(df))
  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(propGroupVars))) |>
    dplyr::mutate(Proportion = .data$Value / sum(.data$Value, na.rm = TRUE)) |>
    dplyr::ungroup()

  if (hasSim && is.null(Sim)) {
    sumGroupVars <- c(setdiff(propGroupVars, 'Sim'), classCol)
    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(sumGroupVars))) |>
      dplyr::summarise(Proportion = stats::median(.data$Proportion, na.rm = TRUE), .groups = 'drop')
  }

  .BuildCompBubblePlot(df, classCol, ylab)
}

# `silent` suppresses the seasonal note and the 'nothing to plot' message
# (PlotData() omits such panels quietly). `gridFallback = TRUE` keeps fleets
# separate, with a message, when `byFleet = FALSE` can't sum size
# compositions because fleets use different size-class grids, instead of
# raising an error.
.PlotDataCompBubble <- function(object, what, ylab, byFleet, AggregateYear = NULL,
                                silent = FALSE, gridFallback = FALSE) {
  .CheckClass(object, 'data', 'object')
  comp     <- slot(object, what)
  isAtSize <- grepl('AtSize$', what)
  classCol <- if (isAtSize) 'Class' else 'Age'
  fn       <- if (grepl('^Landings', what)) 'Landings' else 'Discards'

  # all-NA or all-zero compositions have no proportions to plot
  if (EmptyObject(comp) || !any(!is.na(comp@Value) & comp@Value > 0)) {
    if (!silent) {
      if (EmptyObject(comp))
        cli::cli_alert_info("No {.field {what}} data found in this `data` object; nothing to plot.")
      else
        cli::cli_alert_info("Every {.field {what}} value is zero or missing; nothing to plot.")
    }
    return(invisible(NULL))
  }

  object <- .SeasonalPlotData(object, AggregateYear, silent = silent,
                              sources = character(0))$object

  byFleetRes  <- if (is.null(byFleet)) nFleet(object) > 1 else byFleet
  extract <- function(byFleet) {
    extractArgs <- list(object = object, byFleet = byFleet)
    extractArgs[[if (isAtSize) 'bySize' else 'byAge']] <- TRUE
    do.call(fn, extractArgs)
  }
  df <- if (gridFallback && !byFleetRes && isAtSize) {
    tryCatch(extract(FALSE), error = function(e) {
      if (!grepl('size-class grid', conditionMessage(e))) stop(e)
      cli::cli_inform(c("i" = "{.field {what}}: fleets use different size-class grids, so they're shown separately instead of summed."))
      extract(TRUE)
    })
  } else extract(byFleetRes)

  propGroupVars <- intersect(c('Fleet', 'Year'), colnames(df))
  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(propGroupVars))) |>
    dplyr::mutate(Proportion = .data$Value / sum(.data$Value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    # years (per fleet) with no catch sampled have no proportions
    dplyr::filter(is.finite(.data$Proportion))

  units <- unique(stats::na.omit(comp@Units))
  if (length(units) == 1) ylab <- .AppendUnits(ylab, units)
  title <- paste(fn, if (isAtSize) 'at Size' else 'at Age')

  .BuildCompBubblePlot(df, classCol, ylab, title = title)
}

# Checks that every group sharing `groupVar` (e.g. every Stock, for a given
# Fleet) uses the exact same class grid before they get summed together --
# fleets/stocks are not required to share one (see compdata-class), so
# summing across a mismatched grid would silently combine incompatible
# bins. Only meaningful for *AtSize; ages are always shared.
.CheckGridMatch <- function(df, what, groupVar) {
  if (!'Class' %in% colnames(df) || !groupVar %in% colnames(df))
    return(invisible(NULL))

  byVars <- intersect(c('Fleet', 'MP'), colnames(df))
  grids <- df |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(byVars, groupVar, 'Class')))) |>
    dplyr::arrange(.data$Class) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(byVars, groupVar)))) |>
    dplyr::summarise(grid = paste(.data$Class, collapse = ','), .groups = 'drop')

  mismatched <- grids |>
    dplyr::group_by(dplyr::across(dplyr::all_of(byVars))) |>
    dplyr::summarise(ok = dplyr::n_distinct(.data$grid) == 1, .groups = 'drop')

  if (any(!mismatched$ok))
    cli::cli_abort(c(
      "Cannot sum {.field {what}} across {tolower(groupVar)}s with `by{groupVar} = FALSE`.",
      "x" = "{groupVar}s do not share the same size-class grid.",
      "i" = "Set `by{groupVar} = TRUE` to keep {tolower(groupVar)}s separate instead."
    ), call = NULL)
}

.BuildCompBubblePlot <- function(df, classCol, ylab, title = NULL) {
  facetVars <- intersect(c('Stock', 'Fleet', 'MP'), colnames(df))
  facetVars <- facetVars[purrr::map_lgl(facetVars, \(v) length(unique(df[[v]])) > 1)]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Year, y = .data[[classCol]],
                                        size = .data$Proportion)) +
    ggplot2::geom_point(shape = 21, fill = 'steelblue', color = 'grey20',
                        alpha = 0.7, stroke = 0.2, na.rm = TRUE) +
    ggplot2::scale_size_area(max_size = 8) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .YearLabels) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab, size = 'Proportion', title = title)

  if (length(facetVars))
    p <- p + ggplot2::facet_wrap(facetVars)

  p
}
