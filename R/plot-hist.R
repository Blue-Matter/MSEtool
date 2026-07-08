#' Plot Historical and Projected Dynamics
#'
#' Plotting functions for [hist-class] and [mse-class] objects, summarizing
#' simulated numbers, biomass, spawning biomass, spawning production,
#' landings, discards, and removals over time.
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param byStock Logical. Facet by stock? Default (`NULL`) facets
#'   automatically when `object` has more than one stock.
#' @param byFleet Logical. Facet by fleet? Default (`NULL`) facets
#'   automatically when `object` has more than one fleet, except in
#'   `PlotDynamics()`/`plot()` where it defaults to `FALSE`. Only applies to
#'   `PlotLandings()`/`PlotDiscards()`/`PlotRemovals()`.
#' @param byFemale Logical. See Details. Only applies to
#'   `PlotSBiomass()`/`PlotSProduction()`.
#' @param byMP Logical. For [mse-class] objects with more than one MP, facet
#'   by MP instead of coloring by MP. Only takes effect when `byStock` and
#'   `byFleet` are not *both* already faceting (i.e. there is a free facet
#'   dimension); otherwise MP falls back to color. Default `FALSE`.
#' @param probs Numeric vector of length 2. Lower and upper quantiles of the
#'   shaded ribbon across simulations. Default `c(0.05, 0.95)`.
#' @param nsim Integer. Number of individual simulation replicates to
#'   overlay as thin lines, in addition to the median/ribbon. Default `0`
#'   (none).
#' @param Years Optional numeric vector. Subset the time series to these
#'   years before plotting.
#' @param free_y Logical. When faceting by stock and/or fleet, let each
#'   panel's y-axis scale independently. Default `TRUE`.
#' @param IncHist Logical. For [mse-class] objects, include the historical
#'   period? `FALSE` is a shortcut for subsetting `Years` to the projection
#'   period only, while still starting each MP's line from the last
#'   historical value. Default `TRUE`. No effect for [hist-class] objects.
#'
#' @details
#' Each function extracts the relevant time series with the corresponding
#' accessor ([Number()], [Biomass()], [SBiomass()], [SProduction()],
#' [Landings()], [Discards()]), summarizes across simulations as a median
#' line with a shaded quantile ribbon, and returns a `ggplot` object. Set
#' `nsim` to overlay that many individual simulation replicates as thin
#' lines (the background grid is dropped in this case so the replicates
#' stand out).
#'
#' For [mse-class] objects, each MP is drawn in its own color on the same
#' panel by default (historical years are shown in a fixed neutral grey);
#' set `byMP = TRUE` to facet by MP instead where there's a free facet
#' dimension (see `byMP` above). The last historical year is duplicated
#' into each MP's projection series so the line continues without a visual
#' gap at the historical/projection boundary.
#'
#' `PlotRemovals()` draws `Landings()` and `Discards()` together on the same
#' panel(s) (see [Removals()] for the summed total): for [hist-class]
#' objects they are distinguished by color; for [mse-class] objects color is
#' used for MP and linetype distinguishes Landings (solid) from Discards
#' (dashed).
#'
#' `byStock` and (for `PlotLandings()`/`PlotDiscards()`/`PlotRemovals()`)
#' `byFleet` control faceting: when both are `TRUE` panels are arranged in a
#' `Stock x Fleet` grid, matching the behavior of [Landings()]/[Discards()].
#' When `FALSE`, the corresponding dimension is summed before plotting. When
#' faceting is used, `free_y` (default `TRUE`) lets each panel's y-axis scale
#' independently, since stocks or fleets of very different magnitude (e.g. a
#' large and a small stock) are otherwise hard to compare on a shared scale.
#'
#' For `PlotSBiomass()` and `PlotSProduction()` with `byStock = FALSE` and
#' `byFemale = TRUE`, stocks within the same `OM@Complexes` group are not
#' naively summed (which would double-count male and female components of
#' the same population). Instead, within each complex, a stock whose name
#' matches `"female"` (case-insensitive) is taken as that complex's
#' contribution to the total. If a complex has more than one stock and no
#' single stock can be unambiguously identified as female, faceting by
#' stock is used instead and a message is printed.
#'
#' `PlotDynamics()` arranges `PlotNumber()`, `PlotBiomass()`,
#' `PlotSBiomass()`, `PlotSProduction()`, `PlotLandings()`, and
#' `PlotDiscards()` into a single figure with [patchwork::wrap_plots()].
#' Its own `byFleet` argument (and `plot()`'s, which dispatches to it)
#' defaults to `FALSE`, unlike the individual `Plot*()` functions; each
#' argument is only forwarded to the panels that accept it.
#'
#' @return A `ggplot` object (a `patchwork` object for `PlotDynamics()`).
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' PlotBiomass(Hist)
#' PlotLandings(Hist, nsim = 10)
#' PlotRemovals(Hist)
#'
#' MSE <- Project(Hist, ExampleMPs())
#' PlotBiomass(MSE)
#' PlotBiomass(MSE, IncHist = FALSE)
#' PlotBiomass(MSE, byMP = TRUE)
#' PlotRemovals(MSE)
#' PlotDynamics(MSE)
#'
#' MHist <- Simulate(MultiStockOM)
#' PlotSBiomass(MHist, byStock = TRUE)
#' PlotLandings(MHist, byStock = TRUE, byFleet = TRUE)
#' PlotDynamics(MHist)
#'
#' }
#'
#' @name plot_hist
#' @seealso [Number()], [Biomass()], [SBiomass()], [SProduction()],
#'   [Landings()], [Discards()], [Removals()]
#' @include class-hist.R
#' @include class-mse.R
NULL

#' @rdname plot_hist
#' @export
PlotNumber <- function(object,
                       byStock = NULL,
                       probs   = c(0.05, 0.95),
                       nsim    = 0,
                       Years   = NULL,
                       free_y  = TRUE,
                       IncHist = TRUE,
                       byMP    = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1

  df <- Number(object, df = TRUE) |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_years(Years)
  if (!byStock)
    df <- .sum_over_stock(df)

  .build_ts_plot(df, byStock = byStock, byFleet = FALSE,
                ylab = 'Number', probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP)
}

#' @rdname plot_hist
#' @export
PlotBiomass <- function(object,
                        byStock = NULL,
                        probs   = c(0.05, 0.95),
                        nsim    = 0,
                        Years   = NULL,
                        free_y  = TRUE,
                        IncHist = TRUE,
                        byMP    = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1

  df <- Biomass(object, df = TRUE) |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_years(Years)
  if (!byStock)
    df <- .sum_over_stock(df)

  .build_ts_plot(df, byStock = byStock, byFleet = FALSE,
                ylab = 'Biomass', probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP)
}

#' @rdname plot_hist
#' @export
PlotSBiomass <- function(object,
                         byStock  = NULL,
                         byFemale = TRUE,
                         probs    = c(0.05, 0.95),
                         nsim     = 0,
                         Years    = NULL,
                         free_y   = TRUE,
                         IncHist  = TRUE,
                         byMP     = FALSE) {
  .plot_spawning(object, slot_name = 'SBiomass', ylab = 'Spawning Biomass',
                byStock = byStock, byFemale = byFemale, probs = probs,
                nsim = nsim, Years = Years, free_y = free_y,
                IncHist = IncHist, byMP = byMP)
}

#' @rdname plot_hist
#' @export
PlotSProduction <- function(object,
                            byStock  = NULL,
                            byFemale = TRUE,
                            probs    = c(0.05, 0.95),
                            nsim     = 0,
                            Years    = NULL,
                            free_y   = TRUE,
                            IncHist  = TRUE,
                            byMP     = FALSE) {
  .plot_spawning(object, slot_name = 'SProduction', ylab = 'Spawning Production',
                byStock = byStock, byFemale = byFemale, probs = probs,
                nsim = nsim, Years = Years, free_y = free_y,
                IncHist = IncHist, byMP = byMP)
}

#' @rdname plot_hist
#' @export
PlotLandings <- function(object,
                         byStock = NULL,
                         byFleet = NULL,
                         probs   = c(0.05, 0.95),
                         nsim    = 0,
                         Years   = NULL,
                         free_y  = TRUE,
                         IncHist = TRUE,
                         byMP    = FALSE) {
  .plot_catch(object, slot_name = 'Landings', ylab = 'Landings',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP)
}

#' @rdname plot_hist
#' @export
PlotDiscards <- function(object,
                         byStock = NULL,
                         byFleet = NULL,
                         probs   = c(0.05, 0.95),
                         nsim    = 0,
                         Years   = NULL,
                         free_y  = TRUE,
                         IncHist = TRUE,
                         byMP    = FALSE) {
  .plot_catch(object, slot_name = 'Discards', ylab = 'Discards',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP)
}

#' @rdname plot_hist
#' @export
PlotRemovals <- function(object,
                         byStock = NULL,
                         byFleet = NULL,
                         probs   = c(0.05, 0.95),
                         nsim    = 0,
                         Years   = NULL,
                         free_y  = TRUE,
                         IncHist = TRUE,
                         byMP    = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1

  L <- Landings(object, df = TRUE, byFleet = TRUE) |> .bridge_mp_gap()
  D <- Discards(object, df = TRUE, byFleet = TRUE) |> .bridge_mp_gap()
  df <- dplyr::bind_rows(L, D) |>
    .drop_historical(IncHist) |>
    .filter_years(Years)
  df$Variable <- factor(df$Variable, levels = c('Landings', 'Discards'), ordered = TRUE)

  if (!byFleet)
    df <- .sum_over_fleet(df)
  if (!byStock)
    df <- .sum_over_stock(df)

  isMSE <- inherits(object, 'mse')
  .build_ts_plot(df, byStock = byStock, byFleet = byFleet,
                ylab = 'Removals', probs = probs, nsim = nsim,
                free_y = free_y, byMP = byMP,
                colorVar    = if (isMSE) 'MP' else 'Variable',
                linetypeVar = if (isMSE) 'Variable' else NULL)
}

#' @rdname plot_hist
#' @export
PlotDynamics <- function(object,
                         byStock  = NULL,
                         byFleet  = FALSE,
                         byFemale = TRUE,
                         byMP     = FALSE,
                         probs    = c(0.05, 0.95),
                         nsim     = 0,
                         Years    = NULL,
                         free_y   = TRUE,
                         IncHist  = TRUE) {
  CheckClass(object, c('hist', 'mse'), 'object')

  common <- list(object = object, byStock = byStock, byMP = byMP, probs = probs,
                 nsim = nsim, Years = Years, free_y = free_y, IncHist = IncHist)

  panels <- list(
    do.call(PlotNumber,      common),
    do.call(PlotBiomass,     common),
    do.call(PlotSBiomass,    c(common, list(byFemale = byFemale))),
    do.call(PlotSProduction, c(common, list(byFemale = byFemale))),
    do.call(PlotLandings,    c(common, list(byFleet = byFleet))),
    do.call(PlotDiscards,    c(common, list(byFleet = byFleet)))
  )

  patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_layout(guides = 'collect')
}

setMethod('plot', 'hist', function(x, y, ...) {
  PlotDynamics(x, ...)
})

setMethod('plot', 'mse', function(x, y, ...) {
  PlotDynamics(x, ...)
})

# ---- internal helpers ----

.filter_years <- function(df, Years) {
  if (is.null(Years))
    return(df)
  dplyr::filter(df, .data$Year %in% Years)
}

.drop_historical <- function(df, IncHist) {
  if (IncHist || !'MP' %in% colnames(df))
    return(df)
  dplyr::filter(df, .data$MP != 'Historical')
}

.sum_over_stock <- function(df) {
  cnames <- colnames(df)
  group_vars <- cnames[!cnames %in% c('Stock', 'Value')]
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(Value = sum(.data$Value), .groups = 'drop') |>
    dplyr::mutate(Stock = 'Total')
}

.sum_over_fleet <- function(df) {
  cnames <- colnames(df)
  group_vars <- cnames[!cnames %in% c('Fleet', 'Value')]
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(Value = sum(.data$Value), .groups = 'drop') |>
    dplyr::mutate(Fleet = 'Total')
}

# Identify, for each complex, the single stock (if any) that should stand in
# for the complex's spawning total: the only stock when a complex has one,
# or the unambiguous "female"-named stock when it has more than one.
.female_stock_names <- function(OM) {
  complexes   <- OM@Complexes
  stock_names <- StockNames(OM)

  keep      <- character(0)
  ambiguous <- FALSE

  for (cx in complexes) {
    nms <- stock_names[cx]
    if (length(nms) == 1) {
      keep <- c(keep, nms)
      next
    }
    female <- nms[grepl('female', nms, ignore.case = TRUE)]
    if (length(female) == 1) {
      keep <- c(keep, female)
    } else {
      ambiguous <- TRUE
    }
  }

  list(stocks = keep, ambiguous = ambiguous)
}

.plot_spawning <- function(object, slot_name, ylab, byStock, byFemale,
                          probs, nsim, Years, free_y, IncHist, byMP) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1

  df <- do.call(slot_name, list(object = object, df = TRUE)) |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_years(Years)

  if (!byStock) {
    if (byFemale && nStock(object) > 1) {
      female <- .female_stock_names(object@OM)
      if (female$ambiguous) {
        cli::cli_alert_info(
          "Cannot unambiguously identify a female stock in one or more complexes; faceting by stock instead."
        )
        byStock <- TRUE
      } else {
        df <- dplyr::filter(df, .data$Stock %in% female$stocks)
        df <- .sum_over_stock(df)
        df$Stock <- 'Total (Female)'
      }
    } else {
      df <- .sum_over_stock(df)
    }
  }

  .build_ts_plot(df, byStock = byStock, byFleet = FALSE,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP)
}

.plot_catch <- function(object, slot_name, ylab, byStock, byFleet,
                       probs, nsim, Years, free_y, IncHist, byMP) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1

  df <- do.call(slot_name, list(object = object, df = TRUE, byFleet = TRUE)) |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_years(Years)

  if (!byFleet)
    df <- .sum_over_fleet(df)
  if (!byStock)
    df <- .sum_over_stock(df)

  .build_ts_plot(df, byStock = byStock, byFleet = byFleet,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP)
}

# Duplicate the last historical year's rows into each MP's projection
# series, so the line continues without a gap at the historical/projection
# boundary.
.bridge_mp_gap <- function(df) {
  if (!'MP' %in% colnames(df))
    return(df)

  mps <- unique(df$MP)
  mps <- mps[mps != 'Historical']
  if (!length(mps))
    return(df)

  boundary_year <- max(df$Year[df$MP == 'Historical'])
  boundary_rows <- df[df$MP == 'Historical' & df$Year == boundary_year, , drop = FALSE]

  bridge <- purrr::map_dfr(mps, function(mp) {
    rows        <- boundary_rows
    rows$MP     <- mp
    rows$Period <- 'Projection'
    rows
  })

  dplyr::bind_rows(df, bridge)
}


.replicate_hist_per_mp <- function(df) {
  mps <- unique(df$MP)
  mps <- mps[mps != 'Historical']
  if (!length(mps))
    return(df)

  hist_rows <- df[df$MP == 'Historical', , drop = FALSE]
  proj_rows <- df[df$MP != 'Historical', , drop = FALSE]

  hist_dup <- purrr::map_dfr(mps, function(mp) {
    rows    <- hist_rows
    rows$MP <- mp
    rows
  })

  dplyr::bind_rows(hist_dup, proj_rows)
}

.year_labels <- function(breaks) {
  labels <- rep(NA_character_, length(breaks))
  ok     <- !is.na(breaks)
  whole  <- ok & abs(breaks - round(breaks)) < 1e-6
  labels[whole] <- format(round(breaks[whole]))

  frac <- ok & !whole
  if (any(frac))
    labels[frac] <- format(lubridate::date_decimal(breaks[frac]), '%Y-%m')

  labels
}

.gg_hue_pal <- function(n) {
  if (n < 1) return(character(0))
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
}

.mp_color_values <- function(levels) {
  other  <- levels[levels != 'Historical']
  values <- stats::setNames(.gg_hue_pal(length(other)), other)
  if ('Historical' %in% levels)
    values <- c(Historical = 'grey30', values)
  values[levels]
}

.build_ts_plot <- function(df, byStock, byFleet, ylab, probs, nsim,
                          free_y = TRUE, colorVar = NULL, linetypeVar = NULL,
                          byMP = FALSE) {

  mpVals    <- if ('MP' %in% colnames(df)) unique(df$MP) else character(0)
  nMPLevels <- length(mpVals[mpVals != 'Historical'])
  nStockRaw <- if ('Stock' %in% colnames(df)) length(unique(df$Stock)) else 1
  nFleetRaw <- if ('Fleet' %in% colnames(df)) length(unique(df$Fleet)) else 1

  doFacetMP <- isTRUE(byMP) && nMPLevels > 1 &&
    !(byStock && byFleet && nStockRaw > 1 && nFleetRaw > 1)

  if (doFacetMP) {
    df <- .replicate_hist_per_mp(df)
    colorVar <- NULL
  }

  group_vars <- colnames(df)
  group_vars <- group_vars[!group_vars %in% c('Sim', 'Value')]

  summ <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      Lower  = stats::quantile(.data$Value, min(probs)),
      Median = stats::median(.data$Value),
      Upper  = stats::quantile(.data$Value, max(probs)),
      .groups = 'drop'
    )

  hasColor <- !is.null(colorVar) && colorVar %in% colnames(summ) &&
    length(unique(summ[[colorVar]])) > 1
  hasLinetype <- !is.null(linetypeVar) && linetypeVar %in% colnames(summ) &&
    length(unique(summ[[linetypeVar]])) > 1

  groupCols  <- c(if (hasColor) colorVar, if (hasLinetype) linetypeVar)
  summ$.group <- if (length(groupCols)) interaction(summ[groupCols], drop = TRUE) else 1

  p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$Year))

  if (nsim > 0) {
    simIDs <- sort(unique(df$Sim))
    simIDs <- utils::head(simIDs, nsim)
    simdf  <- dplyr::filter(df, .data$Sim %in% simIDs)

    groupCols     <- c('Sim', if (hasColor) colorVar, if (hasLinetype) linetypeVar)
    simdf$.group  <- interaction(simdf[groupCols], drop = TRUE)

    simMapping <- ggplot2::aes(y = .data$Value, group = .data$.group)
    if (hasColor)
      simMapping <- utils::modifyList(simMapping, ggplot2::aes(color = .data[[colorVar]]))
    if (hasLinetype)
      simMapping <- utils::modifyList(simMapping, ggplot2::aes(linetype = .data[[linetypeVar]]))

    simArgs <- list(data = simdf, mapping = simMapping, alpha = 0.4, linewidth = 0.3)
    if (!hasColor)
      simArgs$color <- 'grey60'
    p <- p + do.call(ggplot2::geom_line, simArgs)
  }

  ribbonMapping <- ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper, group = .data$.group)
  if (hasColor)
    ribbonMapping <- utils::modifyList(ribbonMapping, ggplot2::aes(fill = .data[[colorVar]]))
  p <- p + ggplot2::geom_ribbon(mapping = ribbonMapping, alpha = 0.2, color = NA)

  lineMapping <- ggplot2::aes(y = .data$Median, group = .data$.group)
  if (hasColor)
    lineMapping <- utils::modifyList(lineMapping, ggplot2::aes(color = .data[[colorVar]]))
  if (hasLinetype)
    lineMapping <- utils::modifyList(lineMapping, ggplot2::aes(linetype = .data[[linetypeVar]]))
  p <- p + ggplot2::geom_line(mapping = lineMapping, linewidth = 0.7)

  if (hasColor && colorVar == 'MP') {
    mpValues <- .mp_color_values(unique(summ$MP))
    p <- p +
      ggplot2::scale_color_manual(values = mpValues) +
      ggplot2::scale_fill_manual(values = mpValues)
  }

  if (hasColor && colorVar == 'Variable') {
    varLevels <- levels(summ$Variable)
    if (is.null(varLevels)) varLevels <- sort(unique(summ$Variable))
    varValues <- stats::setNames(.gg_hue_pal(length(varLevels)), varLevels)
    p <- p +
      ggplot2::scale_color_manual(values = varValues) +
      ggplot2::scale_fill_manual(values = varValues)
  }

  if (hasLinetype && linetypeVar == 'Variable') {
    p <- p + ggplot2::scale_linetype_manual(values = c(Landings = 'solid', Discards = 'dashed'))
  }

  nStockLevels <- length(unique(summ$Stock))
  nFleetLevels <- if ('Fleet' %in% colnames(summ)) length(unique(summ$Fleet)) else 1
  facetScales  <- if (free_y) 'free_y' else 'fixed'

  if (doFacetMP) {
    if (byStock && nStockLevels > 1 && !(byFleet && nFleetLevels > 1)) {
      p <- p + ggplot2::facet_grid(Stock ~ MP, scales = facetScales)
    } else if (byFleet && nFleetLevels > 1) {
      p <- p + ggplot2::facet_grid(Fleet ~ MP, scales = facetScales)
    } else {
      p <- p + ggplot2::facet_wrap(~MP, scales = facetScales)
    }
  } else {
    if (byStock && !byFleet && nStockLevels > 1)
      p <- p + ggplot2::facet_wrap(~Stock, scales = facetScales)

    if (byFleet && !byStock && nFleetLevels > 1)
      p <- p + ggplot2::facet_wrap(~Fleet, scales = facetScales)

    if (byStock && byFleet && nStockLevels > 1 && nFleetLevels > 1)
      p <- p + ggplot2::facet_grid(Stock ~ Fleet, scales = facetScales)
  }

  p <- p +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .year_labels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab,
                 color    = if (hasColor) colorVar else NULL,
                 fill     = if (hasColor) colorVar else NULL,
                 linetype = if (hasLinetype) linetypeVar else NULL)

  # Individual sim lines are hard to see against a full background grid.
  if (nsim > 0)
    p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())

  p
}
