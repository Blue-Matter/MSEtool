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
#'   panel's y-axis scale independently. Default (`NULL`) is `TRUE`, except
#'   for `PlotBiomass()`/`PlotSBiomass()`/`PlotSProduction()` with
#'   `relative != "none"`, where it defaults to `FALSE` (ratios are already
#'   on a comparable scale across stocks).
#' @param IncHist Logical. For [mse-class] objects, include the historical
#'   period? `FALSE` is a shortcut for subsetting `Years` to the projection
#'   period only, while still starting each MP's line from the last
#'   historical value. Default `TRUE`. No effect for [hist-class] objects.
#' @param relative Character. For `PlotBiomass()`, `PlotSBiomass()`, and
#'   `PlotSProduction()`: plot absolute values (`"none"`, default), or
#'   relative to unfished (`"B0"`, via [B_B0()]/[SB_SB0()]/[SP_SP0()]) or to
#'   the MSY reference point (`"BMSY"`, via
#'   [B_BMSY()]/[SB_SBMSY()]/[SP_SPMSY()]). When `relative != "none"` and
#'   there is more than one stock, `byStock = FALSE` is not allowed (ratios
#'   cannot be meaningfully summed across stocks) and faceting by stock is
#'   used instead, with a message.
#' @param type Character. One of `"Equilibrium"` or `"Dynamic"`. Which
#'   unfished baseline to use when `relative = "B0"`; see [B_B0()]. Ignored
#'   otherwise.
#' @param Season Integer. For seasonal models (`OM@Seasons > 1`), restrict
#'   `PlotNumber()`/`PlotBiomass()`/`PlotSBiomass()`/`PlotSProduction()` to a
#'   single season (1 = the first timestep of each year), giving one
#'   snapshot per year instead of the full sub-annual sawtooth. These are
#'   point-in-time quantities, so no season should be summed/averaged to get
#'   an annual value the way `Landings()`/`Discards()` can. Default `NULL`
#'   (no filtering). Ignored for non-seasonal models.
#' @param AggregateYear Logical. For seasonal models, sum
#'   `PlotLandings()`/`PlotDiscards()`/`PlotRemovals()` sub-annual values
#'   into whole-year totals, unlike `Season` above (these are flows, so
#'   summing across a year is valid, unlike the point-in-time state
#'   variables). Default `FALSE`. Ignored for non-seasonal models.
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
#' `PlotBiomass()`, `PlotSBiomass()`, and `PlotSProduction()` can plot values
#' relative to unfished or MSY reference points instead of absolute values;
#' see `relative` and `type` above.
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
#' PlotBiomass(MSE, relative = 'BMSY')
#' PlotSBiomass(MSE, relative = 'B0', type = 'Dynamic')
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
#'   [Landings()], [Discards()], [Removals()], [B_B0()], [B_BMSY()]
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
                       free_y  = NULL,
                       IncHist = TRUE,
                       byMP    = FALSE,
                       Season  = NULL) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1
  if (is.null(free_y))  free_y  <- TRUE

  df <- Number(object, df = TRUE) |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_season(Season, object) |>
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
                        byStock  = NULL,
                        probs    = c(0.05, 0.95),
                        nsim     = 0,
                        Years    = NULL,
                        free_y   = NULL,
                        IncHist  = TRUE,
                        byMP     = FALSE,
                        relative = c('none', 'B0', 'BMSY'),
                        type     = c('Equilibrium', 'Dynamic'),
                        Season   = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1
  if (is.null(free_y))  free_y  <- relative == 'none'

  if (relative != 'none' && !byStock && nStock(object) > 1) {
    cli::cli_alert_info(
      "Relative time series (`relative = '{relative}'`) cannot be meaningfully summed across stocks; faceting by stock instead."
    )
    byStock <- TRUE
  }

  if (relative == 'none') {
    df <- Biomass(object, df = TRUE)
  } else {
    extractArgs <- list(object = object, df = TRUE)
    if (relative == 'B0') extractArgs$type <- type
    df <- do.call(.relative_fn_name('Biomass', relative), extractArgs)
  }
  ylab <- if (relative == 'none') 'Biomass' else .relative_ylab('Biomass', relative)

  df <- df |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_season(Season, object) |>
    .filter_years(Years)
  if (!byStock)
    df <- .sum_over_stock(df)

  .build_ts_plot(df, byStock = byStock, byFleet = FALSE,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
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
                         free_y   = NULL,
                         IncHist  = TRUE,
                         byMP     = FALSE,
                         relative = c('none', 'B0', 'BMSY'),
                         type     = c('Equilibrium', 'Dynamic'),
                         Season   = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  if (is.null(free_y)) free_y <- relative == 'none'
  .plot_spawning(object, slot_name = 'SBiomass', ylab = 'Spawning Biomass',
                byStock = byStock, byFemale = byFemale, probs = probs,
                nsim = nsim, Years = Years, free_y = free_y,
                IncHist = IncHist, byMP = byMP,
                relative = relative, type = type, Season = Season)
}

#' @rdname plot_hist
#' @export
PlotSProduction <- function(object,
                            byStock  = NULL,
                            byFemale = TRUE,
                            probs    = c(0.05, 0.95),
                            nsim     = 0,
                            Years    = NULL,
                            free_y   = NULL,
                            IncHist  = TRUE,
                            byMP     = FALSE,
                            relative = c('none', 'B0', 'BMSY'),
                            type     = c('Equilibrium', 'Dynamic'),
                            Season   = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  if (is.null(free_y)) free_y <- relative == 'none'
  .plot_spawning(object, slot_name = 'SProduction', ylab = 'Spawning Production',
                byStock = byStock, byFemale = byFemale, probs = probs,
                nsim = nsim, Years = Years, free_y = free_y,
                IncHist = IncHist, byMP = byMP,
                relative = relative, type = type, Season = Season)
}

#' @rdname plot_hist
#' @export
PlotLandings <- function(object,
                         byStock       = NULL,
                         byFleet       = NULL,
                         probs         = c(0.05, 0.95),
                         nsim          = 0,
                         Years         = NULL,
                         free_y        = NULL,
                         IncHist       = TRUE,
                         byMP          = FALSE,
                         AggregateYear = FALSE) {
  if (is.null(free_y)) free_y <- TRUE
  .plot_catch(object, slot_name = 'Landings', ylab = 'Landings',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP, AggregateYear = AggregateYear)
}

#' @rdname plot_hist
#' @export
PlotDiscards <- function(object,
                         byStock       = NULL,
                         byFleet       = NULL,
                         probs         = c(0.05, 0.95),
                         nsim          = 0,
                         Years         = NULL,
                         free_y        = NULL,
                         IncHist       = TRUE,
                         byMP          = FALSE,
                         AggregateYear = FALSE) {
  if (is.null(free_y)) free_y <- TRUE
  .plot_catch(object, slot_name = 'Discards', ylab = 'Discards',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP, AggregateYear = AggregateYear)
}

#' @rdname plot_hist
#' @export
PlotRemovals <- function(object,
                         byStock       = NULL,
                         byFleet       = NULL,
                         probs         = c(0.05, 0.95),
                         nsim          = 0,
                         Years         = NULL,
                         free_y        = NULL,
                         IncHist       = TRUE,
                         byMP          = FALSE,
                         AggregateYear = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1
  if (is.null(free_y))  free_y  <- TRUE

  L <- Landings(object, df = TRUE, byFleet = TRUE) |>
    .aggregate_year(AggregateYear, object) |>
    .bridge_mp_gap()
  D <- Discards(object, df = TRUE, byFleet = TRUE) |>
    .aggregate_year(AggregateYear, object) |>
    .bridge_mp_gap()
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
                         byStock       = NULL,
                         byFleet       = FALSE,
                         byFemale      = TRUE,
                         byMP          = FALSE,
                         probs         = c(0.05, 0.95),
                         nsim          = 0,
                         Years         = NULL,
                         free_y        = NULL,
                         IncHist       = TRUE,
                         relative      = c('none', 'B0', 'BMSY'),
                         type          = c('Equilibrium', 'Dynamic'),
                         Season        = NULL,
                         AggregateYear = FALSE) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  CheckClass(object, c('hist', 'mse'), 'object')

  common <- list(object = object, byStock = byStock, byMP = byMP, probs = probs,
                 nsim = nsim, Years = Years, free_y = free_y, IncHist = IncHist)
  relArgs    <- list(relative = relative, type = type)
  seasonArgs <- list(Season = Season)
  aggArgs    <- list(byFleet = byFleet, AggregateYear = AggregateYear)

  panels <- list(
    do.call(PlotNumber,      c(common, seasonArgs)),
    do.call(PlotBiomass,     c(common, relArgs, seasonArgs)),
    do.call(PlotSBiomass,    c(common, relArgs, seasonArgs, list(byFemale = byFemale))),
    do.call(PlotSProduction, c(common, relArgs, seasonArgs, list(byFemale = byFemale))),
    do.call(PlotLandings,    c(common, aggArgs)),
    do.call(PlotDiscards,    c(common, aggArgs))
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

# Keep only the rows belonging to one season of a seasonal model. Season is
# determined positionally within the full ordered Year sequence for the
# object (season 1 = the first timestep, then cycling every `OM@Seasons`
# steps), not from the decimal fraction of Year directly -- leap years
# shift that fraction slightly from year to year even for the same season.
# No-op for non-seasonal models (`OM@Seasons` <= 1) or when Season = NULL.
.filter_season <- function(df, Season, object) {
  if (is.null(Season))
    return(df)

  nSeason <- object@OM@Seasons
  if (is.null(nSeason) || nSeason <= 1)
    return(df)

  all_years  <- Years(object)
  season_idx <- ((seq_along(all_years) - 1) %% nSeason) + 1
  keep_years <- all_years[season_idx == Season]

  dplyr::filter(df, .data$Year %in% keep_years)
}

# Sum sub-annual (seasonal) rows into whole-year totals -- valid for flow
# variables (Landings/Discards) unlike the snapshot state variables handled
# by `.filter_season()`. Must run *before* `.bridge_mp_gap()`, otherwise the
# synthetic boundary bridge row would be double-counted in the sum. No-op
# for non-seasonal models or AggregateYear = FALSE.
.aggregate_year <- function(df, AggregateYear, object) {
  if (!isTRUE(AggregateYear))
    return(df)

  nSeason <- object@OM@Seasons
  if (is.null(nSeason) || nSeason <= 1)
    return(df)

  df$Year <- floor(df$Year)

  cnames     <- colnames(df)
  group_vars <- cnames[!cnames %in% 'Value']
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(Value = sum(.data$Value), .groups = 'drop')
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
                          probs, nsim, Years, free_y, IncHist, byMP,
                          relative, type, Season) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1

  if (relative != 'none' && !byStock && nStock(object) > 1) {
    cli::cli_alert_info(
      "Relative time series (`relative = '{relative}'`) cannot be meaningfully summed across stocks; faceting by stock instead."
    )
    byStock <- TRUE
  }

  if (relative == 'none') {
    df <- do.call(slot_name, list(object = object, df = TRUE))
  } else {
    extractArgs <- list(object = object, df = TRUE)
    if (relative == 'B0') extractArgs$type <- type
    df <- do.call(.relative_fn_name(slot_name, relative), extractArgs)
  }
  ylab <- if (relative == 'none') ylab else .relative_ylab(ylab, relative)

  df <- df |>
    .bridge_mp_gap() |>
    .drop_historical(IncHist) |>
    .filter_season(Season, object) |>
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
                       probs, nsim, Years, free_y, IncHist, byMP, AggregateYear) {
  CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byStock)) byStock <- nStock(object) > 1
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1

  df <- do.call(slot_name, list(object = object, df = TRUE, byFleet = TRUE)) |>
    .aggregate_year(AggregateYear, object) |>
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

# Map a base variable ('Biomass'/'SBiomass'/'SProduction') and a `relative`
# choice to the corresponding relative_ref extractor function name.
.relative_fn_name <- function(slot_name, relative) {
  switch(slot_name,
    Biomass     = if (relative == 'B0') 'B_B0'   else 'B_BMSY',
    SBiomass    = if (relative == 'B0') 'SB_SB0'  else 'SB_SBMSY',
    SProduction = if (relative == 'B0') 'SP_SP0'  else 'SP_SPMSY'
  )
}

.relative_ylab <- function(ylab, relative) {
  abbr <- switch(ylab,
    'Biomass'             = 'B',
    'Spawning Biomass'    = 'SB',
    'Spawning Production' = 'SP',
    ylab
  )
  if (relative == 'B0') paste0(abbr, '/', abbr, '0') else paste0(abbr, '/', abbr, 'MSY')
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
