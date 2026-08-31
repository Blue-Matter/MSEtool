#' Plot Historical and Projected Dynamics
#'
#' Plotting functions for [hist-class] and [mse-class] objects, summarizing
#' simulated numbers, biomass, spawning biomass, spawning production,
#' fishing mortality, vulnerable biomass, landings, discards, interactions,
#' removals, and effort over time.
#'
#' @param object A [hist-class] or [mse-class] object. `PlotLandings()`,
#'   `PlotDiscards()`, `PlotRemovals()`, and `PlotEffort()` also accept a
#'   [data-class] object (e.g. `Hist@Data[[1]][[1]]`), in which case a single
#'   observed timeseries (no ribbon) is plotted, with `byFleet` and
#'   `AggregateYear` applied as described in [plot_data]; other arguments
#'   (`byStock`, `probs`, `nsim`, `Years`, `free_y`, `IncHist`, `byMP`) have
#'   no effect in that case. For `PlotRemovals()`, the `data`-class series is
#'   `Landings + Discards` summed together.
#' @param byStock One of `TRUE`, `FALSE`, `'sum'`, or `NULL` (default).
#'   `TRUE` facets by stock. `FALSE` colors each stock as its own line on a
#'   single panel, without summing. `'sum'` sums across stocks into a single
#'   line (relative time series, `relative != 'none'`, cannot be meaningfully
#'   summed this way and fall back to faceting instead - see `relative`).
#'   `NULL` (default) resolves to `TRUE` when `object` has more than one
#'   (selected) stock, otherwise `FALSE`. When `FALSE` and `object` has more
#'   than one MP, MP is faceted instead of colored, since stock now occupies
#'   the color channel.
#' @param byFleet Logical. Facet by fleet? Default (`NULL`) facets
#'   automatically when `object` has more than one fleet, except in
#'   `PlotDynamics()`/`plot()` where it defaults to `FALSE`. Only applies to
#'   `PlotF()`/`PlotLandings()`/`PlotDiscards()`/`PlotInteractions()`/
#'   `PlotRemovals()`/`PlotEffort()`. Ignored by `PlotF()` when
#'   `relative = 'FMSY'` (always summed over fleets). When `object` is a
#'   [data-class] object, `byFleet` instead follows the three-state
#'   `NULL`/`TRUE`/`FALSE` behavior described in [plot_data] (default `NULL`
#'   colors by fleet rather than auto-faceting).
#' @param units For `PlotEffort()`: character or `NULL`. One of `"Effort"`
#'   (the raw effort index), `"Trips"` (`Effort x TripsScalar`), or
#'   `"Anglers"` (`Effort x TripsScalar x AnglerPerTrip`); `TripsScalar`/
#'   `AnglerPerTrip` are read from the first stock's `Fleet` object for each
#'   fleet (see [Effort()]). `NULL` (default) auto-selects, per fleet, the
#'   most specific unit with the required data. When `byFleet = TRUE`,
#'   each fleet's facet uses its own resolved unit (labelled in the facet
#'   strip if they differ); otherwise the weakest unit available across all
#'   plotted fleets is used for all of them, since a raw index and real trip
#'   counts can't share one axis or line. 
#'
#'   For `PlotNumber()`, `PlotBiomass()`, `PlotSBiomass()`, `PlotSProduction()`,
#'   `PlotVBiomass()`, `PlotLandings()`, `PlotDiscards()`, and `PlotRemovals()`:
#'   logical or a character unit string. `TRUE` (default) labels the y-axis
#'   with the unit implied by the stock's own `Units` slots -- e.g. a
#'   `Weight@Units` of `"kg"` combined with an `SRR@Units` of `1000` (R0 in
#'   thousands of fish) labels `Biomass` in `"t"`, since the raw values are
#'   already `Number() x WeightAtAge`. `PlotNumber()` instead shows the
#'   `SRR@Units` scaling alone (e.g. `"Number (thousands)"`). The label is
#'   only added when the relevant `Units` are set and agree across every
#'   plotted stock; otherwise it's silently omitted. `FALSE` suppresses the
#'   unit label entirely (e.g. when a model's `Units` slots aren't set
#'   correctly). A character string (mass unit for all of these except
#'   `PlotNumber()`, e.g. `"kg"`, `"t"`, `"lb"`) both relabels the axis *and*
#'   rescales the plotted values into that unit; requesting a unit the
#'   stored units can't be converted to (e.g. a target when `Weight@Units`
#'   is unset, or an unrecognized string) is an error. Ignored (`relative`
#'   ratios are already unitless) when `PlotBiomass()`/`PlotSBiomass()`/
#'   `PlotSProduction()` are called with `relative != "none"`.
#' @param what Character. For `PlotF()`, which fishing mortality series to
#'   plot: `"FDead"` (default, mortality of fish killed), `"FInteract"`
#'   (mortality of all fish encountered), or `"FRetain"` (mortality of fish
#'   retained). See [FDead()]/[FInteract()]/[FRetain()]. Apical (max-over-age)
#'   fishing mortality is plotted. Ignored when `relative = 'FMSY'`, which is
#'   always based on `FDead`.
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
#'   [B_BMSY()]/[SB_SBMSY()]/[SP_SPMSY()]). For `PlotF()`: `"none"` (default,
#'   apical `what` fishing mortality) or `"FMSY"` (via [F_FMSY()]). When
#'   `relative != "none"` and there is more than one stock, `byStock = FALSE`
#'   is not allowed (ratios cannot be meaningfully summed across stocks) and
#'   faceting by stock is used instead, with a message.
#' @param type Character. One of `"Equilibrium"` or `"Dynamic"`. Which
#'   unfished baseline to use when `relative = "B0"`; see [B_B0()]. Ignored
#'   otherwise.
#' @param VBType Character. For `PlotVBiomass()`, `"Removals"` (default,
#'   weights biomass by fish that die from the encounter, landed or
#'   discarded) or `"Landings"` (weights by landed fish only); see
#'   [VBiomass()].
#' @param Season Integer. For seasonal models (`OM@Seasons > 1`), restrict
#'   `PlotNumber()`/`PlotBiomass()`/`PlotSBiomass()`/`PlotSProduction()`/
#'   `PlotF()`/`PlotVBiomass()` to a single season (1 = the first timestep
#'   of each year), giving one snapshot per year instead of the full
#'   sub-annual sawtooth.
#'   These are point-in-time quantities, so no season should be
#'   summed/averaged to get an annual value the way
#'   `Landings()`/`Discards()` can. Default `NULL` (no filtering). Ignored
#'   for non-seasonal models.
#' @param AggregateYear Logical. For seasonal models, sum
#'   `PlotLandings()`/`PlotDiscards()`/`PlotInteractions()`/
#'   `PlotRemovals()`/`PlotEffort()` sub-annual values into whole-year
#'   totals, unlike `Season` above (these are flows, so summing across a
#'   year is valid, unlike the point-in-time state variables). Default
#'   `FALSE`. Ignored for non-seasonal models.
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks). `byStock` and, for `PlotSBiomass()`/
#'   `PlotSProduction()`, the automatic `byFemale` stock selection (see
#'   Details) are evaluated against this selection rather than the full set
#'   of stocks in `object` -- e.g. `Stocks = "Female"` with `byStock = FALSE`
#'   plots that one stock's series under its own name, without needing
#'   `byFemale` to guess at it; providing `Stocks` explicitly always disables
#'   the automatic `byFemale` complex-matching logic (`byStock = 'sum'` then
#'   sums exactly the stocks selected, instead of guessing the female one).
#'
#' @details
#' Each function extracts the relevant time series with the corresponding
#' accessor ([Number()], [Biomass()], [SBiomass()], [SProduction()],
#' [FDead()]/[FInteract()]/[FRetain()], [Landings()], [Discards()],
#' [Interactions()], [Effort()]), summarizes across simulations as a median
#' line with a shaded quantile ribbon, and returns a `ggplot` object. Set
#' `nsim` to overlay that many individual simulation replicates as thin
#' lines (the background grid is dropped in this case so the replicates
#' stand out).
#'
#' `PlotF()` plots apical (max-over-age) fishing mortality; see `what` above
#' for which of `FDead()`/`FInteract()`/`FRetain()` is used, and `relative`
#' for plotting `F/FMSY` instead.
#'
#' `PlotInteractions()` plots [Interactions()] (total fish encountered,
#' before discard mortality is applied), following the same `byStock`/
#' `byFleet`/`AggregateYear` behavior as `PlotLandings()`/`PlotDiscards()`.
#' Unlike those two, it has no [data-class] counterpart -- `Interactions` is
#' not an observed quantity.
#'
#' `PlotVBiomass()` plots [VBiomass()] -- biomass weighted by the fraction
#' of it actually exposed to each fleet's gear; see `VBType` above (it has
#' no `relative` argument of its own). For the projection period of
#' [mse-class] objects, this reflects any change to a fleet's selectivity,
#' retention, or discard mortality made by the MP (see [Advice()]), not
#' just the OM baseline.
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
#' (dashed). If `Discards()` is all zero (or essentially so) across the
#' plotted selection, the Discards series is dropped and only Landings is
#' shown. For [data-class] objects (see `object` above) they're summed
#' into a single `Landings + Discards` series instead, since there's no
#' color channel to spare.
#'
#' See [PlotEffort()] for fishing effort, documented on its own page. See
#' `units` above for plotting effort as real trip (or angler) counts
#' instead of the raw index, where `TripsScalar`/`AnglerPerTrip` are
#' available.
#'
#' `byStock` and (for `PlotLandings()`/`PlotDiscards()`/`PlotRemovals()`)
#' `byFleet` control faceting: when both are `TRUE` panels are arranged in a
#' `Stock x Fleet` grid, matching the behavior of [Landings()]/[Discards()].
#' `byFleet = FALSE` sums fleets before plotting. `byStock` has a third state
#' that `byFleet` doesn't: `byStock = FALSE` colors each stock as its own
#' line instead of summing (see `byStock` above); `byStock = 'sum'` is what
#' sums stocks before plotting. When faceting is used, `free_y` (default
#' `TRUE`) lets each panel's y-axis scale independently, since stocks or
#' fleets of very different magnitude (e.g. a large and a small stock) are
#' otherwise hard to compare on a shared scale.
#'
#' For `PlotSBiomass()` and `PlotSProduction()` with `byStock = 'sum'` and
#' `byFemale = TRUE`, stocks within the same `OM@Complexes` group are not
#' naively summed (which would double-count male and female components of
#' the same population). Instead, within each complex, a stock whose name
#' matches `"female"` (case-insensitive) is taken as that complex's
#' contribution to the total. If a complex has more than one stock and no
#' single stock can be unambiguously identified as female, faceting by
#' stock is used instead and a message is printed. `byFemale` has no effect
#' when `byStock = FALSE` (every stock, male and female, is shown colored).
#'
#' `PlotBiomass()`, `PlotSBiomass()`, `PlotSProduction()`, and `PlotF()` can
#' plot values relative to unfished or MSY reference points instead of
#' absolute values; see `relative` and `type` above. Relative time series
#' cannot be meaningfully summed across stocks, so `byStock = 'sum'` with
#' `relative != 'none'` falls back to faceting instead (`byStock = FALSE`,
#' coloring each stock's ratio, is unaffected by this restriction).
#'
#' `PlotDynamics()` arranges `PlotNumber()`, `PlotBiomass()`,
#' `PlotSBiomass()`, and `PlotRemovals()` into a single, terse figure with
#' [patchwork::wrap_plots()] -- one panel per row/column of a `2 x 2` grid.
#' `PlotSProduction()` and the separate `PlotLandings()`/`PlotDiscards()`
#' panels are deliberately left out of this default overview (call them
#' directly for that level of detail). Its own `byFleet` argument (and
#' `plot()`'s, which dispatches to it) defaults to `FALSE`, unlike the
#' individual `Plot*()` functions; each argument is only forwarded to the
#' panels that accept it. `byStock` is forwarded as-is, including the
#' `'sum'` option described above.
#'
#' @return A `ggplot` object (a `patchwork` object for `PlotDynamics()`).
#'
#' @example man-examples/plot-hist.R
#'
#' @name plot_hist
#' @seealso [Number()], [Biomass()], [SBiomass()], [SProduction()],
#'   [FDead()], [FInteract()], [FRetain()], [VBiomass()], [Landings()],
#'   [Discards()], [Interactions()], [Removals()], [Effort()], [B_B0()],
#'   [B_BMSY()], [F_FMSY()]
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
                       Season  = NULL,
                       units   = TRUE,
                       Stocks  = NULL) {
  .CheckClass(object, c('hist', 'mse'), 'object')
  stockNames <- .ResolveStocks(object, Stocks)
  byStock <- .ResolveByStock(byStock, .NSelStock(object, stockNames))
  if (is.null(free_y))  free_y  <- TRUE

  df <- Number(object, df = TRUE) |>
    .FilterStock(stockNames) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterSeason(Season, object) |>
    .FilterYears(Years)
  if (identical(byStock, 'sum'))
    df <- .SumOverStock(df)

  ylab <- 'Number'
  if (!isFALSE(units)) {
    scale <- .GetStockUnits(object@OM, 'SRR', stockNames)
    ylab  <- .AppendUnits(ylab, .CountScaleLabel(scale))
  }

  .BuildTsPlot(df, byStock = byStock, byFleet = FALSE,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
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
                        Season   = NULL,
                        units    = TRUE,
                        Stocks   = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  .CheckClass(object, c('hist', 'mse'), 'object')
  stockNames <- .ResolveStocks(object, Stocks)
  nSel <- .NSelStock(object, stockNames)
  byStock <- .ResolveByStock(byStock, nSel)
  if (is.null(free_y))  free_y  <- relative == 'none'

  if (relative != 'none' && identical(byStock, 'sum') && nSel > 1) {
    cli::cli_alert_info(
      "Relative time series (`relative = '{relative}'`) cannot be meaningfully summed across stocks; faceting by stock instead."
    )
    byStock <- TRUE
  }

  if (relative == 'none') {
    df <- Biomass(object, df = TRUE)
    uinfo <- .MassUnitInfo(object, stockNames, units, 'Weight', 'Biomass')
    df$Value <- df$Value * uinfo$factor
    ylab <- .AppendUnits('Biomass', uinfo$label)
  } else {
    extractArgs <- list(object = object, df = TRUE)
    if (relative == 'B0') extractArgs$type <- type
    df <- do.call(.RelativeFnName('Biomass', relative), extractArgs)
    ylab <- .RelativeYlab('Biomass', relative)
  }

  df <- df |>
    .FilterStock(stockNames) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterSeason(Season, object) |>
    .FilterYears(Years)
  if (identical(byStock, 'sum'))
    df <- .SumOverStock(df)

  .BuildTsPlot(df, byStock = byStock, byFleet = FALSE,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP, relative = relative != 'none')
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
                         Season   = NULL,
                         units    = TRUE,
                         Stocks   = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  if (is.null(free_y)) free_y <- relative == 'none'
  .PlotSpawning(object, slot_name = 'SBiomass', ylab = 'Spawning Biomass',
                byStock = byStock, byFemale = byFemale, probs = probs,
                nsim = nsim, Years = Years, free_y = free_y,
                IncHist = IncHist, byMP = byMP,
                relative = relative, type = type, Season = Season,
                units = units, base_slot = 'Weight', Stocks = Stocks)
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
                            Season   = NULL,
                            units    = TRUE,
                            Stocks   = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  if (is.null(free_y)) free_y <- relative == 'none'
  .PlotSpawning(object, slot_name = 'SProduction', ylab = 'Spawning Production',
                byStock = byStock, byFemale = byFemale, probs = probs,
                nsim = nsim, Years = Years, free_y = free_y,
                IncHist = IncHist, byMP = byMP,
                relative = relative, type = type, Season = Season,
                units = units, base_slot = 'Fecundity', Stocks = Stocks)
}

#' @rdname plot_hist
#' @export
PlotF <- function(object,
                  what     = c('FDead', 'FInteract', 'FRetain'),
                  byStock  = NULL,
                  byFleet  = NULL,
                  probs    = c(0.05, 0.95),
                  nsim     = 0,
                  Years    = NULL,
                  free_y   = NULL,
                  IncHist  = TRUE,
                  byMP     = FALSE,
                  relative = c('none', 'FMSY'),
                  Season   = NULL,
                  Stocks   = NULL) {
  what     <- match.arg(what)
  relative <- match.arg(relative)
  .CheckClass(object, c('hist', 'mse'), 'object')

  stockNames <- .ResolveStocks(object, Stocks)
  byStock <- .ResolveByStock(byStock, .NSelStock(object, stockNames))
  if (is.null(free_y)) free_y <- relative == 'none'

  if (relative == 'FMSY') {
    if (what != 'FDead')
      cli::cli_alert_info("`relative = 'FMSY'` is only defined for `what = 'FDead'`; using `FDead` instead of `{what}`.")
    df      <- F_FMSY(object, df = TRUE)
    ylab    <- 'F/FMSY'
    byFleet <- FALSE
  } else {
    if (is.null(byFleet)) byFleet <- nFleet(object) > 1
    df   <- do.call(what, list(object = object, df = TRUE, byAge = FALSE,
                               byArea = FALSE, byFleet = TRUE))
    ylab <- what
  }

  df <- df |>
    .FilterStock(stockNames) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterSeason(Season, object) |>
    .FilterYears(Years)

  if (relative == 'none' && !byFleet)
    df <- .SumOverFleet(df)
  if (identical(byStock, 'sum'))
    df <- .SumOverStock(df)

  .BuildTsPlot(df, byStock = byStock, byFleet = isTRUE(byFleet),
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP, relative = relative != 'none')
}

#' @rdname plot_hist
#' @export
PlotVBiomass <- function(object,
                         byStock = NULL,
                         byFleet = NULL,
                         probs   = c(0.05, 0.95),
                         nsim    = 0,
                         Years   = NULL,
                         free_y  = NULL,
                         IncHist = TRUE,
                         byMP    = FALSE,
                         VBType  = c('Removals', 'Landings'),
                         Season  = NULL,
                         units   = TRUE,
                         Stocks  = NULL) {
  VBType <- match.arg(VBType)
  .CheckClass(object, c('hist', 'mse'), 'object')
  stockNames <- .ResolveStocks(object, Stocks)
  byStock <- .ResolveByStock(byStock, .NSelStock(object, stockNames))
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1
  if (is.null(free_y))  free_y  <- TRUE

  df <- VBiomass(object, df = TRUE, byFleet = TRUE, type = VBType)
  uinfo <- .MassUnitInfo(object, stockNames, units, 'Weight', 'Vulnerable Biomass')
  df$Value <- df$Value * uinfo$factor
  ylab <- .AppendUnits('Vulnerable Biomass', uinfo$label)

  df <- df |>
    .FilterStock(stockNames) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterSeason(Season, object) |>
    .FilterYears(Years)

  if (!byFleet)
    df <- .SumOverFleet(df)
  if (identical(byStock, 'sum'))
    df <- .SumOverStock(df)

  .BuildTsPlot(df, byStock = byStock, byFleet = byFleet,
                ylab = ylab, probs = probs, nsim = nsim,
                free_y = free_y, byMP = byMP, colorVar = 'MP')
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
                         AggregateYear = FALSE,
                         units         = TRUE,
                         Stocks        = NULL) {
  if (inherits(object, 'data'))
    return(.PlotDataTs(object, 'Landings', 'Landings', byFleet = byFleet,
                         AggregateYear = AggregateYear, units = units))

  if (is.null(free_y)) free_y <- TRUE
  .PlotCatch(object, slot_name = 'Landings', ylab = 'Landings',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP, AggregateYear = AggregateYear,
             units = units, Stocks = Stocks)
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
                         AggregateYear = FALSE,
                         units         = TRUE,
                         Stocks        = NULL) {
  if (inherits(object, 'data'))
    return(.PlotDataTs(object, 'Discards', 'Discards', byFleet = byFleet,
                         AggregateYear = AggregateYear, units = units))

  if (is.null(free_y)) free_y <- TRUE
  .PlotCatch(object, slot_name = 'Discards', ylab = 'Discards',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP, AggregateYear = AggregateYear,
             units = units, Stocks = Stocks)
}

#' @rdname plot_hist
#' @export
PlotInteractions <- function(object,
                             byStock       = NULL,
                             byFleet       = NULL,
                             probs         = c(0.05, 0.95),
                             nsim          = 0,
                             Years         = NULL,
                             free_y        = NULL,
                             IncHist       = TRUE,
                             byMP          = FALSE,
                             AggregateYear = FALSE,
                             Stocks        = NULL) {
  if (is.null(free_y)) free_y <- TRUE
  .PlotCatch(object, slot_name = 'Interactions', ylab = 'Interactions',
             byStock = byStock, byFleet = byFleet, probs = probs,
             nsim = nsim, Years = Years, free_y = free_y,
             IncHist = IncHist, byMP = byMP, AggregateYear = AggregateYear,
             Stocks = Stocks)
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
                         AggregateYear = FALSE,
                         units         = TRUE,
                         Stocks        = NULL) {
  if (inherits(object, 'data'))
    return(.PlotDataRemovals(object, byFleet = byFleet,
                               AggregateYear = AggregateYear, units = units))

  .CheckClass(object, c('hist', 'mse'), 'object')
  stockNames <- .ResolveStocks(object, Stocks)
  byStock <- .ResolveByStock(byStock, .NSelStock(object, stockNames))
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1
  if (is.null(free_y))  free_y  <- TRUE

  L <- Landings(object, df = TRUE, byFleet = TRUE) |>
    .FilterStock(stockNames) |>
    .AggregateYear(AggregateYear, object) |>
    .BridgeMpGap()
  D <- Discards(object, df = TRUE, byFleet = TRUE) |>
    .FilterStock(stockNames) |>
    .AggregateYear(AggregateYear, object) |>
    .BridgeMpGap()

  discardsAllZero <- !nrow(D) || all(abs(D$Value) < 1e-8 | is.na(D$Value))
  if (discardsAllZero) D <- D[0, ]

  df <- dplyr::bind_rows(L, D) |>
    .DropHistorical(IncHist) |>
    .FilterYears(Years)
  df$Variable <- factor(df$Variable, levels = c('Landings', 'Discards'), ordered = TRUE)
  if (discardsAllZero) df$Variable <- droplevels(df$Variable)

  uinfo <- .MassUnitInfo(object, stockNames, units, 'Weight', 'Removals')
  df$Value <- df$Value * uinfo$factor
  ylab <- .AppendUnits('Removals', uinfo$label)

  if (!byFleet)
    df <- .SumOverFleet(df)
  if (identical(byStock, 'sum'))
    df <- .SumOverStock(df)

  isMSE <- inherits(object, 'mse')
  .BuildTsPlot(df, byStock = byStock, byFleet = byFleet,
                ylab = ylab, probs = probs, nsim = nsim,
                free_y = free_y, byMP = byMP,
                colorVar    = if (isMSE) 'MP' else 'Variable',
                linetypeVar = if (isMSE) 'Variable' else NULL)
}

#' Plot Fishing Effort
#'
#' For a bare [fleet-class] or [om-class] `object` (nothing simulated
#' yet), `PlotEffort()` plots the specified input effort trend
#' (`PlotEffortCurve()`, `Fleet@Effort@Effort`, historical years only).
#' For a [hist-class]/[mse-class] `object`, it plots the realized
#' historical/projection effort trajectory.
#'
#' @param object A [fleet-class] object, an [om-class], [hist-class], or
#'   [mse-class] object, or a [data-class] object (plots `Effort(object)`
#'   directly).
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `object`. Ignored otherwise. Default
#'   `NULL` uses an example stock, with a message noting this.
#' @param units Character or `NULL`. `NULL` (default) uses the coarsest
#'   unit common to every plotted fleet (`"Effort"`, `"Trips"`, or
#'   `"Anglers"`, depending on what each fleet's [Effort()] object
#'   supplies). Set explicitly to force one; requesting a unit some fleet
#'   lacks the data for is an error.
#' @param byStock,byFleet One of `TRUE`, `FALSE`, or `NULL` (default,
#'   facets automatically when `object` has more than one stock/fleet).
#'   `FALSE` sums effort across fleets instead of faceting/coloring by
#'   fleet (only when every summed fleet shares the same `units`).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param probs Numeric vector of length 2. Lower and upper quantiles of
#'   the across-simulation ribbon. Default `c(0.05, 0.95)`.
#' @param nsim Integer. Number of individual simulation trajectories to
#'   overlay as thin lines, in addition to the median/ribbon. Default `0`.
#' @param Years Optional numeric vector, or `"all"`. Default `NULL` plots
#'   every available year.
#' @param free_y Logical. Free the y-axis scale across facets? Default
#'   `NULL` resolves to `TRUE`.
#' @param IncHist,byMP,AggregateYear `.PlotEffortRealized()` only (via
#'   `PlotEffort()` on a [hist-class]/[mse-class] `object`); see
#'   [plot_hist].
#'
#' @return `PlotEffortCurve()` returns a `ggplot` object; `PlotEffort()`
#'   returns a `ggplot` object too (`.PlotEffortRealized()`'s, for
#'   [hist-class]/[mse-class] `object`, otherwise `PlotEffortCurve()`'s).
#'
#' @seealso [Effort()], [PlotCatchability()], [Fleet()]
#' @export
PlotEffort <- function(object,
                       Stock         = NULL,
                       units         = NULL,
                       byStock       = NULL,
                       byFleet       = NULL,
                       Stocks        = NULL,
                       probs         = c(0.05, 0.95),
                       nsim          = 0,
                       Years         = NULL,
                       free_y        = NULL,
                       IncHist       = TRUE,
                       byMP          = FALSE,
                       AggregateYear = FALSE) {
  if (inherits(object, 'data'))
    return(.PlotDataTs(object, 'Effort', 'Effort', byFleet = byFleet,
                         AggregateYear = AggregateYear))

  .CheckClass(object, c('fleet', 'om', 'hist', 'mse'), 'object')
  wasFleet <- inherits(object, 'fleet')
  if (wasFleet) object <- .FleetToShellHist(object, Stock)

  if (wasFleet || !inherits(object, c('hist', 'mse')))
    return(PlotEffortCurve(object, units = units, byStock = byStock, byFleet = byFleet,
                           Stocks = Stocks, probs = probs, nsim = nsim, Years = Years,
                           free_y = free_y))

  .PlotEffortRealized(object, units = units, byFleet = byFleet, probs = probs,
                      nsim = nsim, Years = Years, free_y = free_y, IncHist = IncHist,
                      byMP = byMP, AggregateYear = AggregateYear)
}

#' @rdname PlotEffort
#' @export
PlotEffortCurve <- function(object, Stock = NULL, units = NULL, byStock = NULL, byFleet = NULL,
                            Stocks = NULL, probs = c(0.05, 0.95), nsim = 0, Years = NULL,
                            free_y = NULL) {
  .CheckClass(object, c('fleet', 'om', 'hist', 'mse'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)

  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  fleetNames <- FleetNames(OM)
  if (is.null(byFleet)) byFleet <- length(fleetNames) > 1
  if (is.null(free_y))  free_y  <- TRUE

  df <- purrr::map(stockNames, \(nm) {
    purrr::map(fleetNames, \(fl) {
      Array2DF(OM@Fleet[[nm]][[fl]]@Effort@Effort) |>
        dplyr::mutate(Stock = nm, Fleet = fl)
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  fleetUnits <- .ResolveEffortUnits(object, unique(df$Fleet), units, byFleet)
  df <- .ApplyEffortUnits(df, object, fleetUnits) |> .FilterYears(Years)

  if (!byFleet) {
    df <- .SumOverFleet(df)
  } else if (length(unique(fleetUnits)) > 1) {
    df$Fleet <- .LabelFleetUnits(df$Fleet, fleetUnits)
  }

  ylab <- if (length(unique(fleetUnits)) == 1) .EffortYlab(fleetUnits[[1]]) else 'Effort'

  .BuildTsPlot(df, byStock = byStock, byFleet = byFleet,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y)
}

.PlotEffortRealized <- function(object,
                                units         = NULL,
                                byFleet       = NULL,
                                probs         = c(0.05, 0.95),
                                nsim          = 0,
                                Years         = NULL,
                                free_y        = NULL,
                                IncHist       = TRUE,
                                byMP          = FALSE,
                                AggregateYear = FALSE) {
  .CheckClass(object, c('hist', 'mse'), 'object')
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1
  if (is.null(free_y))  free_y  <- TRUE

  df <- Effort(object, df = TRUE)

  fleetUnits <- .ResolveEffortUnits(object, unique(df$Fleet), units, byFleet)
  df <- .ApplyEffortUnits(df, object, fleetUnits)

  df <- df |>
    .AggregateYear(AggregateYear, object) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterYears(Years)

  if (!byFleet) {
    df <- .SumOverFleet(df)
  } else if (length(unique(fleetUnits)) > 1) {
    df$Fleet <- .LabelFleetUnits(df$Fleet, fleetUnits)
  }

  ylab <- if (length(unique(fleetUnits)) == 1) .EffortYlab(fleetUnits[[1]]) else 'Effort'

  .BuildTsPlot(df, byStock = FALSE, byFleet = byFleet,
                ylab = ylab, probs = probs, nsim = nsim,
                free_y = free_y, colorVar = 'MP', byMP = byMP)
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
                         AggregateYear = FALSE,
                         units         = TRUE,
                         Stocks        = NULL) {
  relative <- match.arg(relative)
  type     <- match.arg(type)
  .CheckClass(object, c('hist', 'mse'), 'object')

  common <- list(object = object, byStock = byStock, byMP = byMP, probs = probs,
                 nsim = nsim, Years = Years, free_y = free_y, IncHist = IncHist,
                 units = units, Stocks = Stocks)
  relArgs    <- list(relative = relative, type = type)
  seasonArgs <- list(Season = Season)
  aggArgs    <- list(byFleet = byFleet, AggregateYear = AggregateYear)

  panels <- list(
    do.call(PlotNumber,   c(common, seasonArgs)),
    do.call(PlotBiomass,  c(common, relArgs, seasonArgs)),
    do.call(PlotSBiomass, c(common, relArgs, seasonArgs, list(byFemale = byFemale))),
    do.call(PlotRemovals, c(common, aggArgs))
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

.FilterYears <- function(df, Years) {
  if (is.null(Years) || identical(Years, 'all'))
    return(df)
  dplyr::filter(df, .data$Year %in% Years)
}

.DropHistorical <- function(df, IncHist) {
  if (IncHist || !'MP' %in% colnames(df))
    return(df)
  dplyr::filter(df, .data$MP != 'Historical')
}


.FilterSeason <- function(df, Season, object) {
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


.AggregateYear <- function(df, AggregateYear, object) {
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

.MassUnitInfo <- function(object, stockNames, units, base_slot = 'Weight', what = 'value') {
  OM        <- object@OM
  base_unit <- .GetStockUnits(OM, base_slot, stockNames)
  scale     <- .GetStockUnits(OM, 'SRR', stockNames)
  .ResolveUnitInfo(.mass_units_g, base_unit, scale, units, what)
}

.effort_unit_rank <- c(Effort = 1L, Trips = 2L, Anglers = 3L)

.EffortYlab <- function(unit) {
  if (identical(unit, 'Effort')) 'Effort' else paste0('Effort (', unit, ')')
}

.EffortUnitAvail <- function(object, fleets) {
  OM     <- .ResolveOM(object)
  stock1 <- StockNames(OM)[1]
  purrr::map_chr(fleets, function(fl) {
    eff <- OM@Fleet[[stock1]][[fl]]@Effort
    if (!is.null(eff@TripsScalar) && !is.null(eff@AnglerPerTrip)) return('Anglers')
    if (!is.null(eff@TripsScalar)) return('Trips')
    'Effort'
  }) |> stats::setNames(fleets)
}

.ResolveEffortUnits <- function(object, fleets, units, byFleet) {
  avail <- .EffortUnitAvail(object, fleets)

  if (!is.null(units)) {
    units <- match.arg(units, names(.effort_unit_rank))
    bad <- fleets[.effort_unit_rank[avail] < .effort_unit_rank[units]]
    if (length(bad))
      cli::cli_abort(c(
        "x" = "`units = '{units}'` requested, but fleet{?s} {.val {bad}} {?doesn't/don't} have the data required for it.",
        "i" = "Available units by fleet: {.val {avail}}"
      ))
    return(stats::setNames(rep(units, length(fleets)), fleets))
  }

  if (isTRUE(byFleet))
    return(avail)

  uniform <- names(.effort_unit_rank)[min(.effort_unit_rank[avail])]
  stats::setNames(rep(uniform, length(fleets)), fleets)
}

.ApplyEffortUnits <- function(df, object, fleet_units) {
  OM     <- .ResolveOM(object)
  stock1 <- StockNames(OM)[1]

  purrr::map_dfr(unique(df$Fleet), function(fl) {
    sub  <- df[df$Fleet == fl, , drop = FALSE]
    unit <- fleet_units[[fl]]
    if (identical(unit, 'Effort'))
      return(sub)

    eff        <- OM@Fleet[[stock1]][[fl]]@Effort
    factor_arr <- eff@TripsScalar
    if (identical(unit, 'Anglers'))
      factor_arr <- ArrayMultiply(factor_arr, eff@AnglerPerTrip)

    factor_df <- Array2DF(factor_arr) |> dplyr::rename(Factor = .data$Value)
    if (length(unique(factor_df$Sim)) == 1)
      factor_df <- dplyr::select(factor_df, -"Sim")
    join_cols <- intersect(c('Sim', 'Year'), colnames(factor_df))

    sub |>
      dplyr::left_join(factor_df, by = join_cols) |>
      dplyr::mutate(Value = .data$Value * .data$Factor) |>
      dplyr::select(-"Factor")
  })
}

.LabelFleetUnits <- function(fleet_col, fleet_units) {
  lvl <- levels(fleet_col)
  if (is.null(lvl)) lvl <- unique(as.character(fleet_col))
  new_lvl <- paste0(lvl, ' (', fleet_units[lvl], ')')
  factor(new_lvl[match(as.character(fleet_col), lvl)], levels = new_lvl)
}


.ResolveStocks <- function(object, Stocks) {
  if (is.null(Stocks))
    return(NULL)

  allNames <- StockNames(object)
  n        <- length(allNames)

  if (is.numeric(Stocks)) {
    idx <- as.integer(Stocks)
    bad <- idx[is.na(idx) | idx < 1 | idx > n]
    if (length(bad))
      cli::cli_abort(
        "`Stocks` index {.val {bad}} out of range; `object` has {n} stock{?s}."
      )
    return(allNames[idx])
  }

  if (is.character(Stocks)) {
    bad <- Stocks[!Stocks %in% allNames]
    if (length(bad))
      cli::cli_abort(c(
        "{.val {bad}} {?is/are} not a valid stock name.",
        "i" = "Available stocks: {.val {allNames}}."
      ))
    return(Stocks)
  }

  cli::cli_abort("`Stocks` must be a character vector of stock names or a numeric vector of stock indices.")
}

.FilterStock <- function(df, stockNames) {
  if (is.null(stockNames) || !'Stock' %in% colnames(df))
    return(df)
  dplyr::filter(df, as.character(.data$Stock) %in% stockNames)
}


.NSelStock <- function(object, stockNames) {
  if (is.null(stockNames)) nStock(object) else length(stockNames)
}

.ResolveByStock <- function(byStock, nSel) {
  if (is.null(byStock))
    return(nSel > 1)

  if (!identical(byStock, TRUE) && !identical(byStock, FALSE) && !identical(byStock, 'sum'))
    cli::cli_abort("`byStock` must be `TRUE`, `FALSE`, `'sum'`, or `NULL`.")

  byStock
}

.SumOverStock <- function(df) {
  cnames <- colnames(df)
  group_vars <- cnames[!cnames %in% c('Stock', 'Value')]
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(Value = sum(.data$Value), .groups = 'drop') |>
    dplyr::mutate(Stock = 'Total')
}

.SumOverFleet <- function(df) {
  cnames <- colnames(df)
  group_vars <- cnames[!cnames %in% c('Fleet', 'Value')]
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(Value = sum(.data$Value), .groups = 'drop') |>
    dplyr::mutate(Fleet = 'Total')
}


.FemaleStockNames <- function(OM) {
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

.PlotSpawning <- function(object, slot_name, ylab, byStock, byFemale,
                          probs, nsim, Years, free_y, IncHist, byMP,
                          relative, type, Season, units = TRUE,
                          base_slot = 'Weight', Stocks = NULL) {
  .CheckClass(object, c('hist', 'mse'), 'object')
  stockNames <- .ResolveStocks(object, Stocks)
  nSel <- .NSelStock(object, stockNames)
  byStock <- .ResolveByStock(byStock, nSel)

  if (relative != 'none' && identical(byStock, 'sum') && nSel > 1) {
    cli::cli_alert_info(
      "Relative time series (`relative = '{relative}'`) cannot be meaningfully summed across stocks; faceting by stock instead."
    )
    byStock <- TRUE
  }

  if (relative == 'none') {
    df <- do.call(slot_name, list(object = object, df = TRUE))
    uinfo <- .MassUnitInfo(object, stockNames, units, base_slot, ylab)
    df$Value <- df$Value * uinfo$factor
    ylab <- .AppendUnits(ylab, uinfo$label)
  } else {
    extractArgs <- list(object = object, df = TRUE)
    if (relative == 'B0') extractArgs$type <- type
    df <- do.call(.RelativeFnName(slot_name, relative), extractArgs)
    ylab <- .RelativeYlab(ylab, relative)
  }

  df <- df |>
    .FilterStock(stockNames) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterSeason(Season, object) |>
    .FilterYears(Years)

  if (identical(byStock, 'sum')) {
    if (is.null(stockNames) && byFemale && nSel > 1) {
      female <- .FemaleStockNames(object@OM)
      if (female$ambiguous) {
        cli::cli_alert_info(
          "Cannot unambiguously identify a female stock in one or more complexes; faceting by stock instead."
        )
        byStock <- TRUE
      } else {
        df <- dplyr::filter(df, .data$Stock %in% female$stocks)
        df <- .SumOverStock(df)
        df$Stock <- 'Total (Female)'
      }
    } else {
      df <- .SumOverStock(df)
    }
  }

  .BuildTsPlot(df, byStock = byStock, byFleet = FALSE,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP, relative = relative != 'none')
}

.PlotCatch <- function(object, slot_name, ylab, byStock, byFleet,
                       probs, nsim, Years, free_y, IncHist, byMP, AggregateYear,
                       units = FALSE, Stocks = NULL) {
  .CheckClass(object, c('hist', 'mse'), 'object')
  stockNames <- .ResolveStocks(object, Stocks)
  byStock <- .ResolveByStock(byStock, .NSelStock(object, stockNames))
  if (is.null(byFleet)) byFleet <- nFleet(object) > 1

  df <- do.call(slot_name, list(object = object, df = TRUE, byFleet = TRUE))

  if (!isFALSE(units)) {
    uinfo <- .MassUnitInfo(object, stockNames, units, 'Weight', ylab)
    df$Value <- df$Value * uinfo$factor
    ylab <- .AppendUnits(ylab, uinfo$label)
  }

  df <- df |>
    .FilterStock(stockNames) |>
    .AggregateYear(AggregateYear, object) |>
    .BridgeMpGap() |>
    .DropHistorical(IncHist) |>
    .FilterYears(Years)

  if (!byFleet)
    df <- .SumOverFleet(df)
  if (identical(byStock, 'sum'))
    df <- .SumOverStock(df)

  .BuildTsPlot(df, byStock = byStock, byFleet = byFleet,
                ylab = ylab, probs = probs, nsim = nsim, free_y = free_y,
                colorVar = 'MP', byMP = byMP)
}


.BridgeMpGap <- function(df) {
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


.ReplicateHistPerMp <- function(df) {
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

.YearLabels <- function(breaks) {
  labels <- rep(NA_character_, length(breaks))
  ok     <- !is.na(breaks)
  whole  <- ok & abs(breaks - round(breaks)) < 1e-6
  labels[whole] <- format(round(breaks[whole]))

  frac <- ok & !whole
  if (any(frac))
    labels[frac] <- format(lubridate::date_decimal(breaks[frac]), '%Y-%m')

  labels
}


.RelativeFnName <- function(slot_name, relative) {
  switch(slot_name,
    Biomass     = if (relative == 'B0') 'B_B0'   else 'B_BMSY',
    SBiomass    = if (relative == 'B0') 'SB_SB0'  else 'SB_SBMSY',
    SProduction = if (relative == 'B0') 'SP_SP0'  else 'SP_SPMSY'
  )
}

.RelativeYlab <- function(ylab, relative) {
  abbr <- switch(ylab,
    'Biomass'             = 'B',
    'Spawning Biomass'    = 'SB',
    'Spawning Production' = 'SP',
    ylab
  )
  if (relative == 'B0') paste0(abbr, '/', abbr, '0') else paste0(abbr, '/', abbr, 'MSY')
}

.GgHuePal <- function(n) {
  if (n < 1) return(character(0))
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
}

.MpColorValues <- function(levels) {
  other  <- levels[levels != 'Historical']
  values <- stats::setNames(.GgHuePal(length(other)), other)
  if ('Historical' %in% levels)
    values <- c(Historical = 'grey30', values)
  values[levels]
}

.BuildTsPlot <- function(df, byStock, byFleet, ylab, probs, nsim,
                          free_y = TRUE, colorVar = NULL, linetypeVar = NULL,
                          byMP = FALSE, relative = FALSE) {

  mpVals    <- if ('MP' %in% colnames(df)) unique(df$MP) else character(0)
  nMPLevels <- length(mpVals[mpVals != 'Historical'])
  nStockRaw <- if ('Stock' %in% colnames(df)) length(unique(df$Stock)) else 1
  nFleetRaw <- if ('Fleet' %in% colnames(df)) length(unique(df$Fleet)) else 1

  colorStock <- identical(byStock, FALSE) && nStockRaw > 1
  if (colorStock) {
    if (nMPLevels > 1) byMP <- TRUE
    if (!is.null(colorVar) && colorVar != 'MP' && is.null(linetypeVar))
      linetypeVar <- colorVar
    colorVar <- 'Stock'
  }

  doFacetMP <- isTRUE(byMP) && nMPLevels > 1 &&
    !(isTRUE(byStock) && byFleet && nStockRaw > 1 && nFleetRaw > 1)

  if (doFacetMP) {
    df <- .ReplicateHistPerMp(df)
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
    mpValues <- .MpColorValues(unique(summ$MP))
    p <- p +
      ggplot2::scale_color_manual(values = mpValues) +
      ggplot2::scale_fill_manual(values = mpValues)
  }

  if (hasColor && colorVar == 'Variable') {
    varLevels <- levels(summ$Variable)
    if (is.null(varLevels)) varLevels <- sort(unique(summ$Variable))
    varValues <- stats::setNames(.GgHuePal(length(varLevels)), varLevels)
    p <- p +
      ggplot2::scale_color_manual(values = varValues) +
      ggplot2::scale_fill_manual(values = varValues)
  }

  if (hasColor && colorVar == 'Stock') {
    stockLevels <- levels(summ$Stock)
    if (is.null(stockLevels)) stockLevels <- sort(unique(summ$Stock))
    stockValues <- stats::setNames(.GgHuePal(length(stockLevels)), stockLevels)
    p <- p +
      ggplot2::scale_color_manual(values = stockValues) +
      ggplot2::scale_fill_manual(values = stockValues)
  }

  if (hasLinetype && linetypeVar == 'Variable') {
    p <- p + ggplot2::scale_linetype_manual(values = c(Landings = 'solid', Discards = 'dashed'))
  }

  nStockLevels <- if ('Stock' %in% colnames(summ)) length(unique(summ$Stock)) else 1
  nFleetLevels <- if ('Fleet' %in% colnames(summ)) length(unique(summ$Fleet)) else 1
  facetScales  <- if (free_y) 'free_y' else 'fixed'

  if (doFacetMP) {
    if (isTRUE(byStock) && nStockLevels > 1 && !(byFleet && nFleetLevels > 1)) {
      p <- p + ggplot2::facet_grid(Stock ~ MP, scales = facetScales)
    } else if (byFleet && nFleetLevels > 1) {
      p <- p + ggplot2::facet_grid(Fleet ~ MP, scales = facetScales)
    } else {
      p <- p + ggplot2::facet_wrap(~MP, scales = facetScales)
    }
  } else {
    if (isTRUE(byStock) && !byFleet && nStockLevels > 1)
      p <- p + ggplot2::facet_wrap(~Stock, scales = facetScales)

    if (byFleet && !isTRUE(byStock) && nFleetLevels > 1)
      p <- p + ggplot2::facet_wrap(~Fleet, scales = facetScales)

    if (isTRUE(byStock) && byFleet && nStockLevels > 1 && nFleetLevels > 1)
      p <- p + ggplot2::facet_grid(Stock ~ Fleet, scales = facetScales)
  }

  p <- p +
    ggplot2::expand_limits(y = if (relative) c(0, 1) else 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .YearLabels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab,
                 color    = if (hasColor) colorVar else NULL,
                 fill     = if (hasColor) colorVar else NULL,
                 linetype = if (hasLinetype) linetypeVar else NULL)

  if (nsim > 0)
    p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())

  p
}
