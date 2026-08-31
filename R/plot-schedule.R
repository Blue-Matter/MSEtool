#' Plot Biological and Gear Schedules
#'
#' Line plots of an age-based schedule (selectivity, retention, discard
#' mortality, length, weight, maturity, natural mortality, fecundity) against
#' age, or (via `x = "Length"`) against length instead. By default, each
#' independent curve (the historical schedule, and each MP's projection
#' schedule for [mse-class] objects) is drawn for its own last available
#' year only; pass `Years` to see more.
#'
#' `PlotSelectivityCurve()`, `PlotRetentionCurve()`, and
#' `PlotDiscardMortalityCurve()` plot a single fleet-level gear curve
#' (`[Fleet()]`'s `Selectivity`/`Retention`/`DiscardMortality` sub-objects)
#' on a chosen `x` axis. For the projection period of [mse-class] objects,
#' these reflect any change made by the MP via `Advice()` (see
#' [VBiomass()], which resolves the same effective curves). Each has a
#' combining counterpart -- [PlotSelectivity()], [PlotRetention()],
#' [PlotDiscardMortality()] -- that arranges every plot available for that
#' schedule into one figure.
#'
#' `PlotLengthCurve()`, `PlotWeightCurve()`, `PlotMaturityCurve()`,
#' `PlotNaturalMortalityCurve()`, and `PlotFecundityCurve()` plot a single
#' stock-level biological curve on a chosen `x` axis. Each has a combining
#' counterpart -- [PlotLength()], [PlotWeight()], [PlotMaturity()],
#' [PlotNaturalMortality()], [PlotFecundity()] -- that arranges every plot
#' available for that schedule (the age-based curve, the length-based curve
#' when populated, and an age-size key where one exists) into one figure.
#'
#' @param object An [om-class], [hist-class], or [mse-class] object (`om`
#'   objects are populated via [PopulateOM()] if not already), or -- for the
#'   `*Curve()` functions only -- a [stock-class]/[fleet-class] object.
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
#' @param Years Optional numeric vector, or `"all"`. Default `NULL` shows
#'   each independent curve's own last calendar year, labelled by year in
#'   the legend (legend dropped if the curve is constant over that year).
#'   `"all"` shows every available year. Both `NULL` and `"all"` collapse
#'   consecutive years with an identical curve to just the breakpoint
#'   years. A numeric vector shows exactly those years, uncompressed.
#' @param units Logical or a character unit string. `TRUE` (default) labels
#'   axes with the relevant `Units` slot (e.g. `Ages@Units`; also
#'   `Length@Units`/`Weight@Units` for `PlotLengthCurve()`/
#'   `PlotWeightCurve()`). `FALSE` suppresses unit labelling. For
#'   `PlotLengthCurve()`/`PlotWeightCurve()` only, a unit string (e.g.
#'   `"cm"`, `"kg"`) relabels *and rescales* the y-axis into that unit; an
#'   unconvertible unit is an error. No effect beyond the x-axis label for
#'   the unitless-proportion plots (`PlotMaturityCurve()`,
#'   `PlotSelectivityCurve()`, etc.).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param x `"Age"` (default) or `"Length"`. `"Length"` projects the curve
#'   through the stock's `[Length()]` age-length key (`@ALK`) onto a length
#'   axis instead -- e.g. `PlotWeightCurve(x = "Length")` gives the
#'   weight-length relationship, `PlotMaturityCurve(x = "Length")` gives
#'   maturity-at-length. Not meaningful for `PlotLengthCurve()` itself (see
#'   [PlotLength()]; length-at-length is a trivial identity, and errors if
#'   requested). The length axis is always labelled in the stock's native
#'   `Length@Units`, independent of `units`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' PlotSelectivityCurve(Hist)
#' PlotMaturityCurve(Hist)
#'
#' MSE <- Project(Hist, ExampleMPs())
#' PlotRetentionCurve(MSE)
#' PlotWeightCurve(MSE, Sim = 3)
#' }
#'
#' @name plot_schedule
#' @seealso [Fleet()], [Stock()], [VBiomass()], [PlotSelectivity()],
#'   [PlotRetention()], [PlotDiscardMortality()], [PlotLength()],
#'   [PlotWeight()], [PlotMaturity()], [PlotNaturalMortality()],
#'   [PlotFecundity()]
NULL

#' Plot the Selectivity Schedule
#'
#' `PlotSelectivity()` arranges every plot related to a fleet's
#' [selectivity-class] object -- the age-based curve
#' (`PlotSelectivityCurve()`), the length-based curve (when
#' `Selectivity@MeanAtLength` is populated), and the weight-based curve
#' (when `Selectivity@MeanAtWeight` is populated) -- into a single figure
#' with [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object A [fleet-class] object, or an [om-class], [hist-class], or
#'   [mse-class] object.
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `object` (`Length`/`Weight` for
#'   projecting onto those axes, etc.). Ignored otherwise. Default `NULL`
#'   uses an example stock, with a message noting this.
#'
#' @return `PlotSelectivityCurve()` returns a `ggplot` object;
#'   `PlotSelectivity()` returns a `patchwork` object (or a plain `ggplot`
#'   when only the age-based curve is available).
#'
#' @param x `PlotSelectivityCurve()` only; see [plot_schedule]. Also accepts
#'   `"Weight"`, projecting the curve through the stock's `[Weight()]`
#'   age-weight key (`@AWK`) onto a weight axis; requires
#'   `Weight@CVatAge` to be set.
#'
#' @seealso [plot_schedule]
#' @export
PlotSelectivity <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL, Years = NULL,
                            units = TRUE, Stocks = NULL, Stock = NULL, probs = c(0.05, 0.95)) {
  .CheckClass(object, c('fleet', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)
  .PlotGearScheduleCombined(object, 'Selectivity', hasWeight = TRUE, Sim, byStock, byFleet,
                            Years, units, Stocks, probs)
}

#' @rdname PlotSelectivity
#' @export
PlotSelectivityCurve <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL, Years = NULL,
                                 units = TRUE, Stocks = NULL, Stock = NULL,
                                 x = c('Age', 'Length', 'Weight'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .CheckClass(object, c('fleet', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)
  .PlotGearSchedule(object, 'Selectivity', Sim, byStock, byFleet, Years, units, Stocks, x, probs)
}

#' Plot the Retention Schedule
#'
#' `PlotRetention()` arranges every plot related to a fleet's
#' [retention-class] object -- the age-based curve (`PlotRetentionCurve()`),
#' the length-based curve (when `Retention@MeanAtLength` is populated), and
#' the weight-based curve (when `Retention@MeanAtWeight` is populated) --
#' into a single figure with [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object A [fleet-class] object, or an [om-class], [hist-class], or
#'   [mse-class] object.
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `object`. Ignored otherwise. Default
#'   `NULL` uses an example stock, with a message noting this.
#'
#' @return `PlotRetentionCurve()` returns a `ggplot` object;
#'   `PlotRetention()` returns a `patchwork` object (or a plain `ggplot`
#'   when only the age-based curve is available).
#'
#' @param x `PlotRetentionCurve()` only; see [PlotSelectivityCurve()].
#'
#' @seealso [plot_schedule]
#' @export
PlotRetention <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL, Years = NULL,
                          units = TRUE, Stocks = NULL, Stock = NULL, probs = c(0.05, 0.95)) {
  .CheckClass(object, c('fleet', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)
  .PlotGearScheduleCombined(object, 'Retention', hasWeight = TRUE, Sim, byStock, byFleet,
                            Years, units, Stocks, probs)
}

#' @rdname PlotRetention
#' @export
PlotRetentionCurve <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL, Years = NULL,
                               units = TRUE, Stocks = NULL, Stock = NULL,
                               x = c('Age', 'Length', 'Weight'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .CheckClass(object, c('fleet', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)
  .PlotGearSchedule(object, 'Retention', Sim, byStock, byFleet, Years, units, Stocks, x, probs)
}

#' Plot the Discard Mortality Schedule
#'
#' `PlotDiscardMortality()` arranges every plot related to a fleet's
#' [discardmortality-class] object -- the age-based curve
#' (`PlotDiscardMortalityCurve()`) and the length-based curve (when
#' `DiscardMortality@MeanAtLength` is populated) -- into a single figure
#' with [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object A [fleet-class] object, or an [om-class], [hist-class], or
#'   [mse-class] object.
#' @param Stock A [stock-class] object supplying the biology needed to
#'   populate a bare [fleet-class] `object`. Ignored otherwise. Default
#'   `NULL` uses an example stock, with a message noting this.
#'
#' @return `PlotDiscardMortalityCurve()` returns a `ggplot` object;
#'   `PlotDiscardMortality()` returns a `patchwork` object (or a plain
#'   `ggplot` when only the age-based curve is available).
#'
#' @seealso [plot_schedule]
#' @export
PlotDiscardMortality <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL, Years = NULL,
                                 units = TRUE, Stocks = NULL, Stock = NULL, probs = c(0.05, 0.95)) {
  .CheckClass(object, c('fleet', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)
  .PlotGearScheduleCombined(object, 'DiscardMortality', hasWeight = FALSE, Sim, byStock, byFleet,
                            Years, units, Stocks, probs)
}

#' @rdname PlotDiscardMortality
#' @export
PlotDiscardMortalityCurve <- function(object, Sim = NULL, byStock = NULL, byFleet = NULL, Years = NULL,
                                      units = TRUE, Stocks = NULL, Stock = NULL,
                                      x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .CheckClass(object, c('fleet', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'fleet')) object <- .FleetToShellHist(object, Stock)
  .PlotGearSchedule(object, 'DiscardMortality', Sim, byStock, byFleet, Years, units, Stocks, x, probs)
}

#' Plot the Length-at-Age Schedule
#'
#' `PlotLength()` arranges every plot related to a stock's [length-class]
#' object -- the mean length-at-age curve (`PlotLengthCurve()`) and the
#' age-length key (`PlotALK()`) -- into a single figure with
#' [patchwork::wrap_plots()]. The `ALK` panel is omitted when the
#' stock's `Length@CVatAge` isn't set (so `ALK` was not populated).
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], [mse-class], or
#'   [stock-class] object.
#'
#' @return `PlotLengthCurve()` returns a `ggplot` object; `PlotLength()`
#'   returns a `patchwork` object.
#'
#' @param x `PlotLengthCurve()` only. Always `"Age"`; `"Length"` is not
#'   meaningful (length-at-length is a trivial identity) and errors if
#'   requested.
#'
#' @seealso [PlotALK()], [plot_schedule]
#' @export
PlotLength <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                       probs = c(0.05, 0.95)) {
  panels <- purrr::compact(list(
    Curve = PlotLengthCurve(object, Sim = Sim, byStock = byStock, Years = Years, units = units,
                            Stocks = Stocks, probs = probs),
    ALK   = PlotALK(object, Sim = Sim, byStock = byStock, Stocks = Stocks, Years = Years, units = units)
  ))
  patchwork::wrap_plots(panels, ncol = 1)
}

#' @rdname PlotLength
#' @export
PlotLengthCurve <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                            x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Length', Sim, byStock, Years, units, Stocks, x, probs)
}

#' Plot the Weight-at-Age Schedule
#'
#' `PlotWeight()` arranges every plot related to a stock's [weight-class]
#' object -- the mean weight-at-age curve (`PlotWeightCurve()`), the
#' weight-at-length curve (when `Weight@MeanAtLength` is populated), and
#' the age-weight key (`PlotAWK()`, when `Weight@CVatAge` is set) -- into a
#' single figure with [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], [mse-class], or
#'   [stock-class] object.
#'
#' @return `PlotWeightCurve()` returns a `ggplot` object; `PlotWeight()`
#'   returns a `patchwork` object (or a plain `ggplot` when only the
#'   age-based curve is available).
#'
#' @param x `PlotWeightCurve()` only; see [plot_schedule].
#'
#' @seealso [PlotAWK()], [plot_schedule]
#' @export
PlotWeight <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                       probs = c(0.05, 0.95)) {
  .PlotScheduleCombined(object, 'Weight', PlotAWK, Sim, byStock, Years, units, Stocks, probs)
}

#' @rdname PlotWeight
#' @export
PlotWeightCurve <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                            x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Weight', Sim, byStock, Years, units, Stocks, x, probs)
}

#' Plot the Maturity-at-Age Schedule
#'
#' `PlotMaturity()` arranges every plot related to a stock's
#' [maturity-class] object -- the mean maturity-at-age curve
#' (`PlotMaturityCurve()`), the maturity-at-length curve (when
#' `Maturity@MeanAtLength` is populated), and the maturity-at-weight curve
#' (when `Maturity@MeanAtWeight` is populated) -- into a single figure with
#' [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], [mse-class], or
#'   [stock-class] object.
#'
#' @return `PlotMaturityCurve()` returns a `ggplot` object; `PlotMaturity()`
#'   returns a `patchwork` object (or a plain `ggplot` when only the
#'   age-based curve is available).
#'
#' @param x `PlotMaturityCurve()` only. `"Age"` (default), `"Length"`, or
#'   `"Weight"` -- `"Weight"` projects the curve through the stock's
#'   `[Weight()]` age-weight key (`@AWK`) onto a weight axis, and requires
#'   `Weight@CVatAge` to be set.
#'
#' @seealso [plot_schedule]
#' @export
PlotMaturity <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                         probs = c(0.05, 0.95)) {
  .PlotScheduleCombined(object, 'Maturity', NULL, Sim, byStock, Years, units, Stocks, probs)
}

#' @rdname PlotMaturity
#' @export
PlotMaturityCurve <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                              x = c('Age', 'Length', 'Weight'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Maturity', Sim, byStock, Years, units, Stocks, x, probs)
}

#' Plot the Natural Mortality-at-Age Schedule
#'
#' `PlotNaturalMortality()` arranges every plot related to a stock's
#' [naturalmortality-class] object -- the mean natural-mortality-at-age
#' curve (`PlotNaturalMortalityCurve()`) and the natural-mortality-at-length
#' curve (when `NaturalMortality@MeanAtLength` is populated) -- into a
#' single figure with [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], [mse-class], or
#'   [stock-class] object.
#'
#' @return `PlotNaturalMortalityCurve()` returns a `ggplot` object;
#'   `PlotNaturalMortality()` returns a `patchwork` object (or a plain
#'   `ggplot` when only the age-based curve is available).
#'
#' @param x `PlotNaturalMortalityCurve()` only; see [plot_schedule].
#'
#' @seealso [plot_schedule]
#' @export
PlotNaturalMortality <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                                 probs = c(0.05, 0.95)) {
  .PlotScheduleCombined(object, 'NaturalMortality', NULL, Sim, byStock, Years, units, Stocks, probs)
}

#' @rdname PlotNaturalMortality
#' @export
PlotNaturalMortalityCurve <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                                      x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'NaturalMortality', Sim, byStock, Years, units, Stocks, x, probs)
}

#' Plot the Fecundity-at-Age Schedule
#'
#' `PlotFecundity()` arranges every plot related to a stock's
#' [fecundity-class] object -- the mean fecundity-at-age curve
#' (`PlotFecundityCurve()`) and the fecundity-at-length curve (when
#' `Fecundity@MeanAtLength` is populated) -- into a single figure with
#' [patchwork::wrap_plots()].
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], [mse-class], or
#'   [stock-class] object.
#'
#' @return `PlotFecundityCurve()` returns a `ggplot` object;
#'   `PlotFecundity()` returns a `patchwork` object (or a plain `ggplot`
#'   when only the age-based curve is available).
#'
#' @param x `PlotFecundityCurve()` only; see [plot_schedule].
#'
#' @seealso [plot_schedule]
#' @export
PlotFecundity <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                          probs = c(0.05, 0.95)) {
  .PlotScheduleCombined(object, 'Fecundity', NULL, Sim, byStock, Years, units, Stocks, probs)
}

#' @rdname PlotFecundity
#' @export
PlotFecundityCurve <- function(object, Sim = NULL, byStock = NULL, Years = NULL, units = TRUE, Stocks = NULL,
                               x = c('Age', 'Length'), probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .PlotStockSchedule(object, 'Fecundity', Sim, byStock, Years, units, Stocks, x, probs)
}

# ---- internal helpers ----

.HasMeanAtLength <- function(object, what, Stocks) {
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  any(purrr::map_lgl(stockNames, \(nm) !is.null(slot(OM@Stock[[nm]], what)@MeanAtLength)))
}

.HasMeanAtWeight <- function(object, what, Stocks) {
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  any(purrr::map_lgl(stockNames, \(nm) {
    obj <- slot(OM@Stock[[nm]], what)
    'MeanAtWeight' %in% methods::slotNames(obj) && !is.null(obj@MeanAtWeight)
  }))
}

.PlotScheduleCombined <- function(object, what, keyFn, Sim, byStock, Years, units, Stocks, probs) {
  panels <- list(
    AtAge = .PlotStockSchedule(object, what, Sim, byStock, Years, units, Stocks, 'Age', probs)
  )
  if (.HasMeanAtLength(object, what, Stocks))
    panels$AtLength <- .PlotStockSchedule(object, what, Sim, byStock, Years, units, Stocks, 'Length', probs)
  if (.HasMeanAtWeight(object, what, Stocks))
    panels$AtWeight <- .PlotStockSchedule(object, what, Sim, byStock, Years, units, Stocks, 'Weight', probs)
  if (!is.null(keyFn))
    panels$Key <- keyFn(object, Sim = Sim, byStock = byStock, Stocks = Stocks, Years = Years, units = units)

  panels <- purrr::compact(panels)
  if (length(panels) == 1) return(panels[[1]])
  patchwork::wrap_plots(panels, ncol = 1)
}

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

# Wraps a Fleet (+ companion Stock) in a minimal hist shell, mirroring
# .StockToShellHist(), so the existing hist/mse/om-oriented gear-schedule
# machinery can be reused directly on a bare Fleet. `Stock` supplies the
# biology (Length/Weight/etc.) PopulateFleet() needs; when NULL, an example
# stock is used instead, with a message noting this (a bare Fleet has no
# Stock of its own to draw on).
.FleetToShellHist <- function(Fleet, Stock = NULL) {
  if (is.null(Stock)) {
    cli::cli_alert_info(
      "No {.arg Stock} provided; using example stock {.val {AlbacoreExStock@Name}} to populate this fleet."
    )
    Stock <- AlbacoreExStock
  }
  .CheckClass(Stock, 'stock', 'Stock')

  if (is.null(Stock@Length@MeanAtAge))
    Stock <- PopulateStock(Stock, nYear = 20, pYear = 0, nSim = 5, silent = TRUE)

  Fleet <- PopulateFleet(Fleet, Stock, silent = TRUE)

  stockName <- Stock@Name %||% 'Stock'
  fleetName <- Fleet@Name %||% 'Fleet'

  ShellOM             <- methods::new('om')
  ShellOM@Stock       <- stats::setNames(list(Stock), stockName)
  ShellOM@Fleet       <- stats::setNames(list(stats::setNames(list(Fleet), fleetName)), stockName)
  ShellOM@nYear       <- Stock@nYear
  ShellOM@pYear       <- Stock@pYear
  ShellOM@CurrentYear <- Stock@CurrentYear
  ShellOM@Seasons     <- Stock@Seasons
  ShellHist           <- methods::new('hist')
  ShellHist@OM        <- ShellOM
  ShellHist
}

.HasGearMeanAtLength <- function(object, what, Stocks) {
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  fleetNames <- FleetNames(OM)
  any(purrr::map_lgl(stockNames, \(nm)
    any(purrr::map_lgl(fleetNames, \(fl) !is.null(slot(OM@Fleet[[nm]][[fl]], what)@MeanAtLength)))
  ))
}

.HasGearMeanAtWeight <- function(object, what, Stocks) {
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  fleetNames <- FleetNames(OM)
  any(purrr::map_lgl(stockNames, \(nm)
    any(purrr::map_lgl(fleetNames, \(fl) {
      obj <- slot(OM@Fleet[[nm]][[fl]], what)
      'MeanAtWeight' %in% methods::slotNames(obj) && !is.null(obj@MeanAtWeight)
    }))
  ))
}

.PlotGearScheduleCombined <- function(object, what, hasWeight, Sim, byStock, byFleet, Years, units,
                                       Stocks, probs) {
  panels <- list(
    AtAge = .PlotGearSchedule(object, what, Sim, byStock, byFleet, Years, units, Stocks, 'Age', probs)
  )
  if (.HasGearMeanAtLength(object, what, Stocks))
    panels$AtLength <- .PlotGearSchedule(object, what, Sim, byStock, byFleet, Years, units, Stocks, 'Length', probs)
  if (hasWeight && .HasGearMeanAtWeight(object, what, Stocks))
    panels$AtWeight <- .PlotGearSchedule(object, what, Sim, byStock, byFleet, Years, units, Stocks, 'Weight', probs)

  panels <- purrr::compact(panels)
  if (length(panels) == 1) return(panels[[1]])
  patchwork::wrap_plots(panels, ncol = 1)
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
    } else if (x == 'Weight') {
      schedObj <- .MeanAtAge2MeanAtWeight(schedObj, OM@Stock[[st]]@Weight, replace = TRUE, Years = allYears)
      if (is.null(schedObj@MeanAtWeight))
        cli::cli_abort(c(
          "x" = "{.arg x = \"Weight\"} requires a populated age-weight key for stock {.val {allStocks[st]}}.",
          "i" = "Set {.field Weight@CVatAge} (so {.field AWK} is populated) or supply {.field {what}@MeanAtWeight} directly."
        ))
      arr <- schedObj@MeanAtWeight |> .SubsetYear(allYears)
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
  if (x == 'Weight') {
    if (isFALSE(units)) return('Weight')
    return(.AppendUnits('Weight', .GetStockUnits(OM, 'Weight', stockNames)))
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
