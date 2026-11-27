#' Plot MSE Performance
#'
#' Plots of management procedure performance from one or more [mse-class]
#' objects, drawn with the plotting functions of the \pkg{Slick} package.
#' Each function converts `object` with [MSE2Slick()] and passes the result
#' to the matching \pkg{Slick} function:
#'
#' - `PlotKobe()`: SB/SBMSY against F/FMSY ([Slick::plotKobe()]).
#' - `PlotTradeoff()`: one performance metric against another
#'   ([Slick::plotTradeoff()]).
#' - `PlotPMBoxplot()`: the distribution of each performance metric across
#'   simulations ([Slick::plotBoxplot()]).
#' - `PlotQuilt()`: a table of performance metrics by MP
#'   ([Slick::plotQuilt()]).
#' - `PlotSpider()`: performance metrics on a radial plot
#'   ([Slick::plotSpider()]).
#' - `PlotPMTimeseries()`: time series of a stock status or catch variable by
#'   MP ([Slick::plotTimeseries()]).
#'
#' Requires the \pkg{Slick} package.
#'
#' @param object An [mse-class] object, a named list of [mse-class] objects
#'   (one per operating model; see [MSE2Slick()]), or a `Slick` object
#'   returned by [MSE2Slick()].
#' @param PMs A list of performance-metric specifications, or `NULL`
#'   (default) for [DefaultSlickPMs()]. See [MSE2Slick()]. Ignored when
#'   `object` is a `Slick` object.
#' @param Complex Character, or `NULL` (default: the first complex). The
#'   stock or complex shown in the Kobe plot.
#' @param Years Numeric, or `NULL` (default: the years the MPs are active).
#'   The calendar years of the Kobe plot.
#' @param Limit For `PlotKobe()`, the limits of SB/SBMSY and F/FMSY. Default
#'   `c(0.4, 1)`. For `PlotPMTimeseries()`, a numeric vector named by `Code`
#'   giving the limit lines, or `NULL` for the defaults of [MSE2Slick()].
#' @param Target Numeric vector named by `Code`, or `NULL` (default). The
#'   target lines of `PlotPMTimeseries()`.
#' @param Code Character. The time-series variables available to
#'   `PlotPMTimeseries()`, or `NULL` (default) for
#'   `c('SB_SBMSY', 'F_FMSY', 'Landings')`.
#' @param PI The performance metric (for `PlotPMBoxplot()`) or time-series
#'   variable (for `PlotPMTimeseries()`) to plot, by position or code.
#' @param xPI,yPI The performance metrics on the x and y axes of
#'   `PlotTradeoff()`, by position or code.
#' @param ... Additional arguments passed to the \pkg{Slick} plotting
#'   function.
#'
#' @return The plot returned by the \pkg{Slick} function.
#' @examples
#' \donttest{
#' if (requireNamespace('Slick', quietly = TRUE)) {
#'   MSE <- runMSE(SingleStockOM, MPs = c('CurrentCatch', 'AverageCatch'), nSim = 4)
#'   PlotKobe(MSE)
#'   if (requireNamespace('cowplot', quietly = TRUE))
#'     PlotTradeoff(MSE)
#'   PlotPMBoxplot(MSE)
#'   if (requireNamespace('flextable', quietly = TRUE))
#'     PlotQuilt(MSE)
#'   PlotPMTimeseries(MSE, PI = 'F_FMSY')
#' }
#' }
#' @seealso [MSE2Slick()], [DefaultSlickPMs()], [PM], [plot_hist]
#' @name plot_performance
NULL

#' @rdname plot_performance
#' @export
PlotKobe <- function(object, Complex = NULL, Years = NULL, Limit = c(0.4, 1), ...) {
  slick <- .AsSlick(object, KobeComplex = Complex, KobeYears = Years, KobeLimit = Limit)
  Slick::plotKobe(slick, ...)
}

#' @rdname plot_performance
#' @export
PlotTradeoff <- function(object, PMs = NULL, xPI = 1, yPI = 2, ...) {
  slick <- .AsSlick(object, PMs = PMs)
  Slick::plotTradeoff(slick, xPI = .SlickPI(xPI, Slick::Tradeoff(slick)),
                      yPI = .SlickPI(yPI, Slick::Tradeoff(slick)), ...)
}

#' @rdname plot_performance
#' @export
PlotPMBoxplot <- function(object, PMs = NULL, PI = NULL, ...) {
  slick <- .AsSlick(object, PMs = PMs)
  Slick::plotBoxplot(slick, PI = .SlickPI(PI, Slick::Boxplot(slick)), ...)
}

#' @rdname plot_performance
#' @export
PlotQuilt <- function(object, PMs = NULL, ...) {
  slick <- .AsSlick(object, PMs = PMs)
  Slick::plotQuilt(slick, ...)
}

#' @rdname plot_performance
#' @export
PlotSpider <- function(object, PMs = NULL, ...) {
  slick <- .AsSlick(object, PMs = PMs)
  Slick::plotSpider(slick, ...)
}

#' @rdname plot_performance
#' @export
PlotPMTimeseries <- function(object, PI = 1, Code = NULL, Target = NULL, Limit = NULL, ...) {
  slick <- .AsSlick(object, TimeseriesCode = Code, TimeseriesTarget = Target,
                    TimeseriesLimit = Limit)
  Slick::plotTimeseries(slick, PI = .SlickPI(PI, Slick::Timeseries(slick)), ...)
}

.AsSlick <- function(object, ...) {
  if (inherits(object, 'Slick'))
    return(object)
  MSE2Slick(object, ...)
}

.SlickPI <- function(PI, panel) {
  if (!is.character(PI))
    return(PI)
  codes <- Slick::Code(panel)
  ind   <- match(PI, codes)
  if (anyNA(ind))
    cli::cli_abort(c("x" = "Unknown performance indicator{?s} {.val {PI[is.na(ind)]}}.",
                     "i" = "Available: {.val {codes}}."), call = NULL)
  ind
}
