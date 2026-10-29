
#' Print a `RepList` object
#'
#' Print a summary of a `RepList` object, which is a list of outputs
#' from `r4ss::SS_output`.
#'
#' @param x A `RepList` object.
#' @param ... Additional arguments (currently ignored).
#'
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.RepList <- function(x, ...) {
  n <- length(x)
  cli::cli_h2("A list of length {.val {n}} of output from `r4ss::SS_output`")

  if (!is.null(names(x))) {
    cli::cli_text("Elements: {.val {head(names(x), 5)}}{? ...}")
  }

  invisible(x)
}

#' Print a `PlotStockList` object
#'
#' Draws every panel in `x` to the active graphics device.
#'
#' @param x A `PlotStockList` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.PlotStockList <- function(x, ...) {
  if (!isTRUE(attr(x, 'silent')))
    cli::cli_alert_info("Plotting {.val {length(x)}} panel{?s}: {.val {names(x)}}")
  for (p in x) print(p)
  invisible(x)
}

#' Print a `PlotFleetList` object
#'
#' Draws every panel in `x` to the active graphics device.
#'
#' @param x A `PlotFleetList` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.PlotFleetList <- function(x, ...) {
  if (!isTRUE(attr(x, 'silent')))
    cli::cli_alert_info("Plotting {.val {length(x)}} panel{?s}: {.val {names(x)}}")
  for (p in x) print(p)
  invisible(x)
}

