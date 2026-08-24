#' Plot Initial/Final Depletion
#'
#' Histogram of each stock's sampled depletion values (relative to
#' `Depletion@Reference`, e.g. `"B0"` or `"SB0"`) across simulation
#' replicates; the `Initial`/`Final` slots of `[Depletion()]`. Both are
#' shown, side by side, when a stock has both populated.
#'
#' @param object   [stock-class], [om-class], [hist-class], or [mse-class] object.
#' @param byStock One of `TRUE`, `FALSE`, or `NULL` (default, facets
#'   automatically when `object` has more than one stock).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param bins Integer. Number of histogram bins. Default `15`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [Depletion()], [PlotSRR()], [Stock()]
#' @export
PlotDepletion <- function(object, byStock = NULL, Stocks = NULL, bins = 15) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  allStocks  <- StockNames(OM)

  df <- purrr::map(seq_along(allStocks), \(st) {
    if (!allStocks[st] %in% stockNames) return(NULL)
    Dep <- OM@Stock[[st]]@Depletion
    purrr::map(c('Initial', 'Final'), \(what) {
      val <- slot(Dep, what)
      if (!length(val)) return(NULL)
      data.frame(Stock = allStocks[st], Type = what, Value = as.numeric(val))
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  if (!nrow(df))
    cli::cli_abort("No populated {.field Initial}/{.field Final} depletion values found.")

  refs <- purrr::map_chr(seq_along(allStocks), \(st) OM@Stock[[st]]@Depletion@Reference)
  names(refs) <- allStocks
  xlab <- if (length(unique(refs[stockNames])) == 1)
    paste0('Depletion (', unique(refs[stockNames]), ')') else 'Depletion'

  facetVars <- character(0)
  if (!isFALSE(byStock) && length(unique(df$Stock)) > 1)
    facetVars <- c(facetVars, 'Stock')

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Value, fill = .data$Type)) +
    ggplot2::geom_histogram(bins = bins, position = 'identity', alpha = 0.5, color = NA) +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(x = c(0,1)) +
    ggplot2::labs(x = xlab, y = 'Count', fill = NULL)

  if (length(unique(df$Type)) == 1)
    p <- p + ggplot2::guides(fill = 'none')

  if (length(facetVars))
    p <- p + ggplot2::facet_wrap(facetVars, scales = 'free')

  p
}
