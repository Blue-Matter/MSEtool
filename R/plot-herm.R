#' Plot Stock Transitions (Hermaphroditism)
#'
#' Plots the fraction of individuals remaining in the `From` stock of each
#' [stocktransition-class] in `OM@Herm` (see [Herm()]), by age or by length.
#' Each `From -> To` pair is drawn as its own line. When the operating model
#' has no stock transitions, a message is printed and `NULL` is returned
#' invisibly.
#'
#' @inheritParams plot_schedule
#' @param object An [om-class], [hist-class], or [mse-class] object.
#' @param x `"Age"` (default) or `"Length"`. `"Length"` projects the
#'   at-age fraction onto the length classes of the `From` stock with its
#'   age-length key.
#'
#' @return A `ggplot` object, or `NULL` invisibly when `OM@Herm` is empty.
#' @examples
#' PlotHerm(HermOM)
#' PlotHerm(HermOM, x = 'Length')
#' @seealso [Herm()], [stocktransition-class]
#' @export
PlotHerm <- function(object, Sim = NULL, Years = NULL, units = TRUE, x = c('Age', 'Length'),
                     probs = c(0.05, 0.95)) {
  x <- match.arg(x)
  .CheckClass(object, c('om', 'hist', 'mse'), 'object')
  OM <- .ResolveOM(object)

  HermList <- OM@Herm
  if (inherits(HermList, 'stocktransition')) HermList <- list(HermList)
  if (!length(HermList)) {
    cli::cli_alert_info("No stock transitions ({.field Herm}) in this operating model; nothing to plot.")
    return(invisible(NULL))
  }

  allStocks <- StockNames(OM)
  allYears  <- Years(object, 'Historical')
  if (inherits(object, 'mse')) allYears <- c(allYears, Years(object, 'Projection'))

  fromInd <- purrr::map_int(HermList, \(h) .HermStockIndex(h@From, allStocks))
  df <- purrr::map(seq_along(HermList), \(i) {
    h    <- HermList[[i]]
    from <- fromInd[i]
    to   <- .HermStockIndex(h@To, allStocks)
    arr  <- h@Frac
    if (!'Year' %in% names(dimnames(arr)))
      arr <- AddDimension(arr, 'Year', val = allYears[1])
    if (x == 'Length') {
      Carrier <- methods::new('maturity', MeanAtAge = arr)
      arr <- .MeanAtAge2MeanAtLength(Carrier, OM@Stock[[from]]@Length, replace = TRUE,
                                     Years = allYears)@MeanAtLength
      names(dimnames(arr))[names(dimnames(arr)) == 'Class'] <- 'Age'
    }
    arr <- .SubsetYear(arr, allYears)
    Array2DF(arr) |> dplyr::mutate(Stock = paste(allStocks[from], '->', allStocks[to]))
  }) |> dplyr::bind_rows() |> .FilterYears(Years)

  xlab <- .XAxisLabel(OM, allStocks[unique(fromInd)], units, x)
  p <- .BuildSchedulePlot(df, Sim = Sim, byStock = FALSE, byFleet = FALSE,
                     ylab = 'Fraction remaining in From', xlab = xlab,
                     defaultYears = is.null(Years),
                     breakpointYears = is.null(Years) || identical(Years, 'all'),
                     probs = probs) +
    ggplot2::coord_cartesian(ylim = c(0, 1))
  if (length(HermList) > 1)
    p <- p + ggplot2::labs(color = 'Transition')
  if (length(HermList) == 1)
    p <- p + ggplot2::labs(subtitle = unique(df$Stock))
  p
}
