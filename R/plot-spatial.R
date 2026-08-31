#' Plot Spatial Structure
#'
#' Visualizes the spatial structure of one or more stocks: the area-to-area
#' movement probability matrix (as a heatmap) and the equilibrium
#' (unfished) distribution of biomass across areas (as a bar chart), side
#' by side.
#'
#' Both panels are evaluated at a single age class and calendar year, since
#' the underlying `Movement` and `UnfishedDist` arrays can vary by age and
#' year. Simulation replicates are averaged. Stocks with only one area have
#' no spatial structure to show and are skipped with a message.
#'
#' @param object A [stock-class], [om-class], [hist-class], or [mse-class]
#'   object.
#' @param Stocks Character or numeric vector restricting the plot to
#'   specific stocks, either by name (matching [StockNames()]) or by index.
#'   Default `NULL` includes every stock in `object`.
#' @param byStock Logical. Facet the panels by stock? Default `NULL` facets
#'   automatically when more than one selected stock has more than one
#'   area.
#' @param Age The age class at which to evaluate `Movement`/`UnfishedDist`,
#'   matched against the age dimension of those arrays. Default `NULL` uses
#'   each stock's first age class.
#' @param Year The calendar year at which to evaluate `Movement`/
#'   `UnfishedDist`, matched against the year dimension of those arrays.
#'   Default `NULL` uses the last historical year.
#'
#' @return A `patchwork` object combining the movement-matrix heatmap and
#'   the equilibrium area-distribution bar chart. Invisibly returns `NULL`
#'   (with a message) if no selected stock has more than one area.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SeasonalSpatialOM)
#' PlotSpatial(Hist)
#' PlotSpatial(Hist, Stocks = 1, Age = 5)
#' }
#'
#' @seealso [Movement()], [UnfishedDist()], [Populate()]
#' @export
PlotSpatial <- function(object, Stocks = NULL, byStock = NULL, Age = NULL, Year = NULL) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM <- .ResolveOM(object)

  allNames   <- StockNames(object)
  stockNames <- .ResolveStocks(object, Stocks)
  if (is.null(stockNames)) stockNames <- allNames

  nAreaVec <- purrr::map_int(match(stockNames, allNames), \(st) nArea(OM, st))

  spatialStocks <- stockNames[nAreaVec > 1]
  skipped       <- stockNames[nAreaVec <= 1]

  if (length(skipped))
    cli::cli_alert_info(
      "Skipping stock{?s} with a single area (no spatial structure to plot): {.val {skipped}}"
    )

  if (!length(spatialStocks)) {
    cli::cli_alert_info("No selected stock has more than one area; nothing to plot.")
    return(invisible(NULL))
  }

  facet <- if (is.null(byStock)) length(spatialStocks) > 1 else isTRUE(byStock)

  sliceData <- .SpatialSliceData(OM, spatialStocks, Age, Year)

  moveDF <- purrr::map_dfr(sliceData, function(s) {
    df <- Array2DF(s$Movement)
    df$Stock <- s$Stock
    df
  })
  distDF <- purrr::map_dfr(sliceData, function(s) {
    data.frame(Stock = s$Stock, Area = names(s$Dist), Value = as.numeric(s$Dist))
  })

  pMove <- .PlotMovementHeatmap(moveDF, facet)
  pDist <- .PlotAreaDistribution(distDF, facet)

  patchwork::wrap_plots(list(pMove, pDist), ncol = 2)
}


.SpatialSliceData <- function(OM, stockNames, Age, Year) {
  yearDefault <- max(Years(OM, 'Historical'))

  purrr::map(stockNames, function(nm) {
    sp <- OM@Stock[[nm]]@Spatial
    mv <- Movement(sp)
    ud <- UnfishedDist(sp)
    dn <- dimnames(mv)

    ageChar <- if (is.null(Age)) dn$Age[1] else as.character(Age)

    yearChar <- if (is.null(Year)) {
      availYears <- as.numeric(dn$Year)
      as.character(availYears[which.min(abs(availYears - yearDefault))])
    } else as.character(Year)

    if (!ageChar %in% dn$Age)
      cli::cli_abort(c(
        "x" = "`Age` = {.val {ageChar}} is not a valid age class for stock {.val {nm}}.",
        "i" = "Available age classes: {.val {dn$Age}}"
      ))
    if (!yearChar %in% dn$Year)
      cli::cli_abort(c(
        "x" = "`Year` = {.val {yearChar}} is not a valid year for stock {.val {nm}}.",
        "i" = "Available years span {.val {range(as.numeric(dn$Year))}}"
      ))

    ageMatters  <- is.null(Age)  && (.DimVaries(mv, 'Age')  || .DimVaries(ud, 'Age'))
    yearMatters <- is.null(Year) && (.DimVaries(mv, 'Year') || .DimVaries(ud, 'Year'))
    if (ageMatters || yearMatters)
      cli::cli_alert_info(
        "Stock {.val {nm}}: plotting movement/distribution at Age = {.val {ageChar}}, Year = {.val {yearChar}} (override via {.arg Age}/{.arg Year})."
      )

    moveMat <- apply(mv[, , , ageChar, yearChar, drop = FALSE], c(2, 3), mean, na.rm = TRUE)
    dimnames(moveMat) <- list(FromArea = dn$FromArea, ToArea = dn$ToArea)

    distVec <- apply(ud[, , ageChar, yearChar, drop = FALSE], 2, mean, na.rm = TRUE)
    names(distVec) <- dimnames(ud)$Area

    list(Stock = nm, Movement = moveMat, Dist = distVec)
  })
}

.DimVaries <- function(arr, dimName) {
  dn  <- dimnames(arr)
  pos <- which(names(dn) == dimName)
  if (!length(pos) || dim(arr)[pos] <= 1) return(FALSE)

  otherDims <- setdiff(seq_along(dim(arr)), pos)
  ranges <- apply(arr, otherDims, function(x) diff(range(x, na.rm = TRUE)))
  any(ranges > 1e-6, na.rm = TRUE)
}

.OrderAreaLevels <- function(x) {
  u <- unique(x)
  numOk <- suppressWarnings(!anyNA(as.numeric(u)))
  if (numOk) as.character(sort(as.numeric(u))) else sort(u)
}

.PlotMovementHeatmap <- function(df, facet) {
  areaLevels  <- .OrderAreaLevels(c(df$FromArea, df$ToArea))
  df$FromArea <- factor(df$FromArea, levels = areaLevels)
  df$ToArea   <- factor(df$ToArea,   levels = areaLevels)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$ToArea, y = .data$FromArea, fill = .data$Value)) +
    ggplot2::geom_tile(color = 'white') +
    ggplot2::geom_text(ggplot2::aes(label = sprintf('%.2f', .data$Value)), size = 3) +
    ggplot2::scale_fill_gradient(low = 'white', high = '#2c7fb8', limits = c(0, 1), name = 'Probability') +
    ggplot2::labs(x = 'To Area', y = 'From Area', title = 'Movement Probability') +
    ggplot2::theme_bw()

  if (facet)
    p <- p + ggplot2::facet_wrap(~ .data$Stock)

  p
}

.PlotAreaDistribution <- function(df, facet) {
  areaLevels <- .OrderAreaLevels(df$Area)
  df$Area    <- factor(df$Area, levels = areaLevels)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Area, y = .data$Value))

  if (facet) {
    p <- p +
      ggplot2::geom_col(fill = '#2c7fb8') +
      ggplot2::facet_wrap(~ .data$Stock)
  } else {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data$Stock), position = 'dodge')
  }

  p +
    ggplot2::labs(x = 'Area', y = 'Proportion', title = 'Equilibrium Area Distribution') +
    ggplot2::theme_bw()
}
