# Shared engine behind CompareSS()/CompareBAM()/CompareiSCAM()/CompareWHAM():
# each `Compare<Tool>_<Series>()` adapter builds a combined data.frame and
# calls `.CompareMare()`; `Compare<Tool>()` assembles those and calls
# `.ComparePrintPlot()` per series.

# MARE = |OM - Assess| / Assess * 100, grouped by whichever of Year/Stock/
# Fleet are present. Suppressed (NA) where the reference value is a
# negligible fraction (`scale_tol`) of the comparison's overall scale, so a
# near-zero true value doesn't produce a huge, meaningless relative error.
.CompareMare <- function(df, assess_name, scale_tol = 0.01) {
  group_vars <- intersect(c('Year', 'Stock', 'Fleet'), colnames(df))

  wide <- df |>
    tidyr::pivot_wider(names_from = 'Model', values_from = 'Value')

  scale <- max(abs(wide[[assess_name]]), na.rm = TRUE)

  MARE <- wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      MARE = dplyr::if_else(
        abs(.data[[assess_name]]) < scale_tol * scale,
        NA_real_,
        abs((.data[['OM']] - .data[[assess_name]]) / .data[[assess_name]]) * 100
      ),
      .groups = 'drop'
    )

  list(df = df, MARE = MARE)
}

# Near-square (nrow, ncol) grid for `n` facet panels, matching ggplot2's own
# facet_wrap layout heuristic.
.CompareWrapDims <- function(n) {
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  list(nrow = nrow, ncol = ncol)
}

# Plot size (inches) for a `dims$nrow` x `dims$ncol` panel grid: 6 x 4 for a
# single panel, scaling up per additional row/column so faceted plots (e.g.
# Landings/Discards by fleet) don't come out squashed.
.CompareAutoDims <- function(dims) {
  list(width  = 3.5 + dims$ncol * 2.5,
       height = 2   + dims$nrow * 2)
}

.CompareSave <- function(plot, dir, filename, width, height, dims = list(nrow = 1L, ncol = 1L)) {
  if (is.null(width) || is.null(height)) {
    auto <- .CompareAutoDims(dims)
    if (is.null(width))  width  <- auto$width
    if (is.null(height)) height <- auto$height
  }
  ggplot2::ggsave(file.path(dir, filename), plot, width = width, height = height, create.dir = TRUE)
  invisible(NULL)
}

# Prints/saves a comparison plot for `Out[[name]]` (a `list(df, MARE)`, as
# returned by `.CompareMare()`) when MARE exceeds `thresh`, or
# unconditionally when `plot`/`save_plots` is requested. Facets by `Fleet`
# if present, else by `Stock` if present, else no facet.
.ComparePrintPlot <- function(Out, name, title, plot = FALSE, thresh = 1,
                               save_plots = FALSE, figdir = NULL,
                               width = NULL, height = NULL) {
  if (is.null(Out[[name]]))
    return(Out)

  re <- Out[[name]]$MARE |>
    dplyr::mutate(MARE = abs(.data$MARE)) |>
    dplyr::filter(.data$MARE > thresh)

  exMARE <- nrow(re) > 0

  if (exMARE)
    cli::cli_alert_warning('{.val {name}:} Some Absolute Relative Error > {thresh}%')

  dims <- list(nrow = 1L, ncol = 1L)

  if (exMARE || plot || save_plots) {
    has_fleet <- 'Fleet' %in% names(Out[[name]]$df)
    has_stock <- !has_fleet && 'Stock' %in% names(Out[[name]]$df)

    p <- ggplot2::ggplot(Out[[name]]$df,
                         ggplot2::aes(x = .data$Year, y = .data$Value, color = .data$Model,
                                     linetype = .data$Model, shape = .data$Model)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::labs(x = 'Year', y = name, title = title) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::theme_bw()

    facetVar <- if (has_fleet) 'Fleet' else if (has_stock) 'Stock' else NULL
    if (!is.null(facetVar)) {
      dims <- .CompareWrapDims(length(unique(Out[[name]]$df[[facetVar]])))
      p <- p + ggplot2::facet_wrap(facetVar, scales = 'free_y', nrow = dims$nrow, ncol = dims$ncol)
    }

    if (plot || exMARE) print(p)

    if (save_plots)
      .CompareSave(p, figdir, paste0(name, '.png'), width, height, dims)

    Out[[name]]$plot <- p
  }

  if (!exMARE)
    cli::cli_alert('{.val {name}:} All Absolute Relative Error < {thresh}%')

  Out
}
