.CompareMare <- function(df, assess_name, scale_tol = 0.01) {
  group_vars <- intersect(c('Year', 'Stock', 'Fleet'), colnames(df))
  scale_vars <- setdiff(group_vars, 'Year')

  wide <- df |>
    tidyr::pivot_wider(names_from = 'Model', values_from = 'Value')

  .MaxAbs <- function(x) if (all(is.na(x))) NA_real_ else max(abs(x), na.rm = TRUE)

  if (length(scale_vars)) {
    wide <- wide |>
      dplyr::group_by(dplyr::across(dplyr::all_of(scale_vars))) |>
      dplyr::mutate(.scale = .MaxAbs(.data[[assess_name]])) |>
      dplyr::ungroup()
  } else {
    wide$.scale <- .MaxAbs(wide[[assess_name]])
  }

  MARE <- wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      MARE = dplyr::if_else(
        abs(.data[[assess_name]]) < scale_tol * .data[['.scale']],
        NA_real_,
        abs((.data[['OM']] - .data[[assess_name]]) / .data[[assess_name]]) * 100
      ),
      .groups = 'drop'
    )

  list(df = df, MARE = MARE)
}

.CompareWrapDims <- function(n) {
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  list(nrow = nrow, ncol = ncol)
}

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
