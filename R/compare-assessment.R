# Absolute relative error (%) of the OM against the assessment, by year (and
# stock or fleet). With OM simulation ranges (`Lower`, `Upper`: where the
# simulations differ, e.g. generated recruitment deviations), the error in
# years where the simulations differ is the distance of the assessment value
# outside the range (0 inside it).
.CompareMare <- function(df, assess_name, scale_tol = 0.01) {
  group_vars <- intersect(c('Year', 'Stock', 'Fleet'), colnames(df))
  scale_vars <- setdiff(group_vars, 'Year')

  HasRange <- all(c('Lower', 'Upper') %in% colnames(df))
  Range <- if (HasRange) {
    df |>
      dplyr::filter(.data$Model == 'OM') |>
      dplyr::select(dplyr::all_of(c(group_vars, 'Lower', 'Upper')))
  }

  wide <- df |>
    dplyr::select(-dplyr::any_of(c('Lower', 'Upper'))) |>
    tidyr::pivot_wider(names_from = 'Model', values_from = 'Value')
  if (HasRange)
    wide <- dplyr::left_join(wide, Range, by = group_vars)

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
        .CompareAbsError(.data[['OM']], .data[[assess_name]],
                         if (HasRange) .data$Lower else NA_real_,
                         if (HasRange) .data$Upper else NA_real_) * 100
      ),
      .groups = 'drop'
    )

  list(df = df, MARE = MARE)
}

# relative error of `OM` against `Ref`; where the simulations differ
# (Upper > Lower), the distance of `Ref` outside [Lower, Upper]
.CompareAbsError <- function(OM, Ref, Lower, Upper) {
  Lower <- rep_len(Lower, length(OM)); Upper <- rep_len(Upper, length(OM))
  Varies <- !is.na(Lower) & !is.na(Upper) & (Upper - Lower) > 1e-6 * pmax(abs(Upper), 1e-12)
  Dist <- ifelse(Varies, pmax(0, Lower - Ref, Ref - Upper), abs(OM - Ref))
  Dist / abs(Ref)
}

# ribbon of the OM simulation range (5th-95th percentiles), drawn under the lines
.CompareSimRibbon <- function(df) {
  if (!all(c('Lower', 'Upper') %in% names(df)))
    return(NULL)
  ggplot2::geom_ribbon(
    data = df[df$Model == 'OM' & !is.na(df$Lower), , drop = FALSE],
    ggplot2::aes(x = .data$Year, ymin = .data$Lower, ymax = .data$Upper, fill = .data$Model),
    inherit.aes = FALSE, alpha = 0.25, colour = NA, show.legend = FALSE
  )
}

.CompareSimRibbonCaption <- function(df) {
  if (!all(c('Lower', 'Upper') %in% names(df)))
    return(NULL)
  'Shaded: 5th-95th percentiles of the OM simulations. Line: the compared simulation.'
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
      .CompareSimRibbon(Out[[name]]$df) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::labs(x = 'Year', y = name, title = title,
                    caption = .CompareSimRibbonCaption(Out[[name]]$df)) +
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
