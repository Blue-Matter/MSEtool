#' Compare iSCAM and OM Output
#'
#' `CompareiSCAM()` is not currently available.
#' The iSCAM importer is not yet implemented, and calling this function
#' returns an error. Please contact Blue Matter support
#' (\email{adrian@bluematterscience.com}) for help building an operating
#' model from an iSCAM assessment.
#'
#' @param iSCAMdir Character string giving the path to a directory containing
#'   iSCAM output files, or a list already returned by `load.iscam.files()`.
#' @param OM Optional [om-class] or [hist-class] object.
#' @param sim Integer. Simulation index used to select the OM simulation row.
#'   Default `1`.
#' @param plot Logical. Plot the timeseries for the `OM` and `iSCAM` output?
#'   If `FALSE` (default) plots are only printed if MARE exceeds `thresh`.
#' @param thresh Numeric. MARE threshold (percentage) above which a
#'   comparison is flagged and plotted. Default `1`.
#' @param save_plots Logical. If `TRUE`, diagnostic plots are written as PNG
#'   files to `file.path(outdir, Stock)`. Default `FALSE`.
#' @param outdir Character. Base directory for saved plots. Defaults to
#'   `"figures/diagnostics/iSCAM"`.
#' @param width,height Numeric. Passed to [ggplot2::ggsave()]; `NULL`
#'   (default) sizes automatically.
#' @param silent Logical; suppress informational output. Default `FALSE`.
#'
#' @return None. The function stops with an error.
#'
#' @seealso [ImportiSCAM()]
#' @export
CompareiSCAM <- function(iSCAMdir, OM = NULL, sim = 1, plot = FALSE, thresh = 1,
                         save_plots = FALSE, outdir = 'figures/diagnostics/iSCAM',
                         width = NULL, height = NULL, silent = FALSE) {

  .iSCAMNotAvailable()
  replist <- if (is.character(iSCAMdir)) load.iscam.files(iSCAMdir) else iSCAMdir

  if (is.null(OM))
    OM <- ImportiSCAM(replist, nSim = 1, silent = silent)

  .CheckClass(OM, c('om', 'hist'), 'OM')
  Hist <- if (inherits(OM, 'om')) Simulate(OM, nSim = 1, silent = silent) else OM

  Out <- list()
  Out$Stock    <- Hist@OM@Stock[[1]]@Name
  Out$Recruits <- .CompareiSCAMRecruits(replist, Hist, sim)
  Out$Biomass  <- .CompareiSCAMBiomass(replist, Hist, sim)
  Out$Landings <- .CompareiSCAMLandings(replist, Hist, sim)

  figdir <- file.path(outdir, Out$Stock)

  Out <- .ComparePrintPlot(Out, 'Recruits', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)
  Out <- .ComparePrintPlot(Out, 'Biomass', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)
  Out <- .ComparePrintPlot(Out, 'Landings', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)

  invisible(Out)
}

.CompareiSCAMRecruits <- function(replist, Hist, sim) {
  mpd <- replist$mpd
  Years <- replist$dat$start.yr:replist$dat$end.yr

  OM_Value <- Number(Hist, df = TRUE, byAge = TRUE) |>
    dplyr::filter(.data$Sim == sim, .data$Age == min(.data$Age)) |>
    dplyr::mutate(Model = 'OM') |>
    dplyr::select('Year', 'Value', 'Model')

  rt <- mpd$rt
  rYears <- utils::tail(Years, length(rt))
  iSCAM_Value <- data.frame(Year = rYears, Value = as.numeric(rt), Model = 'iSCAM')

  df <- dplyr::bind_rows(OM_Value, iSCAM_Value) |> dplyr::arrange(.data$Year)
  .CompareMare(df, 'iSCAM')
}

.CompareiSCAMBiomass <- function(replist, Hist, sim) {
  mpd <- replist$mpd
  Years <- replist$dat$start.yr:replist$dat$end.yr

  OM_Value <- SBiomass(Hist, df = TRUE) |>
    dplyr::filter(.data$Sim == sim) |>
    dplyr::mutate(Model = 'OM') |>
    dplyr::select('Year', 'Value', 'Model')

  iSCAM_Value <- data.frame(Year = Years, Value = as.numeric(mpd$sbt)[seq_along(Years)], Model = 'iSCAM')

  df <- dplyr::bind_rows(OM_Value, iSCAM_Value) |> dplyr::arrange(.data$Year)
  .CompareMare(df, 'iSCAM')
}

.CompareiSCAMLandings <- function(replist, Hist, sim) {
  Years <- replist$dat$start.yr:replist$dat$end.yr

  CatchDF <- as.data.frame(replist$dat$catch)
  CatchByYear <- stats::aggregate(value ~ year, data = CatchDF, FUN = sum)
  iSCAM_Value <- data.frame(Year = Years, Value = NA_real_, Model = 'iSCAM')
  iSCAM_Value$Value[match(CatchByYear$year, Years)] <- CatchByYear$value

  OM_Value <- Landings(Hist, byFleet = FALSE) |>
    dplyr::filter(.data$Sim == sim) |>
    dplyr::mutate(Model = 'OM') |>
    dplyr::select('Year', 'Value', 'Model')

  df <- dplyr::bind_rows(OM_Value, iSCAM_Value) |> dplyr::arrange(.data$Year)
  .CompareMare(df, 'iSCAM')
}
