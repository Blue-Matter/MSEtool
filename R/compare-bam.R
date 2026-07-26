
#' Compare BAM and OM Output
#'
#' Compares key population time series between BAM output and a simulated
#' operating model (OM), reporting the mean absolute relative error (MARE) for
#' recruits, total numbers, total biomass, and landings and discards by fleet.
#' 
#'  Series with MARE exceeding `thresh` are plotted automatically.
#'
#' @param Stock Character string matching a stock in `bamExtras`, a list of BAM
#'   output objects containing elements `rdat` and `dat`, or an object of class
#'   `BAMdata` as returned by [GetBAMOutput()].
#' @param OM Optional OM or `hist` object. If `NULL` (default), one is
#'   constructed internally via [ImportBAM()].
#' @param plot Logical. Plot the timeseries for the `OM` and `BAM` output?
#' If `plot=FALSE` (default) the plots will only be printed if `MARE>thresh`
#' in some years.
#' @param thresh Numeric. MARE threshold (as a percentage) above which a
#'   comparison is flagged, printed, and plotted. Default `1%`.
#' @param save_plots Logical. If `TRUE` (default), diagnostic plots are
#'   written as PNG files to `file.path(outdir, Stock)`, independently of
#'   whether they are also printed to the active device (see `plot`).
#' @param outdir Character. Base directory for saved plots. Defaults to
#'   `"figures/diagnostics/BAM"`.
#' @param width,height Numeric. Width/height (inches) passed to
#'   [ggplot2::ggsave()]. If `NULL` (default), each plot is sized
#'   automatically from its number of facet panels: `Recruits`, `Number`, and
#'   `Biomass` (single-panel) use 6 x 4; `Landings`/`Discards`, which facet by
#'   fleet, scale up accordingly. Set either to a number to use that fixed
#'   size for every saved plot instead.
#'
#' @return Invisibly returns a named list with elements `Stock`, `Recruits`,
#'   `Number`, `Biomass`, `Landings`, and `Discards`. Each of these (other
#'   than `Stock`) is a list with elements `df` (long-format data.frame of OM
#'   and BAM values by year), `MARE` (absolute relative error by year), and
#'   `plot` (the `ggplot` diagnostic plot, if generated - see `plot`,
#'   `thresh`, and `save_plots`).
#'
#' @seealso [ImportBAM()], [GetBAMOutput()]
#' @export
CompareBAM <- function(Stock, OM = NULL, plot = FALSE, thresh = 1,
                       save_plots = TRUE, outdir = 'figures/diagnostics/BAM',
                       width = NULL, height = NULL) {

  List <- .ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  OM   <- Hist@OM
  cli::cli_text("Comparing population dynamics between BAM Model {.val {OM@Name}} and {.val OM}  ")
  BAMdata <- List$BAMdata

  Out <- list()
  Out$Stock <- Hist@OM@Stock[[1]]@Name
  Out$Recruits <- .CompareBAMRecruits(BAMdata, Hist)
  Out$Number <- .CompareBAMNumber(BAMdata, Hist)
  Out$Biomass <- .CompareBAMBiomass(BAMdata, Hist)
  Out$Landings <- .CompareBAMLandings(BAMdata, Hist)
  Out$Discards <- .CompareBAMDiscards(BAMdata, Hist)

  figdir <- file.path(outdir, Out$Stock)

  Out <- .ComparePrintPlot(Out, 'Recruits', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)
  Out <- .ComparePrintPlot(Out, 'Number', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)
  Out <- .ComparePrintPlot(Out, 'Biomass', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)
  Out <- .ComparePrintPlot(Out, 'Landings', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)
  Out <- .ComparePrintPlot(Out, 'Discards', title = Out$Stock, plot = plot, thresh = thresh,
                            save_plots = save_plots, figdir = figdir, width = width, height = height)

  invisible(Out)
}


.ProcessBAMArgs <- function(Stock, OM=NULL) {
  if (is.null(OM))
    OM <- ImportBAM(Stock,
                    nSim = 1,
                    pYear = 1)
  
  .CheckClass(OM, c('om', 'hist'))
  
  if (inherits(OM, 'om')) {
    Hist <- Simulate(OM, nSim=1, silent=TRUE)
  } else {
    Hist <- OM
  }
  
  if (inherits(Stock, 'BAMdata')) {
    BAMdata <- Stock
  } else {
    BAMdata <- GetBAMOutput(Stock)
  }
  
  list(Hist=Hist,
       BAMdata=BAMdata)
}

.CalcBAMMARE <- function(df) {
  .CompareMare(df, 'BAM')
}

.CompareBAMNumber <- function(Stock, OM=NULL) {

  List <- .ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata

  OM_Value <- Number(Hist, df=TRUE) |> 
    dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1)

  BAM_Value <- BAMdata$N.age
  dnames <- dimnames(BAM_Value)
  dimnames(BAM_Value) <- list(Year=dnames[[1]],
                              Age=dnames[[2]])

  BAM_Value <- BAM_Value |> Array2DF() |>
    dplyr::mutate(Model='BAM', Variable='Number') |>
    dplyr::group_by(Year, Model) |>
    dplyr::summarise(Value=sum(Value), .groups='drop') |>
    dplyr::arrange(Year)


  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(Year, Value, Model) |>
    dplyr::arrange(Year)

  .CalcBAMMARE(df)
}


.CompareBAMBiomass <- function(Stock, OM=NULL) {

  Biomass <- year <- NULL # CRAN check hacks

  List <- .ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata

  if (BAMdata$info$units.biomass == '1000 lb') {
    BAMdata$t.series$B <- (BAMdata$t.series$B * 1000) |> lb2kg()
  } else if (BAMdata$info$units.biomass == 'metric tons') {
    BAMdata$t.series$B <- BAMdata$t.series$B * 1000
  } else {
    cli::cli_abort('`BAMdata$info$units.biomass`:  {.val {BAMdata$info$units.biomass}} currently not supported', .internal=TRUE)
  }

  OM_Value <- Biomass(Hist, df=TRUE) |> dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1) |>
    dplyr::mutate(Model='OM',
                  Value=Value)

  BAM_Value <- BAMdata$t.series |>
    dplyr::select(Year=year, Value=B) |>
    dplyr::mutate(Variable='Biomass', Model='BAM') |>
    dplyr::filter(Year%in%OM_Value$Year)

  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(Year, Value, Model) |>
    dplyr::arrange(Year)

  .CalcBAMMARE(df)
}

.CompareBAMRecruits <- function(Stock, OM=NULL) {
  List <- .ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata

  OM_Value <- Number(Hist, df=TRUE, byAge=TRUE) |>
    dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1, Age==min(Age)) |>
    dplyr::select(Year, Value, Model)

  BAM_Value <- BAMdata$N.age
  dnames <- dimnames(BAM_Value)
  dimnames(BAM_Value) <- list(Year=dnames[[1]],
                              Age=dnames[[2]])


  BAM_Value <- BAM_Value |>Array2DF() |>
    dplyr::mutate(Model='BAM', Variable='Number') |>
    dplyr::filter(Age==min(Age))

  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(Year, Value, Model) |>
    dplyr::arrange(Year)

  .CalcBAMMARE(df)

}

.BAMPredictedCatch <- function(BAMdata, catchData, prefix) {
  t.series <- BAMdata$t.series
  years    <- t.series$year
  fleets   <- dimnames(catchData@Value)$Fleet

  pr <- array(NA_real_, dim = c(length(years), length(fleets)),
             dimnames = list(Year = years, Fleet = fleets))

  ob_years <- as.numeric(dimnames(catchData@Value)$Year)
  yr_idx   <- match(years, ob_years)

  ob.cols <- grep(paste0('^', prefix, '[.].*[.]ob$'), colnames(t.series), value = TRUE)

  for (fl in fleets) {
    ob.converted <- catchData@Value[yr_idx, fl]
    if (all(is.na(ob.converted))) next

    best.col <- NULL
    best.cv  <- Inf
    for (ob.col in ob.cols) {
      ob.raw <- t.series[[ob.col]]
      valid  <- which(!is.na(ob.raw) & ob.raw != 0 & !is.na(ob.converted))
      if (length(valid) < 2) next

      ratio <- ob.converted[valid] / ob.raw[valid]
      cv <- stats::sd(ratio) / abs(mean(ratio))
      if (is.finite(cv) && cv < best.cv) {
        best.cv <- cv
        best.col <- ob.col
      }
    }
    if (is.null(best.col) || best.cv > 0.01) next

    pr.col <- sub('[.]ob$', '.pr', best.col)
    if (!(pr.col %in% colnames(t.series))) next

    ob.raw <- t.series[[best.col]]
    pr.raw <- t.series[[pr.col]]
    valid  <- which(!is.na(ob.raw) & ob.raw != 0 & !is.na(ob.converted))

    scale <- stats::median(ob.converted[valid] / ob.raw[valid])
    pr[, fl] <- pr.raw * scale
  }
  pr
}

.CompareBAMLandings <- function(Stock, OM = NULL) {

  Variable <- NULL # CRAN checks

  List <- .ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata

  BAM_Landings <- purrr::map(Hist@Data[[1]], \(data) {
    .BAMPredictedCatch(BAMdata, data@Landings, 'L')
  }) |> List2Array(name = 'Stock') |>
    Array2DF() |>
    dplyr::mutate(Model = 'BAM', Variable = 'Landings')
  
  BAM_Units <- data.frame(Fleet  = Hist@Data[[1]][[1]]@Landings@Name,
                          Units  = as.character(Hist@Data[[1]][[1]]@Landings@Units))
                                             
  BAM_Landings <- dplyr::left_join(BAM_Landings, BAM_Units, by = dplyr::join_by(Fleet))
  
  OM_Landings_Biomass <- Landings(Hist, byFleet = TRUE) |>
    dplyr::mutate(Model='OM')
  
  OM_Landings_Number <- Landings(Hist, byFleet = TRUE, byAge = TRUE) |>
    dplyr::mutate(Model='OM') |>
    dplyr::group_by(Sim, Year, Fleet, Variable, Model) |>
    dplyr::summarise(Value = sum(Value), .groups = 'drop')
  
  biomass_fleets <- BAM_Units$Fleet[BAM_Units$Units == 'Biomass']
  number_fleets  <- BAM_Units$Fleet[BAM_Units$Units == 'Number']
  
  OM_Landings <- dplyr::bind_rows(
    dplyr::filter(OM_Landings_Biomass, Fleet %in% biomass_fleets),
    dplyr::filter(OM_Landings_Number,  Fleet %in% number_fleets)
  ) |>
    dplyr::left_join(BAM_Units, by = 'Fleet')
  
  df <- dplyr::bind_rows(OM_Landings, BAM_Landings) |>
    dplyr::select(Year, Value, Model, Fleet) |>
    dplyr::arrange(Year)
  
  .CalcBAMMARE(df)
}

.CompareBAMDiscards <- function(Stock, OM = NULL) {
  
  Variable <- NULL # CRAN checks
  
  List <- .ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata

  if (is.null(Hist@Data[[1]][[1]]@Discards@Value))
    return(NULL)

  BAM_Discards <- purrr::map(Hist@Data[[1]], \(data) {
    .BAMPredictedCatch(BAMdata, data@Discards, 'D')
  }) |> List2Array(name = 'Stock') |>
    Array2DF() |>
    dplyr::mutate(Model = 'BAM', Variable = 'Discards')
  
  BAM_Units <- data.frame(Fleet  = Hist@Data[[1]][[1]]@Discards@Name,
                          Units = as.character(Hist@Data[[1]][[1]]@Discards@Units))
  
  BAM_Discards <- dplyr::left_join(BAM_Discards, BAM_Units, by = dplyr::join_by(Fleet))
  
  OM_Discards_Biomass <- Discards(Hist, byFleet = TRUE) |>
    dplyr::mutate(Model='OM')
  
  OM_Discards_Number <- Discards(Hist, byFleet = TRUE, byAge = TRUE) |>
    dplyr::mutate(Model='OM') |>
    dplyr::group_by(Sim, Year, Fleet, Variable, Model) |>
    dplyr::summarise(Value = sum(Value), .groups = 'drop')
  
  biomass_fleets <- BAM_Units$Fleet[BAM_Units$Units == 'Biomass']
  number_fleets  <- BAM_Units$Fleet[BAM_Units$Units == 'Number']
  
  OM_Discards <- dplyr::bind_rows(
    dplyr::filter(OM_Discards_Biomass, Fleet %in% biomass_fleets),
    dplyr::filter(OM_Discards_Number,  Fleet %in% number_fleets)
  ) |>
    dplyr::left_join(BAM_Units, by = 'Fleet')
  
  df <- dplyr::bind_rows(OM_Discards, BAM_Discards) |>
    dplyr::select(Year, Value, Model, Fleet) |>
    dplyr::arrange(Year)
  
  .CalcBAMMARE(df)
}

