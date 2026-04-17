

ProcessBAMArgs <- function(Stock, OM=NULL) {
  if (is.null(OM))
    OM <- ImportBAM(Stock,
                    nSim = 1,
                    pYear = 1)

  CheckClass(OM, c('om', 'hist'))

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

PrintPlotBAMRE <- function(Out, name, plot=FALSE, thresh=0.5) {
  MARE <- NULL # CRAN
  re <- Out[[name]]$MARE |>
    dplyr::mutate(MARE=abs(MARE)) |>
    dplyr::filter(MARE>thresh)
  
  exMARE <- nrow(re)>0
  
  if (exMARE) {
    cli::cli_alert_warning('{.val {name}:} Some Absolute Relative Error > {thresh}%')
    print(re) 
  }
  
  if (exMARE || plot) {
    p <- ggplot2::ggplot(Out[[name]]$df, 
                         ggplot2::aes(x=Year, y=Value, color=Model, linetype=Model, shape=Model)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x='Year', y=name, title = Out$Stock) +
      ggplot2::expand_limits(y=0) +
      ggplot2::theme_bw()
    
    print(p)
  }

  if (!exMARE)
    cli::cli_alert('{.val {name}:} All Absolute Relative Error < {thresh}%')
  
  return(invisible(NULL))
}



#' Compare BAM and OM Output
#'
#' Compares key population time series between BAM output and a simulated
#' operating model (OM), reporting the mean absolute relative error (MARE) for
#' recruits, total numbers, and total biomass. Series with MARE exceeding
#' `thresh` are printed and plotted automatically.
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
#'   comparison is flagged, printed, and plotted. Default `0.5%`.
#'
#' @return Invisibly returns a named list with elements `Stock`, `Recruits`,
#'   `Number`, and `Biomass`. Each of `Recruits`, `Number`, and `Biomass` is a
#'   list with elements `df` (long-format data.frame of OM and BAM values by
#'   year) and `MARE` (absolute relative error by year).
#'
#' @seealso [ImportBAM()], [GetBAMOutput()]
#' @export
CompareBAM <- function(Stock, OM = NULL, plot = FALSE, thresh = 0.5) {
  
  cli::cli_text("Comparing population dynamics between BAM Model {.val {OM@Name}} and {.val OM}  ")

  List <- ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata

  Out <- list()
  Out$Stock <- Hist@OM@Stock[[1]]@Name
  Out$Recruits <- CompareBAM_Recruits(BAMdata, Hist)
  Out$Number <- CompareBAM_Number(BAMdata, Hist)
  Out$Biomass <- CompareBAM_Biomass(BAMdata, Hist)

  PrintPlotBAMRE(Out, 'Recruits', thresh, plot=plot)
  PrintPlotBAMRE(Out, 'Number', thresh, plot=plot)
  PrintPlotBAMRE(Out, 'Biomass', thresh, plot=plot)

  invisible(Out)
}


CalcBAM_MARE <- function(df) {
  OM <- BAM <- NULL # CRAN check hacks
  MARE <- df |>
    tidyr::pivot_wider(names_from = Model, values_from = Value) |>
    dplyr::group_by(Year) |>
    dplyr::summarise(MARE=abs((OM-BAM)/BAM)*100, .groups='drop')
  list(df=df, MARE=MARE)

}


CompareBAM_Number <- function(Stock, OM=NULL) {

  List <- ProcessBAMArgs(Stock, OM)
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

  CalcBAM_MARE(df)
}

CompareBAM_Biomass <- function(Stock, OM=NULL) {

  Biomass <- year <- NULL # CRAN check hacks

  List <- ProcessBAMArgs(Stock, OM)
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

  CalcBAM_MARE(df)
}

CompareBAM_Recruits <- function(Stock, OM=NULL) {
  List <- ProcessBAMArgs(Stock, OM)
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

  CalcBAM_MARE(df)

}




