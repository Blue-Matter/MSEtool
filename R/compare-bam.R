

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

PrintPlotBAMRE <- function(Out, name, thresh=0.1) {
  re <- Out[[name]]$MARE |> 
    dplyr::mutate(MARE=abs(MARE)) |> 
    dplyr::filter(MARE>thresh)
  if (nrow(re)>0) {
    cli::cli_alert_warning('{.val {name}:} Some Absolute Relative Error > {thresh}%')
    print(re) 
    
    p <- ggplot(Out[[name]]$df, aes(x=Year, y=Value, color=Model)) +
      geom_line() +
      labs(x='Year', y=name, title = Out$Stock) +
      theme_bw()
    
    print(p)
    
  } else {
    cli::cli_alert('{.val {name}:} All Absolute Relative Error < {thresh}%')
  }
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
#' @param thresh Numeric. MARE threshold (as a percentage) above which a
#'   comparison is flagged, printed, and plotted. Default `0.1`.
#'
#' @return Invisibly returns a named list with elements `Stock`, `Recruits`,
#'   `Number`, and `Biomass`. Each of `Recruits`, `Number`, and `Biomass` is a
#'   list with elements `df` (long-format data.frame of OM and BAM values by
#'   year) and `MARE` (absolute relative error by year).
#'
#' @seealso [ImportBAM()], [GetBAMOutput()]
#' @export
CompareBAM <- function(Stock, OM=NULL, thresh=0.1) {
  
  List <- ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata
  
  Out <- list()
  Out$Stock <- Hist@OM@Stock[[1]]@Name
  Out$Recruits <- CompareBAM_Recruits(BAMdata, Hist)
  Out$Number <- CompareBAM_Number(BAMdata, Hist)
  Out$Biomass <- CompareBAM_Biomass(BAMdata, Hist)
  
  PrintPlotBAMRE(Out, 'Recruits', thresh)
  PrintPlotBAMRE(Out, 'Number', thresh)
  PrintPlotBAMRE(Out, 'Biomass', thresh)
  
  invisible(Out)
}


CalcBAM_MARE <- function(df) {
  MARE <- df |> 
    tidyr::pivot_wider(names_from = Model, values_from = Value) |> 
    dplyr::group_by(Year) |>
    dplyr::summarise(MARE=abs((OM-BAM)/BAM*100), .groups='drop') 
  list(df=df, MARE=MARE)
  
}


CompareBAM_Number <- function(Stock, OM=NULL) {
  
  List <- ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata
  
  OM_Value <- Number(Hist) |> dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1)
  
  BAM_Value <- BAMdata$N.age
  dnames <- dimnames(BAM_Value)
  dimnames(BAM_Value) <- list(Year=dnames[[1]],
                              Age=dnames[[2]])
  
  BAM_Value <- BAM_Value |> array2DF() |> 
    ConvertDF() |>
    dplyr::mutate(Model='BAM', Variable='Number') |>
    dplyr::group_by(Year, Model) |>
    dplyr::summarise(Value=sum(Value)) |>
    dplyr::arrange(Year) 
  
  
  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(Year, Value, Model) |>
    dplyr::arrange(Year) 
  
  CalcBAM_MARE(df)
}

CompareBAM_Biomass <- function(Stock, OM=NULL) {
  
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
  
  OM_Value <- Biomass(Hist) |> dplyr::mutate(Model='OM') |>
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
  
  OM_Value <- Number(Hist, byAge=TRUE) |> 
    dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1, Age==min(Age)) |>
    dplyr::select(Year, Value, Model) 
  
  BAM_Value <- BAMdata$N.age
  dnames <- dimnames(BAM_Value)
  dimnames(BAM_Value) <- list(Year=dnames[[1]],
                              Age=dnames[[2]])
  
  
  BAM_Value <- BAM_Value |> array2DF() |> 
    ConvertDF() |>
    dplyr::mutate(Model='BAM', Variable='Number') |>
    dplyr::filter(Age==min(Age)) 
  
  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(Year, Value, Model) |>
    dplyr::arrange(Year) 
  
  CalcBAM_MARE(df)
  
}




