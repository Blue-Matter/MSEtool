

ProcessBAMArgs <- function(Stock, OM=NULL) {
  if (is.null(OM))
    OM <- ImportBAM(Stock, 2,1)
  
  CheckClass(OM, c('om', 'hist'))
  
  if (inherits(OM, 'om')) {
    Hist <- Simulate(OM, nSim=1)
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
    cli::cli_alert('{.val {name}:} Some Absolute Relative Error > {thresh}%')
    print(re) 
    
    p <- ggplot(Out[[name]]$df, aes(x=TimeStep, y=Value, color=Model)) +
      geom_line() +
      labs(x='Year', y=name, title = Out$Stock) +
      theme_bw()
    
    print(p)
    
  } else {
    cli::cli_alert('{.val {name}:} All Absolute Relative Error < {thresh}%')
  }
}


#' @describeIn ImportBAM Compare BAM and OM dynamics
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
    dplyr::group_by(TimeStep) |>
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
  dimnames(BAM_Value) <- list(TimeStep=dnames[[1]],
                              Age=dnames[[2]])
  
  
  BAM_Value <- BAM_Value |> array2DF() |> 
    ConvertDF() |>
    dplyr::mutate(Model='BAM', Variable='Number') |>
    dplyr::group_by(TimeStep, Model) |>
    dplyr::summarise(Value=sum(Value)) |>
    dplyr::arrange(TimeStep) 
  
  
  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(TimeStep, Value, Model) |>
    dplyr::arrange(TimeStep) 
  
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
    dplyr::select(TimeStep=year, Value=B) |>
    dplyr::mutate(Variable='Biomass', Model='BAM') |>
    dplyr::filter(TimeStep%in%OM_Value$TimeStep) 
  
  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(TimeStep, Value, Model) |>
    dplyr::arrange(TimeStep) 
  
  CalcBAM_MARE(df)
}

CompareBAM_Recruits <- function(Stock, OM=NULL) {
  List <- ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata
  
  OM_Value <- Number(Hist, byAge=TRUE) |> 
    dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1, Age==min(Age)) |>
    dplyr::select(TimeStep, Value, Model) 
  
  BAM_Value <- BAMdata$N.age
  dnames <- dimnames(BAM_Value)
  dimnames(BAM_Value) <- list(TimeStep=dnames[[1]],
                              Age=dnames[[2]])
  
  
  BAM_Value <- BAM_Value |> array2DF() |> 
    ConvertDF() |>
    dplyr::mutate(Model='BAM', Variable='Number') |>
    dplyr::filter(Age==min(Age)) 
  
  df <- dplyr::bind_rows(OM_Value, BAM_Value) |>
    dplyr::select(TimeStep, Value, Model) |>
    dplyr::arrange(TimeStep) 
  
  CalcBAM_MARE(df)
  
}




