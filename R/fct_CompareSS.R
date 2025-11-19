# #' @describeIn ImportSS Compare SS3 and OM dynamics
# #' @export

# TODO 
CompareSS <- function(RepList, Hist, sim=1, thresh=0.1) {
  
  CheckClass(Hist, 'hist', 'Hist')
  
  replist <- RepList[[sim]]
  
  # Number 
  
  
  
  # Biomass
  
  # Landings
  
  # Removals 
  
  
  
  
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

CompareSS_Number <- function(RepList, Hist, sim=1) {
  CheckClass(Hist, 'hist', 'Hist')
  replist <- RepList[[sim]]
  
  HistYears <- Years(Hist@OM, 'H')
  
  OM_Value <- Number(Hist) |> dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==sim) |>
    dplyr::arrange(Stock, Year) |>
    dplyr::select(Year, Value, Stock, Model)
  
  SS_Value <- replist$natage |> 
    dplyr::filter(Yr%in%HistYears, `Beg/Mid`=='B') |>
    dplyr::rename(Year=Yr, Stock=Sex) |>
    tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
    dplyr::group_by(Stock, Year, Seas) |>
    dplyr::summarise(Value=sum(value), Model='SS3', .groups='drop') 
  
  SS_Value$Stock <- dplyr::case_match(SS_Value$Stock,
                                      1~'Female',
                                      2~'Male')
  
  SS_Value <- SS_Value |>
    dplyr::select(Stock, Year, Value, Model) |> 
    dplyr::arrange(Stock, Year) |>
    dplyr::mutate(Year=OM_Value$Year,
                  Stock=OM_Value$Stock)
  
  df <- dplyr::bind_rows(OM_Value, SS_Value) |>
    dplyr::select(Year, Value, Stock, Model) |>
    dplyr::arrange(Year)
 
  
  OM_Value |> dplyr::filter(Year%in%HistYears[1:4])
  SS_Value |> dplyr::filter(Year==1975)
  
  
  
  Calc_MARE(df)
  
  Calc_MARE <- function(df) {
    models <- unique(df$Model)
    conditionModel <- models[!models=='OM']
    df2 <- df
    df2$Model[df2$Model == conditionModel] <- 'assess'
    
    MARE <- df2 |> 
      tidyr::pivot_wider(names_from = Model, values_from = Value) |> 
      dplyr::group_by(Year, Stock) |>
      dplyr::summarise(MARE=abs((OM-assess)/assess*100), .groups='drop') 
    
    list(df=df, MARE=MARE)
  }
  
  
  
}
