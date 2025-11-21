# #' @describeIn ImportSS Compare SS3 and OM dynamics
# #' @export

# TODO 
CompareSS <- function(RepList, Hist, sim=1, thresh=0.1) {
 
}

#' @export
CompareSS_Number <- function(RepList, Hist, sim=1) {
  CheckClass(Hist, 'hist', 'Hist')
  replist <- RepList[[sim]]
  
  HistYears <- Years(Hist@OM, 'H')
  
  OM_Value <- Number(Hist) |> dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==sim) |>
    dplyr::arrange(Stock, Year) |>
    dplyr::select(Year, Value, Stock, Model)
  
  AgeClasses <- GetSSAgeClasses(replist)
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
 
  p <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=Value, color=Model)) +
    ggplot2::facet_grid(~Stock) +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y='Number') +
    theme_bw()

  print(p)
  invisible(df)
  
}

#' @export
CompareSS_Biomass <- function(RepList, Hist, sim=1) {
  CheckClass(Hist, 'hist', 'Hist')
  replist <- RepList[[sim]]
  
  HistYears <- Years(Hist@OM, 'H')
  
  OM_Value <- Biomass(Hist) |> dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==sim) |>
    dplyr::arrange(Stock, Year) |>
    dplyr::select(Year, Value, Stock, Model)
  
  AgeClasses <- GetSSAgeClasses(replist)
  SS_Value <- replist$batage |> 
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
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=Value, color=Model)) +
    ggplot2::facet_grid(~Stock) +
    ggplot2::geom_line() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y='Biomass') +
    theme_bw()
  
  print(p)
  invisible(df)
}

#' @export
CompareSS_Landings <- function(RepList, Hist, sim=1) {
  CheckClass(Hist, 'hist', 'Hist')
  replist <- RepList[[sim]]
  
  HistYears <- Years(Hist@OM, 'H')
  
  Landings_OM <- Landings(Hist, byFleet = TRUE) |>
    dplyr::filter(Sim==sim) |>
    dplyr::group_by(Year, Fleet) |>
    dplyr::summarise(Value=sum(Value), .groups='drop') |>
    dplyr::mutate(Model='OM') |>
    dplyr::arrange(Fleet, Year)
  
  FleetNames <- FleetNames(Hist@OM)
  
  Landings_SS <- replist$catch |> dplyr::filter(Yr %in% HistYears) |>
    dplyr::select(Year=Yr,  Fleet, Seas, Value=ret_bio) |>
    dplyr::mutate(Model='SS3') |>
    dplyr::arrange(Fleet, Year, Seas) |>
    dplyr::select(Fleet, Year, Value)
  
  Landings_SS$Year <- Landings_OM$Year
  Landings_SS$Fleet <- FleetNames[Landings_SS$Fleet]
  df <- dplyr::bind_rows(Landings_OM, Landings_SS)
  df$Fleet <- factor(df$Fleet, FleetNames, ordered = TRUE)
  
  p <- ggplot2::ggplot(df, 
                  ggplot2::aes(x=Year, y=Value, color=Model)) +
    ggplot2::facet_wrap(~Fleet, scales='free_y') +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y='Landings')
  
  print(p)
  invisible(df)
  
}

#' @export
CompareSS_Removals <- function(RepList, Hist, sim=1) {
  CheckClass(Hist, 'hist', 'Hist')
  replist <- RepList[[sim]]
  
  HistYears <- Years(Hist@OM, 'H')
  
  Removals_OM <- Removals(Hist, byFleet = TRUE) |>
    dplyr::filter(Sim==sim) |>
    dplyr::group_by(Year, Fleet) |>
    dplyr::summarise(Value=sum(Value), .groups='drop') |>
    dplyr::mutate(Model='OM') |>
    dplyr::arrange(Fleet, Year)
  
  FleetNames <- FleetNames(Hist@OM)

  if (is.null(replist$catch$kill_bio)) 
    replist$catch <- replist$catch |> dplyr::mutate(kill_bio=dead_bio)
  
  Removals_SS <- replist$catch |> dplyr::filter(Yr %in% HistYears) |>
    dplyr::select(Year=Yr,  Fleet, Seas, Value=kill_bio) |>
    dplyr::mutate(Model='SS3') |>
    dplyr::arrange(Fleet, Year, Seas) |>
    dplyr::select(Fleet, Year, Value)
  
  Removals_SS$Year <- Removals_OM$Year
  Removals_SS$Fleet <- FleetNames[Removals_SS$Fleet]
  df <- dplyr::bind_rows(Removals_OM, Removals_SS)
  df$Fleet <- factor(df$Fleet, FleetNames, ordered = TRUE)
  p <- ggplot2::ggplot(df, 
                       ggplot2::aes(x=Year, y=Value, color=Model)) +
    ggplot2::facet_wrap(~Fleet, scales='free_y') +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(y='Removals')
  
  print(p)
  invisible(df)
}


