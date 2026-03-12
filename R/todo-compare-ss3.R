# # #' @describeIn ImportSS Compare SS3 and OM dynamics
# # #' @export
# 
# # TODO 
# CompareSS <- function(RepList, Hist, sim=1, thresh=0.1) {
#  
# }
# 
# #' @export
# CompareSS_Number <- function(RepList, Hist, sim=1) {
#   CheckClass(Hist, 'hist', 'Hist')
#   replist <- RepList[[sim]]
#   
#   HistYears <- Years(Hist@OM, 'H')
#   
#   OM_Value <- Number(Hist) |> dplyr::mutate(Model='OM') |>
#     dplyr::filter(Sim==sim) |>
#     dplyr::arrange(Stock, Year) |>
#     dplyr::select(Year, Value, Stock, Model)
#   
#   AgeClasses <- GetSSAgeClasses(replist)
#   SS_Value <- replist$natage |> 
#     dplyr::filter(Yr%in%HistYears, `Beg/Mid`=='B') |>
#     dplyr::rename(Year=Yr, Stock=Sex) |>
#     tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
#     dplyr::group_by(Stock, Year, Seas) |>
#     dplyr::summarise(Value=sum(value), Model='SS3', .groups='drop') 
#   
#   SS_Value$Stock <- dplyr::case_match(SS_Value$Stock,
#                                       1~'Female',
#                                       2~'Male')
#   
#   SS_Value <- SS_Value |>
#     dplyr::select(Stock, Year, Value, Model) |> 
#     dplyr::arrange(Stock, Year) |>
#     dplyr::mutate(Year=OM_Value$Year,
#                   Stock=OM_Value$Stock)
#   
#   df <- dplyr::bind_rows(OM_Value, SS_Value) |>
#     dplyr::select(Year, Value, Stock, Model) |>
#     dplyr::arrange(Year)
#  
#   p <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=Value, color=Model)) +
#     ggplot2::facet_grid(~Stock) +
#     ggplot2::geom_line() +
#     ggplot2::expand_limits(y=0) +
#     ggplot2::labs(y='Number') +
#     theme_bw()
# 
#   print(p)
#   invisible(df)
#   
# }
# 
# #' @export
# CompareSS_Biomass <- function(RepList, Hist, sim=1) {
#   CheckClass(Hist, 'hist', 'Hist')
#   replist <- RepList[[sim]]
#   
#   HistYears <- Years(Hist@OM, 'H')
#   
#   OM_Value <- Biomass(Hist) |> dplyr::mutate(Model='OM') |>
#     dplyr::filter(Sim==sim) |>
#     dplyr::arrange(Stock, Year) |>
#     dplyr::select(Year, Value, Stock, Model)
#   
#   AgeClasses <- GetSSAgeClasses(replist)
#   SS_Value <- replist$batage |> 
#     dplyr::filter(Yr%in%HistYears, `Beg/Mid`=='B') |>
#     dplyr::rename(Year=Yr, Stock=Sex) |>
#     tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
#     dplyr::group_by(Stock, Year, Seas) |>
#     dplyr::summarise(Value=sum(value), Model='SS3', .groups='drop') 
#   
#   SS_Value$Stock <- dplyr::case_match(SS_Value$Stock,
#                                       1~'Female',
#                                       2~'Male')
#   
#   SS_Value <- SS_Value |>
#     dplyr::select(Stock, Year, Value, Model) |> 
#     dplyr::arrange(Stock, Year) |>
#     dplyr::mutate(Year=OM_Value$Year,
#                   Stock=OM_Value$Stock)
#   
#   df <- dplyr::bind_rows(OM_Value, SS_Value) |>
#     dplyr::select(Year, Value, Stock, Model) |>
#     dplyr::arrange(Year)
#   
#   p <- ggplot2::ggplot(df, ggplot2::aes(x=Year, y=Value, color=Model)) +
#     ggplot2::facet_grid(~Stock) +
#     ggplot2::geom_line() +
#     ggplot2::expand_limits(y=0) +
#     ggplot2::labs(y='Biomass') +
#     theme_bw()
#   
#   print(p)
#   invisible(df)
# }
# 
# #' @export
# CompareSS_Landings <- function(RepList, Hist, sim=1) {
#   CheckClass(Hist, 'hist', 'Hist')
#   replist <- RepList[[sim]]
#   
#   HistYears <- Years(Hist@OM, 'H')
#   
#   Landings_OM <- Landings(Hist, byFleet = TRUE) |>
#     dplyr::filter(Sim==sim) |>
#     dplyr::group_by(Year, Fleet) |>
#     dplyr::summarise(Value=sum(Value), .groups='drop') |>
#     dplyr::mutate(Model='OM') |>
#     dplyr::arrange(Fleet, Year)
#   
#   FleetNames <- FleetNames(Hist@OM)
#   
#   Landings_SS <- replist$catch |> dplyr::filter(Yr %in% HistYears) |>
#     dplyr::select(Year=Yr,  Fleet, Seas, Value=ret_bio) |>
#     dplyr::mutate(Model='SS3') |>
#     dplyr::arrange(Fleet, Year, Seas) |>
#     dplyr::select(Fleet, Year, Value, Model)
#   
#   Landings_SS$Year <- Landings_OM$Year
#   Landings_SS$Fleet <- FleetNames[Landings_SS$Fleet]
#   df <- dplyr::bind_rows(Landings_OM, Landings_SS)
#   df$Fleet <- factor(df$Fleet, FleetNames, ordered = TRUE)
#   
#   p <- ggplot2::ggplot(df, 
#                   ggplot2::aes(x=Year, y=Value, color=Model)) +
#     ggplot2::facet_wrap(~Fleet, scales='free_y') +
#     ggplot2::geom_line() +
#     ggplot2::theme_bw() +
#     ggplot2::expand_limits(y=0) +
#     ggplot2::labs(y='Landings')
#   
#   print(p)
#   invisible(df)
#   
# }
# 
# #' @export
# CompareSS_Removals <- function(RepList, Hist, sim=1) {
#   CheckClass(Hist, 'hist', 'Hist')
#   replist <- RepList[[sim]]
#   
#   HistYears <- Years(Hist@OM, 'H')
#   
#   Removals_OM <- Removals(Hist, byFleet = TRUE) |>
#     dplyr::filter(Sim==sim) |>
#     dplyr::group_by(Year, Fleet) |>
#     dplyr::summarise(Value=sum(Value), .groups='drop') |>
#     dplyr::mutate(Model='OM') |>
#     dplyr::arrange(Fleet, Year)
#   
#   FleetNames <- FleetNames(Hist@OM)
# 
#   if (is.null(replist$catch$kill_bio)) 
#     replist$catch <- replist$catch |> dplyr::mutate(kill_bio=dead_bio)
#   
#   Removals_SS <- replist$catch |> dplyr::filter(Yr %in% HistYears) |>
#     dplyr::select(Year=Yr,  Fleet, Seas, Value=kill_bio) |>
#     dplyr::mutate(Model='SS3') |>
#     dplyr::arrange(Fleet, Year, Seas) |>
#     dplyr::select(Fleet, Year, Value)
#   
#   Removals_SS$Year <- Removals_OM$Year
#   Removals_SS$Fleet <- FleetNames[Removals_SS$Fleet]
#   df <- dplyr::bind_rows(Removals_OM, Removals_SS)
#   df$Fleet <- factor(df$Fleet, FleetNames, ordered = TRUE)
#   p <- ggplot2::ggplot(df, 
#                        ggplot2::aes(x=Year, y=Value, color=Model)) +
#     ggplot2::facet_wrap(~Fleet, scales='free_y') +
#     ggplot2::geom_line() +
#     ggplot2::theme_bw() +
#     ggplot2::expand_limits(y=0) +
#     ggplot2::labs(y='Removals')
#   
#   print(p)
#   invisible(df)
# }
# 
# 
# Compare ----
# 
# CompareSSNumber <- function(replist, Hist, sim = 1) {
#   
#   Mean <- Label <- NULL # CRAN check hacks
#   
#   if (!inherits(Hist, "hist")) {
#     cli::cli_abort("`Hist` must be class `hist`")
#   }
#   
#   mainyrs <- replist$startyr:replist$endyr
#   AgeClasses <- GetSSAgeClasses(replist)
#   
#   NumberHist <- Number(Hist) |>
#     dplyr::mutate(Model = "Import") |>
#     dplyr::filter(Sim == sim)
#   
#   NumberSS <- replist$natage |>
#     dplyr::filter(Yr %in% mainyrs, `Beg/Mid` == "B") |>
#     dplyr::rename(Year = Yr, Stock = Sex) |>
#     tidyr::pivot_longer(cols = as.character(AgeClasses)) |>
#     dplyr::group_by(Stock, Year, Seas) |>
#     dplyr::summarise(Value = sum(value), Model = "SS3", .groups = "drop")
#   
#   NumberSS$Stock <- unique(NumberHist$Stock)[NumberSS$Stock]
#   NumberSS$Year <- NumberHist$Year
#   
#   NumberDF <- dplyr::bind_rows(NumberHist, NumberSS)
#   
#   p1 <- ggplot(NumberDF, ggplot2::aes(x = Year, y = Value, color = Model)) +
#     ggplot2::facet_grid(~Stock) +
#     ggplot2::geom_line() +
#     ggplot2::theme_bw()
#   
#   pDF <- NumberDF |>
#     dplyr::group_by(Stock, Year) |>
#     dplyr::summarise(Mean = mean(Value[Model == "SS3"] / Value[Model != "SS3"]))
#   
#   p2 <- ggplot2::ggplot(pDF, ggplot2::aes(x = Year, y = Mean, color = Stock)) +
#     ggplot2::geom_line() +
#     ggplot2::theme_bw() +
#     ggplot2::labs(y = "Ratio SS3/Model")
#   
#   print(patchwork::wrap_plots(p1, p2, ncol = 1))
#   invisible(NumberDF)
# }
# 
# CompareSSLandings <- function(replist, Hist) {
#   
#   ret_bio <- Mean <- NULL # CRAN check hacks
#   
#   if (!inherits(Hist, "hist")) {
#     cli::cli_abort("`Hist` must be class `hist`")
#   }
#   
#   mainyrs <- replist$startyr:replist$endyr
#   AgeClasses <- GetSSAgeClasses(replist$natage)
#   
#   HistLandings <- Landings(Hist, byFleet = TRUE) |>
#     dplyr::mutate(Model = "Import") |>
#     dplyr::filter(Sim == 1) |>
#     dplyr::group_by(Year, Fleet, Model) |>
#     dplyr::summarise(Value = sum(Value))
#   
#   SS3Landings <- replist$catch |>
#     dplyr::filter(Yr %in% mainyrs) |>
#     dplyr::select(Year = Yr, Fleet, Value = ret_bio) |>
#     dplyr::mutate(Model = "SS3")
#   
#   
#   SS3Landings$Fleet <- Hist@OM@Fleet[[1]]@Name[SS3Landings$Fleet]
#   SS3Landings$Sim <- 1
#   
#   df <- dplyr::bind_rows(
#     HistLandings,
#     SS3Landings
#   ) |>
#     dplyr::group_by(Year, Model, Fleet) |>
#     dplyr::summarise(Value = sum(Value))
#   
#   p1 <- ggplot(df, aes(x = Year, y = Value, color = Model, linetype = Model)) +
#     facet_wrap(~Fleet, ncol = 3, scales = "free") +
#     geom_line() +
#     theme_bw()
#   
#   pDF <- df |>
#     dplyr::group_by(Fleet, Year) |>
#     dplyr::summarise(Mean = mean(Value[Model == "SS3"] / Value[Model != "SS3"]))
#   
#   p2 <- ggplot(pDF, aes(x = Year, y = Mean, color = Fleet)) +
#     geom_line() +
#     theme_bw() +
#     labs(y = "Ratio SS3/Model")
#   
#   print(patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(0.8, 0.2)))
#   invisible(df)
# }
# 
# 
# CompareSSRemovals <- function(replist, Hist) {
#   if (!inherits(Hist, "hist")) {
#     cli::cli_abort("`Hist` must be class `hist`")
#   }
#   
#   mainyrs <- replist$startyr:replist$endyr
#   AgeClasses <- GetSSAgeClasses(replist$natage)
#   
#   HistRemovals <- Removals(Hist, byFleet = TRUE) |>
#     dplyr::mutate(Model = "Import") |>
#     dplyr::filter(Sim == 1) |>
#     dplyr::group_by(Year, Fleet, Model) |>
#     dplyr::summarise(Value = sum(Value))
#   
#   SS3Removals <- replist$catch |> dplyr::filter(Yr %in% mainyrs)
#   if ("dead_bio" %in% names(SS3Removals)) {
#     SS3Removals <- SS3Removals |>
#       dplyr::select(Year = Yr, Fleet, Value = dead_bio) |>
#       dplyr::mutate(Model = "SS3")
#   } else {
#     SS3Removals <- SS3Removals |>
#       dplyr::select(Year = Yr, Fleet, Value = kill_bio) |>
#       dplyr::mutate(Model = "SS3")
#   }
#   
#   SS3Removals$Fleet <- Hist@OM@Fleet[[1]]@Name[SS3Removals$Fleet]
#   SS3Removals$Sim <- 1
#   
#   df <- dplyr::bind_rows(
#     HistRemovals,
#     SS3Removals
#   ) |>
#     dplyr::group_by(Year, Model, Fleet) |>
#     dplyr::summarise(Value = sum(Value))
#   
#   p1 <- ggplot(df, aes(x = Year, y = Value, color = Model, linetype = Model)) +
#     facet_wrap(~Fleet, ncol = 3, scales = "free") +
#     geom_line() +
#     theme_bw()
#   
#   pDF <- df |>
#     dplyr::group_by(Fleet, Year) |>
#     dplyr::summarise(Mean = mean(Value[Model == "SS3"] / Value[Model != "SS3"]))
#   
#   p2 <- ggplot(pDF, aes(x = Year, y = Mean, color = Fleet)) +
#     geom_line() +
#     theme_bw() +
#     labs(y = "Ratio SS3/Model")
#   
#   print(patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(0.8, 0.2)))
#   invisible(df)
# }
# 
# 
# CompareSSRefPoints <- function(replist, Hist) {
#   
#   SBMSY <- SPMSY <- MSY <- MSYRemovals <- 
#     dead_bio <- kill_bio <- Mean <- SSB_MSY <- 
#     MSYLandings # CRAN check hacks
#   
#   refs <- replist$derived_quants |>
#     dplyr::filter(Label %in% c(
#       "Dead_Catch_MSY",
#       "Ret_Catch_MSY",
#       "SSB_MSY",
#       "SPR_MSY"
#     )) |>
#     dplyr::select(Label, Value)
#   
#   OM <- data.frame(
#     Variable = c("SBMSY", "SPMSY", "MSYRemovals", "MSYLandings"),
#     OM = c(
#       SBMSY(Hist) |> dplyr::filter(Sim == 1, Year == max(Year)) |> dplyr::pull(Value),
#       SPMSY(Hist) |> dplyr::filter(Sim == 1, Year == max(Year)) |> dplyr::pull(Value),
#       MSY(Hist) |> dplyr::filter(Sim == 1, Year == max(Year)) |> dplyr::pull(Value),
#       MSY(Hist, type = "Landings") |> dplyr::filter(Sim == 1, Year == max(Year)) |> dplyr::pull(Value)
#     )
#   )
#   
#   SS <- data.frame(
#     Variable = c("SBMSY", "SPMSY", "MSYRemovals", "MSYLandings"),
#     SS = c(
#       refs |> dplyr::filter(Label == "SSB_MSY") |> dplyr::pull(Value),
#       refs |> dplyr::filter(Label == "SSB_MSY") |> dplyr::pull(Value),
#       refs |> dplyr::filter(Label == "Dead_Catch_MSY") |> dplyr::pull(Value),
#       refs |> dplyr::filter(Label == "Ret_Catch_MSY") |> dplyr::pull(Value)
#     )
#   )
#   
#   dplyr::left_join(OM, SS) |> dplyr::mutate("OM/SS" = OM / SS)
# }
# 
