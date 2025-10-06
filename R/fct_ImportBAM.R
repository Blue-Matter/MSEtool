
#' Import an OM from Beaufort Assessment Model (BAM) Output
#' 
#' @param Stock Character string matching ... or a list ...
#' 
#' @export
ImportBAM <- function(Stock='Red Snapper',     
                      nSim=48,
                      pYear=30,
                      populate=TRUE, 
                      silent=FALSE) {
  
  CheckPackage('bamExtras', "pak::pkg_install('nikolaifish/bamExtras')")

  BAMdata <- BAMGetObject(Stock)
  
  if (!silent) {
    cli::cli_h3('Importing OM from {.href [BAM](https://repository.library.noaa.gov/view/noaa/4847)} Output')
    cli::cli_ul()
    cli::cli_li('Title: {.val {BAMdata$info$title}}')
    cli::cli_li('Species: {.val {BAMdata$info$species}}')
    cli::cli_li('Date: {.val {BAMdata$info$date}}')
    cli::cli_end()
  }

  OM <- BAMSetupOM(BAMdata, nSim, pYear)
  
  OM@Stock <- list()
  class(OM@Stock) <- 'StockList'
  OM@Stock[[BAMdata$info$species]] <- BAM2Stock(BAMdata, 
                                                nSim=nSim(OM),
                                                TimeSteps=OM@TimeSteps)
  OM@Fleet <- list()
  class(OM@Fleet) <- 'StockFleetList'
  OM@Fleet[[BAMdata$info$species]] <- BAM2Fleet(x=Stock, Stock=OM@Stock[[1]])
  

  OM@Efactor # TODO 
  
  # TODO - Data
  if (populate) 
    OM <- PopulateOM(OM)
  
  OM
}

#' @describeIn ImportBAM Compare BAM and OM dynamics
#' @export
CompareBAM <- function(Stock, OM=NULL, ConvertUnits=NULL) {
  
  if (is.null(OM))
    OM <- ImportBAM(Stock, 2,1)
  
  CheckClass(OM, c('om', 'hist'))
  
  if (inherits(OM, 'om')) {
    Hist <- Simulate(OM, nSim=1)
  } else {
    Hist <- OM
  }
  
  if (inherits(Stock, 'BAMdata')) {
    BAMOutput <- Stock
  } else {
    BAMOutput <- BAMGetObject(Stock)
  }
  
  # Biomass
  OM_B <- Biomass(Hist) |>
    dplyr::mutate(Model='OM', 
                  Value=Value/1000) # convert to metric tons  
  
  BAM_B <- BAMOutput$t.series |> 
    dplyr::select(TimeStep=year, Value=B) |>
    dplyr::mutate(Variable='Biomass', Model='BAM') |>
    dplyr::filter(TimeStep%in% OM_B$TimeStep) 
  
  if (!is.null(ConvertUnits)) 
    BAM_B <- BAM_B |> dplyr::mutate(Value=Value*ConvertUnits)
  B_DF <- dplyr::bind_rows(OM_B, BAM_B)
  
  p <- ggplot(B_DF, aes(x=TimeStep, y=Value, color=Model, linetype=Model)) +
    geom_line() +
    expand_limits(y=0) +
    labs(x='Year', y='Biomass') +
    theme_bw()
  
  BAM_B$Value[1]
  OM_B$Value[1]
  
  rng <- (range(OM_B$Value/BAM_B$Value)-1) |> round(2) |> unique()
  cli::cli_inform("Range in delta biomass: {rng}")
  
  p 
  # Spawning Biomass
  # BAM_SB <- BAMOutput$t.series |> 
  #   dplyr::select(TimeStep=year, Value=SSB) |>
  #   dplyr::mutate(Variable='SBiomass', Model='BAM')
  # 
  # OM_SB <- SProduction(Hist) |>
  #   dplyr::mutate(Model='OM', 
  #                 Value=Value) # convert to metric tons  
  # 
  # # BAMOutput$info$units.biomass
  # SB_DF <- dplyr::bind_rows(OM_SB, BAM_SB)
  # 
  # ggplot(SB_DF, aes(x=TimeStep, y=Value, color=Model, linetype=Model)) +
  #   geom_line() +
  #   expand_limits(y=0) +
  #   labs(x='Year', y='Biomass') +
  #   theme_bw()
  
  
  
  
  
}