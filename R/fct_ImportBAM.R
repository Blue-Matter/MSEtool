
#' Import an OM from Beaufort Assessment Model (BAM) Output
#' 
#' @param Stock Character string matching ... or a list ...
#' 
#' @export
ImportBAM <- function(Stock='Red Snapper',     
                      nSim=48,
                      pYear=30,
                      DiscMortDF=NULL,
                      populate=TRUE, 
                      silent=FALSE) {
  
  CheckPackage('bamExtras', "pak::pkg_install('nikolaifish/bamExtras')")

  BAMdata <- GetBAMOutput(Stock)
  
  if (!silent) {
    cli::cli_h3('Importing OM from {.href [BAM](https://repository.library.noaa.gov/view/noaa/4847)} Output')
    cli::cli_ul()
    cli::cli_li('Title: {.val {BAMdata$info$title}}')
    cli::cli_li('Species: {.val {BAMdata$info$species}}')
    cli::cli_end()
  }

  OM <- SetupOM_BAM(BAMdata, nSim, pYear)
  
  OM@Stock <- list()
  class(OM@Stock) <- 'StockList'
  OM@Stock[[BAMdata$info$species]] <- BAM2Stock(BAMdata, 
                                                nSim=nSim(OM),
                                                TimeSteps=OM@TimeSteps)
  
  
  OM@Fleet <- list()
  class(OM@Fleet) <- 'StockFleetList'
  OM@Fleet[[BAMdata$info$species]] <- BAM2Fleet(x=Stock, 
                                                Stock=OM@Stock[[1]], 
                                                DiscMortDF)
  

  FleetNames <- names(OM@Fleet[[BAMdata$info$species]])
  OM@Efactor <- list()
  OM@Efactor[[BAMdata$info$species]] <- matrix(1, nSim, length(FleetNames),
                                               dimnames = list(Sim=1:nSim,
                                                               Fleet=FleetNames))
  
  # TODO - Data
  
  if (populate) 
    OM <- PopulateOM(OM)
  
  OM
}




#' @describeIn ImportBAM description
#' @export
GetBAMOutput <- function(Stock='Red Snapper', type=c('rdat', 'dat')) {
  type <- match.arg(type)
  
  if (!inherits(Stock, c('list', 'character')))
    cli::cli_abort("`Stock` must be a character string matching a stock in `bamExtras` or a list of BAM output objects")
  
  if (inherits(Stock, 'character')) {
    stockName <- gsub(' ', '',Stock)
    BAMdata <- try(eval(parse(text=paste0('bamExtras::',paste0(type, '_', stockName)))), silent=TRUE)
    if (inherits(BAMdata, 'try-error'))
      cli::cli_abort(c('Could not import {.val {Stock}} from `bamExtras`',
                       'x'=BAMdata,
                       'i'='Valid stocks in `bamExtras` are: {.val {ListBAMStocks()}}')
      )
    
    if (type=='rdat')
      BAMdata <- ConvertUnitsBAM(BAMdata)
    class(BAMdata) <- 'BAMdata'
    return(BAMdata)
  }
  
  if (inherits(Stock, 'list')) {
    nms <- names(Stock)
    if (!all(c('rdat', 'dat') %in% nms)) {
      cli::cli_abort(c('`Stock` is a list but does not appear to be valid BAM output',
                       'i'='Could not find elements `rdat` and `dat` in list names')
      )
    }
    BAMdata <- Stock[[type]]
    if (type=='rdat')
      class(BAMdata) <- 'BAMdata'
    return(BAMdata)
  }
  
  cli::cli_abort("Function terminated without returning object. Is `Stock` a character string or a list of BAM output objects?")
}


#' @describeIn ImportBAM description
#' @export
ListBAMStocks <- function(type=c('rdat', 'dat')) {
  type <- match.arg(type)
  type <- paste0(type,'_')
  d <- data(package = "bamExtras")
  nms <- d$results[,3]
  val_nms <- nms[grepl(type, nms)]
  gsub(type, '', val_nms)
}


CompareBAM_Number <- function(Stock, OM=NULL) {
  
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
    BAMOutput <- GetBAMOutput(Stock)
  }
  
  OM_Value <- Number(Hist, byAge=TRUE) |> dplyr::mutate(Model='OM') |>
    dplyr::filter(Sim==1)
  
  BAM_Value <- BAMOutput$N.age
  dnames <- dimnames(BAM_Value)
  dimnames(BAM_Value) <- list(TimeStep=dnames[[1]],
                              Age=dnames[[2]])
  
  
  BAM_Value <- BAM_Value |> array2DF() |> 
    ConvertDF() |>
    dplyr::mutate(Model='BAM', Variable='Number')
  
  df <- dplyr::bind_rows(OM_Value, BAM_Value)
  
  ggplot(df,  aes(x=TimeStep, y=Value, color=Model, linetype=Model)) +
    facet_wrap(~Age, scales='free_y') +
    geom_line() +
    expand_limits(y=0) +
    labs(x='Year', y='Number') +
    theme_bw()
  
}

CompareBAM_Biomass <- function(Stock, OM=NULL, ConvertUnits=NULL) {
  
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
    BAMOutput <- GetBAMOutput(Stock)
  }
  
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
  
  rng <- (range(OM_B$Value/BAM_B$Value)-1) |> round(2) |> unique()
  cli::cli_inform("Range in delta biomass: {rng}")
  
  ggplot(B_DF, aes(x=TimeStep, y=Value, color=Model, linetype=Model)) +
    geom_line() +
    expand_limits(y=0) +
    labs(x='Year', y='Biomass') +
    theme_bw()
  
  
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
    BAMOutput <- GetBAMOutput(Stock)
  }
  
  # Number 
  print(CompareBAM_Number(BAMOutput, Hist))
  
  # Biomass
  print(CompareBAM_Biomass(BAMOutput, Hist, ConvertUnits))
  
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
