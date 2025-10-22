
#' Import an OM from Beaufort Assessment Model (BAM) Output
#' 
#' @param Stock Character string matching ... or a list ...
#' 
#' @export
ImportBAM <- function(Stock='Red Snapper',     
                      nSim=48,
                      pYear=30,
                      DiscMortDF=NULL,
                      DiscFleets=NULL,
                      DiscSelFleets=NULL,
                      RetSelFleets=NULL,
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
  OM@Fleet[[BAMdata$info$species]] <- BAM2Fleet(Stock, 
                                                OM, 
                                                DiscMortDF,
                                                DiscFleets,
                                                DiscSelFleets,
                                                RetSelFleets)
  

  FleetNames <- names(OM@Fleet[[BAMdata$info$species]])
  OM@Efactor <- list()
  OM@Efactor[[BAMdata$info$species]] <- matrix(1, nSim, length(FleetNames),
                                               dimnames = list(Sim=1:nSim,
                                                               Fleet=FleetNames))
  
  
  ## ---- Issue with SSB (SProduction in first time step) -----
  # Z_spawn_expected <- (BAMdata$a.series$M + BAMdata$F.age[1,]) * BAMdata$parms$spawn.time
  # Z_spawn_actual <- -log(BAMdata$N.age.spawn[1,]/BAMdata$N.age[1,])
  # 
  # plot(Z_spawn_expected, type='l', ylim=c(0, max(c(Z_spawn_expected, Z_spawn_actual))))
  # lines(Z_spawn_actual, col='blue')
  # 
  # sum(BAMdata$N.age[1,] * exp(-Z_spawn_expected) * BAMdata$a.series$reprod)
  # sum(BAMdata$N.age[1,] * exp(-Z_spawn_actual) * BAMdata$a.series$reprod)
  # BAMdata$t.series$SSB[1]
  
  if (BAMdata$parms$spawn.time>0) {
    OM@Misc$SProduction <- data.frame(Sim=1,
                                      Stock=BAMdata$info$species,
                                      TimeStep=BAMdata$t.series$year[1], 
                                      Value=BAMdata$t.series$SSB[1])
  }
  
  
  
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
  re <- Out[[name]]$RelativeError |> 
    dplyr::mutate(RelativeError=abs(RelativeError)) |> 
    dplyr::filter(RelativeError>thresh)
  if (nrow(re)>0) {
    cli::cli_alert('{.val {name}:} Some Relative Error > {thresh}%')
    print(re) 
    
    p <- ggplot(Out[[name]]$df, aes(x=TimeStep, y=Value, color=Model)) +
      geom_line() +
      labs(x='Year', y=name, title = Out$Stock) +
      theme_bw()
    
    print(p)
    
  } else {
    cli::cli_alert('{.val {name}:} All Relative Error < {thresh}%')
  }
}


#' @describeIn ImportBAM Compare BAM and OM dynamics
#' @export
CompareBAM <- function(Stock, OM=NULL, thresh=0.1) {
  
  List <- ProcessBAMArgs(Stock, OM)
  Hist <- List$Hist
  BAMdata <- List$BAMdata
  
  Out <- list()
  Out$Stock <- BAMdata$info$species
  Out$Recruits <- CompareBAM_Recruits(BAMdata, Hist)
  Out$Number <- CompareBAM_Number(BAMdata, Hist)
  Out$Biomass <- CompareBAM_Biomass(BAMdata, Hist)
  
  
  PrintPlotBAMRE(Out, 'Recruits', thresh)
  PrintPlotBAMRE(Out, 'Number', thresh)
  PrintPlotBAMRE(Out, 'Biomass', thresh)
  

  invisible(Out)
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
  
  RelativeError <- df |> 
    tidyr::pivot_wider(names_from = Model, values_from = Value) |> 
    dplyr::group_by(TimeStep) |>
    dplyr::summarise(RelativeError=(OM-BAM)/BAM*100, .groups='drop') 
  
  list(df=df, RelativeError=RelativeError)
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
  
  RelativeError <- df |> 
    tidyr::pivot_wider(names_from = Model, values_from = Value) |> 
    dplyr::group_by(TimeStep) |>
    dplyr::summarise(RelativeError=(OM-BAM)/BAM*100, .groups='drop') 
  
  list(df=df, RelativeError=RelativeError)

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
  
  RelativeError <- df |> 
    tidyr::pivot_wider(names_from = Model, values_from = Value) |> 
    dplyr::group_by(TimeStep) |>
    dplyr::summarise(RelativeError=(OM-BAM)/BAM*100, .groups='drop') 
  
  list(df=df, RelativeError=RelativeError)
  
}
