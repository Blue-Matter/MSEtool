#' Import BAM Output into an Operating Model
#'
#' Imports output from the Beaufort Assessment Model
#' ([BAM](https://repository.library.noaa.gov/view/noaa/4847)) and converts it
#' into an operating model ([OM()]) object. Stock and fleet objects are populated
#' from BAM parameters and time series, including selectivity, retention,
#' discard mortality, recruitment deviations, and stock-recruitment
#' relationships.
#'
#' @param Stock Character string matching a stock name available in `bamExtras`
#'   (e.g., `'Red Snapper'`), or a list of BAM output objects containing
#'   elements `rdat` and `dat`.
#' @param nSim Integer. Number of simulations. Default `48`.
#' @param pYear Integer. Number of projection years. Default `30`.
#' @param StockName Optional character string to override the species name taken
#'   from the BAM output.
#' @param Source Optional. Character string specifying the source (usually 
#' a link) for the OM.
#' @param DiscMortDF Optional data.frame with columns `Fleet`, `Value`, and
#'   `Year` specifying discard mortality rates by fleet and time block. If
#'   `NULL`, values are extracted directly from BAM parameters. 
#'   
#' @param DiscFleets Optional named character vector mapping retain fleet names
#'   to their corresponding discard fleet names in the BAM output, for cases
#'   where naming conventions differ.
#'   
#' @param DiscSelFleets Optional named character vector mapping retain fleet
#'   names to the selectivity series to use for discards.
#'   
#' @param RetSelFleets Optional named character vector mapping retain fleet
#'   names to alternative retention selectivity series.
#'   
#' @param incData Logical. Include `Data` in the input? Default = `TRUE`
#' @param SurveyNames A character vector of user-facing names for survey
#'   indices detected in `BAMdata`. Must have the same length as the number of
#'   survey indices found (i.e. indices not matched to any fleet name). Use
#'   this to override the default names derived from column names. 
#'   Used by [ImportBAMData()].
#' @param UnitsLandings A character vector of length equal to the number of
#'   landings fleets detected. Each element must be one of:
#'   - `"1000 lb"` — values will be converted from thousands of pounds to kg.
#'   - `"1000 n"` — values will be multiplied by 1,000 (number in thousands).
#'   Used by [ImportBAMData()].
#' @param UnitsDiscards A character vector of length equal to the number of
#'   discard fleets detected. Accepts the same values as `UnitsLandings`.
#'   Used by [ImportBAMData()].
#'     
#' @param populate Logical. If `TRUE` (default), calls [PopulateOM()] to
#'   populate the OM after construction.
#'   
#' @param silent Logical. If `TRUE`, suppresses console messages. Default
#'   `FALSE`.
#'
#' @return `ImportBAM()` returns a populated OM object. `GetBAMOutput()` returns
#'   a BAM output list of class `BAMdata` (for `type = 'rdat'`).
#'   `ListBAMStocks()` returns a character vector of available stock names.
#'
#' `ImportBAM()` requires the `bamExtras` package, which can be installed with
#' `pak::pkg_install('nikolaifish/bamExtras')`.
#'
#' The `DiscMortDF` argument accepts a data.frame with columns:
#' \describe{
#'   \item{Fleet}{Fleet name matching the retain fleet names in the OM.}
#'   \item{Value}{Discard mortality rate (0--1).}
#'   \item{Year}{The year which the value takes effect (i.e., the
#'     value applies to all years after this year).}
#' }
#'
#' When `DiscFleets`, `DiscSelFleets`, or `RetSelFleets` are provided, they
#' should be named character vectors where names are the retain fleet names and
#' values are the corresponding BAM fleet/selectivity names to use instead of
#' the defaults.
#' 
#' @seealso [ImportBAMData()]
#'
#' @export
ImportBAM <- function(Stock='Red Snapper',     
                      nSim=48,
                      pYear=30,
                      StockName=NULL,
                      Source=NULL,
                      DiscMortDF=NULL,
                      DiscFleets=NULL,
                      DiscSelFleets=NULL,
                      RetSelFleets=NULL,
                      incData = TRUE,
                      SurveyNames=NULL,
                      UnitsLandings=NULL,
                      UnitsDiscards=NULL,
                      populate=TRUE, 
                      silent=FALSE) {
  
  CheckPackage('bamExtras', pkg.path="pak::pkg_install('nikolaifish/bamExtras')")

  BAMdata <- GetBAMOutput(Stock)
  
  if (!is.null(StockName))
    BAMdata$info$species <- StockName
  
  if (!silent) {
    cli::cli_h3('Importing OM from {.href [BAM](https://repository.library.noaa.gov/view/noaa/4847)} Output')
    cli::cli_ul()
    cli::cli_li('Name: {.val {BAMdata$info$title}}')
    cli::cli_li('Species: {.val {BAMdata$info$species}}')
    cli::cli_li('Years: {.val {BAMdata$parms$styr} - {BAMdata$parms$endyr}}')
    cli::cli_end()
  }

  OM <- SetupOM_BAM(BAMdata, nSim, pYear, Source=Source)
  
  OM@Stock <- list()
  class(OM@Stock) <- 'StockList'
  OM@Stock[[BAMdata$info$species]] <- BAM2Stock(BAMdata, 
                                                nSim=nSim(OM),
                                                Years=OM@Years)
  
  OM@Fleet <- list()
  class(OM@Fleet) <- 'StockFleetList'
  OM@Fleet[[BAMdata$info$species]] <- BAM2Fleet(Stock, 
                                                OM, 
                                                DiscMortDF,
                                                DiscFleets,
                                                DiscSelFleets,
                                                RetSelFleets)
  
  OM <- ProcessEFactor(OM)
  
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
                                      Year=BAMdata$t.series$year[1], 
                                      Value=BAMdata$t.series$SSB[1])
  }
  
  
  
  if (incData) {
    if (!silent) cli::cli_alert("Importing Data")
    OM <- ImportBAMData(OM, 
                        BAMdata,
                        SurveyNames   = SurveyNames,
                        UnitsLandings = UnitsLandings,
                        UnitsDiscards = UnitsDiscards,
                        DiscFleets    = DiscFleets)
    
  }
  
  if (populate) 
    OM <- PopulateOM(OM, silent = TRUE)
  
  OM
}




#' @rdname ImportBAM
#'
#' @param type Character. Format of BAM output to retrieve: `'rdat'`
#'   (default) returns the R data object; `'dat'` returns the raw input data.
#'
#' @export
GetBAMOutput <- function(Stock='Red Snapper', type=c('rdat', 'dat')) {
  type <- match.arg(type)
  
  if (inherits(Stock, 'BAMdata'))
    return(Stock)
  
  if (!is.list(Stock) && !is.character(Stock))
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
  
  if (is.list(Stock)) {
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


#' @rdname ImportBAM
#' @export
ListBAMStocks <- function(type=c('rdat', 'dat')) {
  type <- match.arg(type)
  type <- paste0(type,'_')
  d <- utils::data(package = "bamExtras")
  nms <- d$results[,3]
  val_nms <- nms[grepl(type, nms)]
  gsub(type, '', val_nms)
}




SetupOM_BAM <- function(BAMdata, nSim=48, pYear=30, Source=NULL) {
  HistYears <- BAMdata$parms$styr:BAMdata$parms$endyr
  om <- OM(Name=paste(BAMdata$info$title, BAMdata$info$species),
           nSim=nSim,
           nYear=length(HistYears),
           pYear=pYear,
           CurrentYear=max(HistYears),
           Source=Source
  )
  
  om@Years <- CalcYears(nYear=om@nYear,
                                pYear=om@pYear,
                                CurrentYear=om@CurrentYear)
  om
}


BAM2Stock <- function(BAMdata, nSim, Years) {
  CurrentYear <- BAMdata$parms$endyr
  histTS <- Years[floor(Years)<=CurrentYear]
  nYear <- length(histTS)
  pYear <- length(Years) - nYear
  
  stock <- Stock(Name=BAMdata$info$species)
  
  stock@nSim <- nSim
  stock@CurrentYear <- CurrentYear
  nYear(stock) <- nYear
  pYear(stock) <- pYear

  AgeSeries <- BAMdata$a.series
  BAM_Ages <-AgeSeries$age
  Ages(stock) <- Ages(MinAge=min(BAM_Ages), MaxAge=max(BAM_Ages)) 
  AgeClasses <- stock |> Ages() |> Classes()
  nAgeClasses <- length(AgeClasses)
  
  
  if (BAMdata$info$units.length == 'mm') {
    Linf <- BAMdata$parms$Linf[1]
  } else if (BAMdata$info$units.length == 'inch') {
    Linf <- inch2mm(BAMdata$parms$Linf[1])
  } else {
    cli::cli_abort('`BAMdata$info$units.length`:  {.val {BAMdata$info$units.length}} currently not supported', .internal=TRUE)
  }
  
  Length(stock) <- Length(Pars=list(Linf=Linf,
                                    K=BAMdata$parms$K[1],
                                    t0=BAMdata$parms$t0[1]),
                          Units= 'mm',
                          CVatAge=AgeSeries$length.cv,
                          Timing=0.5)
  
  if (BAMdata$info$units.weight == 'kg') {
    WeightAtAge <- AgeSeries$weight
  } else if (BAMdata$info$units.weight == 'lb') {
    WeightAtAge <- lb2kg(AgeSeries$weight)
  } else if (BAMdata$info$units.weight == 'lb (gutted)') {
    WeightAtAge <- lb2kg(AgeSeries$weight)
  } else {
    cli::cli_abort('`BAMdata$info$units.weight`:  {.val {BAMdata$info$units.weight}} currently not supported', .internal=TRUE)
  }
  
  Weight(stock) <- Weight(
    Pars=list(),
    MeanAtAge = array(WeightAtAge,
                      dim=c(1, length(AgeClasses), 1),
                      dimnames=list(Sim=1,
                                    Age=AgeClasses,
                                    Year=histTS[1])
    ),
    Units = 'kg')
  
  
  NaturalMortality(stock) <- NaturalMortality(
    Pars=list(), 
    MeanAtAge=array(AgeSeries$M,
                    dim=c(1, length(AgeClasses), 1),
                    dimnames=list(Sim=1,
                                  Age=AgeClasses,
                                  Year=histTS[1])
    )
  )
  
  
  if (is.null(AgeSeries$mat.male)) {
    MaturityAtAge <- AgeSeries$mat.female
  } else {
    if (is.null(AgeSeries$mat.female))
      AgeSeries$mat.female <- AgeSeries$mat.fem.endyr
    
    MaturityAtAge <-  c(AgeSeries$mat.female * AgeSeries$prop.female + 
                          AgeSeries$mat.male * (1 - AgeSeries$prop.female))
  }
  
  Maturity(stock) <- Maturity(
    Pars=list(), 
    MeanAtAge=array(MaturityAtAge,
                    dim=c(1, length(AgeClasses), 1),
                    dimnames=list(Sim=1,
                                  Age=AgeClasses,
                                  Year=histTS[1])
    )
  )
  
  
  Fecundity(stock) <- Fecundity(
    Pars=list(),
    MeanAtAge=array(AgeSeries$reprod,
                    dim=c(1, length(AgeClasses), 1),
                    dimnames=list(Sim=1,
                                  Age=AgeClasses,
                                  Year=histTS[1])
    ),
    Units=BAMdata$info$units.ssb,
  )
  
  h <- ifelse(is.null(BAMdata$parms[["BH.steep"]]), 0.99,
              BAMdata$parms[["BH.steep"]])
  
  R0 <- BAMdata$parms[["R.virgin.bc"]]
  SD <- BAMdata$parms[["R.sigma.logdevs"]]
  ACF <- acf(BAMdata$t.series$logR.dev, lag.max = 1, plot = FALSE, na.action =na.pass)$acf[2]
  
  # Not sure if this is neccessary. 
  # SSBpR is different than phi0 in some cases.
  # Derived h, R0 result in incorrect recruits 
  # 
  # phi0 <- BAMdata$parms[["BH.Phi0"]]
  # if(is.null(phi0)) phi0 <- BAMdata$parms[["Phi0"]]
  # 
  # Arec <- 4*h/(1-h)/phi0
  # Brec <- (5*h-1)/(1-h)/R0/phi0
  # stock2 <- PopulateStock(stock)
  # UnfishedSurv <- CalcUnfishedSurvivalStock(stock2, SP=TRUE)
  # 
  # SSBpR <- sum(AgeSeries$reprod * UnfishedSurv[1,,1])
  # 
  # K <- Arec * SSBpR
  # h <- K/(4 + K)
  # if (h < 0.99) 
  #   R0 <- (5*h-1)/(1-h)/Brec/SSBpR
  
  if (BAMdata$info$units.rec == "number fish") {
    NumberUnits <- 1
  } else {
    cli::cli_abort('`BAMdata$info$units.rec`:  {.val {BAMdata$info$units.rec}} currently not supported', .internal=TRUE)
    
  }
  
  
  SRR(stock) <- SRR(Pars=list(h=h),
                    R0=R0,
                    SD=SD,
                    AC=ACF,
                    SpawnTimeFrac = BAMdata$parms$spawn.time,
                    Units=NumberUnits
  )
  
  # already done in rec devs
  # stock |> Depletion() |> Initial() <- BAMdata$t.series$B.B0[1]
  
  stock <- PopulateStock(Stock = stock, 
                         nYear = stock@nYear, 
                         pYear = stock@pYear, 
                         CurrentYear = stock@CurrentYear,
                         nSim = stock@nSim)
  
  # Recruitment Deviations 
  UnfishedEq <- ArrayMultiply(array1=CalcUnfishedSurvival(stock, Years=Years, Extend = FALSE), 
                              array2=aperm(AddDimension(stock@SRR@R0, 'Age'), c(1,3,2))
  )
  
  N.age <- BAMdata$N.age
  InitRecDevs <- N.age[1,]/UnfishedEq[1,,1]
  
  stock@SRR@RecDevInit <- array(InitRecDevs[2:length(InitRecDevs)],
                                dim=c(1, nAgeClasses-1),
                                dimnames = list(Sim=1,
                                                Age=AgeClasses[-1]))  
  
  # equilibrium recruitment
  SSB0 <- BAMdata$eq.series$SSB.eq[1]
  RecruitsHistEq <- BevertonHolt(BAMdata$t.series$SSB[1:nYear],
                                 SSB0, 
                                 stock@SRR@R0[1,1], 
                                 stock@SRR@Pars$h[1,1])
  
  recruits <- N.age[,1]
  RecYears <- histTS # + BAMdata$parms$rec.lag
  rowInd <- match(RecYears, names(recruits))
  
  ageRec <- colnames(N.age) |> as.numeric() |> min()
  if (ageRec>0) {
    # Calculate equilibrium numbers
    RecruitsHistEq <- c(rep(BAMdata$eq.series$R.eq[1], ageRec), RecruitsHistEq)
    RecruitsHistEq <- RecruitsHistEq[rowInd]
  }
  
  RecDevHist <- recruits[rowInd]/RecruitsHistEq
  
  # RecDevHist[1] <- N.age[1,1]/UnfishedEq[1,1,1] # update for first year
  
  stock@SRR@RecDevHist <- array(RecDevHist,
                                dim=c(1, nYear),
                                dimnames=list(Sim=1,
                                              Year=histTS))
  
  
  
  stock@SRR@RecDevProj <- NULL # reset so it's populated again in Populate(stock)
  stock
}

GetBAMDiscardMortality <- function(Stock, Years, RetainFleets, DiscardFleets, OM, DiscMortDF=NULL) {
  
  BAMdata <- GetBAMOutput(Stock)
  nFleet <- length(RetainFleets)
  
  ind <- grep('D.mort.', names(BAMdata$parms))
  
  if (!length(ind)) { # No discard mortality parameters
    AgesClasses <- OM@Stock[[1]]@Ages@Classes
    nAgeClasses <- length(AgesClasses)
    nYears <- nYear(OM@Stock[[1]])
    
    DiscardMortArray <- array(0, dim=c(nAgeClasses, 1, nFleet))
    dimnames(DiscardMortArray) <- list(Age=AgesClasses, 
                                       Year=Years[1],
                                       Fleet=RetainFleets)
    

    
    return(DiscardMortArray)
  }
  
  RawData <- GetBAMOutput(Stock, 'dat')
  
  if (is.null(DiscMortDF)) {
    DMValues <- BAMdata$parms[ind]
    DMFleets <- gsub('D.mort.', '', names(DMValues))
    DMFleets <- gsub("[0-9]+", '', DMFleets)
    DiscMortDF <- data.frame(Fleet=DMFleets, Value=as.numeric(DMValues))
    DiscMortDF$Year <- Years[1]-1
    
    if (!all(DMFleets %in% RetainFleets))
      cli::cli_abort(c('x'="Could not match fleet names for discard mortality: {.val {DMFleets}} with Fleet Names: {.val {RetainFleets}}",
                       "i"= 'Use argument {.arg DiscMortDF} to provide a data.frame with columns: {.val {c("Fleet", "Value", "Year")}} with {.val {"Fleet"}} matching Fleet Names: {.val {RetainFleets}}',
                       ">"= 'The current {.arg DiscMortDF} looks like this: ',
                       "*"= '{.var Fleet:}  {.val {DiscMortDF$Fleet}}',
                       "*"= '{.var Value:}  {.val {DiscMortDF$Value}}',
                       "*"= '{.var Year:}  {.val {DiscMortDF$Year}}'),
                     call=NULL
      )
    
    ind1 <- grep('discard mortality', RawData)
    ind2 <- grep('#Discard mortality', RawData) 
    
    if (length(ind1) & length(ind2)) {
      text <- RawData[(ind1+1): (ind2-1)]  
      DiscMortDF$Year[(nFleet+1):nrow(DiscMortDF)] <- substr(text, start = 1, stop = 4) |> 
        as.numeric()
    }
  } 
  
  if (!all(c("Fleet", "Value", "Year") %in% colnames(DiscMortDF)))
    cli::cli_abort(c('x'='Incorrect or missing columns in `DiscMortDF`',
                     'i'='Columns must be {.val {c("Fleet", "Value", "Year")}}'),
                   call=NULL)
  
  if (!all(DiscMortDF$Fleet %in% RetainFleets))
    cli::cli_abort(c('x'="Could not match fleet names for discard mortality: {.val {DiscMortDF$Fleet}} with Fleet Names: {.val {RetainFleets}}",
                     "i"= 'Use argument {.arg DiscMortDF} to provide a data.frame with columns: {.val {c("Fleet", "Value", "Year")}} with {.val {"Fleet"}} matching Fleet Names: {.val {RetainFleets}}',
                     ">"= 'The current {.arg DiscMortDF} looks like this: ',
                     "*"= '{.var Fleet:}  {.val {DiscMortDF$Fleet}}',
                     "*"= '{.var Value:}  {.val {DiscMortDF$Value}}',
                     "*"= '{.var Year:}  {.val {DiscMortDF$Year}}'),
                   call=NULL
    )
  
  
  AgesClasses <- OM@Stock[[1]]@Ages@Classes
  nAgeClasses <- length(AgesClasses)
  nYears <- nYear(OM@Stock[[1]])
  DiscardMortArray <- array(0, dim=c(nAgeClasses, nYears, nFleet))
  dimnames(DiscardMortArray) <- list(Age=AgesClasses, 
                                     Year=Years[1:nYears],
                                     Fleet=RetainFleets)
  
  
  for (i in 1:nrow(DiscMortDF)) {
    TSind <- which(dimnames(DiscardMortArray)$Year > DiscMortDF$Year[i])
    fillvalue <- abind::asub(DiscardMortArray,
                             list(TSind, DiscMortDF$Fleet[i]),
                             2:3, drop=FALSE) 
    fillvalue[] <- DiscMortDF$Value[i]
    abind::afill(DiscardMortArray) <- fillvalue
  }
  DiscardMortArray
}



BAM2Fleet <- function(Stock, 
                      OM, 
                      DiscMortDF=NULL, 
                      DiscFleets=NULL, 
                      DiscSelFleets=NULL,
                      RetSelFleets=NULL,
                      silent=FALSE) {
  
  BAMdata <- GetBAMOutput(Stock)
  Years <- Years(OM)
  
  # Combines Retention and Discard fleets 
  FleetNamesList <- GetBAMFleetNames(BAMdata)
  FleetNames     <- FleetNamesList$FleetNames
  FleetNamesOrig <- FleetNamesList$FleetNamesOrig
  RetainFleets  <- FleetNamesList$RetainFleets
  DiscardFleets  <- FleetNamesList$DiscardFleets
  


  
  nFleet <- length(RetainFleets)
  HistTS <- Years[Years<=OM@Stock[[1]]@CurrentYear]
  nHist <- length(HistTS)
  
  # Discard Mortality Values and Time Blocks
  DiscardMortArray <- GetBAMDiscardMortality(Stock, 
                                             Years,
                                             RetainFleets, 
                                             DiscardFleets,
                                             OM, 
                                             DiscMortDF)
  

  # Selectivity, Retention, Effort, Catchability 
  year <- NULL # CRAN check hack
  TimeSeries <- BAMdata$t.series |> dplyr::filter(year %in% HistTS)
  FCols <- paste0('F.', FleetNames)
  chk <- any(!FCols %in% names(TimeSeries))
  if (chk) 
    FCols <- paste0('F.', FleetNamesOrig)
  
  ApicalF <- TimeSeries[FCols]
  names(ApicalF) <- paste0('F.', FleetNames)
  
  AgeSeries <- BAMdata$a.series
  BAM_Ages <- AgeSeries$age
  nAgeClasses <- length(BAM_Ages)
  
  FDeadatAge <- FRetainatAge <- array(0, dim=c(nAgeClasses, 
                                               nHist, 
                                               length(RetainFleets)),
                                      dimnames = list(Age=BAM_Ages,
                                                      Year=Years[1:nHist],
                                                      Fleet=RetainFleets))
  
  for (fl in seq_along(RetainFleets)) {
    fleet <- RetainFleets[fl]
    retain <- paste0('F.', fleet)
    discard <- paste0('F.', fleet, '.D')
    
    if (!is.null(DiscFleets)) {
      ind <- match(fleet, names(DiscFleets))
      if (!is.na(ind)) {
        discard <- as.character(DiscFleets[ind])
      }
    }
    
    selretain <- paste0('sel.m.', fleet)
    if (!is.null(RetSelFleets)) {
      ind <- match(fleet, names(RetSelFleets))
      if (!is.na(ind)) {
        selretain <- paste0('sel.m.', as.character(RetSelFleets[ind]))
      }
    }
    RetainSelect <- BAMdata$sel.age[[selretain]]
    
    seldiscard <- paste0('sel.m.', fleet, '.D')
    if (!is.null(DiscSelFleets)) {
      ind <- match(fleet, names(DiscSelFleets))
      if (!is.na(ind)) {
        seldiscard <- as.character(DiscSelFleets[ind])
      }
    }
    DiscardSelect <- BAMdata$sel.age[[seldiscard]]
    
    if (is.null(DiscardSelect))
      DiscardSelect <- BAMdata$sel.age[["sel.m.D"]]
    
    if (is.null(DiscardSelect)) {
      DiscardSelect <- RetainSelect
      DiscardSelect[] <- 0
    }
    
    ApicalFSelect <- ApicalF[[retain]]
    ApicalFDiscard <- ApicalF[[discard]]
    
    if (is.null(ApicalFDiscard)) {
      ApicalFDiscard <- ApicalFSelect
      ApicalFDiscard[] <- 0
    }
    
    FRetainatAge[,,fl] <- t(ApicalFSelect * RetainSelect)
    FDeadatAge[,,fl] <-  FRetainatAge[,,fl] + t(ApicalFDiscard * DiscardSelect)
  }
  
  FDeadDiscard <- ArraySubtract(FDeadatAge, FRetainatAge)
  FDiscardTotal <- ArrayDivide(FDeadDiscard, DiscardMortArray)
  FDiscardTotal[!is.finite(FDiscardTotal)] <- 0
  FInteractatAge <- ArraySum(FRetainatAge, FDiscardTotal)
  apicalEffort <- apply(FInteractatAge, 2:3, max, na.rm=TRUE)

  FInteractMax <- replicate(nAgeClasses, apply(FInteractatAge, 2:3, max)) |> 
    aperm(c(3,1,2)) 
  dimnames(FInteractMax) <-  dimnames(FInteractatAge)
  
  SelectivityAtAge <- ArrayDivide(FInteractatAge, FInteractMax) 
  SelectivityAtAge[!is.finite(SelectivityAtAge)] <- 0
  
  RetentionAtAge <- ArrayDivide(FRetainatAge, FInteractatAge)
  RetentionAtAge[!is.finite(RetentionAtAge)] <- 0
  
  FleetList <- MakeNamedList(RetainFleets)
  class(FleetList) <- 'FleetList'
  
  for (fl in seq_along(RetainFleets)) {
    fleet <- Fleet(Name=RetainFleets[fl])
    
    fleet@Catchability@Efficiency <- array(1, c(1,1),
                                           dimnames = list(
                                             Sim = 1,
                                             Year = Years[1]
                                           ))
    
    thisFleetF <- apicalEffort[,fl, drop=FALSE] |> DropDimension('Fleet')
    fleet@Effort@Effort <- AddDimension(thisFleetF, 'Sim', pos=1) 

    fleet@Selectivity@MeanAtAge <- SelectivityAtAge[,,fl, drop=FALSE] |> 
      abind::adrop(3) |>
      AddDimension('Sim') |> 
      aperm(c('Sim', 'Age', 'Year'))
    
    fleet@Retention@MeanAtAge <- RetentionAtAge[,,fl, drop=FALSE] |> 
      abind::adrop(3) |>
      AddDimension('Sim') |> 
      aperm(c('Sim', 'Age', 'Year'))

    fleet@DiscardMortality@MeanAtAge <- DiscardMortArray[,,fl, drop=FALSE] |> 
      abind::adrop(3) |>
      AddDimension('Sim') |> 
      aperm(c('Sim', 'Age', 'Year'))
      
    FleetList[[fleet@Name]] <- fleet
  }
  FleetList
}


#' Get and Fix BAM Fleet Names
#'
#' [GetBAMFleetNames()] extracts and classifies fleet names from a BAM stock
#' assessment, separating retained (landings) fleets from discard fleets.
#' [FixFleetNames()] is a helper that standardises raw BAM fleet name strings
#' by stripping leading `"L."` prefixes and converting leading `"D."` prefixes
#' to trailing `".D"` suffixes.
#'
#' @param Stock A character string matching a stock name available in
#'   `bamExtras` (e.g., `'Red Snapper'`), or a list of BAM output objects
#'   containing elements `rdat` and `dat`.
#' @param FleetNames A character vector of raw fleet name strings as extracted
#'   from BAM output.
#'
#' @return
#'
#' [GetBAMFleetNames()] returns a named list with four elements:
#'   - `$FleetNames`: character vector of all fleet names, cleaned via
#'     [FixFleetNames()].
#'   - `$RetainFleets`: character vector of landings (retained catch) fleets,
#'     i.e. all fleets not classified as discard fleets.
#'   - `$DiscardFleets`: character vector of discard fleets, identified as
#'     those ending in `".D"` or, if none found, starting with `"D."`.
#'   - `$FleetNamesOrig`: character vector of original fleet names as they
#'     appear in `BAMdata$parms`, before cleaning.
#'
#' [FixFleetNames()] returns a character vector the same length as
#' `FleetNames` with standardised names.
#'
#' @seealso [GetBAMOutput()]
#'
#' @examples
#' \dontrun{
#' GetBAMFleetNames('Red Snapper')
#' }
#'
#' FixFleetNames(c('L.cHL', 'D.rHB', 'cPT'))
#'
#' @name BAMFleetNames
#' @export
GetBAMFleetNames <- function(Stock) {
  BAMdata <- GetBAMOutput(Stock)
  FleetNamesOrig <- names(BAMdata$parms)[grepl("F.prop", names(BAMdata$parms))] |>
    vapply(function(x) strsplit(x, "F.prop.")[[1]][2], character(1)) |>
    as.character()
  
  FleetNames <- FixFleetNames(FleetNamesOrig)
  
  DiscardFleets <- as.character(FleetNames[endsWith(FleetNames, '.D')])
  
  if (is.null(DiscardFleets) || !length(DiscardFleets)) 
    DiscardFleets <- as.character(FleetNames[startsWith(FleetNames, 'D.')])
  
  RetainFleets <- FleetNames[!FleetNames %in% DiscardFleets] |> as.character()
  
  
  list(FleetNames = FleetNames, 
       RetainFleets = RetainFleets,
       DiscardFleets = DiscardFleets,
       FleetNamesOrig = FleetNamesOrig)
}


#' @rdname BAMFleetNames
#' @export
FixFleetNames <- function(FleetNames) {
  ind <- which(startsWith(FleetNames, 'D.'))
  if (length(ind)) {
    for (i in ind) {
      FleetNames[i] <- gsub('^D.', '',   FleetNames[i])
      FleetNames[i] <- paste0( FleetNames[i], '.D')
    }
  }
  gsub('^L.', '', FleetNames) |> as.character()
}


