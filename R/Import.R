Import <- function(dir=NULL,
                   nSim=48,
                   pYears=50, ...) {
  list.files(dir)
  
}



# ---- BAM ----

BAMGetObject <- function(x='Red Snapper', type=c('rdat', 'dat')) {
  type <- match.arg(type)
  type <- paste0(type,'_')
  stockName <- gsub(' ', '',x)
  BAMdata <- try(eval(parse(text=paste0('bamExtras::',paste0(type, stockName)))), silent=TRUE)
  if (inherits(BAMdata, 'try-error'))
    cli::cli_abort(c('Could not import {.val {x}} from `bamExtras`',
                     'i'=BAMdata)
    )
  BAMdata
}

BAMSetupOM <- function(BAMdata, nSim=48, pYear=50) {
  HistYears <- BAMdata$parms$styr:BAMdata$parms$endyr
  om <- OM(Name=paste(BAMdata$info$title, BAMdata$info$species),
           nSim=nSim,
           nYear=length(HistYears),
           pYear=pYear,
           CurrentYear=max(HistYears)
  )
  
  om@TimeSteps <- CalcTimeSteps(nYear=om@nYear,
                                pYear=om@pYear,
                                CurrentYear=om@CurrentYear,
                                TimeUnits=om@TimeUnits)
  om
}


BAM2Stock <- function(BAMdata, nSim, CurrentYear, TimeSteps) {
  AgeSeries <- BAMdata$a.series
  Ages <- AgeSeries$age
  if (min(Ages) !=0) {
    addAges <- 0:(min(Ages)-1)
    nAdd <- length(addAges)
  }
  
  if (is.null(AgeSeries$mat.male)) {
    MaturityAtAge <- c(rep(0, nAdd), AgeSeries$mat.female)
  } else {
    MaturityAtAge <-  c(rep(0,nAdd), AgeSeries$mat.female * AgeSeries$prop.female + 
                          AgeSeries$mat.male * (1 - AgeSeries$prop.female))
  }
  
  
  
  histTS <- TimeSteps[floor(TimeSteps)<=CurrentYear]
  nYear <- length(histTS)
  pYear <- length(TimeSteps) - nYear
  
  # not used right now
  # phi0 <- BAMdata$parms[["BH.Phi0"]]
  # if(is.null(phi0)) phi0 <-  BAMdata$parms[["Phi0"]]
  
  # different units to AgeSeries$weight
  # BAMdata$parms$wgt.a*AgeSeries$length^ BAMdata$parms$wgt.b
  
  stock <- Stock(Name = BAMdata$info$species,
                 Ages =  Ages(MaxAge=max(Ages)),
                 Length = Length(Pars=list(
                   Linf=BAMdata$parms$Linf[1],
                   K=BAMdata$parms$K[1],
                   t0=BAMdata$parms$t0[1]),
                   Units= BAMdata$info$units.length,
                   CVatAge=c(rep(AgeSeries$length.cv[1], nAdd), AgeSeries$length.cv),
                   Timing=0.5
                 ),
                 Weight=Weight(Pars=list(),
                               MeanAtAge = c(rep(0, nAdd), AgeSeries$weight),
                               Units = BAMdata$info$units.weight),
                 NaturalMortality=NaturalMortality(Pars=list(),
                                                   MeanAtAge = c(rep(tiny, nAdd), AgeSeries$M)),
                 Maturity=Maturity(Pars=list(),
                                   MeanAtAge = MaturityAtAge),
                 Fecundity=Fecundity(Pars=list(),
                                     MeanAtAge = c(rep(tiny + .Machine$double.eps,nAdd),AgeSeries$reprod)), # this might need to be changed for other stocks
                 SRR=SRR(Pars=list(h=ifelse(is.null(BAMdata$parms[["BH.steep"]]), 0.99,
                                            BAMdata$parms[["BH.steep"]])),
                         R0=BAMdata$parms[["R.virgin.bc"]],
                         SD=BAMdata$parms[["R.sigma.logdevs"]],
                         AC=acf(BAMdata$t.series$logR.dev, lag.max = 1, plot = FALSE, na.action =na.pass)$acf[2],
                         RecDevInit = array(0),
                         RecDevHist = array(0),
                         RecDevProj = array(0),
                         SpawnTimeFrac = BAMdata$parms$spawn.time
                 ),
                 nYear = nYear,
                 pYear = pYear,
                 CurrentYear =  CurrentYear,
                 nSim=nSim) |> 
    Populate()
  
  UnfishedEq<- ArrayMultiply(CalcUnfishedSurvival(stock, TimeSteps=TimeSteps), 
                             aperm(AddDimension(GetR0(stock), 'Age'), c(1,3,2))
  )
  
  stock@SRR@RecDevInit <- array(BAMdata$N.age[1,1:ncol(BAMdata$N.age)]/UnfishedEq[1,2:nAge(stock),1],
                                dim=c(1, MaxAge(stock)))  
  dimnames(stock@SRR@RecDevInit) <- list(Sim=1, Age=1:MaxAge(stock))
  
  # equilibrium recruitment
  SSB0 <- BAMdata$eq.series$SSB.eq[1]
  RecruitsHistEq <- BevertonHolt(BAMdata$t.series$SSB[1:nYear],
                                 SSB0, 
                                 GetR0(stock)[1,1], 
                                 stock@SRR@Pars$h[1,1])

  stock@SRR@RecDevHist <- array((BAMdata$N.age[2:(nYear+1),1]/RecruitsHistEq),
                                dim=c(1, nYear)) 
  
  dimnames(stock@SRR@RecDevHist) <- list(Sim=1, `Time Step`=TimeSteps[1:nYear])
  
  stock@SRR@RecDevProj <- NULL # reset so it's populated again in Populate(stock)
  stock
}

BAMGetDiscardMortality <- function(x, TimeSteps, RetainFleets, Stock) {
  nFleet <- length(RetainFleets)
  
  BAMdata <- BAMGetObject(x)
  RawData <- BAMGetObject(x, 'dat')
  
  ind <- grep('D.mort.', names(BAMdata$parms))
  DMValues <- BAMdata$parms[ind]
  DMFleets <- gsub('D.mort.', '', names(DMValues))
  DMFleets <- gsub("[0-9]+", '', DMFleets)
  DiscMortDF <- data.frame(Fleet=DMFleets, Value=as.numeric(DMValues))
  DiscMortDF$Year <- TimeSteps[1]-1
  
  ind1 <- grep('discard mortality', RawData) 
  ind2 <- grep('#Discard mortality', RawData) 
  text <- RawData[(ind1+1): (ind2-1)]
  
  DiscMortDF$Year[(nFleet+1):nrow(DiscMortDF)] <- substr(text, start = 1, stop = 4) |> 
    as.numeric()
  
  DiscardMortArray <- array(NA, dim=c(nAge(Stock), nYear(Stock), nFleet)) |>
    AddDimNames(c("Age", "Time Step", 'Fleet'), TimeSteps = TimeSteps)
  dimnames(DiscardMortArray)$Fleet <- RetainFleets
  
  for (i in 1:nrow(DiscMortDF)) {
    TSind <- which(dimnames(DiscardMortArray)$`Time Step` > DiscMortDF$Year[i])
    fillvalue <- abind::asub(DiscardMortArray,
                             list(TSind, DiscMortDF$Fleet[i]),
                             2:3, drop=FALSE) 
    fillvalue[] <- DiscMortDF$Value[i]
    abind::afill(DiscardMortArray) <- fillvalue
  }
  DiscardMortArray
}

BAM2Fleet <- function(x, Stock) {
  TimeSteps <- TimeSteps(Stock)
  BAMdata <- BAMGetObject(x)
  
  # Combines Retention and Discard fleets 
  FleetNames <- names(BAMdata$parms)[grepl("F.prop", names(BAMdata$parms))] |>
    vapply(function(x) strsplit(x, "F.prop.")[[1]][2], character(1))
  
  DiscardFleets <- FleetNames[grepl('.D', FleetNames)] |> as.character()
  RetainFleets <- FleetNames[!FleetNames %in% DiscardFleets] |> as.character()
  nFleet <- length(RetainFleets)
  
  # Discard Mortality Values and Time Blocks
  DiscardMortArray <- BAMGetDiscardMortality(x, TimeSteps, RetainFleets, Stock)
  
  HistTS <- TimeSteps[TimeSteps<=Stock@CurrentYear]
  nHist <- length(HistTS)
  TimeSeries <- BAMdata$t.series |> dplyr::filter(year %in% HistTS)
  
  FCols <- paste0('F.',FleetNames)
  ApicalF <- TimeSeries[FCols]
  
  SelectACols <- paste0('sel.m.',FleetNames)
  SelectAtAge <- BAMdata$sel.age[SelectACols] 
  
  Ages <- BAMdata$a.series$age
  if (min(Ages) !=0) {
    addAges <- 0:(min(Ages)-1)
    nAdd <- length(addAges)
  }
  
  FDeadatAge <- array(0, dim=c(nHist, max(Ages), length(RetainFleets)))
  FRetainatAge <- array(0, dim=c(nHist, max(Ages), length(RetainFleets)))
  for (fl in seq_along(RetainFleets)) {
    fleet <- RetainFleets[fl]
    retain <- paste0('F.', fleet)
    discard <- paste0('F.', fleet, '.D')
    selretain <- paste0('sel.m.', fleet)
    seldiscard <- paste0('sel.m.', fleet, '.D')
    
    FRetainatAge[,,fl] <- ApicalF[[retain]] * SelectAtAge[[selretain]]
    FDeadatAge[,,fl] <-  FRetainatAge[,,fl] + ApicalF[[discard]] * SelectAtAge[[seldiscard]]
  }
  
  FDeadatAge <- abind::abind(array(0, 
                                   dim=c(length(HistTS), nAdd, length(RetainFleets))),  
                             FDeadatAge, along=2) |>
    aperm(c(2,1,3)) |>
    AddDimNames(c("Age", "Time Step", 'Fleet'), TimeSteps = TimeSteps)
  dimnames(FDeadatAge)$Fleet <- RetainFleets
  
  FRetainatAge <- abind::abind(array(0, 
                                     dim=c(length(HistTS), nAdd, length(RetainFleets))),  
                               FRetainatAge, along=2) |>
    aperm(c(2,1,3)) |>
    AddDimNames(c("Age", "Time Step", 'Fleet'), TimeSteps = TimeSteps)
  dimnames(FRetainatAge)$Fleet <- RetainFleets
  
  
  FDeadDiscard <- ArraySubtract(FDeadatAge,FRetainatAge)
  
  FInteractatAge <- ArrayAdd(FRetainatAge, ArrayDivide(FDeadDiscard, DiscardMortArray))
  Effort <- apply(FInteractatAge, 2:3, max)
  
  FInteractMax <- replicate(nAge(Stock),apply(FInteractatAge, 2:3, max)) |> aperm(c(3,1,2)) 
  dimnames(FInteractMax) <-  dimnames(FInteractatAge)
  
  SelectivityAtAge <- ArrayDivide(FInteractatAge, FInteractMax) 
  SelectivityAtAge[!is.finite(SelectivityAtAge)] <- 0
  
  RetentionAtAge <- ArrayDivide(FRetainatAge, FInteractatAge)
  RetentionAtAge[!is.finite(RetentionAtAge)] <- 0
  
  FleetList <- list()
  class(FleetList) <- 'FleetList'
  for (fl in seq_along(RetainFleets)) {
    fleet <- Fleet(Name=RetainFleets[fl])
    apicalF <-  apply(FDeadatAge, 2:3, max)[,fl, drop=FALSE] |> t()
    dimnames(apicalF) <- list(Sim=1,
                              'Time Step'=  dimnames(apicalF)$`Time Step`)
    
    fleet@FishingMortality <- FishingMortality(ApicalF=apicalF)
    
    discmort <- AddDimension(DiscardMortArray[,,fl], 'Sim') |>
      aperm(c(3,1,2))
    
    fleet@DiscardMortality <- DiscardMortality(MeanAtAge = discmort)
    
    effort <- AddDimension(Effort[,fl, drop=FALSE], 'Sim') |>
      abind::adrop(2) |> aperm(2:1)
    fleet@Effort <- Effort(Effort=effort)
    fleet@Effort@Catchability <- array(1, c(1,1))  |> 
      AddDimNames(c('Sim', 'Time Step'), TimeSteps=TimeSteps)
    
    MeanAtAge <- AddDimension(SelectivityAtAge[,,fl], 'Sim') |> aperm(c(3,1,2))
    fleet@Selectivity <- Selectivity(MeanAtAge=MeanAtAge)
    MeanAtAge <- AddDimension(RetentionAtAge[,,fl], 'Sim') |> aperm(c(3,1,2))
    fleet@Retention <- Retention(MeanAtAge=MeanAtAge)
    
    
    FleetList[[fleet@Name]] <- fleet
  }
  FleetList
}

ImportBAM <- function(x='Red Snapper',     
                      nSim=48,
                      pYear=50, 
                      ...) {
  
  # This works for Red Snapper - SEDAR 73
  # May need to be modified for other stocks, with aim of making it as generic
  # and flexible as possible
  
  if(!requireNamespace("bamExtras", quietly = TRUE)) {
    stop("Package `bamExtras` is required for this function. Install with `pak::pkg_install('nikolaifish/bamExtras')`",
         call. = FALSE)
  }
  
  BAMdata <- BAMGetObject(x)
  OM <- BAMSetupOM(BAMdata, nSim, pYear)
  
  OM@Stock <- list()
  class(OM@Stock) <- 'StockList'
  OM@Stock[[BAMdata$info$species]] <- BAM2Stock(BAMdata, 
                                                nSim=nSim(OM),
                                                CurrentYear=OM@CurrentYear,
                                                TimeSteps=OM@TimeSteps)
  OM@Fleet <- list()
  class(OM@Fleet) <- 'StockFleetList'
  OM@Fleet[[BAMdata$info$species]] <- BAM2Fleet(x, OM@Stock[[1]])
  
  # TODO - Obs & Imp
  
  Populate(OM) 
}

# ---- SS3 ----

ImportSS <- function(SSdir,     
                     nSim=48,
                     pYears=50, 
                     Name = "Imported SS3 Model",
                     ...) {
  
  if(!requireNamespace("r4ss", quietly = TRUE)) 
    cli::cli_abort(c('The `r4ss` package is required to use this function.',
                     'i'="It is recommended to install the Github version with `pak::pkg_install('r4ss/r4ss')`"
    ))
  
  dots <- list(dir = SSdir, ...)
  if(!any(names(dots) == "covar")) dots$covar <- FALSE
  if(!any(names(dots) == "forecast")) dots$forecast <- FALSE
  #if(!any(names(dots) == "ncols")) dots$ncols <- 1e3
  if(!any(names(dots) == "printstats")) dots$printstats <- FALSE
  if(!any(names(dots) == "verbose")) dots$verbose <- FALSE
  if(!any(names(dots) == "warn")) dots$warn <- FALSE
  
  replist <- try(do.call(r4ss::SS_output, dots), silent = TRUE)
  
  if(is.character(replist)) 
    cli::cli_abort(c("`r4ss::SS_output` function returned an error.", 
                     'x' = replist), call. = FALSE)
  
  OM <- OM(Name=Name)
  
}
