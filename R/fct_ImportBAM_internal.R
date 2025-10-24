


SetupOM_BAM <- function(BAMdata, nSim=48, pYear=30) {
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


BAM2Stock <- function(BAMdata, nSim, TimeSteps) {
  CurrentYear <- BAMdata$parms$endyr
  histTS <- TimeSteps[floor(TimeSteps)<=CurrentYear]
  nYear <- length(histTS)
  pYear <- length(TimeSteps) - nYear
  
  stock <- Stock(Name=BAMdata$info$species,
                 nYear=nYear,
                 pYear=pYear,
                 CurrentYear= CurrentYear,
                 nSim=nSim)
  
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
  
  Length(stock) <-  Length(Pars=list(Linf=Linf,
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
  
  Weight(stock) <- Weight(Pars=list(),
                          MeanAtAge = array(WeightAtAge,
                                            dim=c(1, length(AgeClasses), 1),
                                            dimnames=list(Sim=1,
                                                          Age=AgeClasses,
                                                          TimeStep=histTS[1])
                          ),
                          Units = 'kg')
  
  
  NaturalMortality(stock) <- NaturalMortality(Pars=list(), 
                                              MeanAtAge=array(AgeSeries$M,
                                                              dim=c(1, length(AgeClasses), 1),
                                                              dimnames=list(Sim=1,
                                                                            Age=AgeClasses,
                                                                            TimeStep=histTS[1])
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
  Maturity(stock) <- Maturity(Pars=list(), 
                              MeanAtAge=array(MaturityAtAge,
                                              dim=c(1, length(AgeClasses), 1),
                                              dimnames=list(Sim=1,
                                                            Age=AgeClasses,
                                                            TimeStep=histTS[1])
                              )
  )

 
  Fecundity(stock) <- Fecundity(Pars=list(),
                                MeanAtAge=array(AgeSeries$reprod,
                                                dim=c(1, length(AgeClasses), 1),
                                                dimnames=list(Sim=1,
                                                              Age=AgeClasses,
                                                              TimeStep=histTS[1])
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
                    RecDevInit = array(0),
                    RecDevHist = array(0),
                    RecDevProj = array(0),
                    SpawnTimeFrac = BAMdata$parms$spawn.time,
                    Units=NumberUnits
  )
  
  # already done in rec devs
  # stock |> Depletion() |> Initial() <- BAMdata$t.series$B.B0[1]
  
  stock <- PopulateStock(stock)

  # Recruitment Deviations 
  UnfishedEq <- ArrayMultiply(CalcUnfishedSurvival(stock, TimeSteps=TimeSteps, Expand = FALSE), 
                             aperm(AddDimension(stock@SRR@R0, 'Age'), c(1,3,2))
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
  RecTimeSteps <- histTS # + BAMdata$parms$rec.lag
  rowInd <- match(RecTimeSteps, names(recruits))
  
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
                                              TimeStep=histTS))
  
  
  
  stock@SRR@RecDevProj <- NULL # reset so it's populated again in Populate(stock)
  stock
}

GetBAMDiscardMortality <- function(Stock, TimeSteps, RetainFleets, OM, DiscMortDF=NULL) {
  
  BAMdata <- GetBAMOutput(Stock)
  nFleet <- length(RetainFleets)

  ind <- grep('D.mort.', names(BAMdata$parms))
  
  if (!length(ind)) { # No discard mortality parameters
    AgesClasses <- OM@Stock[[1]]@Ages@Classes
    nAgeClasses <- length(AgesClasses)
    DiscardMortArray <- array(tiny, dim=c(nAgeClasses, nYear(OM@Stock[[1]]), nFleet)) |>
      AddDimNames(c("Age", "TimeStep", 'Fleet'), 
                  TimeSteps = TimeSteps, 
                  Ages=AgesClasses,
                  Fleets=RetainFleets)
    
    return(DiscardMortArray)
  }
  
  RawData <- GetBAMOutput(Stock, 'dat')
  
  if (is.null(DiscMortDF)) {
    DMValues <- BAMdata$parms[ind]
    DMFleets <- gsub('D.mort.', '', names(DMValues))
    DMFleets <- gsub("[0-9]+", '', DMFleets)
    DiscMortDF <- data.frame(Fleet=DMFleets, Value=as.numeric(DMValues))
    DiscMortDF$Year <- TimeSteps[1]-1
    
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
  DiscardMortArray <- array(tiny, dim=c(nAgeClasses, nYear(OM@Stock[[1]]), nFleet)) |>
    AddDimNames(c("Age", "TimeStep", 'Fleet'), 
                TimeSteps = TimeSteps, 
                Ages=AgesClasses,
                Fleets=RetainFleets)

  for (i in 1:nrow(DiscMortDF)) {
    TSind <- which(dimnames(DiscardMortArray)$TimeStep > DiscMortDF$Year[i])
    fillvalue <- abind::asub(DiscardMortArray,
                             list(TSind, DiscMortDF$Fleet[i]),
                             2:3, drop=FALSE) 
    fillvalue[] <- DiscMortDF$Value[i]
    abind::afill(DiscardMortArray) <- fillvalue
  }
  DiscardMortArray
}

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


BAM2Fleet <- function(Stock, OM, 
                      DiscMortDF=NULL, 
                      DiscFleets=NULL, 
                      DiscSelFleets=NULL,
                      RetSelFleets=NULL) {
  
  BAMdata <- GetBAMOutput(Stock)
  TimeSteps <- TimeSteps(OM)
  
  # Combines Retention and Discard fleets 
  FleetNames <- names(BAMdata$parms)[grepl("F.prop", names(BAMdata$parms))] |>
    vapply(function(x) strsplit(x, "F.prop.")[[1]][2], character(1)) 
  
  FleetNamesOrig <- FleetNames
  
  FleetNames <- FixFleetNames(FleetNames)

  DiscardFleets <- as.character(FleetNames[endsWith(FleetNames, '.D')])
  RetainFleets <- FleetNames[!FleetNames %in% DiscardFleets] |> as.character()

  nFleet <- length(RetainFleets)
  
  # Discard Mortality Values and Time Blocks
  DiscardMortArray <- GetBAMDiscardMortality(Stock, TimeSteps, RetainFleets, OM, DiscMortDF)
  
  HistTS <- TimeSteps[TimeSteps<=OM@Stock[[1]]@CurrentYear]
  nHist <- length(HistTS)
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

  FDeadatAge <- array(0, dim=c(nAgeClasses, nHist, length(RetainFleets)),
                      dimnames = list(Age=BAM_Ages,
                                      TimeStep=TimeSteps[1:nHist],
                                      Fleet=RetainFleets))
  FRetainatAge <- FDeadatAge
  
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
      ApicalFDiscard[] <- tiny
    }
    
    FRetainatAge[,,fl] <- t(ApicalFSelect * RetainSelect)
    FDeadatAge[,,fl] <-  FRetainatAge[,,fl] + t(ApicalFDiscard * DiscardSelect)
  }

  FDeadDiscard <- ArraySubtract(FDeadatAge, FRetainatAge)
  FDiscardTotal <- ArrayDivide(FDeadDiscard, DiscardMortArray)
  FDiscardTotal[!is.finite(FDiscardTotal)] <- 0
  FInteractatAge <- ArrayAdd(FRetainatAge, FDiscardTotal)
  Effort <- apply(FInteractatAge, 2:3, max, na.rm=TRUE)
  
  FInteractMax <- replicate(nAgeClasses, apply(FInteractatAge, 2:3, max)) |> aperm(c(3,1,2)) 
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
                              'TimeStep'=  dimnames(apicalF)$TimeStep)
    
    fleet@FishingMortality <- FishingMortality(ApicalF=apicalF)
    
    discmort <- AddDimension(DiscardMortArray[,,fl], 'Sim') |>
      aperm(c(3,1,2))
    
    fleet@DiscardMortality <- DiscardMortality(MeanAtAge = discmort)
    
    effort <- AddDimension(Effort[,fl, drop=FALSE], 'Sim') |>
      abind::adrop(2) |> aperm(2:1)
    fleet@Effort <- Effort(Effort=effort)
    fleet@Effort@Catchability <- array(1, c(1,1))  |> 
      AddDimNames(c('Sim', 'TimeStep'), TimeSteps=TimeSteps)
    
    MeanAtAge <- AddDimension(SelectivityAtAge[,,fl], 'Sim') |> aperm(c(3,1,2))
    fleet@Selectivity <- Selectivity(MeanAtAge=MeanAtAge)
    MeanAtAge <- AddDimension(RetentionAtAge[,,fl], 'Sim') |> aperm(c(3,1,2))
    fleet@Retention <- Retention(MeanAtAge=MeanAtAge)
    
    
    FleetList[[fleet@Name]] <- fleet
  }
  FleetList
}
