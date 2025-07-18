Import <- function(dir=NULL,
                   nSim=48,
                   pYear=50, ...) {
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
  
  Ages <- Ages(MaxAge=max(Ages)) 
  stock <- Stock(Name = BAMdata$info$species,
                 Ages =  Ages,
                 Length = Length(Pars=list(
                   Linf=BAMdata$parms$Linf[1],
                   K=BAMdata$parms$K[1],
                   t0=BAMdata$parms$t0[1]),
                   Units= BAMdata$info$units.length,
                   CVatAge=c(rep(AgeSeries$length.cv[1], nAdd), AgeSeries$length.cv),
                   Timing=0.5
                 ),
                 Weight=Weight(Pars=list(),
                               MeanAtAge = array(c(rep(0, nAdd), AgeSeries$weight),
                                                 dim=c(1, length(Ages@Classes), 1),
                                                 dimnames=list(Sim=1,
                                                               Age=Ages@Classes,
                                                               TimeStep=histTS[1])
                               ),
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
  
  dimnames(stock@SRR@RecDevHist) <- list(Sim=1, TimeStep=TimeSteps[1:nYear])
  
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
    AddDimNames(c("Age", "TimeStep", 'Fleet'), TimeSteps = TimeSteps)
  dimnames(DiscardMortArray)$Fleet <- RetainFleets
  
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
    AddDimNames(c("Age", "TimeStep", 'Fleet'), TimeSteps = TimeSteps)
  dimnames(FDeadatAge)$Fleet <- RetainFleets
  
  FRetainatAge <- abind::abind(array(0, 
                                     dim=c(length(HistTS), nAdd, length(RetainFleets))),  
                               FRetainatAge, along=2) |>
    aperm(c(2,1,3)) |>
    AddDimNames(c("Age", "TimeStep", 'Fleet'), TimeSteps = TimeSteps)
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

ImportBAM <- function(x='Red Snapper',     
                      nSim=48,
                      pYear=50,
                      populate=TRUE) {
  
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
  if (populate) 
    OM <- Populate(OM)
  
  OM
}

# ---- SS3 ----
DropXXCols <- function(array) {
  ind <- which(names(array) != 'XX')
  array[,ind]
}

ImportSS <- function(SSdir,     
                     nSim=48,
                     pYear=50, 
                     Name = "Imported SS3 Model",
                     silent=FALSE,
                     ...) {
  
  if(!requireNamespace("r4ss", quietly = TRUE)) 
    cli::cli_abort(c('The `r4ss` package is required to use this function.',
                     'i'="It is recommended to install the Github version with `pak::pkg_install('r4ss/r4ss')`"
    ))
  
  # dots <- list(dir = SSdir) 
  dots <- list(dir = SSdir, ...)
  if(!any(names(dots) == "covar")) dots$covar <- FALSE
  if(!any(names(dots) == "forecast")) dots$forecast <- FALSE
  #if(!any(names(dots) == "ncols")) dots$ncols <- 1e3
  if(!any(names(dots) == "printstats")) dots$printstats <- FALSE
  if(!any(names(dots) == "verbose")) dots$verbose <- FALSE
  if(!any(names(dots) == "warn")) dots$warn <- FALSE
  
  if (!silent)
    cli::cli_progress_message('Importing SS3 output from {.val {SSdir}}')
  replist <- try(do.call(r4ss::SS_output, dots), silent = TRUE)
  if (!silent)
    cli::cli_progress_done()
  
  if(is.character(replist)) 
    cli::cli_abort(c("`r4ss::SS_output` function returned an error.", 
                     'x' = replist), call. = FALSE)
  
  nStock <- replist$nsexes
  nFleet <- replist$nfleets
  
  if(!silent) 
    cli::cli_alert('{.val {nStock}-sex} and {.val {nFleet}-fleet} model detected.')
  
  OM <- OM(Name=Name)
  OM@nSim <- nSim
  mainyrs <- replist$startyr:replist$endyr
  OM@nYear <- length(mainyrs)
  OM@pYear <- pYear
  OM@CurrentYear <- max(mainyrs)
  ProYears <- seq(max(mainyrs)+1, by=1, length.out=pYear)
  OM@TimeSteps <- c(mainyrs, ProYears)
  
  if (nStock==1) {
    StockNames <- 'Female and Male'
    
  } else if (nStock==2) {
    StockNames <- c('Female', 'Male')
  } else {
    cli::cli_abort('`nStock` should be {.val {1} or {2}}')
  }
  
  OM@Stock <- purrr::map(seq_along(StockNames), SS2Stock, 
                         replist=replist, 
                         pYear=pYear, 
                         nSim=nSim)
  
  names(OM@Stock) <- StockNames
  

  OM@Fleet <- MakeNamedList(StockNames, list())
  
  FleetNames <- replist$catch$Fleet_Name  |> unique()
  
  for (st in seq_along(OM@Fleet)) {
    OM@Fleet[[st]] <- MakeNamedList(FleetNames, new('fleet'))
    for (fl in seq_along(FleetNames)) {
      OM@Fleet[[st]][[fl]] <- SS2Fleet(st, fl, replist, OM@Stock[[st]])
    }
  }
  
  
  OM@Stock$Female@Length@Classes
  
  
  # Drop Survey Fleets and Add to Data instead
  
  # Data

  
  
  
  OM
  
  
}

SS2Stock <- function(st, replist, pYear, nSim) {
  
  mainyrs <- replist$startyr:replist$endyr
  nyears <- length(mainyrs)
  
  Stock <- new('stock')
  Stock@Name <- ifelse(st == 1, "Female", "Male")  # TODO - option for single sex combined models
  
  if(!is.null(replist$movement) && nrow(replist$movement) > 0) 
    cli::cli_alert_warning("Movement detected in SS model but not imported right now.")
  
  # Number-at-Age
  if(replist$nsexes > 1) {
    # For the 2 sex model with multiple seasons used to develop this code: 
    # the i-th morph recruits appear in the i-th season Males are indexed morphs 5-8 in a 4-season model. 
    # Morph2 re-maps males to 1-4
    N_at_age <- dplyr::filter(replist$natage, Sex == st, `Beg/Mid` == "B", Era == "VIRG") %>%
      dplyr::mutate(Morph2 = Morph - (st - 1) * replist$nseasons) %>% dplyr::filter(Morph2 == Seas)
  } else {
    # For the single-sex model with multiple seasons used to develop this code: 
    # the annual recruits entering the population is found by matching BirthSeas to Seas
    N_at_age <- dplyr::filter(replist$natage, Sex == st, `Beg/Mid` == "B", Era == "VIRG") %>%
      dplyr::filter(BirthSeas == Seas)
  }
  
  AgeClasses <- suppressWarnings(as.numeric(colnames(N_at_age)))
  AgeClasses <- AgeClasses[!is.na(AgeClasses)]
  n_age <- length(AgeClasses)
  
  R0 <- N_at_age[[as.character(min(AgeClasses))]] |> sum(na.rm=TRUE)
  
  Stock@Ages <- Ages(MaxAge=max(AgeClasses), MinAge=min(AgeClasses))
  Stock@Length <- SS2Length(st, replist, mainyrs, nyears, AgeClasses)
  Stock@Weight <- SS2Weight(st, replist, mainyrs, nyears, AgeClasses)
  Stock@NaturalMortality <- SS2NaturalMortality(st, replist, mainyrs, nyears, AgeClasses)
  Stock@Maturity <- SS2Maturity(st, replist, mainyrs, nyears, AgeClasses)
  Stock@Fecundity <- SS2Fecundity(st, replist, mainyrs, nyears, AgeClasses)
  Stock@Depletion <- SS2Depletion(st, replist, mainyrs)
  
  Stock@SRR <- SS2SRR(st, replist, mainyrs, AgeClasses, R0, Depletion, pYear, nSim)
  Stock@nYear <- length(mainyrs)
  Stock@pYear <- pYear
  Stock@nSim <- nSim
  Stock
}

GetEndGrowth <- function(st,replist) {
  endgrowth <- dplyr::filter(replist$endgrowth, Sex == st, Seas == 1)
  if(!is.null(endgrowth$BirthSeas)) endgrowth <- dplyr::filter(endgrowth, BirthSeas == 1)
  if(!is.null(endgrowth$Settlement)) endgrowth <- dplyr::filter(endgrowth, Settlement == 1)
  endgrowth
}

GetSSAgeClasses <- function(array) {
  AgeClasses <- suppressWarnings(as.numeric(colnames(array)))
  AgeClasses[!is.na(AgeClasses)]
}

SS2Length <- function(st, replist, mainyrs, nyears, AgeClasses) {
  
  endgrowth <- GetEndGrowth(st, replist)
  
  Len_age_df <- dplyr::filter(replist$growthseries, Morph == st, Yr %in% mainyrs)
  
  if(nrow(Len_age_df)>0) { # Would do time-varying
    Len_age <- do.call(rbind, lapply(AgeClasses, function(x) parse(text = paste0("Len_age_df$`", x, "`")) |> eval()))
    if (ncol(Len_age) == (nyears - 1)) Len_age <- cbind(Len_age, endgrowth$Len_Beg)
  } else {
    Len_age <- endgrowth$Len_Beg |>matrix(n_age, nyears) # No time-varying
  }
  
  dimnames(Len_age) <- list(Age=AgeClasses,
                            TimeStep=mainyrs)
  
  Len_age <- process_cpars(Len_age)
  
  Len_age <- AddDimension(Len_age, 'Sim') |> aperm(c('Sim', 'Age', 'TimeStep'))
  
  Length <- Length(MeanAtAge = Len_age)
  Length@CVatAge <-  mean(endgrowth$SD_Beg[-1]/endgrowth$Len_Beg[-1])
  
  
  # TODO - add parameters and models
  # https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#Growth
  Linf <- replist$Growth_Parameters[st, ]$Linf
  K <- replist$Growth_Parameters[st, ]$K
  A1 <-  replist$Growth_Parameters[st, ]$A1 
  A2 <-  replist$Growth_Parameters[st, ]$A2
  L1 <- replist$Growth_Parameters[st, ]$L_a_A1
  L2 <- replist$Growth_Parameters[st, ]$L_a_A2
  t0 <- replist$Growth_Parameters[st, ]$A_a_L0
  
  # len <- Linf + (L1-Linf)*exp(-K*(AgeClasses-A1))
  # 
  # plot(  Len_age[,1], type='l')  
  # lines(len, col='blue')
  # 
  Length@Pars <- list()
  # Length@Model
  
  Length
}

SS2Weight <- function(st, replist, mainyrs, nyears, AgeClasses) {
  Weight <- Weight()
  Weight@Pars <- list(Alpha=replist$Growth_Parameters[st, ]$WtLen1,
                      Beta=replist$Growth_Parameters[st, ]$WtLen2)
  Weight@Model <- FindModel(Weight)
  
  
  if(!is.null(replist$mean_body_wt)) {
    cli::cli_abort('TODO - get array same as M and length', .internal=TRUE)
    # Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == st, ]
    # Wt_age_df <- Wt_age_df[findInterval(mainyrs, Wt_age_df$Yr), ]
    # Wt_age <- do.call(rbind, lapply(0:n_age, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
    # if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1])
  } else {
    endgrowth <- GetEndGrowth(st, replist)
    Wt_age <- matrix(endgrowth$Wt_Beg, length(AgeClasses), nyears)
  }
  
  dimnames(Wt_age) <- list(Age=AgeClasses,
                            TimeStep=mainyrs)
  
  Wt_age <- Wt_age |> 
    process_cpars() |> 
    AddDimension('Sim') |> 
    aperm(c('Sim', 'Age', 'TimeStep'))
  
  Weight@MeanAtAge <- Wt_age
  
  Weight
}

SS2NaturalMortality <- function(st, replist, mainyrs, nyears, AgeClasses) {
  endgrowth <- GetEndGrowth(st, replist)
  
  M_at_age <- replist$M_at_age[replist$M_at_age$Sex == st & replist$M_at_age$Year %in% mainyrs, ]
  if(!nrow(M_at_age)) {
    M_at_age <- replist$M_at_age[replist$M_at_age$Sex == st & replist$M_at_age$Yr %in% mainyrs, ]
  }
  if(!nrow(M_at_age)) {
    M_at_age <- replist$M_at_age[replist$M_at_age$Gender == st & replist$M_at_age$Year %in% mainyrs, ]
  }
  
  M_age <- suppressWarnings(lapply(AgeClasses, function(x) parse(text = paste0("M_at_age$`", x, "`")) |>
                                     eval() |>
                                     as.numeric()))
  M_age <- do.call(rbind, M_age)
  
  if(all(is.na(M_age[nrow(M_age), ]))) 
    M_age[nrow(M_age), ] <- endgrowth$M[length(AgeClasses)]
  if(ncol(M_age) == (nyears - 1)) M_age <- cbind(M_age, endgrowth$M)
  
  dimnames(M_age) <- list(Age=AgeClasses,
                          TimeStep=mainyrs)
  
  M_age <- process_cpars(M_age)
  
  NaturalMortality <- NaturalMortality()
  NaturalMortality@MeanAtAge <- AddDimension(M_age, 'Sim') |> aperm(c('Sim', 'Age', 'TimeStep'))
  
  NaturalMortality
}


SS2Maturity <- function(st, replist, mainyrs, nyears, AgeClasses) {
  endgrowth <- GetEndGrowth(st, replist)
  
  if(any(endgrowth$Age_Mat < 0)) endgrowth$Age_Mat <- abs(endgrowth$Age_Mat) # Should all be 1's
  if(any(endgrowth$Len_Mat < 0)) endgrowth$Len_Mat <- abs(endgrowth$Len_Mat)
  Mat_age <- endgrowth$Len_Mat * endgrowth$Age_Mat
  Mat_age <- matrix(Mat_age, length(AgeClasses), 1)
  
  dimnames(Mat_age) <- list(Age=AgeClasses,
                          TimeStep=mainyrs[1])
  
  Maturity <- Maturity()
  Maturity@Pars <- list()
  Maturity@MeanAtAge <- AddDimension(Mat_age, 'Sim') |> aperm(c('Sim', 'Age', 'TimeStep'))
  Maturity
}


SS2Fecundity <- function(st, replist, mainyrs, nyears, AgeClasses) {
  endgrowth <- GetEndGrowth(st, replist)
  
  if(!is.null(replist$endgrowth[["Mat*Fecund"]])) {
    fec_age <- replist$endgrowth |>
      dplyr::filter(Morph == 1, Seas == 1, Sex==st) |>
      dplyr::select(Age_Beg, Fecundity=`Mat*Fecund`)
    
  } else {
    if(!is.null(replist$endgrowth$Mat_F_wtatage)) {
      fec_age <- replist$endgrowth |>
        dplyr::filter(Morph == 1, Seas == 1, Sex==st) |>
        dplyr::select(Age_Beg, Fecundity=Mat_F_wtatage)
    }
  }
  
  fec_age <- matrix(fec_age$Fecundity, length(AgeClasses), 1)
  dimnames(fec_age) <- list(Age=AgeClasses,
                           TimeStep=mainyrs[1])
  

  Fecundity <- Fecundity()
  Fecundity@Pars <- list()
  Fecundity@MeanAtAge <- AddDimension(fec_age, 'Sim') |> aperm(c('Sim', 'Age', 'TimeStep'))
  Fecundity
}


SS2Depletion <- function(st, replist, mainyrs) {
  
  Depletion <- Depletion()
  Depletion@Reference <- 'SB0'
  
  sb0 <- replist$timeseries |>
    dplyr::filter(Era == "VIRG") |>
    getElement("SpawnBio") |>
    sum(na.rm = TRUE)
  
  sb1 <- replist$timeseries |>
    dplyr::filter(Yr == mainyrs[1]) |>
    getElement("SpawnBio") |>
    sum(na.rm = TRUE)
  
  if (sb1 != sb0) {
    Depletion@Initial <- sb1/sb0  
  }
  
  
  
  if (st==1) {
    sb_curr <- replist$timeseries|>
      dplyr::filter(Yr == max(mainyrs)) |>
      getElement("SpawnBio") |>
      mean(na.rm = TRUE)
    Depletion@Final <- sb_curr/sb0
  } 
  Depletion
}

SS2SRR <- function(st, replist, mainyrs, AgeClasses, R0, Depletion, pYear, nSim) {
  
  SRR <- SRR()
  
  SRR@SD <- replist$sigma_R_in
  SRR@R0 <- R0
  
  # SRR Model and Parameters
  if(replist$SRRtype == 3 || replist$SRRtype == 6) { # Beverton-Holt SR
    par <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]
    h_out <- par$Value
    h_out[h_out < 0.2] <- 0.2
    h_out[h_out > 0.999] <- 0.999
    SRR@Pars$h <- h_out
    
  } else if(replist$SRRtype == 2) {
    SRR@Model <- 'Ricker'
    par <- replist$parameters[grepl("SR_Ricker", rownames(replist$parameters)), ]
    h_out <- par$Value
    h_out[h_out < 0.2] <- 0.2
    SRR@Pars <- list(hR=h_out)
    
  } else if(replist$SRRtype == 7) {
    s_frac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Sfrac"]
    Beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]
    s0 <- 1/SpR0
    z0 <- -log(s0)
    z_min <- z0 * (1 - s_frac)
    h_out <- 0.2 * exp(z0 * s_frac * (1 - 0.2 ^ Beta))
    h_out[h_out < 0.2] <- 0.2
    h_out[h_out > 0.999] <- 0.999
    SRR@Pars$h <- h_out
  } else {
    if(packageVersion("r4ss") == '1.24') {
      SR_ind <- match(mainyrs, replist$recruit$year)
      SSB <- replist$recruit$spawn_bio[SR_ind]
      SSB0 <- replist$derived_quants[replist$derived_quants$LABEL == "SPB_Virgin", 2]
    } else {
      SR_ind <- match(mainyrs, replist$recruit$Yr)
      SSB <- replist$recruit$SpawnBio[SR_ind]
      SSB0 <- replist$derived_quants[replist$derived_quants$Label == "SSB_Virgin", 2]
    }
    rec <- replist$recruit$pred_recr[SR_ind] # recruits to age 0
    SpR0 <- SSB0/(R0 * ifelse(season_as_years, nseas, 1))
    SRrel <- 1
    h_out <- SRopt(1, SSB, rec, SpR0, plot = FALSE, type = ifelse(SRrel == 1, "BH", "Ricker"))
    h_out[h_out < 0.2] <- 0.2
    h_out[h_out > 0.999] <- 0.999
    SRR@Pars$h <- h_out
  }
  
  # Recruit deviations
  Rec_main <- replist$recruit[replist$recruit$Yr %in% mainyrs, ]
  Rdev <- Rec_main$pred_recr/Rec_main$exp_recr
  EarlyYears <- c((min(mainyrs-1)-(length(AgeClasses)-2)):(min(mainyrs-1)))
  Rec_early <- replist$recruit[vapply(EarlyYears, match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]
  Rdev_early <- Rec_early$pred_recr/Rec_early$exp_recr
  Rdev_early[is.na(Rdev_early)] <- 1
  
  
  SRR@RecDevInit <- array(Rdev_early, dim=length(AgeClasses)-1, dimnames = list(Age=rev(AgeClasses[-1]))) |>
    AddDimension('Sim') |>
    aperm(c('Sim', 'Age'))
  
  SRR@RecDevHist <- array(Rdev, dim=length(Rdev), dimnames = list(TimeStep=mainyrs)) |>
    AddDimension('Sim') |>
    aperm(c('Sim', 'TimeStep'))
  
  AC <- log(SRR@RecDevHist[1, ]) |>
    acf(lag.max = 1, plot = FALSE) |>
    getElement("acf") |>
    getElement(2) 
  
  if (any(!is.finite(AC))) 
    AC <- 0
  
  SRR@AC <- AC
  
  if (st==2)
    SRR@SPFrom <- 1
  
  RecDevs <- GenerateRecruitmentDeviations(SD=SRR@SD, 
                                           AC=SRR@AC, 
                                           MaxAge = max(AgeClasses),
                                           nHistTS=length(mainyrs), 
                                           nProjTS=pYear,
                                           nsim=nSim,
                                           RecDevInit=SRR@RecDevInit,
                                           RecDevHist=SRR@RecDevHist)
  
  SRR@RecDevProj <- RecDevs$RecDevProj
  
  ProjectionYears <- seq(max(mainyrs)+1, by=1, length.out=pYear)
  dimnames(SRR@RecDevProj) <- list(Sim=1:nSim,
                                   TimeStep=ProjectionYears)

  SRR
}

SS2Fleet <- function(st, fl, replist, Stock) {
  
  FleetNames <- replist$catch$Fleet_Name  |> unique()
  
  mainyrs <- replist$startyr:replist$endyr
  nyears <- length(mainyrs)
  
  Fleet <- Fleet(FleetNames[fl])
  
  Fleet@DiscardMortality <- SS2DiscardMortality(st, fl, replist, mainyrs, Stock)
  Fleet@FishingMortality <- SS2FishingMortality(st, fl, replist, mainyrs)
  
  Fleet@FishingMortality@DeadAtAge[1,,44] |> max()

  Fleet@Selectivity <- SS2Selectivity(st, fl, replist, mainyrs, 
                                      Fleet@FishingMortality,
                                      Fleet@DiscardMortality, Stock)
  
  Fleet@Retention <- SS2Retention(st, fl, replist, mainyrs, Fleet@Selectivity, Stock)
  
  Fleet@FishingMortality@RetainAtAge <- ArrayMultiply(Fleet@Retention@MeanAtAge, Fleet@FishingMortality@DeadAtAge)
  
  Fleet@Effort <- SS2Effort(st, fl, replist, mainyrs, 
                            FishingMortality=Fleet@FishingMortality, 
                            DiscardMortality=Fleet@DiscardMortality, 
                            Stock=Stock)
  
  Fleet@WeightFleet <- SS2WeightFleet(st, fl, replist, mainyrs)
  Fleet
}

SS2DiscardMortality <- function(st, fl, replist, mainyrs, Stock) {
  DiscardMortality <- DiscardMortality()
  
  DiscardAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                         replist$sizeselex$Sex == st & 
                                         replist$sizeselex$Factor == "Mort", ] |>
    dplyr::filter(Yr %in% mainyrs)
  DiscardYears <- DiscardAtLength$Yr
  LengthClasses <- suppressWarnings(as.numeric(colnames(DiscardAtLength)))
  LengthClasses <- LengthClasses[!is.na(LengthClasses)]
  DiscardAtLength <- DiscardAtLength[,as.character(LengthClasses)] 
  DiscardAtLength <- t(DiscardAtLength)
  
  dimnames(DiscardAtLength) <- list(Class=LengthClasses,
                                    TimeStep=DiscardYears)
  
  DiscardAtLength <- DiscardAtLength |> AddDimension('Sim') |>
    aperm(c('Sim', 'Class', 'TimeStep'))
  
  AgeClasses <- Stock@Ages@Classes
  ALK <- GetSSALK(st, replist, AgeClasses, LengthClasses) |>
    AddDimension('Sim') |>
    AddDimension('TimeStep', min(mainyrs)) |>
    aperm(c('Sim', 'Age', 'Class', 'TimeStep'))
  
  
  Stock@Length@ASK <- ALK

  DiscardMortality@MeanAtLength <- DiscardAtLength
  DiscardMortality@Classes <- LengthClasses
  
  temp <- MeanAtLength2MeanAtAge(DiscardMortality, 
                                 Length=Stock@Length,
                                 Ages=Stock@Ages, 
                                 nsim=Stock@nSim,
                                 TimeSteps = mainyrs,
                                 max1=FALSE)
  
  DiscardMortality@MeanAtAge <- temp@MeanAtAge
  DiscardMortality
}


GetSSALK <- function(st, replist, AgeClasses, LengthClasses) {
  ALK_dim_match <- paste0("Seas: 1 Sub_Seas: 2 Morph: ", st) %in% dimnames(replist$ALK)[[3]] |> any()
  if(ALK_dim_match) {
    ALK <- replist$ALK[, , paste0("Seas: 1 Sub_Seas: 2 Morph: ", st)]
  } else {
    ALK <- replist$ALK[, , paste0("Seas: 1 Morph: ", (st - 1) * replist$nseasons + 1)]
  }
  if (all(!is.na(replist$lbinspop))) {
    ALK <- ALK[match(replist$lbinspop, dimnames(ALK)$Length), match(AgeClasses, dimnames(ALK)$TrueAge)]
  } else {
    lbinspop <- sort(as.numeric(dimnames(ALK)$Length))
    ALK <- ALK[match(lbinspop, dimnames(ALK)$Length), match(AgeClasses, dimnames(ALK)$TrueAge)]
  }
  ALK <- t(ALK)
  
  dimnames(ALK) <- list(Age=AgeClasses,
                        Class=LengthClasses)
  
  maxALK <- apply(ALK, 1, sum)
  maxALK <- matrix(maxALK, nrow(ALK), ncol(ALK)) 
  ALK <- ALK/maxALK
  
  ALK
  
}

SS2FishingMortality <- function(st, fl, replist, mainyrs) {
  
  # Fishing Mortality
  FatAge <- replist$fatage |> dplyr::filter(Sex==st, Fleet==fl, Yr %in% mainyrs)
  AgeClasses <- suppressWarnings(as.numeric(colnames(FatAge)))
  AgeClasses <- AgeClasses[!is.na(AgeClasses)]
  n_age <- length(AgeClasses)

  FatAge <- t(FatAge[,as.character(AgeClasses)])
  dimnames(FatAge) <- list(Age=AgeClasses,
                           TimeStep=mainyrs)
  
  FatAge <- FatAge |> AddDimension('Sim') |> aperm(c('Sim', 'Age', 'TimeStep'))

  FishingMortality <- FishingMortality(ApicalF=apply(FatAge, c('Sim', 'TimeStep'), max),
                                       DeadAtAge=FatAge)

  FishingMortality
}

SS2Selectivity <- function(st, fl, replist, mainyrs, FishingMortality, DiscardMortality, Stock) {
  Selectivity <- Selectivity()
  
  SelectAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                        replist$sizeselex$Sex == st & 
                                        replist$sizeselex$Factor == "Dead", ] |>
    dplyr::filter(Yr %in% mainyrs)
  SelectYears <- SelectAtLength$Yr
  LengthClasses <- suppressWarnings(as.numeric(colnames(SelectAtLength)))
  LengthClasses <- LengthClasses[!is.na(LengthClasses)]
  SelectAtLength <- SelectAtLength[,as.character(LengthClasses)] 
  SelectAtLength <- t(SelectAtLength)
  
  dimnames(SelectAtLength) <- list(Class=LengthClasses,
                                   TimeStep=SelectYears)
  
  SelectAtLength <- SelectAtLength |> AddDimension('Sim') |>
    aperm(c('Sim', 'Class', 'TimeStep'))
  
  Selectivity@Pars <- list()
  Selectivity@MeanAtLength <- SelectAtLength
  Selectivity@Classes <- LengthClasses
  
  
  AgeClasses <- Stock@Ages@Classes
  ALK <- GetSSALK(st, replist, AgeClasses, LengthClasses) |>
    AddDimension('Sim') |>
    AddDimension('TimeStep', min(mainyrs)) |>
    aperm(c('Sim', 'Age', 'Class', 'TimeStep'))
  
  Stock@Length@ASK <- ALK
  Selectivity@MeanAtAge <- FishingMortality2Selectivity(FishingMortality,
                                                        DiscardMortality,
                                                        Ages=Stock@Ages@Classes,
                                                        TimeSteps=mainyrs,
                                                        Length=Stock@Length)
  
  # should give the same as above
  # temp <- MeanAtLength2MeanAtAge(Selectivity, Stock@Length, Stock@Ages@Classes,
  #                            1, mainyrs)
  # Selectivity@MeanAtAge <- temp@MeanAtAge
  
  Selectivity
}


SS2Retention <- function(st, fl, replist, mainyrs, Selectivity, Stock) {
  Retention <- Retention()
  Retention@Pars <- list()
  
  RetainAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                        replist$sizeselex$Sex == st & 
                                        replist$sizeselex$Factor == "Keep", ] |>
    dplyr::filter(Yr %in% mainyrs)
  
  RetainYears <- RetainAtLength$Yr
  LengthClasses <- suppressWarnings(as.numeric(colnames(RetainAtLength)))
  LengthClasses <- LengthClasses[!is.na(LengthClasses)]
  RetainAtLength <- RetainAtLength[,as.character(LengthClasses)] 
  RetainAtLength <- t(RetainAtLength)
  
  dimnames(RetainAtLength) <- list(Class=LengthClasses,
                                   TimeStep=RetainYears)
  
  RetainAtLength <- RetainAtLength |> AddDimension('Sim') |>
    aperm(c('Sim', 'Class', 'TimeStep'))
  
  RetainAtLength <- ArrayDivide(round(RetainAtLength,3),
                                round(Selectivity@MeanAtLength,3)
  )
  

  RetainAtLength[!is.finite(RetainAtLength)] <- 0
  Retention@MeanAtLength <- RetainAtLength
  Retention@Classes <- LengthClasses

  AgeClasses <- Stock@Ages@Classes
  ALK <- GetSSALK(st, replist, AgeClasses, LengthClasses) |>
    AddDimension('Sim') |>
    AddDimension('TimeStep', min(mainyrs)) |>
    aperm(c('Sim', 'Age', 'Class', 'TimeStep'))

  Stock@Length@ASK <- ALK

  temp <- MeanAtLength2MeanAtAge(object=Retention, 
                                 Length=Stock@Length, 
                                 Ages=Stock@Ages, 
                                 nsim=1, 
                                 TimeSteps=mainyrs,
                                 max1=FALSE)
  
  Retention@MeanAtAge <- temp@MeanAtAge
  Retention
}

SS2Effort <- function(st, fl, replist, mainyrs, FishingMortality, DiscardMortality, Stock) {
  Effort <- Effort()
  
  FInteract <- CalculateFInteract(FishingMortality,
                                  DiscardMortality,
                                  Ages=Stock@Ages,
                                  TimeSteps=mainyrs,
                                  Length=Stock@Length)
  
  FInteractApical <- apply(FInteract, c('Sim', 'TimeStep'), max)
  FInteractApicalTerminal <- FInteractApical
  FInteractApicalTerminal[] <- 0
  
  for (i in 1:nrow(FInteractApical)) {
    ind <- which(FInteractApical[i,]>0) |> max()
    FInteractApicalTerminal[] <- FInteractApical[i,ind]
  }

  RelEffort <- ArrayDivide(FInteractApical,FInteractApicalTerminal)
  Effort@Effort <- RelEffort
  Effort@Catchability <- FInteractApicalTerminal
  Effort
}

SS2WeightFleet <- function(st, fl, replist, mainyrs) {
  
  if (inherits(replist$wtatage, 'logical'))
    return(NULL)
  
  if (!is.null(replist$wtatage$Yr)) {
    wt_at_age_c_df <- replist$wtatage |>
      dplyr::filter(abs(Yr) %in% mainyrs, Sex==st, Fleet==fl)
  } else {
    if (!is.null(replist$wtatage$sex)) {
      wt_at_age_c_df <- replist$wtatage |>
        dplyr::filter(abs(year) %in% mainyrs, sex==st, fleet==fl)
    } else {
      wt_at_age_c_df <- replist$wtatage |>
        dplyr::filter(abs(year) %in% mainyrs, Sex==st, Fleet==fl)
    }
    wt_at_age_c_df <- wt_at_age_c_df |> dplyr::rename(Yr=year)
  }  
  
  AgeClasses <- suppressWarnings(as.numeric(colnames(wt_at_age_c_df)))
  AgeClasses <- AgeClasses[!is.na(AgeClasses)]
  n_age <- length(AgeClasses)
  
  Weight_at_Age_array <- wt_at_age_c_df[,as.character(AgeClasses)] |> t()
  
  
  dimnames(Weight_at_Age_array) <- list(Age=AgeClasses,
                                        TimeStep=mainyrs)
  
  Weight_at_Age_array <- Weight_at_Age_array |> AddDimension('Sim') |>
    aperm(c('Sim', 'Age', 'TimeStep'))
  
  
  Weight_at_Age_array
  
}


