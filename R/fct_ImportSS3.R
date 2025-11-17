# TODO 
# - arguments to set Length and Weight units 


GetSSYears <- function(replist, pYear=30) {
  TSperYear <- ifelse(is.null(replist$nseasons), 1, replist$nseasons)
  FirstHistYear <- replist$startyr
  LastHistYear <- replist$endyr
  HistYears <- FirstHistYear:LastHistYear
  nYear <- length(HistYears)
  
  YearsHist <- CalcYears(nYear, pYear, LastHistYear, TSperYear, 'Historical')
  YearsProj <-  CalcYears(nYear, pYear, LastHistYear, TSperYear, 'Projection')

  list(nYear=nYear,
       pYear=pYear,
       CurrentYear=LastHistYear, 
       YearsHist=YearsHist, 
       YearsProj=YearsProj,
       TimeUnits=CalcTSUnits(TSperYear),
       TSperYear=TSperYear
  )
}

ProcessSS_StockName <- function(StockName, nStock) {
  if (is.null(StockName)) {
    if (nStock==1) {
      StockName <- 'Combined Sex'
    } else if (nStock==2) {
      StockName <- c('Female', 'Male')
    } else {
      cli::cli_abort('`nStock` should be {.val {1} or {2}}')
    }
  }
  
  if (length(StockName)!=nStock)
    cli::cli_abort('`StockName` ({.val {StockName}}) should be length `nStock` ({.val {nStock}})')
  StockName
}


ProcessSS_FleetNames <- function(FleetNames, SSFleetNames) {
  nFleet <- length(SSFleetNames)
  if (is.null(FleetNames)) 
    FleetNames <- SSFleetNames
  
  if (length(FleetNames)!=nFleet)
    cli::cli_abort('`FleetNames` should be length `nFleet`: {.val {nFleet}}')
  
  FleetNames
}

## Import SS  ----
#' Import an OM from SS3 Output
#' @param x Either a character string  
#' @export
ImportSS <- function(SSDir,     
                     Name = "Imported SS3 Model",
                     nSim=48,
                     pYear=30, 
                     Agency='',
                     Author='',
                     Email='',
                     Region='',
                     Latitude=numeric(),
                     Longitude=numeric(),
                     Sponsor='',
                     StockName=NULL,
                     CommonName=NULL,
                     Species=NULL,
                     FleetNames=NULL,
                     Interval=1,
                     DataLag=0,
                     silent=FALSE,
                     populate=TRUE,
                     ...) {
  OnExit()
  RepList <- ImportSSReport(SSDir, silent, ...)
  nStock <- RepList[[1]]$nsexes
  nFleet <- RepList[[1]]$nfishfleets
  
  DotsList <- list(...)
  if (!is.null(DotsList$nsim))
    nSim <- DotsList$nsim
  
  if(!silent) 
    cli::cli_alert('{.val {nStock}-sex} and {.val {nFleet}-fleet} model detected.')
  
  if (length(RepList)>1) 
    nSim <- length(RepList)
  
  OM <- OM(Name=Name, 
           Agency=Agency, 
           Author=Author, 
           Email=Email, 
           Region=Region, 
           Latitude=Latitude,
           Longitude=Longitude,
           Sponsor=Sponsor, 
           Interval=Interval,
           DataLag=DataLag,
           nSim=nSim)
  
  YearsList <- GetSSYears(RepList[[1]], pYear)
  OM@nYear <- YearsList$nYear
  OM@pYear <- YearsList$pYear
  OM@CurrentYear <- YearsList$CurrentYear
  OM@Years <- c(YearsList$YearsHist,
                    YearsList$YearsProj)
  OM@TSperYear <- YearsList$TSperYear
  
  # Stock
  StockName <- ProcessSS_StockName(StockName, nStock)
  OM@Stock <- purrr::map(seq_along(StockName), \(st) {
    stock <- SS2Stock(st, RepList, YearsList, nSim)
    stock@Name <- StockName[st]
    stock@CommonName <- CommonName[st]
    stock@Species <- Species[st]
    stock
  })
  names(OM@Stock) <- StockName

  # Fleet
  OM@Fleet <- MakeNamedList(StockName, list())
  FleetNames <- ProcessSS_FleetNames(FleetNames, 
                                     SSFleetNames=unique(RepList[[1]]$catch$Fleet_Name))
  for (st in seq_along(OM@Fleet)) {
    OM@Fleet[[st]] <- MakeNamedList(FleetNames, new('fleet'))
    for (fl in seq_along(FleetNames)) {
      OM@Fleet[[st]][[fl]] <- SS2Fleet(st, fl, RepList, YearsList, FleetNames, Stock=OM@Stock[[st]])
    }
  }
  
  # Data
  OM@Data <- list(ImportSSData(RepList, OM@Name))
  names(OM@Data) <- paste(StockName, collapse=' ')
  
  # Obs
  SurveyNames <- OM@Data[[1]]@Survey@Name
  AllFleetNames <- c(FleetNames, SurveyNames) |> unique()
  OM@Obs <- MakeNamedList(names(OM@Data), MakeNamedList(AllFleetNames, new('obs')))
  OM <- ProcessSurveyObsSelectivity(OM, RepList)

  # OM@Imp - TODO 
  
  # Allocation
  OM <- ProcessSSAllocation(OM, RepList, StockName, FleetNames)
  OM <- ProcessEFactor(OM)
   
  # OM@Complexes
  # OM@SexPars
  # OM@Relations
  if (!populate)
    return(OM)
  PopulateOM(OM)
}

ProcessSSAllocation <- function(OM, RepList, StockName, FleetNames) {
  Allocation <- MakeNamedList(StockName)
  AgeClasses <- GetSSAgeClasses(RepList[[1]])
  YearsList <- GetSSYears(RepList[[1]], pYear=1)
  
  CatchFrac <- RepList[[1]]$catage |> DropXXCols() |>
    dplyr::filter(Yr==max(Yr)) |>
    tidyr::pivot_longer(as.character(AgeClasses)) |>
    dplyr::group_by(Sex, Fleet) |>
    dplyr::summarise(Catch=sum(value), .groups='drop') |>
    dplyr::group_by(Sex) |>
    dplyr::mutate(Catch=Catch/sum(Catch)) |>  
    dplyr::group_by(Sex) |> 
    dplyr::group_split() 
  
  nFleet <- length(FleetNames)
  
  for (st in 1:nStock(OM)) {
    Allocation[[st]] <-  array(CatchFrac[[st]]$Catch, 
                               dim=c(1, nFleet), 
                               dimnames = list(Sim=1,
                                               Fleet=FleetNames))
  } 
  OM@Allocation <- Allocation
  OM
}

## --- Stock ----

SS2Stock <- function(st, RepList, YearsList, nSim) {
  
  if(!is.null(RepList[[1]]$movement) && nrow(RepList[[1]]$movement) > 0) 
    cli::cli_alert_warning("Movement detected in SS model but not imported right now.")
  
  Stock <- Stock(Name=ifelse(st == 1, "Female", "Male")) 
  Stock@Ages <- SS2Ages(st, RepList, YearsList)
  Stock@Length <- SS2Length(st, RepList, YearsList, Ages=Stock@Ages)
  Stock@Weight <- SS2Weight(st, RepList, YearsList, Ages=Stock@Ages)
  Stock@NaturalMortality <- SS2NaturalMortality(st, RepList, YearsList, Ages=Stock@Ages)
  Stock@Maturity <- SS2Maturity(st, RepList, YearsList, Ages=Stock@Ages)
  Stock@Fecundity <- SS2Fecundity(st, RepList, YearsList, Ages=Stock@Ages)
  # Stock@Depletion <- SS2Depletion(st, RepList, YearsList) # not needed - already accounted for in early rec devs
  Stock@SRR <- SS2SRR(st, RepList, YearsList, Ages=Stock@Ages, nSim)
  Stock@nYear <- YearsList$nYear
  Stock@pYear <- YearsList$pYear
  Stock@TSperYear <- YearsList$TSperYear
  Stock@nSim <- nSim
  Stock
}

SS2Ages <- function(st, RepList, YearsList) {
  Ages(MaxAge=CalcSSMaxAgeClass(RepList[[1]], YearsList),
       MinAge=CalcSSMinAgeClass(RepList[[1]], YearsList),
       Units='quarter')
}

GetSSAgeClasses <- function(replist) {
  AgeClasses <- suppressWarnings(as.numeric(colnames(replist$natage)))
  AgeClasses[!is.na(AgeClasses)]
}

GetSSLengthClasses <- function(replist) {
  LengthClasses <- suppressWarnings(as.numeric(colnames(replist$sizeselex)))
  LengthClasses[!is.na(LengthClasses)]
}

CalcSSMaxAgeClass <- function(replist, YearsList) {
  AgeClasses <- GetSSAgeClasses(replist)
  TimeUnits <- YearsList$TimeUnits
  maxAgeYear <- max(AgeClasses)
  validTimeUnits <- c('year', 'half-year', 'quarter', 'month', 'week', 'day')
  if (TimeUnits=='year') {
    return(maxAgeYear)
  } else if (TimeUnits=='half-year') {
    return(maxAgeYear+1/2)
  } else if (TimeUnits=='quarter') {
    return((maxAgeYear+3/4)*4)
  } else if (TimeUnits=='month') {
    return((maxAgeYear+11/12))
  } else if (TimeUnits=='week') {
    return((maxAgeYear+51/52))
  } else if (TimeUnits=='day') {
    return((maxAgeYear+364/365))
  } else {
    cli::cli_abort('`TimeUnits` must be one of: {.val {validTimeUnits}}')
  }
}

CalcSSMinAgeClass <- function(replist, YearsList) {
    AgeClasses <- GetSSAgeClasses(replist)
    TimeUnits <- YearsList$TimeUnits
    if (TimeUnits=='year') 
      return(min(AgeClasses))
      
    birthseas <- GetSSBirthSeas(replist)
    if (birthseas==1) {
      return(min(AgeClasses))
    } 

    MaxAge <- CalcSSMaxAgeClass(replist, YearsList)
    Ages <- seq( min(AgeClasses), by=1/YearsList$TSperYear, to=MaxAge)
    Ages[birthseas]*YearsList$TSperYear
  }

GetSS_Length_at_Age <- function(st, replist, YearsList) {
  YearsHist <- YearsList$YearsHist
  dplyr::filter(replist$endgrowth, Sex == st) |> 
    dplyr::select(Age=Age_Beg, Value=Len_Beg) |>
    dplyr::mutate(Year=YearsHist[1]) |> 
    dplyr::arrange(Age, Year) |>
    dplyr::select(Age, Year, Value) |>
    DF2Array()
}

GetSS_LengthCV_at_Age <-function(st, replist, YearsList) {
  YearsHist <- YearsList$YearsHist
  dplyr::filter(replist$endgrowth, Sex == st) |> 
    dplyr::select(Age=Age_Beg, SD_Beg, Len_Beg) |>
    dplyr::mutate(Year=YearsHist[1], Value=SD_Beg/Len_Beg) |> 
    dplyr::arrange(Age, Year) |>
    dplyr::select(Age, Year, Value) |>
    DF2Array()
}

SS2Length <- function(st, RepList, YearsList, Ages) {
  Length <- Length(Pars=list())
  Length@MeanAtAge <- purrr::map(RepList, \(replist) {
    GetSS_Length_at_Age(st, replist, YearsList)
  }) |> List2Array('Sim', pos=1) |>
    ArraySubsetAge(Ages=Ages@Classes)
  
  Length@CVatAge <- purrr::map(RepList, \(replist) {
    GetSS_LengthCV_at_Age(st, replist, YearsList)
  }) |> List2Array('Sim', pos=1) |>
    ArraySubsetAge(Ages=Ages@Classes)
  
  if (Ages@Classes |> length() != dim(Length@MeanAtAge)[2]) 
    cli::cli_abort(c("x"='Number of age-classes for `Length@MeanAtAge` does not match `Ages@Classes`',
                     "i"="`Length@MeanAtAge` has {.val {dim(Length@MeanAtAge)[2]}} age classes",
                     "i"="`Ages@Classes` has {.val {length(Ages@Classes)}} age classes"))
  # TODO - add parameters and models
  # https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#Growth
  Length
}

GetSS_LengthWeightPars <- function(st, replist) {
  c(Alpha=replist$Growth_Parameters[st, ]$WtLen1,
    Beta=replist$Growth_Parameters[st, ]$WtLen2)
  
}

GetSS_WeightAtAge <- function(st, replist, YearsList) {
  
  if(!is.null(replist$mean_body_wt)) {
    cli::cli_abort('`replist$mean_body_wt` not currently supported.', .internal=TRUE)
    # TODO - update
    # Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == st, ]
    # Wt_age_df <- Wt_age_df[findInterval(mainyrs, Wt_age_df$Yr), ]
    # Wt_age <- do.call(rbind, lapply(0:n_age, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
    # if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1]

  }
  YearsHist <- YearsList$YearsHist
  dplyr::filter(replist$endgrowth, Sex == st) |> 
    dplyr::select(Age=Age_Beg, Value=Wt_Beg) |>
    dplyr::mutate(Year=YearsHist[1]) |> 
    dplyr::arrange(Age, Year) |>
    dplyr::select(Age, Year, Value) |>
    DF2Array()
}

SS2Weight <- function(st, RepList, YearsList, Ages) {
  Weight <- Weight()
  
  LengthWeightPars <- purrr::map(RepList, \(replist)
                                 GetSS_LengthWeightPars(st, replist))
  
  alpha <- purrr::map(LengthWeightPars, \(LW) LW[1]) |> 
    unlist() |> unique()
  
  beta <- purrr::map(LengthWeightPars, \(LW) LW[2]) |> 
    unlist() |> unique()
  
  Weight@Pars <- list(alpha=alpha,
                      beta=beta)
  Weight@Model <- FindModel(Weight)
  
  Weight@MeanAtAge <- purrr::map(RepList, \(replist) {
    GetSS_WeightAtAge(st, replist, YearsList)
  }) |> List2Array('Sim', pos=1) |>
    ArraySubsetAge(Ages=Ages@Classes)
  
  if (Ages@Classes |> length() != dim(Weight@MeanAtAge)[2]) 
    cli::cli_abort(c("x"='Number of age-classes for `Weight@MeanAtAge` does not match `Ages@Classes`',
                     "i"="`Weight@MeanAtAge` has {.val {dim(Weight@MeanAtAge)[2]}} age classes",
                     "i"="`Ages@Classes` has {.val {length(Ages@Classes)}} age classes"))
  
  Weight
}

GetSS_M_at_age <- function(st, replist, YearsList, Ages) {
  AgeClasses <- GetSSAgeClasses(replist) 
  
  M_at_ageDF <- replist$M_at_age |> dplyr::filter(Sex==st)
  if (is.null(M_at_ageDF$Year)) 
    M_at_ageDF$Year <- M_at_ageDF$Yr
  Years <- unique(M_at_ageDF$Year)
  
  M_at_age <- M_at_ageDF |> dplyr::filter(Sex==st) |>
    dplyr::select(dplyr::all_of(as.character(AgeClasses))) |> t()
  dimnames(M_at_age) <- list(Age=AgeClasses,
                             Year=Years)
  
  if (all(is.na(M_at_age[nrow(M_at_age), ]))) {
    endgrowth <- replist$endgrowth |> dplyr::filter(Sex==st)
    M_at_age[nrow(M_at_age), ] <- endgrowth$M[length(AgeClasses)]
  }
  
  if (YearsList$TimeUnits == 'year') {
    return(M_at_age)
  }
  
  # Convert to seasonal M 
  FillValues <- function(Value) {
    for (i in seq_along(Value)[-1]) {
      if (is.na(Value[i]))
        Value[i] <- Value[i-1]
    }
    Value
  }
  
  if (YearsList$TimeUnits == 'quarter') {
    M_at_ageDF <- Array2DF(M_at_age) 
    M_at_ageDF$AgeAnnual <- M_at_ageDF$Age
    
    AgeClassesFull <- seq(0, to=max(Ages@Classes), by=1/4)
    tempDF <- data.frame(Age=AgeClassesFull, 
                         Year=rep(unique(M_at_ageDF$Year), each=length(AgeClassesFull)))

    M_at_ageDF$Value <- M_at_ageDF$Value/YearsList$TSperYear
    M_at_ageDF_seasonal <- dplyr::left_join(tempDF, M_at_ageDF,
                                            by = dplyr::join_by(Age, Year)) |>
      dplyr::select(-AgeAnnual) |>
      dplyr::group_by(Year) |> 
      dplyr::mutate(Value=FillValues(Value)) |>
      dplyr::ungroup()
    return(DF2Array(M_at_ageDF_seasonal))
  } else {
    cli::cli_abort("Currently only 'year' and 'quarter' are supported for `TimeUnits`", .internal=TRUE)
  }

}

SS2NaturalMortality <- function(st, RepList, YearsList, Ages) {
  NaturalMortality <- NaturalMortality(Pars=list())
  NaturalMortality@MeanAtAge <- purrr::map(RepList, \(replist) {
    GetSS_M_at_age(st, replist, YearsList, Ages)
  }) |> List2Array('Sim', pos=1) |>
    ArraySubsetAge(Ages=Ages@Classes)
  NaturalMortality
}

GetSS_Maturity_at_Age <- function(st, replist, YearsList, Ages) {
  endgrowth <- replist$endgrowth |> dplyr::filter(Sex==st)
  if(any(endgrowth$Age_Mat < 0)) endgrowth$Age_Mat <- abs(endgrowth$Age_Mat) # Should all be 1's
  if(any(endgrowth$Len_Mat < 0)) endgrowth$Len_Mat <- abs(endgrowth$Len_Mat)
  Mat_age <- endgrowth$Len_Mat * endgrowth$Age_Mat
  array(Mat_age, dim=c(length(Ages@Classes), 1),
        dimnames = list(Age=Ages@Classes,
                        Year=YearsList$YearsHist[1]))
}

SS2Maturity <- function(st, RepList, YearsList, Ages) {
  Maturity <- Maturity(Pars=list())
  Maturity@MeanAtAge <- purrr::map(RepList, \(replist) {
    GetSS_Maturity_at_Age(st, replist, YearsList, Ages)
  }) |> List2Array('Sim', pos=1) |>
    ArraySubsetAge(Ages=Ages@Classes)
  Maturity
}

GetSS_Fecundity <- function(st, replist, YearsList, Ages) {
  endgrowth <- replist$endgrowth |> dplyr::filter(Sex==st)
  seas <- unique(endgrowth$Seas)
  
  if (length(seas)>1)
    return(
      GetSS_Fecundity_seasonal(st, replist, YearsList, Ages)
    )
  
  if (!is.null(replist$endgrowth[["Mat*Fecund"]])) {
    fec_age <- replist$endgrowth |>
      dplyr::filter(Morph == 1, Sex==st) |>
      dplyr::select(Age=Age_Beg, Fecundity=`Mat*Fecund`)
    
  } else {
    if (!is.null(replist$endgrowth$Mat_F_wtatage)) {
      fec_age <- replist$endgrowth |>
        dplyr::filter(Morph == 1, Sex==st) |>
        dplyr::select(Age_Beg, Fecundity=Mat_F_wtatage)
    }
  }

  array(fec_age$Fecundity, dim=c(length(Ages@Classes),1),
        dimnames = list(
          Age=Ages@Classes,
          Year=YearsList$YearsHist[1]
        )
  )
    
}
 
GetSS_Fecundity_seasonal <- function(st, replist, YearsList, Ages) {
  endgrowth <- replist$endgrowth |> dplyr::filter(Sex==st)
  if (!is.null(replist$endgrowth[["Mat*Fecund"]])) {
    fec_age <- replist$endgrowth |>
      dplyr::filter(Morph == 1, Sex==st) |>
      dplyr::select(Age=Age_Beg, Seas, Fecundity=`Mat*Fecund`) |>
      dplyr::arrange(Age)
  
  } else {
    if (!is.null(replist$endgrowth$Mat_F_wtatage)) {
      fec_age <- replist$endgrowth |>
        dplyr::filter(Morph == 1, Sex==st) |>
        dplyr::select(Age_Beg, Seas, Fecundity=Mat_F_wtatage) |>
        dplyr::arrange(Age)
    }
  }
  
  array(fec_age$Fecundity, dim=c(length(Ages@Classes),1),
        dimnames = list(
          Age=Ages@Classes,
          Year=YearsList$YearsHist[1]
        )
  )
  
}
  
SS2Fecundity <- function(st, RepList, YearsList, Ages) {
  # TODO import model and parameters from SS output 
  # replist$FecPar1
  # replist$FecPar1name
  # replist$FecPar2
  # replist$FecType
  # replist$Fecundity_option
  # can do the same with maturity
  
  Fecundity <- Fecundity(Pars=list())
  Fecundity@MeanAtAge <- purrr::map(RepList, \(replist) {
    GetSS_Fecundity(st, replist, YearsList, Ages)
  }) |> List2Array('Sim', pos=1) |>
    ArraySubsetAge(Ages=Ages@Classes)
  Fecundity
}

SS2Depletion <- function(st, RepList, YearsList) {
  
  Depletion <- Depletion(Reference='SB0')

  SB0 <- purrr::map(RepList, \(replist) {
    replist$timeseries |>
      dplyr::filter(Era == "VIRG") |>
      getElement("SpawnBio") |>
      sum(na.rm = TRUE)
  }) |> unlist()
  
  SB1 <- purrr::map(RepList, \(replist) {
    replist$timeseries |>
      dplyr::filter(Yr == YearsList$YearsHist[1]) |>
      getElement("SpawnBio") |>
      sum(na.rm = TRUE)
  }) |> unlist()
  
  if (!all(round(SB1/SB0,2)==1)) {
    Depletion@Initial <- SB1/SB0
  }
  
  # Don't populate Depletion Final or else it will optimize for q
  # if (st==1) {
  #   SBCurr <- purrr::map(RepList, \(replist) {
  #     replist$timeseries |>
  #       dplyr::filter(Yr == max(mainyrs)) |>
  #       getElement("SpawnBio") |>
  #       sum(na.rm = TRUE)
  #   }) |> unlist()
  #   
  #   Depletion@Final <- SBCurr/SB0
  # }
  
  Depletion
}

GetSS_SRRPars <- function(replist) {
  # SRR Model and Parameters
  if(replist$SRRtype == 3 || replist$SRRtype == 6) { # Beverton-Holt SR
    par <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]
    h_out <- par$Value
    h_out[h_out < 0.2] <- 0.2
    h_out[h_out > 0.999] <- 0.999
    return(list(h=h_out))
  } else if(replist$SRRtype == 2) {
    SRR@Model <- 'Ricker'
    par <- replist$parameters[grepl("SR_Ricker", rownames(replist$parameters)), ]
    h_out <- par$Value
    h_out[h_out < 0.2] <- 0.2
    return(list(hR=h_out))
  } else if(replist$SRRtype == 7) {
    s_frac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Sfrac"]
    Beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]
    s0 <- 1/SpR0
    z0 <- -log(s0)
    z_min <- z0 * (1 - s_frac)
    h_out <- 0.2 * exp(z0 * s_frac * (1 - 0.2 ^ Beta))
    h_out[h_out < 0.2] <- 0.2
    h_out[h_out > 0.999] <- 0.999
    return(list(h=h_out))
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
    return(list(h=h_out))
  }
}

GetSS_RecDevs_Early <- function(replist, YearsList, Ages, st) {
  
  SSAgeClasses <- GetSSAgeClasses(replist)
  YearsHist <- YearsList$YearsHist
  Virg <- replist$natage |> 
    dplyr::filter(Era=='VIRG', `Beg/Mid`=='B', Seas==1, Sex==st) |>
    dplyr::select(as.character(SSAgeClasses), Sex) |>
    tidyr::pivot_longer(as.character(SSAgeClasses),
                        names_to = 'Age', values_to='N0')
  
  Init <- replist$natage |> 
    dplyr::filter(Yr==min(YearsHist), `Beg/Mid`=='B', Seas==1, Sex==st) |>
    dplyr::select(as.character(SSAgeClasses), Sex) |>
    tidyr::pivot_longer(as.character(SSAgeClasses),
                        names_to = 'Age', values_to='N1')
  
  Deviations <- dplyr::left_join(Virg, Init, by = dplyr::join_by(Sex, Age)) |>
    dplyr::mutate(Deviation=N1/N0) |>
    dplyr::mutate(Value=ifelse(is.finite(Deviation), Deviation, 1),
                  Age=as.numeric(Age)) |>
    dplyr::select(Age, Value) |>
    dplyr::arrange(Age)
  
  FullAgeClasses <- seq(0, by=1/CalcTSperYear(Ages@Units), to=max(Ages@Classes))
  
  dev <- array(rep(Deviations$Value, each=replist$nseasons), 
               dim=length(FullAgeClasses), 
               dimnames=list(Age=FullAgeClasses)) |>
    ArraySubsetAge(Ages@Classes[-1])
  
  dev
}


GetSS_RecDevs <- function(replist, YearsList, Ages) {

  YearsHist <- YearsList$YearsHist
  recruit <- replist$recruit
  
  Rec_main <- recruit[recruit$Yr %in% YearsHist, ]
  dev <- Rec_main$pred_recr/Rec_main$exp_recr
  dev <- array(dev, dim=length(dev), 
               dimnames=list(Year=YearsHist[match(Rec_main$Yr, YearsHist)]))
  
  TSperYear <- YearsList$TSperYear
  if (TSperYear==1) 
    return(dev)
  
  # expand for seasons
  dev <- rep(dev, each=TSperYear)
  array(dev, dim=length(dev), 
        dimnames=list(Year=YearsHist))

  
}

GetSSBirthSeas <- function(replist) {
  # NPSWO - birthseas = 2:3 but recruitment is actually only season 3
  # not sure if this generalizes
  ifelse(is.null(replist$birthseas),
         1, 
         max(replist$birthseas)) 
}

GetSS_R0 <- function(st, replist, YearsList) {
  AgeClasses <- GetSSAgeClasses(replist)
  
  birthseas <- GetSSBirthSeas(replist)
                      
  R0 <- dplyr::filter(replist$natage, Sex == st, `Beg/Mid` == "B", Era == "VIRG",
                      Seas==birthseas) |>
    tidyr::pivot_longer(as.character(AgeClasses), names_to = 'Age', values_to = 'Number') |>
    dplyr::mutate(Age=as.numeric(Age)) |>
    dplyr::filter(Age==min(Age)) |>
    dplyr::pull(Number)
  
  nSeas <- replist$natage$Seas |> unique() |> length()
  if (length(R0)!=nSeas) {
    R0Vec <- rep(0, nSeas)
    R0Vec[birthseas] <- R0
    R0 <- R0Vec
  }
  
  
  YearsAll <- c(YearsList$YearsHist, YearsList$YearsProj)
  R0 <- rep(R0, length(YearsAll))
  R0 <- R0[seq_along(YearsAll)]

  array(R0, length(R0), dimnames = list(Year=YearsAll))
}

SS2SRR <- function(st, RepList, YearsList, Ages, nSim) {
  
  SD <- purrr::map(RepList, \(replist) replist$sigma_R_in) |> unlist() |> as.numeric()
  R0 <- purrr::map(RepList, \(replist) GetSS_R0(st, replist, YearsList)) |>
    List2Array('Sim', pos=1)
  
  SpawnTimeFrac <- ifelse(is.na(RepList[[1]]$Spawn_timing_in_season), 
                          0, 
                          RepList[[1]]$Spawn_timing_in_season)
  
  SRR <- SRR(SD=SD, R0=R0, SpawnTimeFrac=SpawnTimeFrac)
  
  Pars <- purrr::map(RepList, \(replist) GetSS_SRRPars(replist)) 
  Pars <- do.call('rbind', Pars)
  ParsList <- list(as.numeric(Pars))
  names(ParsList) <- colnames(Pars)
  SRR@Pars <- ParsList
  
  SRR@RecDevInit <- purrr::map(RepList, \(replist) 
                               GetSS_RecDevs_Early(replist, YearsList, Ages, st)) |>
    List2Array('Sim', pos=1)
  
  SRR@RecDevHist <- purrr::map(RepList, \(replist) 
                        GetSS_RecDevs(replist, YearsList, Ages)) |>
    List2Array('Sim', pos=1)
  
  AC <- log(SRR@RecDevHist) |>
    apply(1, acf, lag.max = 1, plot = FALSE, na.rm=TRUE) |>
    lapply(getElement,'acf') |>
    lapply(getElement,2) |> unlist()
  
  AC[!is.finite(AC)] <- 0
  SRR@AC <- AC
  
  if (st==2)
    SRR@SPFrom <- 1
  
  RecDevs <- GenerateRecruitmentDeviations(SD=SRR@SD, 
                                           AC=SRR@AC, 
                                           Ages = Ages,
                                           HistTS=YearsList$YearsHist, 
                                           ProjTS=YearsList$YearsProj,
                                           nsim=nSim,
                                           RecDevInit=SRR@RecDevInit,
                                           RecDevHist=SRR@RecDevHist)
  
  SRR@RecDevProj <- RecDevs$RecDevProj
  ProjectionYears <- YearsList$YearsProj
  dimnames(SRR@RecDevProj) <- list(Sim=1:nSim,
                                   Year=ProjectionYears)
  SRR
}




## Fleet ----

SS2Fleet <- function(st, fl, RepList, YearsList, FleetNames, Stock) {
  Fleet <- Fleet(FleetNames[fl])
  Fleet@Effort <- SS2Effort(st, fl, RepList, YearsList)
  Fleet@Catchability <- SS2Catchability(st, fl, RepList, YearsList)
  Fleet@DiscardMortality <- SS2DiscardMortality(st, fl, RepList, YearsList, Stock)
  Fleet@Selectivity <- SS2Selectivity(st, fl, RepList, YearsList, Stock)
  Fleet@Retention <- SS2Retention(st, fl, RepList, YearsList, 
                                  Selectivity=Fleet@Selectivity, Stock)
  Fleet@WeightFleet <- SS2WeightFleet(st, fl, RepList, YearsList)
  Fleet
}


GetSS_Effort <- function(st, fl, replist, YearsList, type=c('Effort', 'q')) {
  type <- match.arg(type)

  # Fishing Effort is proportional to FInteract - ie the fishing mortality on
  # all fish that interact with the gear, including those that are discarded alive
  # ie don't suffer discard mortality
  
  AgeClasses <- GetSSAgeClasses(replist)
  
  FInteract <- replist$fatage |> 
    dplyr::filter(Sex==st, Fleet==fl, Yr %in% YearsList$YearsHist) 
    
  nMorph <- FInteract$Morph |> unique() |> length()
  
  if (nMorph>1)
    cli::cli_abort("More than 1 Morph not currently supported", .internal=TRUE)

  FInteract <- t(FInteract[,as.character(AgeClasses)])
  dimnames(FInteract) <- list(Age=AgeClasses,
                              Year=YearsList$YearsHist)
  
  
  FInteractApical <- apply(FInteract, c('Year'), max)
  FInteractApicalTerminal <- FInteractApical
  FInteractApicalTerminal[] <- 0
  
  ind <- which(FInteractApical>0) |> max()
  FInteractApicalTerminal[] <- FInteractApical[ind]
  if (type=='q') {
    return(FInteractApical[ind])
  }
  
  RelEffort <- ArrayDivide(FInteractApical,FInteractApicalTerminal)
  RelEffort <- array(RelEffort, dim=c(length(RelEffort))) 
  dimnames(RelEffort) <- list(
                              Year=YearsList$YearsHist)
  
  RelEffort
}

SS2Effort <- function(st, fl, RepList, YearsList) {
  purrr::map(RepList, \(replist)
             GetSS_Effort(st, fl, replist, YearsList)) |>
    List2Array('Sim', pos=1)
}

SS2Catchability <- function(st, fl, RepList, YearsList) {
  q <- purrr::map(RepList, \(replist)
             GetSS_Effort(st, fl, replist, YearsList, type='q')) |>
    List2Array('Sim', pos=1, dimname='Year') 
  
  dimnames(q)$Year <- YearsList$YearsHist[1]
  q
}

GetSS_DiscardMortalityAtLength <- function(st, fl, replist, YearsList, Stock) {
  
  DiscardAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                         replist$sizeselex$Sex == st & 
                                         replist$sizeselex$Factor == "Mort", ] |>
    dplyr::filter(Yr %in% YearsList$YearsHist)
  DiscardYears <- DiscardAtLength$Yr
  
  LengthClasses <- suppressWarnings(as.numeric(colnames(DiscardAtLength)))
  LengthClasses <- LengthClasses[!is.na(LengthClasses)]
  DiscardAtLength <- DiscardAtLength[,as.character(LengthClasses)] 
  DiscardAtLength <- t(DiscardAtLength)
  
  dimnames(DiscardAtLength) <- list(Class=LengthClasses,
                                    Year=DiscardYears)
  DiscardAtLength
}


GetSSALK_annual <- function(st, replist, AgeClasses, LengthClasses, YearsList) {
  
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

GetSSALK_seasonal <- function(st, replist, AgeClasses, LengthClasses, YearsList) {
  nseasons <- replist$nseasons
  AgeClassesFull <- seq(0, to=max(AgeClasses), by=1/YearsList$TSperYear)
  
  ALK_Out <- array(NA, dim=c(length(AgeClassesFull),
                             length(LengthClasses)),
                   dimnames = list(
                     Age=AgeClassesFull,
                     Class=LengthClasses)
                   )
  for (seas in 1:nseasons) {
    ALK <- replist$ALK[, , paste0("Seas: ", seas, " Sub_Seas: 2 Morph: ", st)]
    lbinspop <- sort(as.numeric(dimnames(ALK)$Length))
    ALK <- ALK[match(lbinspop, dimnames(ALK)$Length), ] |> t()
    names(dimnames(ALK))[1] <- 'Age'
  
    ages <- AgeClassesFull[seq(from=seas, by=YearsList$TSperYear, length.out=nrow(ALK))]
    dimnames(ALK)$Age <- ages
  
    rowind <- match(ages, AgeClassesFull)
    ALK_Out[rowind, ] <- ALK
  }
  ArraySubsetAge(ALK_Out, AgeClasses)
}


GetSSALK <- function(st, replist, Ages, LengthClasses, YearsList) {
  
  if (YearsList$TSperYear==1) 
    return(GetSSALK_annual(st, replist, AgeClasses=Ages@Classes, LengthClasses, YearsList))
  
  GetSSALK_seasonal(st, replist, AgeClasses=Ages@Classes, LengthClasses, YearsList)
}


SS2DiscardMortality <- function(st, fl, RepList, YearsList, Stock) {
  DiscardMortality <- DiscardMortality()
  
  DiscardMortality@MeanAtLength <- purrr::map(RepList, \(replist)
                                         GetSS_DiscardMortalityAtLength (st, fl, replist, YearsList, Stock)
                                         ) |>
    List2Array('Sim', pos=1) |>
    ArrayReduceDims()
  

  AgeClasses <- Stock@Ages@Classes
  LengthClasses <- dimnames(DiscardMortality@MeanAtLength)$Class |> as.numeric()
  DiscardMortality@Classes <- LengthClasses
  
  ALK <- purrr::map(RepList, \(replist) GetSSALK(st, replist, 
                                                 Ages=Stock@Ages, 
                                                 LengthClasses, 
                                                 YearsList)) |>
    List2Array('Sim', pos=1) |>
    AddDimension('Year', YearsList$YearsHist[1])

  Stock@Length@ASK <- ALK
  
  DiscardMortality <- MeanAtLength2MeanAtAge(
    object=DiscardMortality, 
    Length=Stock@Length,
    Ages=Stock@Ages, 
    nsim=Stock@nSim,
    Years = YearsList$YearsHist,
    max1=FALSE)
  
  DiscardMortality@MeanAtAge <- ArrayReduceDims(DiscardMortality@MeanAtAge)
  DiscardMortality
}

GetSS_SelectivityAtAge <- function(st, fl, replist, YearsList, Stock) {
  Ages <- Stock@Ages
  AgeClasses <- Ages@Classes
  SSAgeClasses <- GetSSAgeClasses(replist)
  AgeSelect <- dplyr::filter(replist$ageselex, Fleet == fl, Sex %in% st, 
                         Factor == "Asel2",
                         Yr %in% YearsList$YearsHist)  |> 
    dplyr::select(as.character(SSAgeClasses)) |> t()
  
  
  if (replist$nseasons==1) {
    dimnames(AgeSelect) <- list(
      Age=Ages@Classes,
      Year=YearsList$YearsHist
    )
    
    maxSel <- apply(AgeSelect, 'Year', max) # should be 1
    maxSel <- replicate(nrow(AgeSelect), maxSel) |> t()
    return(AgeSelect/maxSel)
  } 
  
  # Seasonal 
  YearsAll <- c(YearsList$YearsHist, YearsList$YearsProj)
  
  MeanAtAge <- array(NA, dim=c(length(AgeClasses),
                               length(YearsList$YearsHist)),
                     dimnames = list(
                       Age=Ages@Classes,
                       Year=YearsList$YearsHist
                     ))
  
  AgeSelect <- dplyr::filter(replist$ageselex, Fleet == fl, Sex %in% st,
                             Factor == "Asel2",
                             Yr %in% YearsList$YearsHist) |>
    tidyr::pivot_longer(as.character(GetSSAgeClasses(replist)), names_to = 'Age', values_to = 'V') |>
    dplyr::mutate(Age=as.numeric(Age)) 
  
  for (yr in unique(AgeSelect$Yr)) {
    SelectAtAge <- AgeSelect |> dplyr::filter(Yr==yr) |> 
      dplyr::arrange(Age, Seas)
    
    SeasonalAges <- seq(min(SelectAtAge$Age), by=1/YearsList$TSperYear, to=max(SelectAtAge$Age)+(YearsList$TSperYear-1)/YearsList$TSperYear)
    SelectAtAge$Age <- SeasonalAges
    SelectAtAge <- SelectAtAge |>
      dplyr::filter(Age%in%AgeClasses)
    
    ind1 <- match(yr, YearsAll)
    ind2 <- match(yr+1, YearsAll) - 1
    MeanAtAge[,ind1:ind2] <- SelectAtAge$V
  }
  
  maxSel <- apply(MeanAtAge, 'Year', max) # should be 1
  maxSel <- replicate(nrow(MeanAtAge), maxSel) |> t()
  MeanAtAge/maxSel
}

GetSS_SelectivityAtLength <- function(st, fl, replist, YearsList, Stock) {
  SelectAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl &
                                        replist$sizeselex$Sex == st &
                                        replist$sizeselex$Factor == "Lsel", ] |>
    dplyr::filter(Yr %in% YearsList$YearsHist)

  SelectYears <- SelectAtLength$Yr
  LengthClasses <- GetSSLengthClasses(replist)
  SelectAtLength <- SelectAtLength[,as.character(LengthClasses)] |> t()
  dimnames(SelectAtLength) <- list(Class=LengthClasses,
                                   Year=SelectYears)

  maxSel <- apply(SelectAtLength, 'Year', max) # should be 1
  maxSel <- replicate(nrow(SelectAtLength), maxSel) |> t()
  SelectAtLength/maxSel
}

SS2Selectivity <- function(st, fl, RepList, YearsList, Stock) {
  # TODO 
  # - ideally import SS3 selectivity models and parameters 
  Selectivity <- Selectivity(Pars=list())
  Selectivity@MeanAtLength <- purrr::map(RepList, \(replist)
                              GetSS_SelectivityAtLength(st, fl, replist, 
                                                        YearsList, Stock)) |>
    List2Array('Sim', pos=1) |>
    ArrayReduceDims()
  Selectivity@Classes <- as.numeric(dimnames(Selectivity@MeanAtLength)$Class)
  
  Selectivity@MeanAtAge <-  purrr::map(RepList, \(replist)
                                       GetSS_SelectivityAtAge(st, fl, replist, 
                                                              YearsList, Stock)) |>
    List2Array('Sim', pos=1) 
  
  Selectivity
}

GetSS_RetentionAtLength <- function(st, fl, replist, YearsList) {
  RetainAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                        replist$sizeselex$Sex == st & 
                                        replist$sizeselex$Factor == "Ret", ] |>
    dplyr::filter(Yr %in% YearsList$YearsHist)
  
  RetainYears <- RetainAtLength$Yr
  LengthClasses <- GetSSLengthClasses(replist)
  RetainAtLength <- RetainAtLength[,as.character(LengthClasses)] |> t()
  dimnames(RetainAtLength) <- list(Class=LengthClasses,
                                   Year=RetainYears)
  RetainAtLength
}

SS2Retention <- function(st, fl, RepList, YearsList, Selectivity, Stock) {
  Retention <- Retention(Pars=list())
  Retention@MeanAtLength <-  purrr::map(RepList, \(replist)
                              GetSS_RetentionAtLength(st, fl, replist, YearsList)) |>
    List2Array('Sim', pos=1) |>
    ArrayReduceDims()
  
  Retention@MeanAtLength[!is.finite(Retention@MeanAtLength)] <- 0
  Retention@Classes <- as.numeric(dimnames(Retention@MeanAtLength)$Class)

  Stock@Length@Classes <- Retention@Classes
  Stock@Length@ASK <- CalcAgeSizeKey(Stock@Length)
  Retention@MeanAtAge <- AtSize2AtAge(Retention, Stock@Length)
  
  Retention
}


GetSS_EmpiricalWeight <- function(st, fl, replist, YearsList) {
  YearsHist <- YearsList$YearsHist
  if (inherits(replist$wtatage, 'logical'))
    return(NULL)
  
  if (!is.null(replist$wtatage$Yr)) {
    wt_at_age_c_df <- replist$wtatage |>
      dplyr::filter(abs(Yr) %in% mainyrs, Sex==st, Fleet==fl)
  } else {
    if (!is.null(replist$wtatage$sex)) {
      wt_at_age_c_df <- replist$wtatage |>
        dplyr::filter(abs(year) %in% YearsHist, sex==st, fleet==fl)
    } else {
      wt_at_age_c_df <- replist$wtatage |>
        dplyr::filter(abs(year) %in% YearsHist, Sex==st, Fleet==fl)
    }
    wt_at_age_c_df <- wt_at_age_c_df |> dplyr::rename(Yr=year)
  }  
  
  AgeClasses <- suppressWarnings(as.numeric(colnames(wt_at_age_c_df)))
  AgeClasses <- AgeClasses[!is.na(AgeClasses)]
  n_age <- length(AgeClasses)
  Weight_at_Age_array <- wt_at_age_c_df[,as.character(AgeClasses)] |> t()
  dimnames(Weight_at_Age_array) <- list(Age=AgeClasses,
                                        Year=YearsHist)
  Weight_at_Age_array
}

SS2WeightFleet <- function(st, fl, RepList, YearsList) {
  Weight_at_Age_array <- purrr::map(RepList, \(replist)
                                    GetSS_EmpiricalWeight(st, fl, replist, YearsList) 
                                    ) |> 
    List2Array('Sim', pos=1) |> 
    ArrayReduceDims()
  Weight_at_Age_array 
}





ProcessSurveyObsSelectivity <- function(OM, RepList) {
  # Add Selectivity to Obs for Survey indices
  IndexInd <- which(grepl('Obs', OM@Data[[1]]@Survey@Selectivity))
  
  if (!length(IndexInd)) 
    return(OM)
  
  Survey_Ind <- which(!RepList[[1]]$IsFishFleet)
  nStock <- nStock(OM)
  YearsList <- GetSSYears(RepList[[1]], pYear=1)
  
  for (fl in Survey_Ind) {
    OM@Obs[[1]][[fl]]@Survey@Selectivity <- MakeNamedList(StockNames(OM))
    for (st in 1:nStock) {
      SurveySelect <- SS2Selectivity(st, fl, RepList, YearsList, Stock=OM@Stock[[st]])
      OM@Obs[[1]][[fl]]@Survey@Selectivity[[st]] <- SurveySelect@MeanAtAge
    }
  }
  OM
}

GetSSRepList <- function(SSdir, silent=FALSE, ...) {
  if(!requireNamespace("r4ss", quietly = TRUE)) {
    cli::cli_abort("Download the `r4ss` package to use this function. It is recommended to install the Github version with: `remotes::install_github(\"r4ss/r4ss\")`", call. = FALSE)
  }
  
  dots <- list(dir = SSdir)
  dots <- list(dir = SSdir, ...)
  if(!any(names(dots) == "covar")) dots$covar <- FALSE
  if(!any(names(dots) == "forecast")) dots$forecast <- FALSE
  #if(!any(names(dots) == "ncols")) dots$ncols <- 1e3
  if(!any(names(dots) == "printstats")) dots$printstats <- FALSE
  if(!any(names(dots) == "verbose")) dots$verbose <- FALSE
  if(!any(names(dots) == "warn")) dots$warn <- FALSE
  
  # if(!silent) 
  #   cli::cli_alert_info('Using function {.fun {"r4ss::SS_output"}} ({.val v{packageVersion("r4ss")}}) to extract data from SS file structure')
  # 
  if (!silent)
    cli::cli_progress_message('Importing SS3 output from {.val {basename(SSdir)}}')
  
  replist <- try(do.call(r4ss::SS_output, dots), silent = TRUE)
  
  if (!silent)
    cli::cli_progress_done()
  
  if(is.character(replist)) 
    cli::cli_abort(c("`r4ss::SS_output` function returned an error.", 
                     'x' = replist), call. = FALSE)
  
  replist
}

#' @describeIn ImportSS Import SS3 Report
#' @export
ImportSSReport <- function(SSDir, silent=FALSE, parallel=TRUE, ...) {
  OnExit()
  if (inherits(SSDir, 'list')) {
    if (inherits(SSDir[[1]], 'list')) {
      names(SSDir) <- 1:length(SSDir)
      return(SSDir)
    } else if (!is.null(SSDir$SS_version)) {
      RepList <- list(SSDir)
      names(RepList) <- 1:length(RepList)
      return(RepList)
    } else {
      cli::cli_abort('`SSDir` is a list but does not appear to be generated by `r4ss::SS_output`')
    }
  } 
  if (inherits(SSDir, 'character')) {
    if (length(SSDir)>1) {
      # 
      AllSSFiles <- lapply(SSDir, list.files)
      ReportExists <- lapply(AllSSFiles, function(x) sum(grepl('^Report.sso', x))) |> 
        unlist()
      ind <- which(ReportExists<1)
      if (length(ind)>0) {
        cli::cli_alert_warning('Warning: SS3 output is not available in {?directory/directories}: {.val {basename(x[ind])}}. \nSkipping {?this/these} {?directory/directories} ...')
        SSDir <- SSDir[-ind]
      }
      
      
      # TODO parallel
      # THIS ISN'T CORRRECT = very slow and doesn't use progress bar
      # see https://furrr.futureverse.org/articles/progress.html#introduction
      # if (parallel) {
      #   if (!silent)
      #     cli::cli_alert('Reading SS3 Output from {.val {length(x)}} directories')
      #   workers <- future::availableCores()/2
      #   future::plan('multisession', workers=workers)
      #   p <- progressr::progressor(steps = length(x))
      #   
      #   RepList <-  furrr::future_map(x, ~{
      #     p()
      #     GetSSRepList(.x, silent=TRUE, ...)       
      #   })
      #   
      # } else {
      #   RepList <- purrr::map(x, \(SSdir) GetSSRepList(SSdir, silent=TRUE, ...),
      #                         .progress=  list(
      #                           caller = environment(),
      #                           format =  'Reading SS3 Output from {.val {length(x)}} directories {cli::pb_bar} {cli::pb_percent}')
      #   )
      # }
      
      RepList <- purrr::map(SSDir, \(SSdir) GetSSRepList(SSdir, silent=TRUE, ...),
                            .progress=  list(
                              caller = environment(),
                              format =  'Reading SS3 Output from {.val {length(x)}} directories {cli::pb_bar} {cli::pb_percent}')
      )
      
      names(RepList) <- 1:length(RepList)
      return(RepList)
    } else {
      RepList <- list(GetSSRepList(SSDir, silent, ...))
      names(RepList) <- 1:length(RepList)
      return(RepList)
    }
  }
}

## Import SS Data ----

ImportSSData <- function(SSDir,  
                         Name="Imported by ImportSSData", 
                         CommonName = "", 
                         Species = "",
                         silent=FALSE, ...) {
  OnExit()
  RepList <- ImportSSReport(SSDir, silent, ...)
  replist <- RepList[[1]]
  nStock <- replist$nsexes
  
  YearsList <- GetSSYears(RepList[[1]], 1)
  
  # Create Data object
  Data <- new('data')
  Data@Name <- Name
  Data@CommonName <- CommonName
  Data@Species <- Species
  
  # TODO - life history 

  # Data@Agency
  # Data@Author
  # Data@Email
  # Data@Region
  # Data@Latitude
  # Data@Longitude
  
  Data@Years <- YearsList$YearsHist
  Data@YearLH <- YearsList$CurrentYear
  # Data@TimeUnits <- 'year'
  Data@TSperYear <- YearsList$TSperYear
  Data@nArea <- 1 

  Data@Landings <- ImportSSData_Catch(replist, 'Landings', silent)
  Data@Discards <- ImportSSData_Catch(replist, 'Discards', silent)
  
  Data@CPUE <- ImportSSData_Index(replist, 'CPUE')
  Data@Survey <- ImportSSData_Index(replist, 'Survey')
  
  Data@CAA
  Data@CAL
  # Data@Misc
  Data
}

ImportSSData_Catch <- function(replist, Type=c('Landings', 'Discards'), silent=FALSE ) {
  Type <- match.arg(Type)
  
  YearsList <- GetSSYears(replist, pYear=1)
  YearsHist <- YearsList$YearsHist
  nTS <- length(YearsHist)
  FleetNames <- replist$catch$Fleet_Name |> unique()
  nFleet <- length(FleetNames)
  
  CatchOut <- new('catchdata')
  CatchOut@Name <- FleetNames
  CatchOut@Value <- CatchOut@CV <- array(NA, 
                                         dim=c(nTS, nFleet),
                                         dimnames = list(Year=YearsHist,
                                                         Fleet=FleetNames))
  
  CatchOut@Units <-  sapply(replist$catch_units[replist$IsFishFleet], function(x)
    switch(x, '1'='Biomass', '2'='Number'))
  CatchOut@Type <- rep(Type, nFleet)
  
  CatchColNames <- names(replist$catch)
  CatchDF <- replist$catch |> dplyr::filter(Yr%in%YearsHist) 
  
  if ('dead_bio' %in% CatchColNames) {
    DeadDF <- CatchDF |> dplyr::select(Year=Yr, Seas, Fleet, Obs=dead_bio)
  } else if ('kill_bio' %in% CatchColNames) {
    DeadDF <- CatchDF |> dplyr::select(Year=Yr, Seas, Fleet, Obs=kill_bio)
  } else {
    cli::cli_abort("Neither 'kill_bio' or 'dead_bio' found in this SS3 output")
  }
  
  if ('ret_bio' %in% CatchColNames) {
    RetainDF <- CatchDF |> dplyr::select(Year=Yr, Seas, Fleet, Obs=ret_bio)
  } else {
    cli::cli_abort("'ret_bio' not found in this SS3 output")
  }
  Yrs <- unique(RetainDF$Year)
  Seas <- unique(RetainDF$Seas)
  
  if (Type=='Landings') {
    i <- 0
    for (yr in Yrs) {
      for (sea in Seas) {
        i <- i + 1
        CatchOut@Value[i, ] <- RetainDF |> dplyr::filter(Year==yr, Seas==sea) |> dplyr::pull(Obs)
      }
    }
  } else {
    DeadDiscards <- DeadDF
    DeadDiscards$Obs <- DeadDF$Obs - RetainDF$Obs
    i <- 0
    for (yr in Yrs) {
      for (sea in Seas) {
        i <- i + 1
        CatchOut@Value[i, ] <- DeadDiscards |> dplyr::filter(Year==yr, Seas==sea) |> dplyr::pull(Obs)
      }
    }
  }

  # TODO 
  # CatchOut@CV
  CatchOut
}

ImportSSData_Index <- function(replist, Type=c('CPUE', 'Survey')) {
  Type <- match.arg(Type, c('CPUE', 'Survey'))
  
  YearsList <- GetSSYears(replist, pYear=1)
  YearsHist <- YearsList$YearsHist
  nTS <- length(YearsHist)
  
  Indices <- new('indicesdata')
  CPUE <- replist$cpue
  
  if(!nrow(CPUE)) 
    return(Indices)
  
  if (Type=='CPUE') {
    IndFleets <- which(replist$IsFishFleet)
  } else {
    IndFleets <- which(!replist$IsFishFleet)
  }
  
  if (length(IndFleets)<1)
    return(Indices)

  CPUE <- CPUE |> dplyr::filter(Fleet%in%IndFleets)
  
  if (!nrow(CPUE))
    return(Indices)
  
  CPUE_Ind <- CPUE$Fleet |> unique() 
  
  CPUE_Split <- CPUE |> dplyr::group_by(Fleet) |>
    dplyr::group_split()
  
  CPUENames <- purrr::map(CPUE_Split, \(cpue) {
    out <- unique(cpue$Fleet_name)
    ifelse(length(out) == 1, out, NA_character_)
    out
  }) |> unlist() |> as.character()
  
  Indices@Name <- CPUENames
  names(CPUE_Split) <- CPUENames
  nIndex <- length(CPUENames)
  
  Value <- purrr::imap(CPUE_Split, \(cpue, idx) {
    ind <- match(cpue$Yr, YearsHist) + cpue$Seas-1
    Years <- YearsHist[ind]
    index <-  array(cpue$Obs, c(length(Years),1), 
                    dimnames = list(Year=Years,
                                    Fleet=idx)
    )
    index
  }) 
  
  CV <- purrr::imap(CPUE_Split, \(cpue, idx) {
    ind <- match(cpue$Yr, YearsHist) + cpue$Seas-1
    Years <- YearsHist[ind]
    index <-  array(cpue$SE, c(length(Years),1), 
                    dimnames = list(Year=Years,
                                    Fleet=idx)
    )
    index
  })
  
  Indices@Value <- Indices@CV <- array(NA, 
                                       dim=c(nTS, nIndex),
                                       dimnames = list(Year=YearsHist,
                                                       Fleet=as.character(CPUENames)))
  
  Indices@Timing <- rep(0, nIndex) # assume at beginning of time step
  
  for (i in seq_along(Value)) {
    ArrayFill(Indices@Value) <-  Value[[i]]
  }
  for (i in seq_along(CV)) {
    ArrayFill(Indices@CV) <-  CV[[i]]
  }
  
  # https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#surveys-and-indices
  Indices@Units <- sapply(replist$survey_units[CPUE_Ind], function(x)
    switch(as.character(x), 
           '0'="Number",
           '1'='Biomass', 
           '2'='F',
           '30'='Spawning Production',
           '31'='Expected Recruitment Deviation',
           '32'='Spawning Production * exp(recruitment deviation)',
           '33'='Recruitment',
           '34'='Depletion (spawning biomass/virgin spawning biomass)',
           '35'='Survey of a Deviation Vector',
           '36'='Recruitment Deviation'
           )) |> 
    unlist()
  
  if (length(Indices@Units) != length(CPUENames))
    cli::cli_abort(c(
      'x'='CPUE/Survey units do not match fleets'
    ), internal=TRUE)
  
  if (Type=='CPUE') {
    Indices@Selectivity <- IndFleets
  } else {
    Indices@Selectivity <- rep('Obs', length(CPUE_Ind))
  }
  Indices 
}




# Compare ----
DropXXCols <- function(array) {
  ind <- which(names(array) != 'XX')
  array[,ind]
}

CompareSSNumber <- function(replist, Hist) {
  if (!inherits(Hist, 'hist'))
    cli::cli_abort('`Hist` must be class `hist`')
  
  mainyrs <- replist$startyr:replist$endyr
  AgeClasses <- GetSSAgeClasses(replist)
  
  NumberHist <- Number(Hist) |> dplyr::mutate(Model='Import') |>
    dplyr::filter(Sim==1)
  
  NumberSS <- replist$natage |> 
    dplyr::filter(Yr%in%mainyrs, `Beg/Mid`=='B') |>
    dplyr::rename(Year=Yr, Stock=Sex) |>
    tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
    dplyr::group_by(Stock, Year) |>
    dplyr::summarise(Value=sum(value), Model='SS3', .groups='drop')
  
  NumberSS$Stock <- unique(NumberHist$Stock)[NumberSS$Stock]
  
  NumberDF <- dplyr::bind_rows(NumberHist, NumberSS)
  
  p1 <- ggplot(NumberDF, ggplot2::aes(x=Year, y=Value, color=Model)) +
    ggplot2::facet_grid(~Stock) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()
  
  pDF <- NumberDF |> dplyr::group_by(Stock, Year) |>
    dplyr::summarise(Mean=mean(Value[Model=='SS3']/Value[Model!='SS3']))
  
  p2 <- ggplot2::ggplot(pDF, ggplot2::aes(x=Year, y=Mean, color=Stock)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(y='Ratio SS3/Model')
  
  print(patchwork::wrap_plots(p1, p2, ncol=1))
  invisible(NumberDF)
}

CompareSSLandings <- function(replist, Hist) {
  if (!inherits(Hist, 'hist'))
    cli::cli_abort('`Hist` must be class `hist`')
  
  mainyrs <- replist$startyr:replist$endyr
  AgeClasses <- GetSSAgeClasses(replist$natage)
  
  HistLandings <- Landings(Hist, byFleet=TRUE) |>
    dplyr::mutate(Model='Import') |>
    dplyr::filter(Sim==1) |>
    dplyr::group_by(Year, Fleet, Model) |>
    dplyr::summarise(Value=sum(Value))
  
  SS3Landings <- replist$catch |> dplyr::filter(Yr %in% mainyrs) |>
    dplyr::select(Year=Yr,  Fleet, Value=ret_bio) |>
    dplyr::mutate(Model='SS3')
  
  
  SS3Landings$Fleet <- Hist@OM@Fleet[[1]]@Name[SS3Landings$Fleet]
  SS3Landings$Sim <- 1
  
  df <- dplyr::bind_rows(
    HistLandings,
    SS3Landings
  ) |> 
    dplyr::group_by(Year, Model, Fleet) |>
    dplyr::summarise(Value=sum(Value))
  
  p1 <- ggplot(df, aes(x=Year, y=Value, color=Model, linetype=Model)) +
    facet_wrap(~Fleet, ncol=3, scales='free') +
    geom_line() +
    theme_bw()
  
  pDF <- df |> dplyr::group_by(Fleet, Year) |>
    dplyr::summarise(Mean=mean(Value[Model=='SS3']/Value[Model!='SS3']))
  
  p2 <- ggplot(pDF, aes(x=Year, y=Mean, color=Fleet)) +
    geom_line() +
    theme_bw() +
    labs(y='Ratio SS3/Model')
  
  print(patchwork::wrap_plots(p1, p2, ncol=1, heights=c(0.8, 0.2)))
  invisible(df)
}


CompareSSRemovals <- function(replist, Hist) {
  if (!inherits(Hist, 'hist'))
    cli::cli_abort('`Hist` must be class `hist`')
  
  mainyrs <- replist$startyr:replist$endyr
  AgeClasses <- GetSSAgeClasses(replist$natage)
  
  HistRemovals <- Removals(Hist, byFleet=TRUE) |>
    dplyr::mutate(Model='Import') |>
    dplyr::filter(Sim==1) |>
    dplyr::group_by(Year, Fleet, Model) |>
    dplyr::summarise(Value=sum(Value))
  
  SS3Removals <- replist$catch |> dplyr::filter(Yr %in% mainyrs)
  if ('dead_bio' %in% names(SS3Removals)){
    SS3Removals <- SS3Removals |> 
      dplyr::select(Year=Yr,  Fleet, Value=dead_bio) |>
      dplyr::mutate(Model='SS3')
  } else {
    SS3Removals <- SS3Removals |> 
      dplyr::select(Year=Yr,  Fleet, Value=kill_bio) |>
      dplyr::mutate(Model='SS3')
  }

  SS3Removals$Fleet <- Hist@OM@Fleet[[1]]@Name[SS3Removals$Fleet]
  SS3Removals$Sim <- 1
  
  df <- dplyr::bind_rows(
    HistRemovals,
    SS3Removals
  ) |> 
    dplyr::group_by(Year, Model, Fleet) |>
    dplyr::summarise(Value=sum(Value))
  
  p1 <- ggplot(df, aes(x=Year, y=Value, color=Model, linetype=Model)) +
    facet_wrap(~Fleet, ncol=3, scales='free') +
    geom_line() +
    theme_bw()
  
  pDF <- df |> dplyr::group_by(Fleet, Year) |>
    dplyr::summarise(Mean=mean(Value[Model=='SS3']/Value[Model!='SS3']))
  
  p2 <- ggplot(pDF, aes(x=Year, y=Mean, color=Fleet)) +
    geom_line() +
    theme_bw() +
    labs(y='Ratio SS3/Model')
  
  print(patchwork::wrap_plots(p1, p2, ncol=1, heights=c(0.8, 0.2)))
  invisible(df)
}


CompareSSRefPoints <- function(replist, Hist) {
  refs <- replist$derived_quants |> dplyr::filter(Label%in% c('Dead_Catch_MSY',
                                                              'Ret_Catch_MSY',
                                                              'SSB_MSY', 
                                                              'SPR_MSY')) |>
    dplyr::select(Label, Value) 
  
  OM <- data.frame(Variable=c('SBMSY', 'SPMSY', 'MSYRemovals', 'MSYLandings'),
                   OM=c(SBMSY(Hist) |> dplyr::filter(Sim==1, Year==max(Year)) |> dplyr::pull(Value),
                           SPMSY(Hist) |> dplyr::filter(Sim==1, Year==max(Year)) |> dplyr::pull(Value),
                           MSY(Hist) |> dplyr::filter(Sim==1, Year==max(Year)) |> dplyr::pull(Value),
                           MSY(Hist, type='Landings') |> dplyr::filter(Sim==1, Year==max(Year)) |> dplyr::pull(Value))
  ) 

  SS <- data.frame(
                   Variable=c('SBMSY', 'SPMSY', 'MSYRemovals', 'MSYLandings'),
                   SS=c(refs |> dplyr::filter(Label=='SSB_MSY') |> dplyr::pull(Value),
                           refs |> dplyr::filter(Label=='SSB_MSY') |> dplyr::pull(Value),
                           refs |> dplyr::filter(Label=='Dead_Catch_MSY') |> dplyr::pull(Value),
                           refs |> dplyr::filter(Label=='Ret_Catch_MSY') |> dplyr::pull(Value))
  )
  
  dplyr::left_join(OM, SS) |> dplyr::mutate("OM/SS"=OM/SS)
}

# ---- Other Useful Stuff ----


GetSSNatAge <- function(replist, OM, yrs=NULL, sex=1) {
  if (is.null(yrs)) {
    yrs <- replist$natage |> dplyr::filter(Era=='TIME') |>
      dplyr::distinct(Yr) |> dplyr::pull(Yr)
    
  }
    
  AgeClasses <- OM@Stock[[1]]@Ages@Classes
  AgeClassesFull <- seq(0, to=max(AgeClasses), by=1/OM@TSperYear)
  HistYears <- Years(OM, 'H')
  
  SSN <- replist$natage |> dplyr::filter(Yr%in%yrs, `Beg/Mid`=='B', Sex==sex) |>
    dplyr::select(Yr, Time, Seas, as.character(GetSSAgeClasses(replist))) |>
    tidyr::pivot_longer(as.character(GetSSAgeClasses(replist))) |>
    dplyr::mutate(Age=as.numeric(name)) |>
    dplyr::arrange(Age)
  
  SSN_Ages <- SSN$Age
  Seas <- unique(SSN$Seas)
  SSN$Age <- AgeClassesFull
  
  SSN <- SSN |> dplyr::arrange(Yr, Age) |>
    dplyr::filter(Age%in% AgeClasses)

  
  nSeas <- length(Seas)
  nTS <- length(yrs) * nSeas
  Yrs <- SSN$Yr |> unique()
  
  Initial <- array(NA, c(length(AgeClasses),nTS), 
                   dimnames=list(
                     Age=AgeClasses,
                     Year=HistYears[1:nTS]
                   ))
  
  i <- 0
  for (yr in Yrs) {
    for (sea in 1:nSeas) {
      i <- i+1
      n <- SSN |> dplyr::filter(Seas==sea, Yr<yr+1 & Yr>=yr) 
    
      
      
      temp <- array(n$value, dim=c(nrow(n), 1),
                    dimnames=list(
                      Age=n$Age,
                      Year=HistYears[i]
                    )
      )
      
      
      ArrayFill(Initial) <- temp
    }
  }
  
  Initial
  
}
