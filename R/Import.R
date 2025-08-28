Import <- function(dir=NULL,
                   nSim=48,
                   pYear=50, ...) {
  list.files(dir)
  
}





# ---- SS3 ----

## --- Stock ----
GetEndGrowth <- function(st,replist) {
  endgrowth <- dplyr::filter(replist$endgrowth, Sex == st, Seas == 1)
  if(!is.null(endgrowth$BirthSeas)) endgrowth <- dplyr::filter(endgrowth, BirthSeas == 1)
  if(!is.null(endgrowth$Settlement)) endgrowth <- dplyr::filter(endgrowth, Settlement == 1)
  endgrowth
}


GetSSAgeClasses <- function(replist) {
  AgeClasses <- suppressWarnings(as.numeric(colnames(replist$natage)))
  AgeClasses[!is.na(AgeClasses)]
}

GetSS_Length_at_Age <- function(st, replist, mainyrs, AgeClasses) {
  endgrowth <- GetEndGrowth(st, replist)
  nyears <- length(mainyrs)
  
  Len_age_df <- dplyr::filter(replist$growthseries, Morph == st, Yr %in% mainyrs)
  
  if(nrow(Len_age_df)>0) { # Would do time-varying
    Len_age <- do.call(rbind, lapply(AgeClasses, function(x) parse(text = paste0("Len_age_df$`", x, "`")) |> eval()))
    if (ncol(Len_age) == (nyears - 1)) Len_age <- cbind(Len_age, endgrowth$Len_Beg)
  } else {
    Len_age <- endgrowth$Len_Beg |>matrix(length(AgeClasses), nyears) # No time-varying
  }
  Len_age
}

GetSS_LengthCV_at_Age <-function(st, replist, mainyrs, AgeClasses) {
  endgrowth <- GetEndGrowth(st, replist)
  CVatAge <- endgrowth$SD_Beg/endgrowth$Len_Beg  
  array(CVatAge, dim=c(length(AgeClasses), length(mainyrs)))
}


SS2Length <- function(st, RepList, mainyrs, AgeClasses) {
  MeanAtAge <- purrr::map(RepList, \(replist) 
                          GetSS_Length_at_Age(st, replist, mainyrs, AgeClasses)) |>
    List2Array() |>
    AddDimNames(c("Age", "TimeStep", "Sim"), TimeSteps = mainyrs) |>
    aperm(c('Sim', 'Age','TimeStep')) |>
    ArrayReduceDims()
  
  CVAtAge <- purrr::map(RepList, \(replist) 
                        GetSS_LengthCV_at_Age(st, replist, mainyrs, AgeClasses)) |>
    List2Array() |>
    AddDimNames(c("Age", "TimeStep", "Sim"), TimeSteps = mainyrs) |>
    aperm(c('Sim', 'Age','TimeStep')) |>
    ArrayReduceDims()

  Length <- Length()
  Length@MeanAtAge <- MeanAtAge
  Length@CVatAge <- CVAtAge
  
  # TODO - add parameters and models
  # https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#Growth
  Length@Pars <- list()
  Length
}

GetSS_LengthWeightPars <- function(st, replist) {
  c(Alpha=replist$Growth_Parameters[st, ]$WtLen1,
    Beta=replist$Growth_Parameters[st, ]$WtLen2)
  
}

GetSS_WeightAtAge <- function(st, replist, mainyrs, AgeClasses) {
  nyears <- length(mainyrs)
  endgrowth <- GetEndGrowth(st, replist)
  
  if(!is.null(replist$mean_body_wt)) {
    Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == st, ]
    Wt_age_df <- Wt_age_df[findInterval(mainyrs, Wt_age_df$Yr), ]
    Wt_age <- do.call(rbind, lapply(0:n_age, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
    if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1])
  } else {
    Wt_age <- matrix(endgrowth$Wt_Beg, length(AgeClasses), nyears)
  }
  Wt_age 
}

SS2Weight <- function(st, RepList, mainyrs, AgeClasses) {
  LengthWeightPars <- purrr::map(RepList, \(replist)
                                 GetSS_LengthWeightPars(st, replist))
  
  alpha <- purrr::map(LengthWeightPars, \(LW) LW[1]) |> 
    unlist() |> unique()
  
  beta <- purrr::map(LengthWeightPars, \(LW) LW[2]) |> 
    unlist() |> unique()
  
  Weight <- Weight()
  Weight@Pars <- list(alpha=alpha,
                      beta=beta)
  Weight@Model <- FindModel(Weight)
  
  Weight@MeanAtAge <- purrr::map(RepList, \(replist) 
                                 GetSS_WeightAtAge(st, replist, mainyrs, AgeClasses)) |>
    List2Array() |>
    AddDimNames(c("Age", "TimeStep", "Sim"), TimeSteps = mainyrs) |>
    aperm(c('Sim', 'Age','TimeStep')) |>
    ArrayReduceDims()
  
  Weight
}

GetSS_M_at_age <- function(st, replist, mainyrs, AgeClasses) {
  nyears <- length(mainyrs)
  endgrowth <- GetEndGrowth(st, replist)
  M_at_age <- replist$M_at_age[replist$M_at_age$Sex == st & replist$M_at_age$Year %in% mainyrs, ]
  if(!nrow(M_at_age)) {
    M_at_age <- replist$M_at_age[replist$M_at_age$Sex == st & replist$M_at_age$Yr %in% mainyrs, ]
  }
  if(!nrow(M_at_age)) {
    M_at_age <- replist$M_at_age[replist$M_at_age$Gender == st & replist$M_at_age$Year %in% mainyrs, ]
  }
  
  M_at_age <- M_at_age[,as.character(AgeClasses)]
  M_at_age <- t(M_at_age)
  if (all(is.na(M_at_age[nrow(M_at_age), ]))) 
    M_at_age[nrow(M_at_age), ] <- endgrowth$M[length(AgeClasses)]
  if(ncol(M_at_age) == (nyears - 1)) M_at_age <- cbind(M_at_age, endgrowth$M)
  M_at_age
}

SS2NaturalMortality <- function(st, RepList, mainyrs, AgeClasses) {
  MeanAtAge <- purrr::map(RepList, \(replist) GetSS_M_at_age(st, replist, mainyrs, AgeClasses)) |>
    List2Array() |>
    AddDimNames(c("Age", "TimeStep", "Sim"), TimeSteps = mainyrs) |>
    aperm(c('Sim', 'Age','TimeStep')) |>
    ArrayReduceDims()
  
  NaturalMortality <- NaturalMortality()
  NaturalMortality@Pars <- list()
  NaturalMortality@MeanAtAge <- MeanAtAge
  NaturalMortality
}


GetSS_Maturity_at_Age <- function(st, replist, AgeClasses) {
  endgrowth <- GetEndGrowth(st, replist)
  
  if(any(endgrowth$Age_Mat < 0)) endgrowth$Age_Mat <- abs(endgrowth$Age_Mat) # Should all be 1's
  if(any(endgrowth$Len_Mat < 0)) endgrowth$Len_Mat <- abs(endgrowth$Len_Mat)
  Mat_age <- endgrowth$Len_Mat * endgrowth$Age_Mat
  Mat_age <- matrix(Mat_age, length(AgeClasses), 1)
  Mat_age
}

SS2Maturity <- function(st, RepList, mainyrs, AgeClasses) {
  
  Maturity <- Maturity()
  Maturity@Pars <- list()
  
  Maturity@MeanAtAge <- purrr::map(RepList, \(replist) 
                                   GetSS_Maturity_at_Age(st, replist, AgeClasses)) |>
    List2Array() |>
    AddDimNames(c("Age", "TimeStep", "Sim"), TimeSteps = mainyrs) |>
    aperm(c('Sim', 'Age','TimeStep')) |>
    ArrayReduceDims()
  Maturity
}

GetSS_Fecundity <- function(st, replist, AgeClasses) {
  # TODO import model and parameters from SS output 
  # replist$FecPar1
  # replist$FecPar1name
  # replist$FecPar2
  # replist$FecType
  # replist$Fecundity_option
  # can do the same with maturity
  
  endgrowth <- GetEndGrowth(st, replist)
  
  # TODO - this doesn't account for time-varying fecundity
  
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
  fec_age
}

SS2Fecundity <- function(st, RepList, mainyrs, AgeClasses) {
  # TODO import model and parameters from SS output 
  Fecundity <- Fecundity()
  Fecundity@Pars <- list()
  Fecundity@MeanAtAge <- purrr::map(RepList, \(replist) GetSS_Fecundity(st, replist, AgeClasses)) |>
    List2Array() |>
    AddDimNames(c("Age", "TimeStep", "Sim"), TimeSteps = mainyrs) |>
    aperm(c('Sim', 'Age','TimeStep')) |>
    ArrayReduceDims()
  Fecundity
}


SS2Depletion <- function(st, RepList, mainyrs) {
  
  Depletion <- Depletion()
  Depletion@Reference <- 'SB0'
  
  SB0 <- purrr::map(RepList, \(replist) {
    replist$timeseries |>
      dplyr::filter(Era == "VIRG") |>
      getElement("SpawnBio") |>
      sum(na.rm = TRUE)
  }) |> unlist()
  
  
  SB1 <- purrr::map(RepList, \(replist) {
    replist$timeseries |>
      dplyr::filter(Yr == mainyrs[1]) |>
      getElement("SpawnBio") |>
      sum(na.rm = TRUE)
  }) |> unlist()
  
  if (!all(round(SB1/SB0,2)==1)) {
    Depletion@Initial <- SB1/SB0
  }
  
  if (st==1) {
    SBCurr <- purrr::map(RepList, \(replist) {
      replist$timeseries |>
        dplyr::filter(Yr == max(mainyrs)) |>
        getElement("SpawnBio") |>
        sum(na.rm = TRUE)
    }) |> unlist()
    
    Depletion@Final <- SBCurr/SB0
  }
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


GetSS_RecDevs <- function(replist, mainyrs, AgeClasses, period=c('Historical', 'Early')) {
  period <- match.arg(period)
  if (period=='Historical') {
    Rec_main <- replist$recruit[replist$recruit$Yr %in% mainyrs, ]
    return(Rec_main$pred_recr/Rec_main$exp_recr)
  }
  EarlyYears <- c((min(mainyrs-1)-(length(AgeClasses)-2)):(min(mainyrs-1)))
  Rec_early <- replist$recruit[vapply(EarlyYears, match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]
  Rdev_early <- Rec_early$pred_recr/Rec_early$exp_recr
  Rdev_early[is.na(Rdev_early)] <- 1
  Rdev_early
}


GetSS_R0 <- function(st, replist) {
  AgeClasses <- GetSSAgeClasses(replist)
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
  N_at_age[[as.character(min(AgeClasses))]] |> sum(na.rm=TRUE)
}

SS2SRR <- function(st, RepList, mainyrs, AgeClasses, pYear, nSim) {
  
  SRR <- SRR()
  
  SD <- purrr::map(RepList, \(replist) replist$sigma_R_in) |> unlist()
  SRR@SD <- as.numeric(SD) 
  
  # option for time-varying SD?
  # array(SD, dim=c(nSim, length(mainyrs))) |>
  # AddDimNames(c('Sim', 'TimeStep'), mainyrs)
  
  SRR@R0 <- purrr::map(RepList, \(replist) GetSS_R0(st, replist)) |> unlist()
  Pars <- purrr::map(RepList, \(replist) GetSS_SRRPars(replist)) 
  Pars <- do.call('rbind', Pars)
  ParsList <- list(as.numeric(Pars))
  names(ParsList) <- colnames(Pars)
  SRR@Pars <- ParsList
  
  RecDevsEarly <- purrr::map(RepList, \(replist) GetSS_RecDevs(replist, mainyrs, AgeClasses, 'Early'))
  RecDevsEarly <- do.call('cbind', RecDevsEarly)
  SRR@RecDevInit <- array(RecDevsEarly, dim=c(length(AgeClasses)-1,ncol(RecDevsEarly)),
                          dimnames = list(Age=rev(AgeClasses[-1]),
                                          Sim=1:ncol(RecDevsEarly))) |>
    aperm(c('Sim', 'Age')) |>
    ArrayReduceDims()
  
  
  RecDevs <- purrr::map(RepList, \(replist) GetSS_RecDevs(replist, mainyrs, AgeClasses))
  RecDevs <- do.call('cbind', RecDevs)
  SRR@RecDevHist  <- array(RecDevs, dim=c(length(mainyrs),ncol(RecDevs)),
                           dimnames = list(TimeStep=mainyrs,
                                           Sim=1:ncol(RecDevs))) |>
    aperm(c('Sim', 'TimeStep')) |>
    ArrayReduceDims(includeTimeStep=FALSE)
  
  recdevs <- SRR@RecDevHist
  
  AC <- log(recdevs) |>
    apply(1, acf, lag.max = 1, plot = FALSE, na.rm=TRUE) |>
    lapply(getElement,'acf') |>
    lapply(getElement,2) |> unlist()
  
  AC[!is.finite(AC)] <- 0
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

SS2Stock <- function(st, RepList, pYear, nSim) {
  mainyrs <- RepList[[1]]$startyr:RepList[[1]]$endyr
  nyears <- length(mainyrs)
  
  Stock <- Stock(Name=ifelse(st == 1, "Female", "Male")) 
  
  if(!is.null(RepList[[1]]$movement) && nrow(RepList[[1]]$movement) > 0) 
    cli::cli_alert_warning("Movement detected in SS model but not imported right now.")
  
  AgeClasses <- GetSSAgeClasses(RepList[[1]])
  nAge <- length(AgeClasses)
  Stock@Ages <- Ages(MaxAge=max(AgeClasses), MinAge=min(AgeClasses))
  
  Stock@Length <- SS2Length(st, RepList, mainyrs, AgeClasses)
  Stock@Weight <- SS2Weight(st, RepList, mainyrs, AgeClasses)
  Stock@NaturalMortality <- SS2NaturalMortality(st, RepList, mainyrs, AgeClasses)
  Stock@Maturity <- SS2Maturity(st, RepList, mainyrs, AgeClasses)
  Stock@Fecundity <- SS2Fecundity(st, RepList, mainyrs, AgeClasses)
  Stock@Depletion <- SS2Depletion(st, RepList, mainyrs)
  Stock@SRR <- SS2SRR(st, RepList, mainyrs, AgeClasses, pYear, nSim)
  Stock@nYear <- length(mainyrs)
  Stock@pYear <- pYear
  Stock@nSim <- nSim
  Stock
}


## Fleet ----

GetSS_Effort <- function(st, fl, replist, mainyrs, type=c('Effort', 'q')) {
  type <- match.arg(type)
  # Fishing Effort is proportional to FInteract - ie the fishing mortality on
  # all fish that interact with the gear, including those that are discarded alive
  # ie don't suffer discard mortality
  
  FInteract <- replist$fatage |> 
    dplyr::filter(Sex==st, Fleet==fl, Yr %in% mainyrs)
  
  AgeClasses <- suppressWarnings(as.numeric(colnames(FInteract)))
  AgeClasses <- AgeClasses[!is.na(AgeClasses)]
  
  FInteract <- t(FInteract[,as.character(AgeClasses)])
  dimnames(FInteract) <- list(Age=AgeClasses,
                              TimeStep=mainyrs)
  
  FInteractApical <- apply(FInteract, c('TimeStep'), max)
  FInteractApicalTerminal <- FInteractApical
  FInteractApicalTerminal[] <- 0
  
  ind <- which(FInteractApical>0) |> max()
  FInteractApicalTerminal[] <- FInteractApical[ind]
  if (type=='q')
    return(FInteractApicalTerminal)
  
  RelEffort <- ArrayDivide(FInteractApical,FInteractApicalTerminal)
  RelEffort <- array(RelEffort, dim=c(1, length(RelEffort))) 
  dimnames(RelEffort) <- list(Sim=1,
                              TimeStep=mainyrs)
  
  RelEffort
}

SS2Effort <- function(st, fl, RepList, mainyrs) {
  Effort <- Effort()
  RelEffort <- purrr::map(RepList, \(replist)
                          GetSS_Effort(st, fl, replist, mainyrs))
  
  Effort@Effort <- do.call('rbind', RelEffort) |> 
    AddDimNames(c('Sim', 'TimeStep'), mainyrs)
  
  Catchability <- purrr::map(RepList, \(replist)
                             GetSS_Effort(st, fl, replist, mainyrs, 'q'))
  
  Effort@Catchability <-  do.call('rbind', Catchability) |> 
    AddDimNames(c('Sim', 'TimeStep'), mainyrs)
  Effort
}

GetSS_DiscardMortalityAtLength <- function(st, fl, replist, mainyrs, Stock) {
  
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
  DiscardAtLength
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


SS2DiscardMortality <- function(st, fl, RepList, mainyrs, Stock) {
  DiscardMortality <- DiscardMortality()
  
  DiscardMortalityAtLength <- purrr::map(RepList, \(replist)
                                         GetSS_DiscardMortalityAtLength (st, fl, replist, mainyrs, Stock)
                                         ) 
  DiscardMortalityAtLength <- abind::abind(DiscardMortalityAtLength, along=3,
                                           use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(DiscardMortalityAtLength))[[3]] <- 'Sim'
  
  DiscardMortality@MeanAtLength <- DiscardMortalityAtLength |>
    aperm(c("Sim", 'Class', 'TimeStep')) |>
    ArrayReduceDims()

  
  AgeClasses <- Stock@Ages@Classes
  LengthClasses <- dimnames(DiscardMortality@MeanAtLength)[[2]] |> as.numeric()
  DiscardMortality@Classes <- LengthClasses
  
  # TODO - doesn't account for time-varying growth
  ALK <- purrr::map(RepList, \(replist) GetSSALK(st, replist, AgeClasses, LengthClasses)) |>
    abind::abind(along=3,
                 use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(ALK))[[3]] <- 'Sim'
  ALK <- ALK |> AddDimension('TimeStep', mainyrs[1])
  ALK <- aperm(ALK, c('Sim', 'Age', 'Class', 'TimeStep'))
  
  Stock@Length@ASK <- ALK
  
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, 
                                 Length=Stock@Length,
                                 Ages=Stock@Ages, 
                                 nsim=Stock@nSim,
                                 TimeSteps = mainyrs,
                                 max1=FALSE)
  DiscardMortality@MeanAtAge <- ArrayReduceDims(DiscardMortality@MeanAtAge)
  DiscardMortality
}

GetSS_SelectivityAtAge <- function(st, fl, replist, mainyrs) {
  AgeClasses <- GetSSAgeClasses(replist)
  Asel2 <- dplyr::filter(replist$ageselex, Fleet == fl, Sex %in% st, 
                         Factor == "Asel2",
                         Yr %in% mainyrs)
  AgeSelect <- Asel2[,as.character(AgeClasses)] |> t()
  
  dimnames(AgeSelect) <- list(Age=AgeClasses,
                              TimeStep=Asel2$Yr)
  
  maxSel <- apply(AgeSelect, 'TimeStep', max) # should be 1 
  maxSel <- replicate(nrow(AgeSelect), maxSel) |> t()
  
  AgeSelect/maxSel
}

GetSS_SelectivityAtLength <- function(st, fl, replist, mainyrs) {
  
  SelectAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                        replist$sizeselex$Sex == st & 
                                        replist$sizeselex$Factor == "Lsel", ] |>
    dplyr::filter(Yr %in% mainyrs)
  
  SelectYears <- SelectAtLength$Yr
  LengthClasses <- suppressWarnings(as.numeric(colnames(SelectAtLength)))
  
  LengthClasses <- LengthClasses[!is.na(LengthClasses)]
  SelectAtLength <- SelectAtLength[,as.character(LengthClasses)] 
  SelectAtLength <- t(SelectAtLength)
  
  dimnames(SelectAtLength) <- list(Class=LengthClasses,
                                   TimeStep=SelectYears)
  
  maxSel <- apply(SelectAtLength, 'TimeStep', max) # should be 1 
  maxSel <- replicate(nrow(SelectAtLength), maxSel) |> t()
  
  SelectAtLength/maxSel
}


SS2Selectivity <- function(st, fl, RepList, mainyrs, Stock) {
  Selectivity <- Selectivity()
  Selectivity@Pars <- list()
  
  MeanAtAge <- purrr::map(RepList, \(replist)
                          GetSS_SelectivityAtAge(st, fl, replist, mainyrs)
  ) |> abind::abind(along=3,
                 use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(MeanAtAge))[[3]] <- 'Sim'
  
  Selectivity@MeanAtAge <- aperm(MeanAtAge, c('Sim', 'Age', 'TimeStep')) |>
    ArrayReduceDims()
  
  MeanAtLength <-  purrr::map(RepList, \(replist)
                              GetSS_SelectivityAtLength(st, fl, replist, mainyrs)
  ) |> abind::abind(along=3,
                    use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(MeanAtLength))[[3]] <- 'Sim'
  
  Selectivity@MeanAtLength <- aperm(MeanAtLength, c('Sim', 'Class', 'TimeStep')) |>
    ArrayReduceDims()
  Selectivity@Classes <- as.numeric(dimnames(Selectivity@MeanAtLength)[[2]])
  Selectivity
}

GetSS_RetentionAtLength <- function(st, fl, replist, mainyrs) {
  RetainAtLength <- replist$sizeselex[replist$sizeselex$Fleet == fl & 
                                        replist$sizeselex$Sex == st & 
                                        replist$sizeselex$Factor == "Ret", ] |>
    dplyr::filter(Yr %in% mainyrs)
  
  RetainYears <- RetainAtLength$Yr
  LengthClasses <- suppressWarnings(as.numeric(colnames(RetainAtLength)))
  LengthClasses <- LengthClasses[!is.na(LengthClasses)]
  RetainAtLength <- RetainAtLength[,as.character(LengthClasses)] 
  RetainAtLength <- t(RetainAtLength)
  
  dimnames(RetainAtLength) <- list(Class=LengthClasses,
                                   TimeStep=RetainYears)
  RetainAtLength
}

SS2Retention <- function(st, fl, RepList, mainyrs, Selectivity, Stock) {
  Retention <- Retention()
  Retention@Pars <- list()
  
  MeanAtLength <-  purrr::map(RepList, \(replist)
                              GetSS_RetentionAtLength(st, fl, replist, mainyrs)
  ) |> abind::abind(along=3,
                    use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(MeanAtLength))[[3]] <- 'Sim'
  MeanAtLength[!is.finite(MeanAtLength)] <- 0
  
  Retention@MeanAtLength <- MeanAtLength |> aperm(c('Sim', 'Class', 'TimeStep')) |>
    ArrayReduceDims()
  LengthClasses <- as.numeric(dimnames(Retention@MeanAtLength)[[2]])
  Retention@Classes <- LengthClasses
  
  
  AgeClasses <- Stock@Ages@Classes
  # TODO - doesn't account for time-varying growth
  ALK <- purrr::map(RepList, \(replist) GetSSALK(st, replist, AgeClasses, LengthClasses)) |>
    abind::abind(along=3,
                 use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(ALK))[[3]] <- 'Sim'
  ALK <- ALK |> AddDimension('TimeStep', mainyrs[1])
  ALK <- aperm(ALK, c('Sim', 'Age', 'Class', 'TimeStep'))
  Stock@Length@ASK <- ALK

  temp <- MeanAtLength2MeanAtAge(object=Retention, 
                                 Length=Stock@Length, 
                                 Ages=Stock@Ages, 
                                 nsim=1, 
                                 TimeSteps=mainyrs,
                                 max1=FALSE)
  
  Retention@MeanAtAge <-  temp@MeanAtAge |> ArrayReduceDims() # , Selectivity@MeanAtAge)
  Retention
}


GetSS_EmpiricalWeight <- function(st, fl, replist, mainyrs) {
  
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
  Weight_at_Age_array
}

SS2WeightFleet <- function(st, fl, RepList, mainyrs) {
  
  Weight_at_Age_array <- purrr::map(RepList, \(replist)
                                    GetSS_EmpiricalWeight(st, fl, replist, mainyrs) 
                                    ) |> 
    abind::abind(along=3,
                 use.first.dimnames=TRUE, use.dnns=TRUE)
  names(dimnames(Weight_at_Age_array))[[3]] <- 'Sim'
  
  Weight_at_Age_array <- Weight_at_Age_array |> aperm(c('Sim', 'Age', 'TimeStep'))
  Weight_at_Age_array |> ArrayReduceDims()
}

SS2Fleet <- function(st, fl, RepList, mainyrs, Stock) {
  nyears <- length(mainyrs)
  FleetNames <- RepList[[1]]$catch$Fleet_Name |> unique()
  Fleet <- Fleet(FleetNames[fl])
  
  Fleet@Effort <- SS2Effort(st, fl, RepList, mainyrs)
  Fleet@DiscardMortality <- SS2DiscardMortality(st, fl, RepList, mainyrs, Stock)
  Fleet@Selectivity <- SS2Selectivity(st, fl, RepList, mainyrs, Stock)
  Fleet@Retention <- SS2Retention(st, fl, RepList, mainyrs, Fleet@Selectivity, Stock)
  Fleet@WeightFleet <- SS2WeightFleet(st, fl, RepList, mainyrs)
  Fleet
}

## Import SS  ----
#' Import an OM from SS3 Output
#' @param x Either a character string  
#' @export
ImportSS <- function(x,     
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
                     ...) {
  OnExit()
  RepList <- ImportSSReport(x, silent, ...)
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
           DataLag=DataLag)
  
  OM@nSim <- nSim
  FirstHistYear <- RepList[[1]]$startyr
  LastHistYear <- RepList[[1]]$endyr
  
  mainyrs <- FirstHistYear:LastHistYear
  OM@nYear <- length(mainyrs)
  OM@pYear <- pYear
  OM@CurrentYear <- max(mainyrs)
  ProYears <- seq(max(mainyrs)+1, by=1, length.out=pYear)
  OM@TimeSteps <- c(mainyrs, ProYears)
  
  if (is.null(StockName)) {
    if (nStock==1) {
      StockName <- 'Female'
    } else if (nStock==2) {
      StockName <- c('Female', 'Male')
    } else {
      cli::cli_abort('`nStock` should be {.val {1} or {2}}')
    }
  }
  
  if (length(StockName)!=nStock)
    cli::cli_abort('`StockName` ({.val {StockName}}) should be length `nStock` ({.val {nStock}})')
  

  OM@Stock <- purrr::map(seq_along(StockName), \(st) {
    stock <- SS2Stock(st, RepList, pYear, nSim=nSim)
    stock@Name <- StockName[st]
    stock@CommonName <- CommonName[st]
    stock@Species <- Species[st]
    stock
  })
  names(OM@Stock) <- StockName
  
  OM@Fleet <- MakeNamedList(StockName, list())
  SSFleetNames <- RepList[[1]]$catch$Fleet_Name |> unique()
  nFleet <- length(SSFleetNames)
  if (is.null(FleetNames)) 
    FleetNames <- SSFleetNames
    
  if (length(FleetNames)!=nFleet)
    cli::cli_abort('`FleetNames` should be length `nFleet`: {.val {nFleet}}')
  
  
  for (st in seq_along(OM@Fleet)) {
    OM@Fleet[[st]] <- MakeNamedList(FleetNames, new('fleet'))
    for (fl in seq_along(FleetNames)) {
      OM@Fleet[[st]][[fl]] <- SS2Fleet(st, fl, RepList, mainyrs, OM@Stock[[st]])
    }
  }
  
  OM@Data <- list(ImportSSData(RepList, OM@Name))
  names(OM@Data) <- paste(StockName, collapse=' ')
  
  # Obs
  SurveyNames <- OM@Data[[1]]@Survey@Name
  AllFleetNames <- c(FleetNames, CPUENames) |> unique()
  OM@Obs <- MakeNamedList(names(OM@Data), MakeNamedList(AllFleetNames, new('obs')))
  OM <- ProcessSurveyObsSelectivity(OM, RepList)
  
  # OM@Imp - TODO 
  
  Allocation <- MakeNamedList(StockName)
  AgeClasses <- GetSSAgeClasses(RepList[[1]])
  
  for (st in 1:nStock) {
    CatchFrac <-  RepList[[1]]$catage |> DropXXCols() |> 
      dplyr::filter(Sex==st, Yr==max(mainyrs)) |>
      tidyr::pivot_longer(as.character(AgeClasses)) |>
      dplyr::group_by(Fleet) |>
      dplyr::summarise(Catch=sum(value), .groups='drop') |>
      dplyr::reframe(Catch=Catch/sum(Catch)) |> 
      dplyr::pull(Catch)
                    
    Allocation[[st]] <-  array(CatchFrac, 
                               dim=c(1, nFleet), 
                               dimnames = list(Sim=1,
                                               Fleet=FleetNames))
    
  }
  OM@Allocation <- Allocation
  

  # OM@Efactor - TODO - update to different name 
  # OM@Complexes
  # OM@SexPars
  # OM@Relations
  
  # CatchFrac
  # Data
  # etc 
  
  OM
}



ProcessSurveyObsSelectivity <- function(OM, RepList) {
  # Add Selectivity to Obs for Survey indices
  IndexInd <- which(grepl('Obs', OM@Data[[1]]@Survey@Selectivity))
  if (length(IndexInd)>0) {
    Survey_Ind <- which(!RepList[[1]]$IsFishFleet)
    nStock <- nStock(OM)
    
    for (ind in Survey_Ind) {
      SelectAtAge <- purrr::map(RepList, \(replist) {
        selectList <- list()
        for (st in 1:nStock) {
          selectList[[st]] <- GetSS_SelectivityAtAge(st=st, ind, replist, mainyrs) |> 
            ArrayReduceDims()
        }
        names(selectList) <- StockNames(OM)
        List2Array(selectList, 'Stock')
      }) |>
        List2Array('Sim') |> aperm(c('Sim', 'Stock', 'Age', 'TimeStep'))
      OM@Obs[[1]][[ind]]@Survey@Selectivity <- SelectAtAge
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
ImportSSReport <- function(x, silent=FALSE, parallel=TRUE, ...) {
  OnExit()
  if (inherits(x, 'list')) {
    if (inherits(x[[1]], 'list')) {
      names(x) <- 1:length(x)
      return(x)
    } else if (!is.null(x$SS_version)) {
      RepList <- list(x)
      names(RepList) <- 1:length(RepList)
      return(RepList)
    } else {
      cli::cli_abort('`x` is a list but does not appear to be generated by `r4ss::SS_output`')
    }
  } 
  if (inherits(x, 'character')) {
    if (length(x)>1) {
      # 
      AllSSFiles <- lapply(x, list.files)
      ReportExists <- lapply(AllSSFiles, function(x) sum(grepl('^Report.sso', x))) |> 
        unlist()
      ind <- which(ReportExists<1)
      if (length(ind)>0) {
        cli::cli_alert_warning('Warning: SS3 output is not available in {?directory/directories}: {.val {basename(x[ind])}}. \nSkipping {?this/these} {?directory/directories} ...')
        x <- x[-ind]
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
      
      RepList <- purrr::map(x, \(SSdir) GetSSRepList(SSdir, silent=TRUE, ...),
                            .progress=  list(
                              caller = environment(),
                              format =  'Reading SS3 Output from {.val {length(x)}} directories {cli::pb_bar} {cli::pb_percent}')
      )
      
      names(RepList) <- 1:length(RepList)
      return(RepList)
    } else {
      RepList <- list(GetSSRepList(x, silent, ...))
      names(RepList) <- 1:length(RepList)
      return(RepList)
    }
  }
}

## Import SS Data ----

ImportSSData <- function(x,  
                         Name="Imported by ImportSSData", 
                         CommonName = "", 
                         Species = "",
                         silent=FALSE, ...) {
  OnExit()
  RepList <- ImportSSReport(x, silent, ...)
  replist <- RepList[[1]]
  nStock <- replist$nsexes
  
  mainyrs <- replist$startyr:replist$endyr
  
  # SeasonsAsYears <- function(replist) {
  #   if (replist$nseasons == 1 && replist$seasduration < 1) {
  #     cli::cli_abort('Seasonal SS3 not done yet', .internal=TRUE)
  #     return(TRUE)
  #   }
  #   
  # }
  

  # TODO - add this to Data@Log 
  season_as_years <- FALSE
  if (replist$nseasons == 1 && replist$seasduration < 1) {
    cli::cli_alert_warning('Seasonal SS3 not done yet in `ImportSSData.` Not importing Data')
    return(new('data'))
    if (!silent) message(paste("Season-as-years detected in SS model. There is one season in the year with duration of", replist$seasduration, "year."))
    season_as_years <- TRUE
    nseas <- 1/replist$seasduration
    if (!silent) message("MSEtool operates on annual basis. Since the SS model is seasonal, we need to aggregate over seasons.\n")
  } else {
    nseas <- replist$nseasons
    if(nseas > 1) {
      cli::cli_abort('Seasonal SS3 not done yet', .internal=TRUE)
      if (!silent) message("MSEtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
    }
  }
  
  # TODO - life history 
  
  # Create Data object
  Data <- new('data')
  Data@Name <- Name
  Data@CommonName <- CommonName
  Data@Species <- Species
  # Data@Agency
  # Data@Author
  # Data@Email
  # Data@Region
  # Data@Latitude
  # Data@Longitude
  
  Data@TimeSteps <- mainyrs
  Data@TimeStepLH <- max(mainyrs)
  Data@TimeUnits <- 'year'
  Data@TimeStepsPerYear <- 1
  Data@nArea <- 1 

  Data@Removals <- ImportSSData_Catch(replist, 'Removals', silent)
  Data@Landings <- ImportSSData_Catch(replist, 'Landings', silent)
  
  Data@CPUE <- ImportSSData_Index(replist, 'CPUE')
  Data@Survey <- ImportSSData_Index(replist, 'Survey')
  
  Data@CAA
  Data@CAL
  # Data@Misc
  Data
}

ImportSSData_Catch <- function(replist, Type=c('Removals', 'Landings'), silent=FALSE ) {
  Type <- match.arg(Type)
  CatchOut <- new('catchdata')
  
  mainyrs <- replist$startyr:replist$endyr
  nTS <- length(mainyrs)
  FleetNames <- replist$catch$Fleet_Name |> unique()
  nFleet <- length(FleetNames)
  
  CatchOut@Name <- FleetNames
  CatchOut@Value <- array(NA, 
                          dim=c(nTS, nFleet),
                          dimnames = list(TimeStep=mainyrs,
                                          Fleet=FleetNames))
  
  CatchOut@CV <- CatchOut@Value
  CatchOut@Units <-   sapply(replist$catch_units[replist$IsFishFleet], function(x)
    switch(x, '1'='Biomass', '2'='Number'))
  
  CatchOut@Type <- rep(Type, nFleet)
  
  CatchColNames <- names(replist$catch)
  if (Type=='Removals') {
    CatchDF <- replist$catch |> dplyr::filter(Yr%in%mainyrs) 
    if ('dead_bio' %in% CatchColNames) {
      CatchDF <- CatchDF |> dplyr::select(Year=Yr, Fleet=Fleet, Obs=dead_bio)
    } else if ('kill_bio' %in% CatchColNames) {
      CatchDF <- CatchDF |> dplyr::select(Year=Yr, Fleet=Fleet, Obs=kill_bio)
    } else {
      cli::cli_abort("Neither 'kill_bio' or 'dead_bio' found in this SS3 output")
    }
    
  } else {
    CatchDF <- replist$catch |> dplyr::filter(Yr%in%mainyrs) 
    if ('ret_bio' %in% CatchColNames) {
      CatchDF <- CatchDF |> dplyr::select(Year=Yr, Fleet=Fleet, Obs=ret_bio)
    } else {
      cli::cli_abort("'ret_bio' not found in this SS3 output")
    }
  }

  CatchDF <- tidyr::pivot_wider(CatchDF, values_from = Obs, names_from='Fleet')
  CatchColumns <- 2:ncol(CatchDF)
  CatchOut@Value[] <- as.matrix(CatchDF[,CatchColumns, drop=FALSE])
  
  # TODO aggregate over seasons?

  
  # TODO CatchCV
  
  # CatchSD <- replist$catch_error
  # if(is.null(CatchSD) && packageVersion("r4ss") > '1.34') CatchSD <- replist$catch_se
  # if(!all(is.na(CatchSD))) {
  #   CatchSD <- CatchSD[!is.na(CatchSD)]
  #   CatchSD[CatchSD <= 0] <- NA
  #   if(packageVersion("DLMtool") < '5.4') {
  #     Csd_weighted <- stats::weighted.mean(CatchSD, colSums(cbind(cat_weight, cat_numbers)), na.rm = TRUE)
  #     Data@CV_Cat <- sqrt(exp(Csd_weighted^2) - 1)
  #     if (!silent) message(paste0("CV of Catch (weighted by catch of individual fleets), Data@CV_Cat = ", round(Data@CV_Cat, 2)))
  #   } else {
  #     Csd_weighted <- colSums(t(cbind(cat_weight, cat_numbers)) * CatchSD, na.rm = TRUE)/rowSums(cbind(cat_weight, cat_numbers))
  #     Data@CV_Cat <- matrix(sqrt(exp(Csd_weighted^2) - 1), 1)
  #     if (!silent) message("Annual CV of Catch (Data@CV_Cat) is weighted by catch of individual fleets. Range: ",
  #                          paste(signif(range(Data@CV_Cat, na.rm = TRUE), 3), collapse = " - "))
  #   }
  # }
  
  
  CatchOut
}

ImportSSData_Index <- function(replist, Type=c('CPUE', 'Survey'), 
                               season_as_years = FALSE, 
                               nseas = 1, 
                               index_season = "mean") {

  Type <- match.arg(Type)
  
  mainyrs <- replist$startyr:replist$endyr
  Indices <- new('indicesdata')
  
  CPUE <- replist$cpue
  
  if(nrow(CPUE) == 0) return(Indices)
  
  if (Type=='CPUE') {
    IndFleets <- which(replist$IsFishFleet)
  } else {
    IndFleets <- which(!replist$IsFishFleet)
  }
  
  if (length(IndFleets)<1)
    return(Indices)

  CPUE <- CPUE |> dplyr::filter(Fleet%in%IndFleets)
  mainyrs <- replist$startyr:replist$endyr
  nTS <- length(mainyrs)
  
  
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
  
  Indices@Value <- array(NA, 
                       dim=c(nTS, nIndex),
                       dimnames = list(TimeStep=mainyrs,
                                       Fleet=as.character(CPUENames)))
  Indices@CV <- Indices@Value
  
  Indices@Timing <- rep(0, nIndex) # assume at beginning of time step
  
  # TODO season_as_years and nyears>1
  Value <- purrr::imap(CPUE_Split, \(cpue, idx) {
    index <-  array(NA, c(length(mainyrs),1), 
                    dimnames = list(TimeStep=mainyrs,
                                    Fleet=CPUENames[idx])
    )
    
    TSind <- match(cpue$Yr, mainyrs)
    index[TSind,] <- cpue |> dplyr::filter(Yr%in% mainyrs) |>
      dplyr::pull('Obs')
    index
  }) 

  CV <- purrr::imap(CPUE_Split, \(cpue, idx) {
    index <-  array(NA, c(length(mainyrs),1), 
                    dimnames = list(TimeStep=mainyrs,
                                    Fleet=CPUENames[idx])
    )
    
    TSind <- match(cpue$Yr, mainyrs)
    index[TSind,] <- cpue |> dplyr::filter(Yr%in% mainyrs) |>
      dplyr::pull('SE')
    index
  })
  
  Indices@Value[] <- do.call('cbind', Value)
  Indices@CV[] <- do.call('cbind', CV)

  Indices@Units <- sapply(replist$survey_units[CPUE_Ind], function(x)
    switch(as.character(x), 
           '0'="Number",
           '1'='Biomass', 
           '2'='F')) |> unlist()
  
  if (Type=='CPUE') {
    Indices@Selectivity <- IndFleets
  } else {
    Indices@Selectivity <- rep('Obs', length(CPUE_Ind))
  }
  
  
  # Biomass (total)
  # SBiomass
  # Numeric array - nrow n age classes - maximum of 1 ncol nfleet
  # Numeric array - nrow n length classes - maximum of 1 ncol nfleet
  # Fleet Number - map to a fleet
  # Character - Obs - 

  
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
    dplyr::rename(TimeStep=Yr, Stock=Sex) |>
    tidyr::pivot_longer(cols=as.character(AgeClasses)) |>
    dplyr::group_by(Stock, TimeStep) |>
    dplyr::summarise(Value=sum(value), Model='SS3', .groups='drop')
  
  NumberSS$Stock <- unique(NumberHist$Stock)[NumberSS$Stock]
  
  NumberDF <- dplyr::bind_rows(NumberHist, NumberSS)
  
  p1 <- ggplot(NumberDF, ggplot2::aes(x=TimeStep, y=Value, color=Model)) +
    ggplot2::facet_grid(~Stock) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()
  
  pDF <- NumberDF |> dplyr::group_by(Stock, TimeStep) |>
    dplyr::summarise(Mean=mean(Value[Model=='SS3']/Value[Model!='SS3']))
  
  p2 <- ggplot2::ggplot(pDF, ggplot2::aes(x=TimeStep, y=Mean, color=Stock)) +
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
  
  HistLandings <- Landings(Hist, ByFleet=TRUE) |>
    dplyr::mutate(Model='Import') |>
    dplyr::filter(Sim==1) |>
    dplyr::group_by(TimeStep, Fleet, Model) |>
    dplyr::summarise(Value=sum(Value))
  
  SS3Landings <- replist$catch |> dplyr::filter(Yr %in% mainyrs) |>
    dplyr::select(TimeStep=Yr,  Fleet, Value=ret_bio) |>
    dplyr::mutate(Model='SS3')
  
  
  SS3Landings$Fleet <- Hist@OM@Fleet[[1]]@Name[SS3Landings$Fleet]
  SS3Landings$Sim <- 1
  
  df <- dplyr::bind_rows(
    HistLandings,
    SS3Landings
  ) |> 
    dplyr::group_by(TimeStep, Model, Fleet) |>
    dplyr::summarise(Value=sum(Value))
  
  p1 <- ggplot(df, aes(x=TimeStep, y=Value, color=Model, linetype=Model)) +
    facet_wrap(~Fleet, ncol=3, scales='free') +
    geom_line() +
    theme_bw()
  
  pDF <- df |> dplyr::group_by(Fleet, TimeStep) |>
    dplyr::summarise(Mean=mean(Value[Model=='SS3']/Value[Model!='SS3']))
  
  p2 <- ggplot(pDF, aes(x=TimeStep, y=Mean, color=Fleet)) +
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
  
  HistRemovals <- Removals(Hist, ByFleet=TRUE) |>
    dplyr::mutate(Model='Import') |>
    dplyr::filter(Sim==1) |>
    dplyr::group_by(TimeStep, Fleet, Model) |>
    dplyr::summarise(Value=sum(Value))
  
  SS3Removals <- replist$catch |> dplyr::filter(Yr %in% mainyrs)
  if ('dead_bio' %in% names(SS3Removals)){
    SS3Removals <- SS3Removals |> 
      dplyr::select(TimeStep=Yr,  Fleet, Value=dead_bio) |>
      dplyr::mutate(Model='SS3')
  } else {
    SS3Removals <- SS3Removals |> 
      dplyr::select(TimeStep=Yr,  Fleet, Value=kill_bio) |>
      dplyr::mutate(Model='SS3')
  }

  SS3Removals$Fleet <- Hist@OM@Fleet[[1]]@Name[SS3Removals$Fleet]
  SS3Removals$Sim <- 1
  
  df <- dplyr::bind_rows(
    HistRemovals,
    SS3Removals
  ) |> 
    dplyr::group_by(TimeStep, Model, Fleet) |>
    dplyr::summarise(Value=sum(Value))
  
  p1 <- ggplot(df, aes(x=TimeStep, y=Value, color=Model, linetype=Model)) +
    facet_wrap(~Fleet, ncol=3, scales='free') +
    geom_line() +
    theme_bw()
  
  pDF <- df |> dplyr::group_by(Fleet, TimeStep) |>
    dplyr::summarise(Mean=mean(Value[Model=='SS3']/Value[Model!='SS3']))
  
  p2 <- ggplot(pDF, aes(x=TimeStep, y=Mean, color=Fleet)) +
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
                   OM=c(SBMSY(Hist) |> dplyr::filter(Sim==1, TimeStep==max(TimeStep)) |> dplyr::pull(Value),
                           SPMSY(Hist) |> dplyr::filter(Sim==1, TimeStep==max(TimeStep)) |> dplyr::pull(Value),
                           MSY(Hist) |> dplyr::filter(Sim==1, TimeStep==max(TimeStep)) |> dplyr::pull(Value),
                           MSY(Hist, type='Landings') |> dplyr::filter(Sim==1, TimeStep==max(TimeStep)) |> dplyr::pull(Value))
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

