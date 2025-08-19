OM2stock <- function(OM, cpars=NULL, TimeSteps=NULL, nSim, seed=NULL) {
  stock <- Stock()
  if (inherits(OM, 'OM')) {
    stock@Name <- SubOM(OM, 'Stock')@Name
  } else {
    stock@Name <- OM@Name
  }
  
  stock@CommonName <- OM@Common_Name
  stock@Species <- OM@Species
  stock@Ages <- Ages(OM@maxage)
  
  Length(stock) <- OM2Length(OM, cpars, TimeSteps) |> 
    PopulateLength(Ages=stock@Ages,
                   nsim=nSim,
                   as.numeric(unlist(TimeSteps)),
                   ASK=TRUE,
                   seed)
  
  Weight(stock) <- OM2Weight(OM, cpars) |>
    PopulateWeight(Ages=stock@Ages,
                   Length=stock@Length,
                   nsim=nSim,
                   as.numeric(unlist(TimeSteps)),
                   ASK=FALSE,
                   seed=seed)
  
  NaturalMortality(stock) <- OM2NaturalMortality(OM, cpars) |>
    PopulateNaturalMortality(
      Ages=stock@Ages,
      Length=stock@Length,
      nsim=nSim,
      TimeSteps=as.numeric(unlist(TimeSteps)),
      seed=seed
    )
  
  Maturity(stock) <- OM2Maturity(OM, cpars) |>
    PopulateMaturity(Ages=stock@Ages,
                     Length=stock@Length,
                     Weight=stock@Weight,
                     nsim=nSim,
                     TimeSteps=as.numeric(unlist(TimeSteps)),
                     CalcAtLength=TRUE,
                     seed=seed)
  
  Fecundity(stock) <- OM2Fecundity(OM, cpars) |> 
    PopulateFecundity(
      Ages=stock@Ages,
      Length=stock@Length,
      Weight=stock@Weight,
      Maturity=stock@Maturity,
      nsim=nSim,
      TimeSteps=as.numeric(unlist(TimeSteps)),
      seed=seed
    )
    
  SRR(stock) <- OM2SRR(OM, cpars, TimeSteps) |>
    PopulateSRR(MaxAge = stock@Ages@MaxAge,
                CurrentYear = max(TimeSteps$HistTS),
                TimeSteps=as.numeric(unlist(TimeSteps)),
                nsim=nSim,
                seed=seed)
  
  Spatial(stock) <- OM2Spatial(OM, cpars, TimeSteps) |>
    PopulateSpatial(Ages=stock@Ages,
                    TimeSteps=as.numeric(unlist(TimeSteps)),
                    nsim=nSim,
                    seed=seed)
    
    
  Depletion(stock) <- OM2Depletion(OM, cpars) |>
    PopulateDepletion(nSim, seed)
  

  stock
}

OM2Length <- function(OM, cpars=NULL, TimeSteps=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    Length <- cpars2Length(cpars)
  } else {
    Length <- Length()
  }
  
  pars <- Pars(Length)
  if (!is.numeric(pars$Linf)) {
    pars$Linf <- OM@Linf
  }
  if (!is.numeric(pars$K)) {
    pars$K <- OM@K
  }
  
  if (!is.numeric(pars$t0)) {
    pars$t0 <- OM@t0
  }
  Pars(Length) <- pars
  
  if (!all(OM@Linfsd==0)) {
    Length@Pars$Linfsd <- OM@Linfsd
  }
  if (!all(OM@Ksd==0)) {
    Length@Pars$Ksd <- OM@Ksd
  }
  Length@CVatAge <- OM@LenCV
  
  # ASK
  if (!is.null(Length@Classes)) {
    dd <- dim(Length@MeanAtAge)
    AllTimeSteps <- c(TimeSteps$HistTS, TimeSteps$ProjTS)
    DimNames <- list(Sim=1:dd[1],
                     Age=0:OM@maxage,
                     TimeSteps=AllTimeSteps[1:dd[3]])
    dimnames(Length@MeanAtAge) <- DimNames
    dimnames(Length@CVatAge) <- DimNames
    
    Length@ASK <- CalcAgeSizeKey(MeanAtAge=Length@MeanAtAge, 
                                 CVatAge=Length@CVatAge,
                                 Classes=Length@Classes,  
                                 TruncSD=Length@TruncSD)
  }
  
  Length
}

cpars2Length <- function(cpars) {
  Length <- Length()
  MeanAtAge(Length) <- process_cpars(cpars$Len_age)
  CVatAge(Length) <- process_cpars(cpars$LatASD) / MeanAtAge(Length)
  CVatAge(Length)[!is.finite(CVatAge(Length))] <- tiny
  
  Classes(Length) <- cpars$CAL_binsmid
  pars <- Pars(Length)
  pars$Linf <- process_cpars(cpars$Linf)
  pars$K <- process_cpars(cpars$K)
  pars$t0  <-  process_cpars(cpars$t0)
  Pars(Length) <- pars
  Length
}

OM2Weight <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    Weight <- cpars2Weight(cpars)
  } else {
    Weight <- Weight()
  }
  
  pars <- list()
  if (!is.numeric(pars$alpha)) {
    pars$alpha <- OM@a
  }
  if (!is.numeric(pars$beta)) {
    pars$beta <- OM@b
  }
  Pars(Weight) <- pars
  Weight
}

cpars2Weight <- function(cpars) {
  Weight <- Weight()
  MeanAtAge(Weight) <- process_cpars(cpars$Wt_age)
  pars <- list()
  pars$a <- process_cpars(cpars$Wa)
  pars$b <- process_cpars(cpars$Wb)
  Pars(Weight) <- pars
  Weight
}

OM2NaturalMortality <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    NaturalMortality <- cpars2NaturalMortality(cpars)
  } else {
    NaturalMortality <- NaturalMortality()
  }
  pars <- Pars(NaturalMortality)
  if (!is.numeric(pars$M)) {
    pars$M <- OM@M
  }
  if (!all(OM@Msd==0)) {
    if (!is.numeric(pars$Msd)) {
      pars$Msd <- OM@Msd    }
  }
  Pars(NaturalMortality) <- pars
  NaturalMortality
}

cpars2NaturalMortality <- function(cpars) {
  NaturalMortality <- NaturalMortality()
  MeanAtAge(NaturalMortality) <- process_cpars(cpars$M_ageArray)
  pars <- Pars(NaturalMortality)
  pars$M <- process_cpars(cpars[['M']])
  pars$Msd <- process_cpars(cpars[['Msd']])
  Pars(NaturalMortality) <- pars
  NaturalMortality
}

OM2Maturity <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    Maturity <- cpars2Maturity(cpars)
  } else {
    Maturity <- Maturity()
  }
  
  pars <- Pars(Maturity)
  if (!is.numeric(pars$L50)) {
    l50 <- OM@L50
    if (!all(l50==0))
      pars$L50 <- l50
  }
  if (!is.numeric(pars$L50_95)) {
    L50_95 <- OM@L50_95
    if (!all(L50_95==0))
      pars$L50_95 <- L50_95
  }
  Pars(Maturity) <- pars
  Maturity
}

cpars2Maturity <- function(cpars) {
  Maturity <- Maturity()
  MeanAtAge(Maturity) <- process_cpars(cpars$Mat_age)
  pars <- Pars(Maturity)
  pars$L50 <- process_cpars(cpars$L50)
  pars$L50_95 <- process_cpars(cpars$L50_95)
  Pars(Maturity) <- pars
  Maturity
}

OM2Fecundity <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    Fecundity <- cpars2Fecundity(cpars)
  } else {
    Fecundity <- Fecundity()
  }
  Fecundity
}

cpars2Fecundity <- function(cpars) {
  Fecundity <- Fecundity()
  MeanAtAge(Fecundity) <- process_cpars(cpars$Fec_age)
  Fecundity
}

switchSRR <- function(SRrel) {
  if (is.null(SRrel))
    return(NULL)
  switch(SRrel,
         '1'='BevertonHolt',
         '2'='Ricker')
}

OM2SRR <- function(OM, cpars=NULL, TimeSteps=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    SRR <- cpars2SRR(cpars)
  } else {
    SRR <- SRR()
  }
  
  pars <- Pars(SRR)
  if (!is.numeric(pars[['h']])) {
    pars[['h']] <- OM@h
  }
  Pars(SRR) <- pars
  
  SRR@Model <- switchSRR(OM@SRrel[1])
  if (!is.finite(SRR@R0) || is.null(SRR@R0)) 
    SRR@R0 <- OM@R0
  if (!is.finite(SRR@SD) || is.null(SRR@SD))
    SRR@SD <- OM@Perr
  if (!is.finite(SRR@AC) || is.null(SRR@AC)) 
    SRR@AC <- OM@AC
  
  # Add dimnames
  if (!is.null(SRR@RecDevInit) && !all(is.na(SRR@RecDevInit))) {
    dimnames(SRR@RecDevInit) <- list(
      Sim=1:nrow(SRR@RecDevInit),
      Age=1:ncol(SRR@RecDevInit)
    )
  }
  
  if (!is.null(SRR@RecDevHist)  && !all(is.na(SRR@RecDevHist))) {
    dimnames(SRR@RecDevHist) <- list(
      Sim=1:nrow(SRR@RecDevHist),
      TimeStep= TimeSteps$HistTS
    )
  }
  
  if (!is.null(SRR@RecDevProj)  && !all(is.na(SRR@RecDevProj))) {
    dimnames(SRR@RecDevProj) <- list(
      Sim=1:nrow(SRR@RecDevProj),
      TimeStep= TimeSteps$ProjTS
    )
  }
  
  SRR
}

cpars2SRR <- function(cpars, nYear=NULL, maxage=NULL) {
  SRR <- SRR()
  Pars(SRR)$h <- process_cpars(cpars$h)
  SRR@R0 <- process_cpars(cpars$R0)
  SRR@Model <- switchSRR(cpars$SRrel[1])
  SRR@SD <- process_cpars(cpars[['Perr']])
  SRR@AC <- process_cpars(cpars[['AC']])
  
  # Perr_y
  perr_y <- cpars[['Perr_y']]
  if (!is.null(perr_y)) {
    if (is.null(nYear)) {
      nYear <- dim(cpars$Find)[2]
    }
    if (is.null(maxage)) {
      maxage <- dim(cpars$Len_age)[2] -1
    }
    
    if (is.null(nYear))
      stop("`nYear` not found")
    if (is.null(maxage))
      stop("`maxage` not found")
    
    dd <- dim(perr_y)[2]
    proyears <- dd - nYear - maxage
    
    
    init_age_classes <- perr_y[,1:maxage]
    
    if (IdenticalSim(init_age_classes)) {
      SRR@RecDevInit <- matrix(init_age_classes[1,], nrow=1)
    } else {
      SRR@RecDevInit <- init_age_classes
    }
    
    hist_yrs <- perr_y[,(maxage+1):(nYear+maxage)]
    if (IdenticalSim(hist_yrs)) {
      SRR@RecDevHist <- matrix(hist_yrs[1,], nrow=1)
    } else {
      SRR@RecDevHist <- hist_yrs
    }
    
    pro_yrs <- perr_y[,(nYear+maxage+1):(nYear+maxage+proyears)]
    if (IdenticalSim(pro_yrs)) {
      SRR@RecDevProj <- matrix(pro_yrs[1,], nrow=1)
    } else {
      SRR@RecDevProj <- pro_yrs
    }
  }
  
  if (!is.null(cpars$spawn_time_frac))
    SRR@SpawnTimeFrac <- cpars$spawn_time_frac[1]
  
  SRR
}

OM2Depletion <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  if (!EmptyObject(cpars)) {
    Depletion <- cpars2Depletion(cpars)
  } else {
    Depletion <- Depletion()
  }
  
  if (length(Depletion@Final)<1) 
    Depletion@Final <- OM@D
  Depletion
}

OM2Spatial <- function(OM, cpars=NULL, TimeSteps=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  if (!EmptyObject(cpars)) {
    Spatial <- cpars2Spatial(cpars, TimeSteps)
  } else {
    Spatial <- Spatial()
  }
  
  narea <- NA
  if (!is.null(Spatial@Movement)) {
    dd <- dim(Spatial@Movement)
    narea <- dd[2]
    nsim <- dd[1]
    if (is.null(Spatial@RelativeSize)) {
      Spatial@RelativeSize <- array(1/narea, dim=c(1, narea))
      dimnames(Spatial@RelativeSize) <- list(Sim=1,
                                             Area=1:narea)
    }
  } else {
    Spatial@RelativeSize <- OM@Size_area_1
    Spatial@ProbStaying <- OM@Prob_staying
    Spatial@UnfishedDist <- OM@Frac_area_1
  }
  if (all(Spatial@RelativeSize == 0.5) &
      all(Spatial@ProbStaying == 0.5) &
      all(Spatial@UnfishedDist == 0.5)) 
    return(Spatial())
  Spatial
}

process_mov <- function(mov, nage=1, nts=1) {
  dd <- dim(mov)
  if (is.null(dd)) return(NULL)
  if (length(dd)<3)
    stop('`mov` must be an array with at dimensions `nsim`, `narea`, `narea`')
  if (length(dd)==3) {
    # add age and time-step
    mov <- abind::abind(mov, array(0, dim=c(0,dd)), along=1)
    mov <- abind::abind(mov, array(0, dim=c(0,1,dd)), along=1)
    mov <- aperm(mov, c(3,1,2,4,5))
  }
  if (length(dd)==4) {
    # add time-step
    mov <- abind::abind(mov, array(0, dim=c(0,dd)), along=1)
    mov <- aperm(mov, c(2,1,3,4,5))
  }
  
  mov <- aperm(mov, c(1,4,5,3,2)) |>
    AddDimNames(c('Sim', 'Area', 'Area', 'Age', 'TimeStep')) |>
    ArrayReduceDims()
  
  mov
}

cpars2Spatial <- function(cpars, TimeSteps) {
  Spatial <- Spatial()
  Spatial@RelativeSize <- process_cpars(cpars$Asize)
  Spatial@Movement <- process_mov(cpars$mov)
  Spatial <- CalcUnfishedDist(Spatial, c(TimeSteps$HistTS, TimeSteps$ProjTS))
  Spatial
}

cpars2Depletion <- function(cpars) {
  Depletion <- Depletion()
  Depletion@Initial <- process_cpars(cpars$initD)
  Depletion@Final <- process_cpars(cpars[['D']])
  Depletion
}


