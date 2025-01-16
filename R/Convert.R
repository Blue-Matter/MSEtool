#' Convert old style and new style object classes
#' 
#' Converts `OM` and `MOM` class objects to new `om` class, and 
#' back again
#'
#' @param OM An old style `OM` or `MOM` class object, or a
#' new OM object (class `om`)
#' @param Author Optional. Character vector with author name(s)
#' @param CurrentYear Optional. Current calendar year. Otherwise will try grab from
#' the OM object if it exists
#' 
#' @return A object of class `OM`, `MOM`, or `om`
#' @export
#' 
Convert <- function(OM, Author='', CurrentYear=NULL) {
  if (methods::is(OM, 'OM') | methods::is(OM, 'MOM')) {
    return(OM2om(OM, Author, CurrentYear))
  } else if (methods::is(OM, 'om')) {
    if (nStock(OM)>1 | nFleet(OM)>1)
      return(om2MOM(OM))
    return(om2OM(OM))
  } else {
    cli::cli_abort('`OM` must be class `OM`, `MOM`, or `om`')
  }
}


OM2om <- function(OM, Author='', CurrentYear=NULL) {

  if (!methods::is(OM, 'OM') & !methods::is(OM, 'MOM'))
    cli::cli_abort('Argument `OM` must be class `OM` or `MOM`')

  cparsdrop <- cpars_info |>
    dplyr::filter(Type%in%c('Stock', 'Fleet'))
  
  om <- OM()

  om@Name <- OM@Name
  om@Agency <-  OM@Agency
  om@Region <-  OM@Region
  om@Author <- Author
  om@Longitude <- OM@Longitude
  om@Latitude <- OM@Latitude
  om@Sponsor <- OM@Sponsor
  om@nSim <- OM@nsim
  if (methods::is(OM, 'MOM')) {
    om@nYear <- OM@Fleets[[1]][[1]]@nyears
  } else {
    om@nYear <- OM@nyears
  }
  
  om@pYear <- OM@proyears
  om@Interval <- OM@interval
  om@Seed <- OM@seed
  om@pStar <- OM@pstar
  om@maxF <- OM@maxF
  om@nReps <- OM@reps
  om@Source <- OM@Source
  
  if (is.null(CurrentYear)) {
    if (methods::is(OM, 'MOM')) {
      om@CurrentYear <- OM@Fleets[[1]][[1]]@CurrentYr
    } else {
      om@CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
    }
  } else {
    om@CurrentYear <- CurrentYear
  }
    
  om@TimeUnits <- 'Year'
  om@TimeStepsPerYear <- 1
  om@TimeSteps <- CalcTimeSteps(nYear=om@nYear,
                                pYear=om@pYear,
                                CurrentYear=om@CurrentYear,
                                TimeUnits=om@TimeUnits)

  if (methods::is(OM, 'OM')) {
    om@Stock <- OM2stock(OM, cpars=OM@cpars)
    om@Fleet <- OM2fleet(OM, OM@cpars, OM@Fdisc)
    om@Obs <- SubOM(OM, 'Obs')
    om@Imp <- SubOM(OM, 'Imp')
    
    om@Stock@Length <- Populate(om@Stock@Length,
                                om@Stock@Ages,
                                om@nSim,
                                TimeSteps(om),
                                ASK=TRUE,
                                seed=om@Seed)

    om@Stock@Weight <- Populate(om@Stock@Weight,
                                om@Stock@Ages,
                                om@Stock@Length,
                                om@nSim,
                                TimeSteps(om),
                                ASK=FALSE,
                                seed=om@Seed)
    
    if (as.logical(OM@isRel)) {
      # multiply by L50 
      om@Stock@Maturity <- Populate(om@Stock@Maturity,
                                    om@Stock@Ages,
                                    om@Stock@Length,
                                    om@nSim,
                                    TimeSteps(om),
                                    CalcAtLength=TRUE,
                                    seed=om@Seed)
      
      om@Fleet@Selectivity <- Populate(om@Fleet@Selectivity,
                                       Ages=om@Stock@Ages,
                                       Length=om@Stock@Length,
                                       nsim=om@nSim,
                                       TimeSteps=om@TimeSteps,
                                       seed=om@Seed)
      
      om@Fleet@Selectivity@Pars$L5 <- MultiplyArrays(
        array1=GetLengthClass(om@Stock@Maturity),
        array2=om@Fleet@Selectivity@Pars$L5
        )
      
      om@Fleet@Selectivity@Pars$LFS <- MultiplyArrays(
        array1=GetLengthClass(om@Stock@Maturity),
        array2=om@Fleet@Selectivity@Pars$LFS
      )
      
      om@Fleet@Retention <- Populate(om@Fleet@Retention,
                                       Ages=om@Stock@Ages,
                                       Length=om@Stock@Length,
                                       nsim=om@nSim,
                                       TimeSteps=om@TimeSteps,
                                       seed=om@Seed)
      
      om@Fleet@Retention@Pars$LR5 <- MultiplyArrays(
        array1=GetLengthClass(om@Stock@Maturity),
        array2=om@Fleet@Retention@Pars$LR5
      )
      
      om@Fleet@Retention@Pars$LFR <- MultiplyArrays(
        array1=GetLengthClass(om@Stock@Maturity),
        array2=om@Fleet@Retention@Pars$LFR
      )
      
    }
    # update because Vmaxlen and Rmaxlen now correspond with maximum length class
    om <- SolveForVmaxlen(om) 
    om <- SolveForRmaxlen(om)
    om <- Populate(om, messages=FALSE)
    return(om)
  }
  
  if (methods::is(OM, 'MOM')) {
    om@Stock <- ConvertToList(MOM2stock(OM))
    names(om@Stock) <- names(OM@Stocks)
    om@Fleet <- list()
    for (st in 1:length(om@Stock)) {
      om@Fleet[[st]] <- MOM2fleet(OM, st)
      names(om@Fleet[[st]]) <- names(OM@Fleets[[st]])
    }
    om@Obs <- OM@Obs
    om@Imp <- OM@Imps
    om@CatchFrac <- OM@CatchFrac
    om@Allocation <- OM@Allocation
    om@SexPars <- ImportSexPars(OM)
    om@Efactor <- OM@Efactor
    om@Complexes<- OM@Complexes
    om@Relations <- OM@Rel
    
    om@Data <- vector('list', nStock(om))
    om@Misc$cpars <- vector('list', nStock(om))
    for (st in 1:nStock(om)) {
      om@Data[[st]] <- vector('list', nFleet(om))
      om@Misc$cpars[[st]] <- vector('list', nFleet(om))
      for (fl in 1:nFleet(om)) {
        l <- OM@cpars[[st]][[fl]]
        om@Data[[st]][[fl]] <- l$Data
        l$Data <- NULL
        l <- l[!names(l)%in%cparsdrop$Var]
        om@Misc$cpars[[st]][[fl]] <- l
      }
    }
  }
  Populate(om)
}

ImportSexPars <- function(OM) {
  sexpars <- new('sexpars')
  if (!is.null(OM@SexPars$SSBfrom)) {
    SPFrom(sexpars) <- OM@SexPars$SSBfrom
    rownames(SPFrom(sexpars)) <- names(OM@Stocks)
    colnames(SPFrom(sexpars)) <- names(OM@Stocks)
  }
    
  
  if (!is.null(OM@SexPars$Herm))
    Herm(sexpars) <- OM@SexPars$Herm
  
  if (!is.null(OM@SexPars$share_par))
    SharePar(sexpars) <- OM@SexPars$share_par
  sexpars
}


OM2stock <- function(OM, cpars=NULL) {
  stock <- Stock()
  stock@Name <- OM@Name
  stock@CommonName <- OM@Common_Name
  stock@Species <- OM@Species
  stock@Ages <- Ages(OM@maxage)
  Length(stock) <- OM2Length(OM, cpars)
  Weight(stock) <- OM2Weight(OM, cpars)
  NaturalMortality(stock) <- OM2NaturalMortality(OM, cpars)
  Maturity(stock) <- OM2Maturity(OM, cpars)
  Fecundity(stock) <- OM2Fecundity(OM, cpars)
  SRR(stock) <- OM2SRR(OM, cpars)
  Spatial(stock) <- OM2Spatial(OM, cpars)
  Depletion(stock) <- OM2Depletion(OM, cpars)
  stock
}

OM2Length <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    Length <- cpars2Length(cpars)
  } else {
    Length <- Length()
  }

  pars <- Pars(Length)
  if (!is.numeric(pars$Linf)) {
    pars$Linf <- process_cpars(OM@Linf) 
  }
  if (!is.numeric(pars$K)) {
    pars$K <- process_cpars(OM@K) 
  }
  if (!is.numeric(pars$t0)) {
    pars$t0 <- process_cpars(OM@t0) 
  }
  Pars(Length) <- pars

  if (!all(OM@Linfsd==0)) {
    Length@Pars$Linfsd <- process_cpars(OM@Linfsd) 
  }
  if (!all(OM@Ksd==0)) {
    Length@Pars$Ksd <- process_cpars(OM@Ksd) 
  }

  if (is.null(CVatAge(Length))) {
    CVatAge(Length) <- process_cpars(OM@LenCV) |> Structure()
  }
  
  # ASK
  if (!is.null(Length@Classes))
    Length@ASK <- CalcASK(MeanAtAge=Length@MeanAtAge, 
                          Length@CVatAge,
                          Length@Classes,  
                          Length@TruncSD)

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
    pars$alpha <- process_cpars(OM@a)
  }
  if (!is.numeric(pars$beta)) {
    pars$beta <- process_cpars(OM@b)
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
    pars$M <- process_cpars(OM@M)
  }
  if (!all(OM@Msd==0)) {
    if (!is.numeric(pars$Msd)) {
      pars$Msd <- process_cpars(OM@Msd)
    }
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
    l50 <- process_cpars(OM@L50)
    if (!all(l50==0))
      pars$L50 <- l50
  }
  if (!is.numeric(pars$L50_95)) {
    L50_95 <- process_cpars(OM@L50_95)
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

OM2SRR <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    SRR <- cpars2SRR(cpars)
  } else {
    SRR <- SRR()
  }

  pars <- Pars(SRR)
  if (!is.numeric(pars[['h']])) {
    pars[['h']] <- process_cpars(OM@h)
  }
  Pars(SRR) <- pars
  
  if (is.null(SRR@Model)) 
    SRR@Model <- switchSRR(OM@SRrel[1])
  if (is.null(SRR@R0)) 
    SRR@R0 <- process_cpars(OM@R0)
  if (is.null(SRR@SD))
    SRR@SD <- process_cpars(OM@Perr)
  if (is.null(SRR@AC)) 
    SRR@AC <- process_cpars(OM@AC)
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

    if (identical_sim(init_age_classes)) {
      SRR@RecDevInit <- matrix(init_age_classes[1,], nrow=1)
    } else {
      SRR@RecDevInit <- init_age_classes
    }

    hist_yrs <- perr_y[,(maxage+1):(nYear+maxage)]
    if (identical_sim(hist_yrs)) {
      SRR@RecDevHist <- matrix(hist_yrs[1,], nrow=1)
    } else {
      SRR@RecDevHist <- hist_yrs
    }

    pro_yrs <- perr_y[,(nYear+maxage+1):(nYear+maxage+proyears)]
    if (identical_sim(pro_yrs)) {
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

OM2Spatial <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  if (!EmptyObject(cpars)) {
    Spatial <- cpars2Spatial(cpars)
  } else {
    Spatial <- Spatial()
  }

  narea <- NA
  if (!is.null(Spatial@Movement)) {
    dd <- dim(Spatial@Movement)
    narea <- dd[2]
    nsim <- dd[1]
    if (is.null(Spatial@RelativeSize))
      Spatial@RelativeSize <- array(1/narea, dim=c(1, narea))

  } else {
    Spatial@RelativeSize <- process_cpars(OM@Size_area_1)
    Spatial@ProbStaying <- process_cpars(OM@Prob_staying)
    Spatial@UnfishedDist <- process_cpars(OM@Frac_area_1)
    
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
  
  # c('sim', 'area', 'area', 'age', 'ts'))
  mov <- aperm(mov, c(1,4,5,3,2)) |>
    AddDimNames(c('sim', 'area', 'area', 'age', 'ts')) |>
    process_cpars()
  
  mov
}


cpars2Spatial <- function(cpars) {
  Spatial <- Spatial()
  Spatial@RelativeSize <- process_cpars(cpars$Asize)
  Spatial@Movement <- process_mov(cpars$mov)
  Spatial <- CalcUnfishedDist(Spatial)
  Spatial
}

cpars2Depletion <- function(cpars) {
  Depletion <- Depletion()
  Depletion@Initial <- process_cpars(cpars$initD)
  Depletion@Final <- process_cpars(cpars[['D']])
  Depletion
}

AdjustEffort <- function(FishingMortality, DiscardMortality) {
  d2 <- dim(DiscardMortality@MeanAtAge)

  if (max(FishingMortality@nVessels)>0 & !is.null(d2)) {
    nyear <- FishingMortality@nYear
    if (d2[3]>1) {
      nyear <- min(d2[3], nyear)
      meanFdisc <- apply(DiscardMortality@MeanAtAge[,,1:nyear, drop=FALSE],c(1,3) , mean)
      FishingMortality@nVessels <- FishingMortality@nVessels/meanFdisc
    } else {
      meanFdisc <- apply(DiscardMortality@MeanAtAge[,,1, drop=FALSE],1 , mean)
      FishingMortality@nVessels <- FishingMortality@nVessels/meanFdisc
    }
  }
  FishingMortality
}


OM2fleet <- function(OM, cpars=NULL, Fdisc=NULL) {
  fleet <- Fleet()
  if (methods::is(OM, 'Fleet'))
    Name(fleet) <- OM@Name
  
  FishingMortality(fleet) <- OM2FishingMortality(OM, cpars)
  DiscardMortality(fleet) <- OM2DiscardMortality(OM, cpars, Fdisc)
  Effort(fleet) <- OM2Effort(OM, cpars)
  Selectivity(fleet) <- OM2Selectivity(OM, cpars)
  Retention(fleet) <- OM2Retention(OM, cpars)

  # SpatTarg
  Distribution(fleet) <- OM2Distribution(OM, cpars)

  # Weight
  Weight(fleet) <- process_cpars(cpars$Wt_age_C)

  # BioEco

  fleet

}

OM2FishingMortality <- function(OM, cpars=NULL) {
  if (is.null(cpars) & methods::is(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    FishingMortality <- cpars2FishingMortality(cpars)
  } else {
    FishingMortality <- FishingMortality()
  }
 
  FishingMortality
}

cpars2FishingMortality <- function(cpars) {
  FishingMortality <- FishingMortality()
  FishingMortality

}



OM2Selectivity <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    Selectivity  <- cpars2Selectivity(cpars)
  } else {
    Selectivity  <- Selectivity()
    Pars(Selectivity) <- list(L5=OM@L5,
                              LFS=OM@LFS,
                              Vmaxlen=OM@Vmaxlen)
  }
  Selectivity
}

cpars2Selectivity <- function(cpars) {
  Selectivity <- Selectivity()
  Selectivity@MeanAtAge <- process_cpars(cpars[['V']])
  Selectivity@MeanAtLength <- process_cpars(cpars[['SLarray']])
  Selectivity@Classes <- process_cpars(cpars$CAL_binsmid)
  Selectivity
}


OM2Retention <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    Retention  <- cpars2Retention(cpars)
  } else {
    Retention  <- Retention()
    Pars(Retention) <- list(LR5=OM@LR5,
                            LFR=OM@LFR,
                            Rmaxlen=OM@Rmaxlen)
  }

  Retention
}

cpars2Retention <- function(cpars) {
  Retention <- Retention()
  Retention@MeanAtAge <- process_cpars(cpars[['retA']])
  Retention@MeanAtLength <- process_cpars(cpars[['retL']])
  Retention@Classes <- process_cpars(cpars$CAL_binsmid)
  Retention
}


OM2Distribution <- function(OM, cpars) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    Distribution  <- cpars2Distribution(cpars)
  } else {
    Distribution <- Distribution()
  }

  # TODO still

  Distribution
}

cpars2Distribution <- function(cpars) {
  Distribution <- Distribution()
  Distribution@Closure <- process_cpars(cpars$MPA)
  Distribution
}






OM2Effort <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!EmptyObject(cpars)) {
    Effort <- cpars2Effort(cpars)
  } else {
    Effort <- Effort()
  }
  
  if (is.null(Effort@Effort)) {
    Effort@Effort <- data.frame(TimeStep=OM@EffYears,
                                Lower=OM@EffLower,
                                Upper=OM@EffUpper,
                                CV=OM@Esd[1])
  } 
  
  Effort@qCV <- OM@qcv
  Effort@qInc <- OM@qinc
  Effort
  
}

cpars2Effort <- function(cpars) {
  Effort <- Effort()
  Effort@Effort <- process_cpars(cpars$Find)
  Effort@Catchability <- process_cpars(cpars$qs)
  Effort
}


OM2DiscardMortality <- function(OM, cpars=NULL, Fdisc=NULL) {

  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars

  if (!EmptyObject(cpars)) {
    DiscardMortality <- cpars2DiscardMortality(cpars)
  } else {
    DiscardMortality <- DiscardMortality()
  }

  DiscardMortality
}

cpars2DiscardMortality <- function(cpars) {
  DiscardMortality <- DiscardMortality()

  DiscardMortality@MeanAtAge <- process_cpars(cpars$Fdisc_array1)
  DiscardMortality@MeanAtLength <- process_cpars(cpars$Fdisc_array2)
  DiscardMortality@Classes <- process_cpars(cpars$CAL_binsmid)

  DiscardMortality
}

MOM2fleet <- function(MOM, st) {

  nfleets <- length(MOM@Fleets[[1]])
  FleetNames <- names(MOM@Fleets[[1]])
  if (is.null(FleetNames))
    FleetNames <- paste('Fleet', 1:nfleets)

  FleetList <- list()

  for (fl in 1:nfleets) {
    Fleet <- MOM@Fleets[[st]][[fl]]
    cpars <- MOM@cpars[[st]][[fl]]
    FleetList[[fl]] <- OM2fleet(Fleet, cpars)
  }
  names(FleetList) <- FleetNames
  if (nfleets==1) return(FleetList[[1]])
  FleetList
}



MOM2stock <- function(MOM) {
  StockList <- list()
  stocks <- MOM@Stocks
  nstocks <- length(stocks)

  for (st in 1:nstocks) {
    Stock <- stocks[[st]]
    cpars <- MOM@cpars[[st]][[1]]
    StockList[[st]] <- OM2stock(Stock, cpars)
  }
  if (nstocks==1) return(StockList[[1]])
  StockList
}


identical_sim <- function(value) {
  dd <- dim(value)
  if (is.null(dd))
    return(mean(value) == value[1])

  temp <- apply(value, 1, mean)
  mean(temp) == temp[1]

}

identical_year <- function(value) {

  dd <- dim(value)

  if (length(dd)==2)
    return(prod(value[1,1]==value[1,dd[2]]))

  if (length(dd)==3)
    return(prod(value[1,,1]==value[1,,dd[3]]))

}

get_cpars <- function(OM, st=1, fl=1) {
  cpars <- OM@cpars
  if (!length(cpars)) return(NULL)

  if (inherits(OM, 'OM')) {
    return(OM@cpars)
  }

  if (inherits(OM, 'MOM')) {
    cpars <- try(MOM@cpars[[st]][[fl]], silent = TRUE)
    if (inherits(cpars, 'list')) {
      return(cpars)
    } else {
      return(NULL)
    }
  }
}

process_cpars <- function(value) {
  if (is.null(value)) return()
  dd <- dim(value)
  if (is.null(dd)) {
    if (identical_sim(value)) {
      return(value[1])
    } else {
      return(value)
    }
  }

  if (length(dd)==2) {
    if (identical_sim(value) & identical_year(value)) {
      return(value[1,1, drop=FALSE])
    }
    if (identical_sim(value) & !identical_year(value)) {
      return(value[1,, drop=FALSE])
    }
    if (!identical_sim(value) & identical_year(value)) {
      return(value[,1, drop=FALSE])
    }
    if (!identical_sim(value) & !identical_year(value)) {
      return(value)
    }

  }

  if (length(dd)==3) {
    if (identical_sim(value) & identical_year(value)) {
      return(value[1,,1, drop=FALSE])
    }
    if (identical_sim(value) & !identical_year(value)) {
      return(value[1,,, drop=FALSE])
    }
    if (!identical_sim(value) & identical_year(value)) {
      return(value[,,1, drop=FALSE])
    }
    if (!identical_sim(value) & !identical_year(value)) {
      return(value)
    }
  }

  if (length(dd)==4) {
    if (identical_sim(value))
      return(value[1,,,,drop=FALSE])
  }
  if (length(dd)==5) {
    if (identical_sim(value))
      return(value[1,,,,,drop=FALSE])
  }

}


getUniformBounds <- function(object,name='NaturalMortality', Par="M") {
  sl <- slot(object, name)
  val <- sl@Pars[[Par]]
  if (methods::is(val, 'array')) {
    stop('not done')
  }
  if (methods::is(val, 'numeric')) {
    if (length(val)==1)
      return(rep(val,2))
    if (length(val)==2)
      return(val)
  }
  if (is.null(val))
    return(numeric())
  val
}

setCparValues <- function(val, dim=1, nsim, nTS=NULL) {
  dd <- dim(val)
  
  if (length(dd)==2 & dim==1)
    return(rep(val[1,], nsim)[1:nsim])
  
  
  if (length(dd)==2 & dim==2) {
    if (dd[1]==nsim & dd[2]==nTS)
      return(val)
    if (dd[1]==1 & dd[2]==nTS) {
      val <- abind::adrop(replicate(nsim, val), 1) |> t()
    }
    if (dd[1]==1 & dd[2]==1) {
      val <- matrix(val, nsim, nTS)
    }
    return(val)
  }
  
  
  if (length(dd)==3) {
    if (all(dd==1))
      return(rep(val[1,1,1], nsim))
    
    
    if (dd[1]==1) {
      val <- abind::adrop(replicate(nsim, val), drop=1) |> aperm(c(3,1,2))
    }
    if (dd[3]==1) {
      val <- abind::adrop(replicate(nTS, val), drop=3) 
    }
    return(val)
    
  }
  if (is.null(dd) & dim==1)
    return(rep(val, nsim)[1:nsim])
  
  
}

getRecDevs <- function(Stock, nsim) {
  RecDevInit <- Stock@SRR@RecDevInit
  if (is.null(dim(RecDevInit)))
    RecDevInit <-  matrix(RecDevInit, nrow=nsim, ncol=length(RecDevInit), byrow=TRUE)
  
  RecDevHist <- Stock@SRR@RecDevHist
  if (is.null(dim(RecDevHist)))
    RecDevHist <- matrix(Stock@SRR@RecDevHist, nrow=nsim, ncol=length(Stock@SRR@RecDevHist), byrow=TRUE)
  
  RecDevProj <- Stock@SRR@RecDevProj
  if (is.null(dim(RecDevProj)))
    RecDevProj <- matrix(Stock@SRR@RecDevProj, nrow=nsim, ncol=length(Stock@SRR@RecDevProj), byrow=TRUE)
  
  
  cbind(RecDevInit, RecDevHist, RecDevProj)
} 


om2Stock <- function(OM, st=NULL) {
  
  stock <- new('Stock')
  if (methods::is(OM@Stock, 'list')) {
    stock@Name <- OM@Stock[[st]]@Name
    stock@Common_Name <- OM@Stock[[st]]@CommonName
    stock@Species <- OM@Stock[[st]]@Species
    stock@maxage <- OM@Stock[[st]]@Ages@MaxAge
    stock@R0 <- OM@Stock[[st]]@SRR@Pars$R0
    stock@h <- getUniformBounds(OM@Stock[[st]], 'SRR', 'h')
  } else {
    stock@Name <- OM@Stock@Name
    stock@Common_Name <- OM@Stock@CommonName
    stock@Species <- OM@Stock@Species
    stock@maxage <- OM@Stock@Ages@MaxAge
    stock@R0 <- OM@Stock@SRR@Pars$R0
    stock@h <- getUniformBounds(OM@Stock, 'SRR', 'h')
  }
  
  sltnames <- slotNames('Stock')
  sltnames <- sltnames[!sltnames %in% c('Name', 'Common_Name',
                                        'Species', 'maxage', 'R0',
                                        'Source', 'SRrel', 'h')]
  
  stock@SRrel <- 1 # hard-coded to BH right now
  for (sl in sltnames) {
    slot(stock, sl) <- rep(0,2)
  }
  stock
}

om2Fleet <- function(OM, st=NULL, fl=NULL) {
  
  
  fleet <- new('Fleet')
  sltnames <- slotNames('Fleet')
  sltnames <- sltnames[!sltnames %in% c('Name', 'Misc',
                                        'isRel',
                                        'MPA',
                                        'CurrentYr')]
  for (sl in sltnames) {
    slot(fleet, sl) <- rep(0,2)
  }
  
  if (methods::is(OM@Fleet, 'list')) {
    fleet@Name <- OM@Fleet[[st]][[fl]]@Name
  } else {
    fleet@Name <- OM@Fleet@Name
  }
  
  
  fleet@isRel <- FALSE
  fleet@nYear <- nYear(OM)
  fleet@CurrentYr <- OM@CurrentYear
  fleet
}

om2mov <- function(Movement, nsim, maxage, narea, nTS) {
  if (is.null(Movement))
    return(NULL)
  
  movement <- Movement |> aperm(c(1,4,2,3,5)) # different order 
  
  dd <- dim(movement)
  if (dd[2] ==1) {
    movement <- abind::adrop(replicate(maxage+1, movement),2) |>
      aperm(c(1,5,2,3,4))
  }
  
  if (dd[5]==1) {
    movement <- abind::adrop(movement[,,,,1, drop=FALSE], 5)
  } 
  movement
}

om2Cpars <- function(OM, st=NULL, fl=NULL) {
  Cpars <- list()
  nsim <- OM@nSim
  nTS <- length(OM@TimeSteps)
  
  if (isS4(OM@Data)) {
    Cpars$Data <- OM@Data
  } else {
    Cpars$Data <- OM@Data[[st]][[fl]]
  }
  
  if (isS4(OM@Stock)) {
    Cpars$R0 <- setCparValues(OM@Stock@SRR@Pars$R0, 1, nsim)
    Cpars$M <- setCparValues(OM@Stock@NaturalMortality@Pars$M, 1, nsim)
    Cpars$Msd <- setCparValues(OM@Stock@NaturalMortality@Pars$MSD, 1, nsim)
    Cpars$h <- setCparValues(OM@Stock@SRR@Pars$h, 1, nsim)
    Cpars$Perr <-  setCparValues(OM@Stock@SRR@SD, 1, nsim)
    Cpars$AC <-  setCparValues(OM@Stock@SRR@AC, 1, nsim)
    
    Cpars$Linf <-  setCparValues(OM@Stock@Length@Pars$Linf, 1, nsim)
    Cpars$Linfsd <-  setCparValues(OM@Stock@Length@Pars$LinfSD, 1, nsim)
    Cpars$K <-  setCparValues(OM@Stock@Length@Pars$K, 1, nsim)
    Cpars$Ksd <-  setCparValues(OM@Stock@Length@Pars$KSD, 1, nsim)
    
    Cpars$t0 <-  setCparValues(OM@Stock@Length@Pars$t0, 1, nsim)
    dd <- dim(OM@Stock@Length@CVatAge)
    if (all(dd==1)) {
      Cpars$LenCV <-  setCparValues(OM@Stock@Length@CVatAge, 1, nsim, nTS)
    }
    Cpars$L50 <- setCparValues(OM@Stock@Maturity@Pars$L50, 1, nsim)
    Cpars$L50_95 <- setCparValues(OM@Stock@Maturity@Pars$L50_95, 1, nsim)
    Cpars$D <- setCparValues(OM@Stock@Depletion@Final, 1, nsim)
    
    # TODO
    Cpars$Size_area_1 <- rep(0.5, nsim) # setCparValues(OM@Stock[[st]]@Spatial@RelativeSize, 1, nsim)
    Cpars$Frac_area_1 <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@UnfishedDist, 1, nsim)
    Cpars$Prob_staying <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@ProbStaying, 1, nsim)
    
    Cpars$hs <- setCparValues(OM@Stock@SRR@Pars$h, 1, nsim)
    
    
    Cpars$Perr_y <- getRecDevs(OM@Stock, nsim)
    
    Cpars$Len_age <- setCparValues(val=OM@Stock@Length@MeanAtAge, 3, nsim, nTS)
    
    Cpars$LatASD <- setCparValues(val=OM@Stock@Length@CVatAge, 3, nsim, nTS) *
      setCparValues(val=OM@Stock@Length@MeanAtAge, 3, nsim, nTS)
    
    Cpars$Wt_age  <- setCparValues(val=OM@Stock@Weight@MeanAtAge, 3, nsim, nTS)
    Cpars$Mat_age <- setCparValues(val=OM@Stock@Maturity@MeanAtAge, 3, nsim, nTS)
    
    Cpars$M_ageArray <- setCparValues(val=OM@Stock@NaturalMortality@MeanAtAge, 3, nsim, nTS)
    Cpars$Fec_age <- setCparValues(val=OM@Stock@Fecundity@MeanAtAge, 3, nsim, nTS)
    
    Cpars$CAL_binsmid <- OM@Stock@Length@Classes
    by <- Cpars$CAL_binsmid[2] - Cpars$CAL_binsmid[1]
    Cpars$CAL_bins <- seq(Cpars$CAL_binsmid[1]-0.5*by, by=by, length.out=length(Cpars$CAL_binsmid)+1)
    Cpars$binWidth <- by
    
    Cpars$mov <- om2mov(OM@Stock@Spatial@Movement, nsim, OM@Stock@Ages@MaxAge, nArea(OM), length(TimeSteps(OM)))
    Cpars$initD <- setCparValues(val=OM@Stock@Depletion@Initial, 1, nsim, nTS)
    
  } else {
    if (fl==1) {
      Cpars$R0 <- setCparValues(OM@Stock[[st]]@SRR@Pars$R0, 1, nsim)
      Cpars$M <- setCparValues(OM@Stock[[st]]@NaturalMortality@Pars$M, 1, nsim)
      Cpars$Msd <- setCparValues(OM@Stock[[st]]@NaturalMortality@Pars$MSD, 1, nsim)
      Cpars$h <- setCparValues(OM@Stock[[st]]@SRR@Pars$h, 1, nsim)
      Cpars$Perr <-  setCparValues(OM@Stock[[st]]@SRR@SD, 1, nsim)
      Cpars$AC <-  setCparValues(OM@Stock[[st]]@SRR@AC, 1, nsim)
      
      Cpars$Linf <-  setCparValues(OM@Stock[[st]]@Length@Pars$Linf, 1, nsim)
      Cpars$Linfsd <-  setCparValues(OM@Stock[[st]]@Length@Pars$LinfSD, 1, nsim)
      Cpars$K <-  setCparValues(OM@Stock[[st]]@Length@Pars$K, 1, nsim)
      Cpars$Ksd <-  setCparValues(OM@Stock[[st]]@Length@Pars$KSD, 1, nsim)
      
      Cpars$t0 <-  setCparValues(OM@Stock[[st]]@Length@Pars$t0, 1, nsim)
      dd <- dim(OM@Stock[[st]]@Length@CVatAge)
      if (all(dd==1)) {
        Cpars$LenCV <-  setCparValues(OM@Stock[[st]]@Length@CVatAge, 1, nsim, nTS)
      }
      
      Cpars$L50 <- setCparValues(OM@Stock[[st]]@Maturity@Pars$L50, 1, nsim)
      Cpars$L50_95 <- setCparValues(OM@Stock[[st]]@Maturity@Pars$L50_95, 1, nsim)
      Cpars$D <- setCparValues(OM@Stock[[st]]@Depletion@Final, 1, nsim)
      
      # TODO
      Cpars$Size_area_1 <- rep(0.5, nsim) # setCparValues(OM@Stock[[st]]@Spatial@RelativeSize, 1, nsim)
      Cpars$Frac_area_1 <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@UnfishedDist, 1, nsim)
      Cpars$Prob_staying <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@ProbStaying, 1, nsim)
      
      Cpars$hs <- setCparValues(OM@Stock[[st]]@SRR@Pars$h, 1, nsim)
      
      Cpars$Perr_y <- getRecDevs(OM@Stock[[st]], nsim)
      
      Cpars$Len_age <- setCparValues(val=OM@Stock[[st]]@Length@MeanAtAge, 3, nsim, nTS)
      
      Cpars$LatASD <- setCparValues(val=OM@Stock[[st]]@Length@CVatAge, 3, nsim, nTS) *
        setCparValues(val=OM@Stock[[st]]@Length@MeanAtAge, 3, nsim, nTS)
      
      Cpars$Wt_age  <- setCparValues(val=OM@Stock[[st]]@Weight@MeanAtAge, 3, nsim, nTS)
      Cpars$Mat_age <- setCparValues(val=OM@Stock[[st]]@Maturity@MeanAtAge, 3, nsim, nTS)
      
      Cpars$M_ageArray <- setCparValues(val=OM@Stock[[st]]@NaturalMortality@MeanAtAge, 3, nsim, nTS)
      Cpars$Fec_age <- setCparValues(val=OM@Stock[[st]]@Fecundity@MeanAtAge, 3, nsim, nTS)
      
      Cpars$CAL_binsmid <- OM@Stock[[st]]@Length@Classes
      by <- Cpars$CAL_binsmid[2] - Cpars$CAL_binsmid[1]
      Cpars$CAL_bins <- seq(Cpars$CAL_binsmid[1]-0.5*by, by=by, length.out=length(Cpars$CAL_binsmid)+1)
      Cpars$binWidth <- by
      
      Cpars$mov <- om2mov(OM@Stock[[st]]@Spatial@Movement, nsim, OM@Stock@Ages@MaxAge, nArea(OM), length(TimeSteps(OM)))
      Cpars$initD <- setCparValues(val=OM@Stock[[st]]@Depletion@Initial, 1, nsim, nTS)
    }
  }
  
  if (isS4(OM@Fleet)) {
    Cpars$Fdisc <- setCparValues(OM@Fleet@DiscardMortality@MeanAtAge[,1,1], 1, nsim)
    Cpars$qinc <- setCparValues(OM@Fleet@Effort@qInc, 1, nsim)
    Cpars$qcv <- setCparValues(OM@Fleet@Effort@qCV, 1, nsim)
    
    Cpars$L5 <- setCparValues(OM@Fleet@Selectivity@Pars$L5, 1, nsim)
    Cpars$LFS <- setCparValues(OM@Fleet@Selectivity@Pars$LFS, 1, nsim)
    Cpars$Vmaxlen <- setCparValues(OM@Fleet@Selectivity@Pars$Vmaxlen, 1, nsim)
    
    Cpars$LR5 <- setCparValues(OM@Fleet@Selectivity@Pars$LR5, 1, nsim)
    Cpars$LFR <- setCparValues(OM@Fleet@Selectivity@Pars$LFR, 1, nsim)
    Cpars$Rmaxlen <- setCparValues(OM@Fleet@Selectivity@Pars$Rmaxlen, 1, nsim)
    
    Cpars$Find <- setCparValues(val=OM@Fleet@Effort@Effort, 2, nsim, 
                                nTS=length(TimeSteps(OM, 'Historical')))
    Cpars$qs <- setCparValues(val=OM@Fleet@Effort@Catchability, 1, nsim)
    
    Cpars$Wt_age_C <- setCparValues(val=OM@Fleet@Weight, 3, nsim, nTS)
    
    Cpars$V <- setCparValues(val=OM@Fleet@Selectivity@MeanAtAge, 3, nsim, nTS)
    Cpars$SLarray <- setCparValues(val=OM@Fleet@Selectivity@MeanAtLength, 3, nsim, nTS)
    
    Cpars$retA <- setCparValues(val=OM@Fleet@Retention@MeanAtAge, 3, nsim, nTS)
    Cpars$retL <- setCparValues(val=OM@Fleet@Retention@MeanAtLength, 3, nsim, nTS)
    Cpars$DR <- rep(0, nsim)
  } else {
    Cpars$Fdisc <- setCparValues(OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge[,1,1], 1, nsim)
    Cpars$qinc <- setCparValues(OM@Fleet[[st]][[fl]]@Effort@qInc, 1, nsim)
    Cpars$qcv <- setCparValues(OM@Fleet[[st]][[fl]]@Effort@qCV, 1, nsim)
    
    Cpars$L5 <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$L5, 1, nsim)
    Cpars$LFS <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LFS, 1, nsim)
    Cpars$Vmaxlen <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$Vmaxlen, 1, nsim)
    
    Cpars$LR5 <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LR5, 1, nsim)
    Cpars$LFR <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LFR, 1, nsim)
    Cpars$Rmaxlen <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$Rmaxlen, 1, nsim)
    
    Cpars$Find <- setCparValues(val=OM@Fleet[[st]][[fl]]@Effort@Effort, 2, nsim, 
                                nTS=length(TimeSteps(OM, 'Historical')))
    Cpars$qs <- setCparValues(val=OM@Fleet[[st]][[fl]]@Effort@Catchability, 1, nsim)
    
    Cpars$Wt_age_C <- setCparValues(val=OM@Fleet[[st]][[fl]]@Weight, 3, nsim, nTS)
    
    Cpars$V <- setCparValues(val=OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge, 3, nsim, nTS)
    Cpars$SLarray <- setCparValues(val=OM@Fleet[[st]][[fl]]@Selectivity@MeanAtLength, 3, nsim, nTS)
    
    Cpars$retA <- setCparValues(val=OM@Fleet[[st]][[fl]]@Retention@MeanAtAge, 3, nsim, nTS)
    Cpars$retL <- setCparValues(val=OM@Fleet[[st]][[fl]]@Retention@MeanAtLength, 3, nsim, nTS)
    Cpars$DR <- rep(0, nsim)
    
    # Cpars$qvar
    # Cpars$V_real
    # Cpars$retA_real
    # Cpars$retL_real
    # Cpars$SLarray_real
    
    Cpars$Fdisc_array1 <- setCparValues(val=OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge, 3, nsim, nTS)
    Cpars$Fdisc_array2 <- setCparValues(val=OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtLength, 3, nsim, nTS)
    
    
  }
  
  if (methods::is(OM@Misc$cpars, 'list')) {
    Cpars <- c(Cpars,OM@Misc$cpars[[st]][[fl]]) # any Obs and Imp stuff  
  } else {
    Cpars <- c(Cpars,OM@Misc$cpars) # any Obs and Imp stuff  
  }
  Cpars
}

om2OM <- function(OM) {
  OM <- Populate(OM, messages=FALSE)
  nsim <- nSim(OM)
  nTS <- length(TimeSteps(OM))
  
  OMout <- new('OM', Stock=om2Stock(OM), 
               Fleet=om2Fleet(OM)
               )
  OMout@cpars <- om2Cpars(OM)
  OMout@Name <- OM@Name
  OMout@Agency <- OM@Agency
  OMout@Region <- OM@Region
  OMout@Sponsor <- OM@Sponsor
  OMout@Latitude <- OM@Latitude
  OMout@Longitude <- OM@Longitude
  OMout@nsim <- OM@nSim
  OMout@proyears <- OM@pYear
  OMout@interval <- OM@Interval
  OMout@pstar <- OM@pStar
  OMout@maxF <- OM@maxF
  OMout@reps <- OM@nReps
  OMout@seed <- OM@Seed
  OMout@Source <- OM@Source
  OMout <- Replace(OMout, OM@Obs, silent=TRUE)
  OMout <- Replace(OMout, OM@Imp, silent=TRUE)
 
  OMout
}

om2MOM <- function(OM) {
  OM <- Populate(OM, messages=FALSE)
  nsim <- nSim(OM)
  nTS <- length(TimeSteps(OM))
  
  # everything is done in cpars
  Stocks <- vector('list', nStock(OM))
  names(Stocks) <- names(OM@Stock)
  Fleets <- vector('list', nStock(OM))
  names(Fleets) <- names(OM@Stock)
  cpars <- vector('list', nStock(OM))
  names(cpars) <- names(OM@Stock)
  for (st in 1:nStock(OM)) {
    Fleets[[st]] <- vector('list', nFleet(OM))
    names(Fleets[[st]]) <- names(OM@Fleet[[st]])
    cpars[[st]] <- vector('list', nFleet(OM))
    names(cpars[[st]]) <- names(OM@Fleet[[st]])
    
    Stocks[[st]] <- om2Stock(OM, st)
    for (fl in 1:nFleet(OM)) {
      Fleets[[st]][[fl]] <- om2Fleet(OM, st, fl)
      cpars[[st]][[fl]] <- om2Cpars(OM, st, fl)
    }
  }
  
  MOM <- new('MOM',
             nsim=nsim,
             proyears=pYear(OM),
             Stocks=Stocks,
             Fleets=Fleets,
             Obs=OM@Obs, # currently unchanged
             Imps=OM@Imp, # currently unchanged
             CatchFrac=OM@CatchFrac,
             cpars=cpars,
             interval=OM@Interval,
             pstar=OM@pStar,
             maxF=OM@maxF,
             reps=OM@nReps,
             Allocation=OM@Allocation,
             Efactor=OM@Efactor,
             Complexes=OM@Complexes,
             SexPars=OM@SexPars,
             Rel=OM@Relations)
  
  MOM@Name <- OM@Name
  MOM@Agency <- OM@Agency
  MOM@Region <- OM@Region
  MOM@Sponsor <- OM@Sponsor
  MOM@Latitude <- OM@Latitude
  MOM@Longitude <- OM@Longitude
  MOM@nsim <- OM@nSim
  MOM@proyears <- OM@pYear
  MOM@interval <- OM@Interval
  MOM@pstar <- OM@pStar
  MOM@maxF <- OM@maxF
  MOM@reps <- OM@nReps
  MOM@seed <- OM@Seed
  MOM@Source <- OM@Source
  MOM
  
}