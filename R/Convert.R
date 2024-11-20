
#' Convert `OM` and `MOM` class objects to new `om` class
#'
#' @rdname OM2om
#' @name OM2om
#'
NULL


#' @describeIn OM2om Convert an `OM` or `MOM` class object
#'  to an `om` object
#' @param OM An `OM` or `MOM` class object
#' @export
OM2om <- function(OM, Author='', CurrentYear=NULL) {

  if (!methods::is(OM, 'OM') & !methods::is(OM, 'MOM'))
    cli::cli_abort('Argument `OM` must be class `OM` or `MOM`')

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
    om@nYears <- OM@Fleets[[1]][[1]]@nyears
  } else {
    om@nYears <- OM@nyears
  }
 
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
  om@TimeSteps <- CalcTimeSteps(nYears=om@nYears,
                                pYears=om@pYears,
                                CurrentYear=om@CurrentYear,
                                TimeUnits=om@TimeUnits)


 

  if (methods::is(OM, 'OM')) {
    om@Stock <- OM2stock(OM, cpars=OM@cpars)
    om@Fleet <- OM2fleet(OM, OM@cpars, OM@Fdisc)
    return(om)
  }

  if (methods::is(OM, 'MOM')) {
    
    om@Stock <- MOM2stock(OM)
    om@Fleet <- list()
    for (st in 1:length(om@Stock)) {
      om@Fleet[[st]] <- MOM2fleet(OM, st)
    }
  }


  om@Obs <- OM@Obs
  om@Imp <- OM@Imps
  om@CatchFrac <- OM@CatchFrac
  om@Allocation <- OM@Allocation
  om@SexPars <- OM@SexPars
  om@Efactor <- OM@Efactor
  om@Complexes<- OM@Complexes
  om@Relations <- OM@Rel
  
  
  
  
  om
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
  CVatAge(Length) <- process_cpars(cpars$LenCV)
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

  pars <- Pars(Weight)
  if (!is.numeric(pars$a)) {
    pars$a <- process_cpars(OM@a)
  }
  if (!is.numeric(pars$b)) {
    pars$b <- process_cpars(OM@b)
  }
  Pars(Weight) <- pars


  Weight
}

cpars2Weight <- function(cpars) {
  Weight <- Weight()
  MeanAtAge(Weight) <- process_cpars(cpars$Wt_age)

  pars <- Pars(Weight)
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
  switch(SRrel,
         '1'='BH',
         '2'='RK')
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
  if (!is.numeric(pars$R0)) {
    pars$R0 <- process_cpars(OM@R0)
  }
  if (!is.numeric(pars[['h']])) {
    pars[['h']] <- process_cpars(OM@h)
  }

  Pars(SRR) <- pars

  SRR@Model <- switchSRR(OM@SRrel[1])
  SRR@SD <- process_cpars(OM@Perr)
  SRR@AC <- process_cpars(OM@AC)
  SRR
}

cpars2SRR <- function(cpars, nyears=NULL, maxage=NULL) {
  SRR <- SRR()
  Pars(SRR)$R0 <- process_cpars(cpars$R0)
  Pars(SRR)$h <- process_cpars(cpars$h)
  SRR@Model <- process_cpars(cpars$SRrel)
  SRR@SD <- process_cpars(cpars[['Perr']])
  SRR@AC <- process_cpars(cpars[['AC']])

  # Perr_y
  perr_y <- cpars[['Perr_y']]
  if (!is.null(perr_y)) {
    if (is.null(nyears)) {
      nyears <- dim(cpars$Find)[2]
    }
    if (is.null(maxage)) {
      maxage <- dim(cpars$Len_age)[2] -1
    }

    if (is.null(nyears))
      stop("`nyears` not found")
    if (is.null(maxage))
      stop("`maxage` not found")

    dd <- dim(perr_y)[2]
    proyears <- dd - nyears - maxage


    init_age_classes <- perr_y[,1:maxage]

    if (identical_sim(init_age_classes)) {
      SRR@RecDevInit <- init_age_classes[1,]
    } else {
      SRR@RecDevInit <- init_age_classes
    }

    hist_yrs <- perr_y[,(maxage+1):(nyears+maxage)]
    if (identical_sim(hist_yrs)) {
      SRR@RecDevHist <- hist_yrs[1,]
    } else {
      SRR@RecDevHist <- hist_yrs
    }

    pro_yrs <- perr_y[,(nyears+maxage+1):(nyears+maxage+proyears)]
    if (identical_sim(pro_yrs)) {
      SRR@RecDevProj <- pro_yrs[1,]
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
  
  if (is.null(Depletion@Final))
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
    narea <- dd[4]
    nsim <- dd[1]
    if (is.null(Spatial@RelativeSize))
      Spatial@RelativeSize <- array(1/narea, dim=c(1, narea))

  } else {
    Spatial@RelativeSize <- process_cpars(OM@Size_area_1)
    Spatial@ProbStaying <- process_cpars(OM@Prob_staying)
    Spatial@UnfishedDist <- process_cpars(OM@Frac_area_1)
  }
  if (Spatial@RelativeSize == 0.5 &
      Spatial@ProbStaying == 0.5 &
      Spatial@UnfishedDist == 0.5) 
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
  mov
}


cpars2Spatial <- function(cpars) {
  Spatial <- Spatial()
  Spatial@RelativeSize <- process_cpars(cpars$Asize)
  Spatial@Movement <- process_cpars(process_mov(cpars$mov))
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
    Effort@Effort <- data.frame(TimeSteps=OM@EffYears,
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

    


    # for (fl in 1:nfleets) {
    #   StockList[[st]]@Fleet
    #
    #
    #   StockList[[st]]@Fleet
    # }



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
