OM2fleet <- function(OM, cpars=NULL, Fdisc=NULL) {
  fleet <- Fleet()
  if (inherits(OM, 'OM')) {
    fleet@Name <- SubOM(OM, 'Fleet')@Name
  } else {
    fleet@Name <- OM@Name
  }
 
  FishingMortality(fleet) <- OM2FishingMortality(OM, cpars)
  DiscardMortality(fleet) <- OM2DiscardMortality(OM, cpars, Fdisc)
  Effort(fleet) <- OM2Effort(OM, cpars)
  Selectivity(fleet) <- OM2Selectivity(OM, cpars)
  Retention(fleet) <- OM2Retention(OM, cpars)
  
  # SpatTarg
  Distribution(fleet) <- OM2Distribution(OM, cpars)
  
  # Weight
  fleet@WeightFleet <- process_cpars(cpars$Wt_age_C)
  
  # BioEco
  
  fleet
}

OM2FishingMortality <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
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
  FishingMortality@ApicalF <- process_cpars(cpars$qs) * process_cpars(cpars$Find)
  FishingMortality
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
  # TODO SpatTarg?
  Distribution
}

cpars2Distribution <- function(cpars) {
  Distribution <- Distribution()
  Distribution@Closure <- process_cpars(cpars$MPA)
  Distribution
}
