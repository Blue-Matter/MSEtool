OM2fleet <- function(OM, cpars=NULL, Fdisc=NULL) {
  fleet <- Fleet()
  if (inherits(OM, 'OM')) {
    fleet@Name <- SubOM(OM, 'Fleet')@Name
  } else {
    fleet@Name <- OM@Name
  }
  fleet@Name <- gsub("REPLACED -- ", '', fleet@Name)
  
  # FishingMortality(fleet) <- OM2FishingMortality(OM, cpars)
  
  fleet@Effort <- OM2Effort(OM, cpars)
  fleet@Catchability <- OM2Catchability(OM, cpars) 
  fleet@qCV <- OM@qcv
  fleet@qInc <- OM@qinc
  
  Selectivity(fleet) <- OM2Selectivity(OM, cpars)
  Retention(fleet) <- OM2Retention(OM, cpars)
  fleet@DiscardMortality <- OM2DiscardMortality(OM, cpars, Fdisc)

  # fleet@Closure <- # OM@MPA # TODO
  fleet@Targeting <- OM@Spat_targ
  
  fleet@WeightFleet <- cpars$Wt_age_C
  
  # BioEco
  
  fleet
}

OM2Catchability <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  
  if (!is.null(cpars$qs))
    return(matrix(cpars$qs, nrow=length(cpars$qs), ncol=1))
  matrix(tiny, nrow=1, ncol=1)
}

# OM2FishingMortality <- function(OM, cpars=NULL) {
#   if (is.null(cpars) & inherits(OM, 'OM'))
#     cpars <- OM@cpars
#   if (!EmptyObject(cpars)) {
#     FishingMortality <- cpars2FishingMortality(cpars)
#   } else {
#     FishingMortality <- FishingMortality()
#   }
#   
#   FishingMortality
# }

cpars2FishingMortality <- function(cpars) {
  FishingMortality <- FishingMortality()
  FishingMortality@ApicalF <- cpars$qs * cpars$Find
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
  DiscardMortality@MeanAtAge <- cpars$Fdisc_array1
  DiscardMortality@MeanAtLength <- cpars$Fdisc_array2
  DiscardMortality@Classes <- cpars$CAL_binsmid
  DiscardMortality
}

OM2Effort <- function(OM, cpars=NULL) {
  if (is.null(cpars) & inherits(OM, 'OM'))
    cpars <- OM@cpars
  if (!EmptyObject(cpars)) {
    return(cpars$Find)
  }
  
  data.frame(Year=OM@EffYears,
             Lower=OM@EffLower,
             Upper=OM@EffUpper,
             CV=OM@Esd[1])
}

cpars2Effort <- function(cpars) {
  Effort <- Effort()
  Effort@Effort <- cpars$Find
  Effort@Catchability <- cpars$qs
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
  Selectivity@MeanAtAge <- cpars[['V']]
  Selectivity@MeanAtLength <- cpars[['SLarray']]
  Selectivity@Classes <- cpars$CAL_binsmid
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
  Retention@MeanAtAge <- cpars[['retA']]
  Retention@MeanAtLength <- cpars[['retL']]
  Retention@Classes <- cpars$CAL_binsmid
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
  Distribution@Closure <- cpars$MPA
  Distribution
}
