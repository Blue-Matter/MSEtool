OM2fleet <- function(OM, cpars=NULL, Fdisc=NULL) {
  if (inherits(OM, 'OM')) {
    cpars <- OM@cpars
  }
  
  fleet <- Fleet2Name(OM)
  fleet@Effort <- OM2Effort(OM, cpars)
  fleet@Catchability <- OM2Catchability(OM, cpars) 
  fleet@Selectivity <- OM2Selectivity(OM, cpars)
  fleet@Retention  <- OM2Retention(OM, cpars)
  fleet@DiscardMortality <- OM2DiscardMortality(OM, cpars, Fdisc)
  fleet@Closure <- OM2Closure(OM, cpars)
  # fleet@Targeting  <- OM@Spat_targ # TODO 
  fleet@WeightFleet <- OM2WeightFleet(OM, cpars)
  # BioEco
  
  fleet
}

OM2Effort <- function(OM, cpars=list()) {
  Effort <- Fleet2Effort(OM)
  if (!length(cpars)) {
    return(Effort)
  }
  
  HistYears <- GetOMYears(OM, 'H')
  
  if (!is.null(cpars$Find)) {
    Effort@Value <- array(cpars$Find, 
                          dim=dim(cpars$Find),
                          dimnames = list(
                            Sim=1:nrow(cpars$Find),
                            Year=HistYears
                          )
    ) |>
      ReduceDims()
  }
  Effort
}

OM2Catchability <- function(OM, cpars=list()) {
  Catchability <- Fleet2Catchability(OM)
  if (!length(cpars)) {
    return(Catchability)
  }
  
  if (!is.null(cpars$qs)) {
    HistYears <- GetOMYears(OM, 'H')
    Catchability@Value <- array(cpars$qs, c(length(cpars$qs),1),
                                dimnames = list(
                                  Sim=seq_along(cpars$qs),
                                  Year=HistYears[1]
                                )) |>
      ReduceDims()
  }
  Catchability
}

OM2Selectivity <- function(OM, cpars=list()) {
  Selectivity <- Fleet2Selectivity(OM)
  if (!length(cpars)) {
    return(Selectivity)
  }
  
  if (!is.null(cpars[['V']])) {
    AgeClasses <- GetStockAges(OM)
    Years <- GetOMYears(OM)
    dd <- dim(cpars[['V']])
    Selectivity@MeanAtAge <- array(cpars[['V']],
                                   dim=dd,
                                   dimnames = list(
                                     Sim=1:dd[1],
                                     Age=AgeClasses,
                                     Year=Years
                                   )
    ) |>
      ReduceDims()
  }
  
  if (!is.null(cpars[['SLarray']])) {
    Classes <- cpars$CAL_binsmid
    Selectivity@Classes <- Classes
    Years <- GetOMYears(OM)
    dd <- dim(cpars[['SLarray']])
    Selectivity@MeanAtLength <- array(cpars[['SLarray']],
                                   dim=dd,
                                   dimnames = list(
                                     Sim=1:dd[1],
                                     Class=Classes,
                                     Year=Years
                                   )
    ) |>
      ReduceDims()
  }
  
  Selectivity
}

OM2Retention <- function(OM, cpars=list()) {
  Retention <- Fleet2Retention(OM)
  if (!length(cpars)) {
    return(Retention)
  }
  
  if (!is.null(cpars[['retA']])) {
    AgeClasses <- GetStockAges(OM)
    Years <- GetOMYears(OM)
    dd <- dim(cpars[['retA']])
    Retention@MeanAtAge <- array(cpars[['retA']],
                                   dim=dd,
                                   dimnames = list(
                                     Sim=1:dd[1],
                                     Age=AgeClasses,
                                     Year=Years
                                   )
    ) |>
      ReduceDims()
  }
  
  if (!is.null(cpars[['retL']])) {
    Classes <- cpars$CAL_binsmid
    Retention@Classes <- Classes
    Years <- GetOMYears(OM)
    dd <- dim(cpars[['retL']])
    Retention@MeanAtLength <- array(cpars[['retL']],
                                      dim=dd,
                                      dimnames = list(
                                        Sim=1:dd[1],
                                        Class=Classes,
                                        Year=Years
                                      )
    ) |>
      ReduceDims()
  }
  Retention
}

OM2DiscardMortality <- function(OM, cpars=list(), Fdisc=NULL) {
  DiscardMortality <- DiscardMortality()
  
  if (inherits(OM, 'OM')) {
    Fdisc <- OM@Fdisc
  }
  
  # TODO - need to convert to an array with correct dimensions
  # requires sampling from uniform for OM@Fdisc
  # not used much ...
  # if (any(Fdisc>0.01)) {
  #   ageclasses <- GetStockAges(OM)
  #   nage <- length(ageclasses)
  #   Fdisc
  #   
  #   DiscardMortality <- DiscardMortality(MeanAtAge=Fdisc)
  # }
  
 
  if (!length(cpars)) {
    return(DiscardMortality)
  }
  if (!is.null(cpars$Fdisc_array1)) {
    AgeClasses <- GetStockAges(OM)
    Years <- GetOMYears(OM)
    dd <- dim(cpars[['V']])
    DiscardMortality@MeanAtAge <- array(cpars[['Fdisc_array1']],
                                   dim=dd,
                                   dimnames = list(
                                     Sim=1:dd[1],
                                     Age=AgeClasses,
                                     Year=Years
                                   )
    ) |>
      ReduceDims()
    
  }
  
  if (!is.null(cpars[['Fdisc_array2']])) {
    Classes <- cpars$CAL_binsmid
    DiscardMortality@Classes <- Classes
    Years <- GetOMYears(OM)
    dd <- dim(cpars[['Fdisc_array2']])
    DiscardMortality@MeanAtLength <- array(cpars[['Fdisc_array2']],
                                    dim=dd,
                                    dimnames = list(
                                      Sim=1:dd[1],
                                      Class=Classes,
                                      Year=Years
                                    )
    ) |>
      ReduceDims()
  }
  DiscardMortality
}

OM2Closure <- function(OM, cpars=list()) {
  if (is.null(cpars$MPA)) {
    return(NULL)
  }
  
  array(cpars$MPA, dim=cpars$MPA,
        dimnames = list(
          Sim=1:nrow(cpars$MPA),
          Area=1:ncol(cpars$MPA)
        ))
  
}

OM2WeightFleet <- function(OM, cpars=list()) {
  if (is.null(cpars$Wt_age_C)) {
    return(NULL)
  }
  Years <- GetOMYears(OM)
  AgeClasses <- GetStockAges(OM)
  array(cpars$Wt_age_C, dim=cpars$Wt_age_C,
        dimnames = list(
          Sim=1:nrow(cpars$Wt_age_C),
          Age=AgeClasses,
          Year=Years
        )) |>
    ReduceDims()
  
}

