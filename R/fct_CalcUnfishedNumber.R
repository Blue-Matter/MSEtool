# NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
# but does NOT account for expected recruitment from the stock-recruit relationship.
# i.e., an expected change in recruitment if Fecundity-at-Age changes over time 
# e.g., change in Weight-at-Age etc
# - that should already by accounted for in R0 


CalcUnfishedNumber <- function(OM, SP=FALSE) {
  
  if (IsSeasonalRecruitment(OM)) {
    return(
      CalcUnfishedNumber_seasonal(OM, SP)
    )
  } 
  
  R0List <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0 |> 
      AddDimension('Age') |>
      aperm(c('Sim', 'Age', 'Year'))
  })
  
  UnfishedSurvival_List <- CalcUnfishedSurvival(OM, SP) 
  UnfishedNumberAtAge <- purrr::map2(UnfishedSurvival_List, R0List, ArrayMultiply)
  UnfishedNumberAtAge
}

CalcUnfishedNumber_seasonal <- function(OM, SP=FALSE) {
  # This matches North Pacific Swordfish SS3 model 
  # Need to check if it generalizes to all seasonal models
  OM <- PopulateOM(OM)
  nStock <- nStock(OM)
  nSim <- OM@nSim
  Years <- OM@Years
  nYear <- length(Years)
  UnfishedNumberAtAge <- MakeNamedList(StockNames(OM))
  
  for (st in 1:nStock) {
    Stock <- OM@Stock[[st]]
    AgeClasses <- Stock@Ages@Classes
    MaxAge <- Stock@Ages@MaxAge/OM@TSperYear
    MaxAgeAnnual <- floor(MaxAge)
    nAge <- length(AgeClasses)
    R0 <- Stock@SRR@R0 |> ExtendYears(Years) |> ExtendSims(nSim) 
    NaturalMortality <- Stock@NaturalMortality@MeanAtAge |> 
      ExtendYears(Years) |> ExtendSims(nSim)
    PlusGroup <- Stock@Ages@PlusGroup
    SpawnTimeFrac <- ifelse(SP, Stock@SRR@SpawnTimeFrac, 0)
    Semelparous <- Stock@Maturity@Semelparous
    
    if (is.logical(Semelparous)) 
      Semelparous <- array(0, dim = c(nSim, nAge, nYear),
                           dimnames = list(
                             Sim=1:nSim,
                             Age=AgeClasses,
                             Year=Years))
    
    Semelparous <- Semelparous |> ExtendYears(Years) |> ExtendSims(nSim) 
    
    UnfishedNumberAtAgeStock <- array(0, c(nSim, nAge, nYear),
                                       dimnames = list(
                                         Sim=1:nSim,
                                         Age=AgeClasses,
                                         Year=Years)
    )
    
    UnfishedNumberAtAgeStock[,1,] <- R0
    Survival <- array(1, dim=dim(UnfishedNumberAtAgeStock),
                      dimnames = dimnames(UnfishedNumberAtAgeStock))
    
    # Initial Year 
    for (age in seq_along(AgeClasses)[-1]) {
      Age <- AgeClasses[age]
      MLastAge <- NaturalMortality[,age-1,1, drop=FALSE]
      MThisAge <- NaturalMortality[,age,1, drop=FALSE]
      PostSpawnMortalityLastAge <- Semelparous[,age-1,1]
      
      Survival[,age,1] <- Survival[,age-1,1] * exp(-(MLastAge*(1-SpawnTimeFrac)+MThisAge*SpawnTimeFrac)) *
        (1-PostSpawnMortalityLastAge)
      
      UnfishedNumberAtAgeStock[,age,1] <- R0[,age] * Survival[,age,1]
      
      if (PlusGroup && Age == MaxAgeAnnual) {
        Survival[,age,1] <- exp(-NaturalMortality[,age,1]*OM@TSperYear)
        UnfishedNumberAtAgeStock[,age,1] <- UnfishedNumberAtAgeStock[,age,1]/(1-Survival[,age,1]) 
      }
    }

    for (ts in 2:length(Years)) {
      for (age in seq_along(AgeClasses)[-1]) {
        Age <- AgeClasses[age]
        MLastAge <- NaturalMortality[,age-1,ts-1, drop=FALSE]
        MThisAge <- NaturalMortality[,age,ts-1, drop=FALSE]
        PostSpawnMortalityLastAge <- Semelparous[,age-1,ts-1]
        
        Survival[,age,ts] <- exp(-(MLastAge*(1-SpawnTimeFrac)+MThisAge*SpawnTimeFrac)) *
          (1-PostSpawnMortalityLastAge)
        
        UnfishedNumberAtAgeStock[,age,ts] <- UnfishedNumberAtAgeStock[,age-1,ts-1, drop=FALSE] * Survival[,age, ts]
        
        if (PlusGroup && Age == MaxAgeAnnual) {
          Survival[,age,ts] <- Survival[,age,ts]/(1-exp(-NaturalMortality[,age,ts]))
          UnfishedNumberAtAgeStock[,age,ts] <- UnfishedNumberAtAgeStock[,age,ts, drop=FALSE]/Survival[,age,ts]
        }
      }
    }
    UnfishedNumberAtAge[[st]] <- UnfishedNumberAtAgeStock
  }
  UnfishedNumberAtAge
}


IsSeasonalRecruitment <- function(OM) {
  if (OM@TSperYear==1)
    return(FALSE)
  
  R0Array <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0 
  }) |> List2Array('Stock') |> 
    aperm(c('Sim', 'Stock', 'Year')) |>
    ArrayReduceDims()
  
  dim(R0Array)[[3]]>1
  
}