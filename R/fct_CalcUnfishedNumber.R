# NOTE: Equilibrium N-at-Age is calculated from R0 (which may vary over time)
# but does NOT account for expected recruitment from the stock-recruit relationship.
# i.e., an expected change in recruitment if Fecundity-at-Age changes over time
# e.g., change in Weight-at-Age etc
# - that should already by accounted for in R0


CalcUnfishedNumber <- function(OM, SP = FALSE) {
  if (IsSeasonalRecruitment(OM)) {
    return(
      CalcUnfishedNumber_seasonal(OM, SP)
    )
  }

  R0List <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0 |>
      AddDimension("Age") |>
      aperm(c("Sim", "Age", "Year"))
  })

  UnfishedSurvival_List <- CalcUnfishedSurvival(OM, SP)
  UnfishedNumberAtAge <- purrr::map2(UnfishedSurvival_List, R0List, ArrayMultiply)
  UnfishedNumberAtAge
}

CalcUnfishedNumber_seasonal_stock <- function(Stock, SP=FALSE) {
  AgeClasses <- Stock@Ages@Classes
  MaxAge <- max(AgeClasses)
  nAge <- length(AgeClasses)
  
  
  
}
CalcUnfishedNumber_seasonal <- function(OM, SP = FALSE) {
  OM <- PopulateOM(OM)
  nStock <- nStock(OM)
  nSim <- OM@nSim
  Years <- OM@Years
  nYear <- length(Years)
  nSeason <- Seasons(OM)
  UnfishedNumberAtAge <- MakeNamedList(StockNames(OM))

  for (st in 1:nStock) {
    Stock <- OM@Stock[[st]]
    AgeClasses <- Stock@Ages@Classes
    MaxAge <- max(AgeClasses)
    nAge <- length(AgeClasses)
    
    R0 <- Stock@SRR@R0 |>
      ExtendYears(Years) |>
      ExtendSims(nSim)
    
    NaturalMortality <- Stock@NaturalMortality@MeanAtAge |>
      ExtendYears(Years) |>
      ExtendSims(nSim)
    PlusGroup <- Stock@Ages@PlusGroup
    SpawnTimeFrac <- ifelse(SP, Stock@SRR@SpawnTimeFrac, 0)
    Semelparous <- Stock@Maturity@Semelparous

    if (is.logical(Semelparous)) {
      Semelparous <- array(0,
        dim = c(nSim, nAge, nYear),
        dimnames = list(
          Sim = 1:nSim,
          Age = AgeClasses,
          Year = Years
        )
      )
    }

    Semelparous <- Semelparous |>
      ExtendYears(Years) |>
      ExtendSims(nSim)

    UnfishedNumberAtAgeStock <- array(0, c(nSim, nAge, nYear),
      dimnames = list(
        Sim = 1:nSim,
        Age = AgeClasses,
        Year = Years
      )
    )

    # Initial Year - runs out pop dynamics to account for plus group with seasonal recruitment
    UnfishedInitialYear <- CalcUnfishedNumber_seasonal_initial_year(R0, nSim, AgeClasses, 
                                                                    nSeason, NaturalMortality, 
                                                                    Semelparous, SpawnTimeFrac, PlusGroup)
    UnfishedNumberAtAgeStock[, , 1:nSeason] <- UnfishedInitialYear
    UnfishedNumberAtAgeStock[,1,] <- R0

    # Year 2+
    for (ts in (nSeason + 1):length(Years)) {
      for (age in seq_along(AgeClasses)[-1]) {
        Age <- AgeClasses[age]
        MLastAge <- NaturalMortality[, age - 1, ts - 1, drop = FALSE]
        MThisAge <- NaturalMortality[, age, ts - 1, drop = FALSE]
        PostSpawnMortalityLastAge <- Semelparous[, age - 1, ts - 1]

        surv <- exp(-(MLastAge * (1 - SpawnTimeFrac) + MThisAge * SpawnTimeFrac)) *
          (1 - PostSpawnMortalityLastAge)

        UnfishedNumberAtAgeStock[, age, ts] <- UnfishedNumberAtAgeStock[, age - 1, ts - 1, drop = FALSE] * surv
      }
      if (PlusGroup) {
        PostSpawnMortalityThisAge <- Semelparous[, age, ts]
        UnfishedNumberAtAgeStock[, age, ts] <- UnfishedNumberAtAgeStock[, age, ts] + UnfishedNumberAtAgeStock[, age, ts - 1] * exp(-MThisAge) * (1 - PostSpawnMortalityThisAge)
      }
    }

    UnfishedNumberAtAge[[st]] <- UnfishedNumberAtAgeStock
  }
  UnfishedNumberAtAge
}


IsSeasonalRecruitment <- function(OM) {
  if (OM@Seasons == 1) {
    return(FALSE)
  }

  R0Array <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0
  }) |>
    List2Array("Stock") |>
    aperm(c("Sim", "Stock", "Year")) |>
    ArrayReduceDims()

  dim(R0Array)[[3]] > 1
}

CalcUnfishedNumber_seasonal_initial_year <- function(R0, nSim, AgeClasses, nSeason, NaturalMortality, Semelparous, SpawnTimeFrac, PlusGroup) {
  nAge <- length(AgeClasses)

  # Equilibrium Age-Structure for all seasons in the First Year
  runoutSeasons <- nAge * 2 * nSeason
  InitialAgeStructure <- array(0, c(nSim, nAge, runoutSeasons))

  # no doubt a better way to do this, but this works for now ...
  InitialAgeStructure[, 1, ] <- R0[, 1:nSeason]

  for (ts in 2:runoutSeasons) {
    for (age in seq_along(AgeClasses)[-1]) {
      ThisSeasonIndex <- ts %% nSeason
      if (ThisSeasonIndex == 0) {
        ThisSeasonIndex <- nSeason
      }
      LastSeasonIndex <- ThisSeasonIndex - 1
      if (LastSeasonIndex == 0) {
        LastSeasonIndex <- nSeason
      }

      MLastAge <- NaturalMortality[, age - 1, LastSeasonIndex, drop = FALSE]
      PostSpawnMortalityLastAge <- Semelparous[, age - 1, LastSeasonIndex]
      MThisAge <- NaturalMortality[, age, ThisSeasonIndex, drop = FALSE]
      PostSpawnMortalityThisAge <- Semelparous[, age, ThisSeasonIndex]

      surv <- exp(-(MLastAge * (1 - SpawnTimeFrac) + MThisAge * SpawnTimeFrac)) * (1 - PostSpawnMortalityLastAge)
      
      InitialAgeStructure[, age, ts] <- InitialAgeStructure[, age - 1, ts - 1] * surv
    }
    if (PlusGroup) {
      InitialAgeStructure[, age, ts] <- InitialAgeStructure[, age, ts] + InitialAgeStructure[, age, ts - 1] * exp(-MThisAge) * (1 - PostSpawnMortalityThisAge)
    }
  }
  InitialAgeStructure[, , (runoutSeasons - nSeason + 1):runoutSeasons, drop = FALSE]
}
