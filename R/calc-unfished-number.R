# TODO - need to add Herm
# TODO - speed up over identical Sims, Years

#' Calculate Equilibrium Unfished Number-at-Age
#' 
#' Calculates the equilibrium unfished number-at-age
#' 
#' Equilibrium N-at-Age is calculated from `R0` (which may vary over time) 
#' but does NOT account for expected recruitment from the stock-recruit relationship.
#' 
#' Any changes in `R0` due to time-varying biology (Fecundity-at-Age changes over time)
#' should be accounted for in `R0`.
#'   
#' @param OM An [OM()] object
#' @param SP Logical. Account for `SpawnTimeFrac`? Accounts for spawning timing within a time step to 
#' calculate the number-at-age at the time of spawning
#' 
#' @return A named list of length [nStock()] with each element an array with dimensions
#' Sim, Age, and Year
#' 
#' @export
CalcUnfishedNumber <- function(OM, SP = FALSE) {
  
  if (OM@Seasons > 1) 
    return(.CalcUnfishedNumberSeasonal(OM, SP))
  
  # Unfished survival by Sim, Age, and Year for each Stock
  UnfishedSurvival_List <- CalcUnfishedSurvival(OM, SP, Years=Years(OM,'Hist')) 
  
  # R0 for each Stock 
  R0List <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0 |>
      AddDimension("Age", val =  min(Stock@Ages@Classes)) |>
      .Aperm(c("Sim", "Age", "Year"))
  })
  
  # Multiply R0 by Survival
  purrr::map2(UnfishedSurvival_List, R0List, ArrayMultiply)
  
}


# Is the OM seasonal with R0 changing over seasons?
.IsSeasonalRecruitment <- function(OM) {
  if (OM@Seasons == 1) 
    return(FALSE)
  
  R0Array <- purrr::map(OM@Stock, \(Stock) {
    Stock@SRR@R0
  }) |>
    List2Array("Stock") |>
    .Aperm(c("Sim", "Stock", "Year")) |>
    ReduceDims(IncYear = TRUE)
  
  dim(R0Array)[[3]] > 1
}

.CalcUnfishedNumberSeasonal <- function(OM, SP = FALSE) {
  OM        <- PopulateOM(OM, silent = TRUE)
  Years     <- Years(OM,'Hist')
  nYear     <- OM@nYear
  StockList <- MakeNamedList(StockNames(OM))
  
  for (st in 1:nStock(OM)) {
    Stock      <- OM@Stock[[st]]
    AgeClasses <- Stock@Ages@Classes
    MaxAge     <- max(AgeClasses)
    nAge       <- length(AgeClasses)
    nSim       <- Stock@nSim
    nSeason    <- OM@Seasons
    
    N_Stock <- array(NA, dim=c(nSim, nAge, length(Years)),
                     dimnames=list(Sim=1:nSim, Age=AgeClasses, Year=Years))
    
    # R0 Sim by Year                              
    R0 <- Extend(Stock@SRR@R0, nSim = nSim,Years = Years)
    
    # Sim, Age, Year
    NaturalMortality <- Extend(Stock@NaturalMortality@MeanAtAge, 
                               nSim = nSim, 
                               AgeClasses = AgeClasses, 
                               Years = Years)
    
    PlusGroup     <- Stock@Ages@PlusGroup
    SpawnTimeFrac <- ifelse(SP, Stock@SRR@SpawnTimeFrac, 0)
    SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[1:nSim]
    Semelparous   <- Extend(Stock@Maturity@Semelparous, 
                            nSim = nSim, 
                            AgeClasses = AgeClasses, 
                            Years = Years)

    # Check if seasonal values vary over years
    GetSeasonBlock2d <- function(x, y, nSeason) {
      idx <- ((y - 1) * nSeason + 1):(y * nSeason)
      x[, idx, drop = FALSE]
    }
    
    GetSeasonBlock3d <- function(x, y, nSeason) {
      idx <- ((y - 1) * nSeason + 1):(y * nSeason)
      x[, , idx, drop = FALSE]
    }
    
    identical_years <- all(
      purrr::map_lgl(2:nYear, ~
                isTRUE(all.equal(GetSeasonBlock2d(R0, 1, nSeason),
                                 GetSeasonBlock2d(R0, .x, nSeason),
                                 check.attributes = FALSE)) &&
                isTRUE(all.equal(GetSeasonBlock3d(NaturalMortality, 1, nSeason),
                                 GetSeasonBlock3d(NaturalMortality, .x, nSeason),
                                 check.attributes = FALSE)) &&
                isTRUE(all.equal(GetSeasonBlock3d(Semelparous, 1, nSeason),
                                 GetSeasonBlock3d(Semelparous, .x, nSeason),
                                 check.attributes = FALSE))
      )
    )
    
    if (identical_years) {
      SeasonInd <- 1:nSeason
      N_eq <- .CalcUnfishedNumberEquilibriumSeason(
        R0_season     = R0[, SeasonInd, drop = FALSE],
        M_season      = NaturalMortality[, , SeasonInd, drop = FALSE],
        Semel_season  = Semelparous[, , SeasonInd, drop = FALSE],
        SpawnTimeFrac = SpawnTimeFrac,
        AgeClasses    = AgeClasses,
        PlusGroup     = PlusGroup,
        nSeason       = nSeason
      )
      N_Stock[] <- array(rep(N_eq, nYear), dim = dim(N_Stock))
    } else {
      # Loop over years
      for (y in 1:nYear) {
        SeasonInd <- ((y - 1) * nSeason + 1):(y * nSeason)
        N_Stock[,,SeasonInd] <- .CalcUnfishedNumberEquilibriumSeason(R0_season=R0[,SeasonInd, drop=FALSE],
                                                                      M_season=NaturalMortality[,,SeasonInd, drop=FALSE],
                                                                      Semel_season=Semelparous[,,SeasonInd, drop=FALSE],
                                                                      SpawnTimeFrac,
                                                                      AgeClasses,
                                                                      PlusGroup,
                                                                      nSeason)

      }
    }
    StockList[[st]] <- N_Stock
    
  }
  StockList
}


# Iterates until reaching stable age structure
.CalcUnfishedNumberEquilibriumSeason <- function(R0_season,
                                                  M_season,
                                                  Semel_season,
                                                  SpawnTimeFrac,
                                                  AgeClasses,
                                                  PlusGroup,
                                                  nSeason,
                                                  tol = 1e-10,
                                                  max_iter = 5000) {

  nSim <- dim(R0_season)[1]
  nAge <- length(AgeClasses)

  NumberSeason <- array(1, dim = c(nSim, nAge, nSeason))

  for (iter in seq_len(max_iter)) {
    Number_old <- NumberSeason

    for (s in seq_len(nSeason)) {
      s_prev     <- ifelse(s == 1, nSeason, s - 1)
      N_prev     <- abind::adrop(NumberSeason[, , s_prev, drop = FALSE], 3)
      N_new      <- matrix(0, nrow = nSim, ncol = nAge)
      N_new[, 1] <- R0_season[, s]
      M_s        <- abind::adrop(M_season[, , s, drop = FALSE], 3)
      Z_pre      <- sweep(M_s, 1L, SpawnTimeFrac, `*`)
      N_spawn    <- N_prev * exp(-Z_pre)

      N_post_spawn <- N_spawn * (1 - abind::adrop(Semel_season[, , s, drop=FALSE], 3))

      Z_post    <- sweep(M_s, 1L, 1 - SpawnTimeFrac, `*`)
      N_survive <- N_post_spawn * exp(-Z_post)

      N_new[, 2:nAge] <- N_survive[, 1:(nAge - 1)]

      if (PlusGroup) {
        N_new[, nAge] <- N_new[, nAge] + N_survive[, nAge]
      }

      NumberSeason[, , s] <- N_new
    }

    if (max(abs(NumberSeason - Number_old)) < tol) break
  }

  if (iter == max_iter) {
    cli::cli_alert_warning("Equilibrium not reached for at least one simulation in `.CalcUnfishedNumberEquilibriumSeason`")
  }

  SpawnNumberSeason <- array(0, dim = c(nSim, nAge, nSeason))
  for (s in seq_len(nSeason)) {
    M_s   <- abind::adrop(M_season[, , s, drop = FALSE], 3)
    Z_pre <- sweep(M_s, 1L, SpawnTimeFrac, `*`)
    SpawnNumberSeason[, , s] <- abind::adrop(NumberSeason[, , s, drop = FALSE], 3) * exp(-Z_pre)
  }
  SpawnNumberSeason
}




