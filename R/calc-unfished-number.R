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
  N_independent <- purrr::map2(UnfishedSurvival_List, R0List, ArrayMultiply)

  if (!length(OM@Herm))
    return(N_independent)

  .CalcUnfishedNumberHerm(OM, N_independent, SP)
}


.CalcUnfishedNumberHerm <- function(OM, N_independent, SP = FALSE,
                                    tol = 1e-10, max_iter = 5000) {

  Years  <- Years(OM, 'Hist')
  nYear  <- length(Years)
  nSim   <- OM@nSim
  nStock <- nStock(OM)

  Resolved <- .HermResolvePairs(OM, Years)
  From <- Resolved$From; To <- Resolved$To; HazardList <- Resolved$Hazard

  Components <- .HermConnectedComponents(From, To, nStock)
  stock_names <- StockNames(OM)

  OutList <- N_independent

  for (comp in Components) {
    if (length(comp) < 2L) next  # not Herm-linked; N_independent already correct

    AgeClasses <- OM@Stock[[comp[1]]]@Ages@Classes
    for (s in comp[-1]) {
      if (!identical(OM@Stock[[s]]@Ages@Classes, AgeClasses))
        cli::cli_abort(c(
          "x" = "Herm-linked stocks must share identical age classes.",
          "i" = "Stock {.val {stock_names[comp[1]]}} and {.val {stock_names[s]}} have different {.field Ages@Classes}."
        ))
    }
    nAge <- length(AgeClasses)

    # per-stock full-year (SpawnTimeFrac = 0) inputs, extended to full nSim/Years
    Z <- purrr::map(comp, \(s) {
      Extend(OM@Stock[[s]]@NaturalMortality@MeanAtAge, nSim = nSim,
            AgeClasses = AgeClasses, Years = Years) |> .ArraySubsetYear(Years)
    }) |> stats::setNames(comp)

    Semel <- purrr::map(comp, \(s) {
      Extend(OM@Stock[[s]]@Maturity@Semelparous, nSim = nSim,
            AgeClasses = AgeClasses, Years = Years) |> .ArraySubsetYear(Years)
    }) |> stats::setNames(comp)

    R0 <- purrr::map(comp, \(s) {
      Extend(OM@Stock[[s]]@SRR@R0, nSim = nSim, Years = Years) |> .ArraySubsetYear(Years)
    }) |> stats::setNames(comp)

    PlusGroupFlag <- purrr::map_lgl(comp, \(s) isTRUE(OM@Stock[[s]]@Ages@PlusGroup)) |>
      stats::setNames(comp)

    Step <- purrr::map(comp, \(s) exp(-Z[[as.character(s)]]) * (1 - Semel[[as.character(s)]])) |>
      stats::setNames(comp)

    pair_idx <- which(From %in% comp)

    N <- stats::setNames(vector('list', length(comp)), comp)
    for (s in comp) {
      N[[as.character(s)]] <- array(0, dim = c(nSim, nAge, nYear),
                                    dimnames = list(Sim = seq_len(nSim),
                                                    Age = AgeClasses,
                                                    Year = Years))
      N[[as.character(s)]][, 1, ] <- R0[[as.character(s)]]
    }

    ApplyPairsAtAge <- function(RawList, a) {
      for (p in pair_idx) {
        f <- as.character(From[p]); t <- as.character(To[p])
        Nmov <- RawList[[f]] * HazardList[[p]][, a, ]
        RawList[[t]] <- RawList[[t]] + Nmov
        RawList[[f]] <- RawList[[f]] - Nmov
      }
      RawList
    }

    if (nAge > 2L) {
      for (a in 2:(nAge - 1L)) {
        Raw <- stats::setNames(
          purrr::map(comp, \(s) N[[as.character(s)]][, a - 1, ] * Step[[as.character(s)]][, a - 1, ]),
          comp
        )
        Raw <- ApplyPairsAtAge(Raw, a)
        for (s in comp) N[[as.character(s)]][, a, ] <- Raw[[as.character(s)]]
      }
    }

    # terminal age: direct if no PlusGroup among comp, else iterate 
    if (nAge >= 2L) {
      Inflow <- stats::setNames(
        purrr::map(comp, \(s) N[[as.character(s)]][, nAge - 1, ] * Step[[as.character(s)]][, nAge - 1, ]),
        comp
      )
    } else {
      Inflow <- stats::setNames(purrr::map(comp, \(s) R0[[as.character(s)]]), comp)
    }
    SelfSurv <- stats::setNames(
      purrr::map(comp, \(s) exp(-Z[[as.character(s)]][, nAge, ]) * (1 - Semel[[as.character(s)]][, nAge, ])),
      comp
    )

    if (!any(PlusGroupFlag)) {
      Raw <- Inflow
      Raw <- ApplyPairsAtAge(Raw, nAge)
      for (s in comp) N[[as.character(s)]][, nAge, ] <- Raw[[as.character(s)]]
    } else {
      Term <- stats::setNames(purrr::map(comp, \(s) N_independent[[s]][, nAge, ]), comp)

      converged <- FALSE
      for (iter in seq_len(max_iter)) {
        TermOld <- Term
        Raw <- stats::setNames(
          purrr::map(comp, \(s) {
            sc <- as.character(s)
            Inflow[[sc]] + if (PlusGroupFlag[[sc]]) SelfSurv[[sc]] * Term[[sc]] else 0
          }),
          comp
        )
        Raw <- ApplyPairsAtAge(Raw, nAge)
        Term <- Raw

        delta <- max(purrr::map_dbl(comp, \(s) {
          sc <- as.character(s)
          max(abs(Term[[sc]] - TermOld[[sc]]))
        }))
        if (delta < tol) { converged <- TRUE; break }
      }

      if (!converged)
        cli::cli_alert_warning(
          "Equilibrium not reached for at least one simulation in `.CalcUnfishedNumberHerm` (stocks {.val {stock_names[comp]}})"
        )

      for (s in comp) N[[as.character(s)]][, nAge, ] <- Term[[as.character(s)]]
    }

    for (s in comp) {
      Nfull <- N[[as.character(s)]]
      if (isTRUE(SP)) {
        frac <- OM@Stock[[s]]@SRR@SpawnTimeFrac
        frac <- rep(frac, nSim)[seq_len(nSim)]
        Zs <- Z[[as.character(s)]]
        SpawnMult <- exp(-sweep(Zs, 1L, frac, `*`))
        Nfull <- Nfull * SpawnMult
      }
      OutList[[s]] <- Nfull
    }
  }

  OutList
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
  nSeason   <- OM@Seasons
  StockList <- MakeNamedList(StockNames(OM))

  Resolved   <- .HermResolvePairs(OM, Years)
  Components <- .HermConnectedComponents(Resolved$From, Resolved$To, nStock(OM))

  for (comp in Components) {
    if (length(comp) < 2L) {
      st <- comp
      StockList[[st]] <- .CalcUnfishedNumberSeasonalStock(
        OM@Stock[[st]], Years, nYear, nSeason, SP
      )
    } else {
      StockList[comp] <- .CalcUnfishedNumberHermSeasonal(
        OM, comp, Resolved, Years, nYear, nSeason, SP
      )
    }
  }
  StockList
}


.CalcUnfishedNumberSeasonalStock <- function(Stock, Years, nYear, nSeason, SP = FALSE) {
  AgeClasses <- Stock@Ages@Classes
  nAge       <- length(AgeClasses)
  nSim       <- Stock@nSim

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
  N_Stock
}


# Joint seasonal unfished-equilibrium calc for a Herm-connected group of
# stocks: mirrors .CalcUnfishedNumberSeasonalStock()'s per-season recursion,
# but for all stocks in `comp` at once, transferring numbers-at-age between
# stocks (via the resolved Herm hazard) at every season step rather than once
# per age/year as .CalcUnfishedNumberHerm() does for the non-seasonal case.
.CalcUnfishedNumberHermSeasonal <- function(OM, comp, Resolved, Years, nYear, nSeason,
                                            SP = FALSE, tol = 1e-10, max_iter = 5000) {
  From <- Resolved$From; To <- Resolved$To; HazardList <- Resolved$Hazard
  pair_idx <- which(From %in% comp)
  stock_names <- StockNames(OM)

  AgeClasses <- OM@Stock[[comp[1]]]@Ages@Classes
  for (s in comp[-1]) {
    if (!identical(OM@Stock[[s]]@Ages@Classes, AgeClasses))
      cli::cli_abort(c(
        "x" = "Herm-linked stocks must share identical age classes.",
        "i" = "Stock {.val {stock_names[comp[1]]}} and {.val {stock_names[s]}} have different {.field Ages@Classes}."
      ))
  }
  nAge <- length(AgeClasses)
  nSim <- OM@nSim

  R0List    <- stats::setNames(purrr::map(comp, \(s)
    Extend(OM@Stock[[s]]@SRR@R0, nSim = nSim, Years = Years)), comp)
  MList     <- stats::setNames(purrr::map(comp, \(s)
    Extend(OM@Stock[[s]]@NaturalMortality@MeanAtAge, nSim = nSim,
          AgeClasses = AgeClasses, Years = Years)), comp)
  SemelList <- stats::setNames(purrr::map(comp, \(s)
    Extend(OM@Stock[[s]]@Maturity@Semelparous, nSim = nSim,
          AgeClasses = AgeClasses, Years = Years)), comp)
  PlusGroupFlag <- stats::setNames(purrr::map_lgl(comp, \(s)
    isTRUE(OM@Stock[[s]]@Ages@PlusGroup)), comp)
  SpawnTimeFracList <- stats::setNames(purrr::map(comp, \(s) {
    frac <- ifelse(SP, OM@Stock[[s]]@SRR@SpawnTimeFrac, 0)
    rep(frac, nSim)[seq_len(nSim)]
  }), comp)

  OutArrays <- stats::setNames(
    purrr::map(comp, \(s) array(NA, dim = c(nSim, nAge, length(Years)),
                                dimnames = list(Sim=1:nSim, Age=AgeClasses, Year=Years))),
    comp)

  for (y in 1:nYear) {
    SeasonInd <- ((y - 1) * nSeason + 1):(y * nSeason)

    R0_season     <- stats::setNames(purrr::map(comp, \(s)
      R0List[[as.character(s)]][, SeasonInd, drop=FALSE]), comp)
    M_season      <- stats::setNames(purrr::map(comp, \(s)
      MList[[as.character(s)]][, , SeasonInd, drop=FALSE]), comp)
    Semel_season  <- stats::setNames(purrr::map(comp, \(s)
      SemelList[[as.character(s)]][, , SeasonInd, drop=FALSE]), comp)
    Hazard_season <- stats::setNames(purrr::map(pair_idx, \(p)
      HazardList[[p]][, , SeasonInd, drop=FALSE]), pair_idx)

    N_eq <- .CalcUnfishedNumberHermEquilibriumSeason(
      comp = comp, From = From, To = To, pair_idx = pair_idx,
      R0_season = R0_season, M_season = M_season, Semel_season = Semel_season,
      Hazard_season = Hazard_season, SpawnTimeFracList = SpawnTimeFracList,
      AgeClasses = AgeClasses, PlusGroupFlag = PlusGroupFlag, nSeason = nSeason,
      tol = tol, max_iter = max_iter
    )
    for (s in comp) OutArrays[[as.character(s)]][,,SeasonInd] <- N_eq[[as.character(s)]]
  }
  OutArrays
}


# Iterates until reaching a stable seasonal age structure, jointly across all
# stocks in `comp`, transferring numbers-at-age between paired stocks (per
# `Hazard_season`) at every season step, ages 2:nAge 
.CalcUnfishedNumberHermEquilibriumSeason <- function(comp, From, To, pair_idx,
                                                     R0_season, M_season, Semel_season,
                                                     Hazard_season, SpawnTimeFracList,
                                                     AgeClasses, PlusGroupFlag, nSeason,
                                                     tol = 1e-10, max_iter = 5000) {
  nSim <- dim(R0_season[[as.character(comp[1])]])[1]
  nAge <- length(AgeClasses)

  NumberSeason <- stats::setNames(
    purrr::map(comp, \(s) array(1, dim = c(nSim, nAge, nSeason))), comp)

  ApplyPairs <- function(RawList, s) {
    for (p in pair_idx) {
      f <- as.character(From[p]); t <- as.character(To[p])
      HazSlice <- abind::adrop(Hazard_season[[as.character(p)]][, 2:nAge, s, drop=FALSE], 3)
      Nmov <- RawList[[f]] * HazSlice
      RawList[[t]] <- RawList[[t]] + Nmov
      RawList[[f]] <- RawList[[f]] - Nmov
    }
    RawList
  }

  converged <- FALSE
  for (iter in seq_len(max_iter)) {
    Old <- NumberSeason

    for (s in seq_len(nSeason)) {
      s_prev <- ifelse(s == 1, nSeason, s - 1)

      New <- stats::setNames(vector('list', length(comp)), comp)
      for (st in comp) {
        sc     <- as.character(st)
        N_prev <- abind::adrop(NumberSeason[[sc]][, , s_prev, drop = FALSE], 3)
        M_s    <- abind::adrop(M_season[[sc]][, , s, drop = FALSE], 3)
        SpawnTimeFrac <- SpawnTimeFracList[[sc]]

        Z_pre     <- sweep(M_s, 1L, SpawnTimeFrac, `*`)
        N_spawn   <- N_prev * exp(-Z_pre)
        N_post    <- N_spawn * (1 - abind::adrop(Semel_season[[sc]][, , s, drop=FALSE], 3))
        Z_post    <- sweep(M_s, 1L, 1 - SpawnTimeFrac, `*`)
        N_survive <- N_post * exp(-Z_post)

        N_new <- matrix(0, nrow = nSim, ncol = nAge)
        N_new[, 1] <- R0_season[[sc]][, s]
        N_new[, 2:nAge] <- N_survive[, 1:(nAge - 1)]
        if (PlusGroupFlag[[sc]]) {
          N_new[, nAge] <- N_new[, nAge] + N_survive[, nAge]
        }
        New[[sc]] <- N_new
      }

      Raw <- stats::setNames(purrr::map(comp, \(st) New[[as.character(st)]][, 2:nAge, drop=FALSE]), comp)
      Raw <- ApplyPairs(Raw, s)
      for (st in comp) {
        sc <- as.character(st)
        New[[sc]][, 2:nAge] <- Raw[[sc]]
        NumberSeason[[sc]][, , s] <- New[[sc]]
      }
    }

    delta <- max(purrr::map_dbl(comp, \(st) {
      sc <- as.character(st)
      max(abs(NumberSeason[[sc]] - Old[[sc]]))
    }))
    if (delta < tol) { converged <- TRUE; break }
  }

  if (!converged)
    cli::cli_alert_warning(
      "Equilibrium not reached for at least one simulation in `.CalcUnfishedNumberHermEquilibriumSeason`"
    )

  stats::setNames(purrr::map(comp, \(st) {
    sc <- as.character(st)
    SpawnNumberSeason <- array(0, dim = c(nSim, nAge, nSeason))
    for (s in seq_len(nSeason)) {
      M_s   <- abind::adrop(M_season[[sc]][, , s, drop = FALSE], 3)
      Z_pre <- sweep(M_s, 1L, SpawnTimeFracList[[sc]], `*`)
      SpawnNumberSeason[, , s] <- abind::adrop(NumberSeason[[sc]][, , s, drop = FALSE], 3) * exp(-Z_pre)
    }
    SpawnNumberSeason
  }), comp)
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




