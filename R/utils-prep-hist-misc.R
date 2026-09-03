# Prepares Misc slot in Hist for temp objects that appropriately structured for
# C++ code

# This prepares arrays for easy access in the C++ code
# temporary elements of Misc are removed later
.PrepHistMisc <- function(Hist, Period = c('Historical', 'Projection')) {
  Period <- match.arg(Period)
  
  YearVec        <- Years(Hist, Period)
  saveMisc       <- Hist@Misc
  Hist@Misc      <- list()
  Hist@Misc$SAVE <- saveMisc
  
  stock_names <- StockNames(Hist)
  nStock      <- length(stock_names)
  fleet_names <- FleetNames(Hist)
  nFleet      <- length(fleet_names)
  
  Hist@Misc$maxF <- Hist@OM@maxF

  # ---- Stock -----
  
  ## ---- Vector ----
  
  Hist@Misc$SPFrom <- purrr::map(Hist@OM@Stock, \(stock) {
    match(stock@SRR@SPFrom, stock_names)
  }) |> unlist() |> array(dim=nStock, dimnames = list(Stock=stock_names))
  
  Hist@Misc$PlusGroup <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Ages@PlusGroup
  }) |> unlist() |> as.numeric() |> array(dim=nStock(Hist), dimnames = list(Stock=StockNames(Hist)))

  Hist@Misc$Mode <- purrr::map_int(Hist@OM@Fleet[[1]], \(fleet) {
    mode <- fleet@Effort@Mode
    if (mode == 'Density') return(1)
    0
    })
  
  ## ---- 2D Array ----
  # Sim, Year 
  Hist@Misc$RelSize <- Hist@OM@Stock[[1]]@Spatial@RelativeSize
  
  # Sim Stock
  Hist@Misc$SpawnTimeFrac <- purrr::map(Hist@OM@Stock, \(stock) {
    array(rep(stock@SRR@SpawnTimeFrac, Hist@OM@nSim)[1:Hist@OM@nSim],
          dim=Hist@OM@nSim, 
          dimnames = list(Sim=1:Hist@OM@nSim)
    )
  }) |> List2Array('Stock') |>
    ReduceDims(IncYear=FALSE)
  
  ## ---- Stock Lists ----
  
  # Sim, Age, Year
  Hist@Misc$LengthList <- purrr::map(Hist@OM@Stock, \(stock) {
    if (!is.null(stock@Length@MeanAtAge))
      return(stock@Length@MeanAtAge)

    # hasn't been specified. Make a dummy array
    array(0, dim=c(1, 1, 1))
    
  })
 
  # Sim, Age, Year
  Hist@Misc$WeightList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Weight@MeanAtAge
  })
 
  # Sim, Age, Year
  Hist@Misc$NaturalMortalityList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@NaturalMortality@MeanAtAge
  })
  
  # Sim, Age, Year
  Hist@Misc$MaturityList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Maturity@MeanAtAge
  })
  
  # Sim, Age, Year
  Hist@Misc$SemelparousList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Maturity@Semelparous
  })
  
  #  Sim, Age, Year
  Hist@Misc$FecundityList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Fecundity@MeanAtAge
  })
  
  #  Sim, FromArea, ToArea, Age, Year
  Hist@Misc$MovementList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Spatial@Movement
  })
  
  ## ---- SRR ----
  
  # Calculate recruitment lag
  Hist@Misc$RecLag <- purrr::map(Hist@OM@Stock, \(stock) {
    if (!is.null(stock@SRR@SpawnLag))
      return(as.integer(round(stock@SRR@SpawnLag)))
    MinAge <- min(stock@Ages@Classes)
    MaxAge <- max(stock@Ages@Classes)
    nSeasons <- stock@Seasons
    AllAges <- seq(from = 0, to=MaxAge, by=1/nSeasons) |> round(3)
    which(AllAges == MinAge) - 1
  }) |> unlist()
  
  # SRR parameters
  Hist@Misc$SRR_Pars <- purrr::map(Hist@OM@Stock, \(stock) stock@SRR@Pars)
  
  # SRR rec devs
  Hist@Misc$RecDevs <- purrr::map(Hist@OM@Stock, \(stock) {
    dd <- dim(stock@SRR@RecDevProj)
    hist <- ExtendSims(stock@SRR@RecDevHist, dd[1])
    proj <- stock@SRR@RecDevProj 
    abind::abind(hist, proj, along=2, 
                 use.first.dimnames=TRUE,
                 use.dnns=TRUE) 
  })
  
  # SRR Model
  Hist@Misc$SRR_Model <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@SRR@Model
  }) |> unlist()

  srr_models <- SRRModels(FALSE, FALSE)  
  if (!all(Hist@Misc$SRR_Model %in% srr_models)) {
    cli::cli_abort(c("x" = "Invalid SRR model(s). Must be one of {.val {srr_models}}",
                   "i"= "Currently: {.val { Hist@Misc$SRR_Model}}")
    )
  }
  for (i in seq_along(Hist@Misc$SRR_Model)) {
    Hist@Misc$SRR_Model[i] <- switch( Hist@Misc$SRR_Model[i],
                                      "BevertonHolt"=0,
                                      "Ricker"=1,
                                      "HockeyStick"=2)
  }
  Hist@Misc$SRR_Model <- as.numeric(Hist@Misc$SRR_Model)

  # Unfished Recruitment (R0)
  R0_list <- purrr::map(Hist@OM@Stock, \(stock){
    stock@SRR@R0
  })
  nSim_R0 <- lapply(R0_list, dim) |> lapply('[',1) |> unlist() |> max()
  Hist@Misc$R0 <- purrr::map(R0_list, \(st) ExtendSims(st, nSim_R0)) |>
    List2Array('Stock', pos=2)

  # Unfished Spawning Production: SP0(y) = phi0_ref(season) x R0(y + RecLag).
  # phi0_ref is frozen at its first-year value per season (so alpha/beta
  # implied by a constant steepness don't drift under time-varying M/growth/
  # maturity), broadcast via ExtendYears(maintain_seasonal_pattern=TRUE)
  # rather than a flat slice since R0/phi0_ref can be zero in non-spawning
  # seasons. R0 is shifted forward by RecLag before pairing with SP0's season
  # (R0's nonzero season is birthseas, SP0's is the spawning season, offset
  # by RecLag) -- otherwise phi0_ref divides by R0's zero spawn-season value
  # and SP0 collapses to zero exactly where C++ CalcRecruitment() evaluates it.
  RawSP0 <- Hist@Unfished@Equilibrium@SProduction |> ExtendSims(nSim = nSim(Hist))
  SP0Dim <- which(names(dimnames(RawSP0)) == 'Year')
  R0Dim  <- which(names(dimnames(Hist@Misc$R0)) == 'Year')

  AllYears <- as.numeric(dimnames(RawSP0)[[SP0Dim]])
  Yr1Ind   <- which(floor(AllYears) == floor(min(AllYears)))

  # Size the shift off R0's own Year axis -- it need not match RawSP0's
  # Year axis length (e.g. if the unfished equilibrium is only computed
  # over the historical period while R0 spans historical + projection).
  nYearR0   <- dim(Hist@Misc$R0)[R0Dim]
  R0Shifted <- Hist@Misc$R0
  for (st in seq_len(nStock(Hist))) {
    lag <- Hist@Misc$RecLag[st]
    if (lag == 0) next
    shiftInd <- pmin(seq_len(nYearR0) + lag, nYearR0)
    R0Shifted[, st, ] <- Hist@Misc$R0[, st, shiftInd]
  }

  SP0Yr1      <- abind::asub(RawSP0,    Yr1Ind, SP0Dim, drop = FALSE)
  R0Yr1       <- abind::asub(R0Shifted, Yr1Ind, R0Dim,  drop = FALSE)
  Phi0RefSeed <- ArrayDivide(SP0Yr1, R0Yr1)

  Phi0RefFull <- ExtendYears(Phi0RefSeed, Years = AllYears,
                             maintain_seasonal_pattern = TRUE)
  Hist@Misc$SP0 <- ArrayMultiply(Phi0RefFull, R0Shifted)

  # Unfished distribution
  Hist@Misc$RecDist <- purrr::map(Hist@OM@Stock, \(stock) {
    abind::adrop(stock@Spatial@UnfishedDist[,,1,, drop=FALSE], 3) 
  }) |> List2Array("Stock", pos=2) |>
    .Aperm(c('Sim', 'Stock', 'Year', 'Area'))
  
  
  # ---- Fleet ----
  
  # 4D Array: Sim, Stock, Year, Fleet
  Hist@Misc$Catchability <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      Extend(fleet@Catchability@Efficiency, nSim = Hist@OM@nSim)
    }) |> List2Array(pos = 3) # Sim, Year, Fleet
  }) |> List2Array(pos = 2, "Stock") # Sim, Stock, Year, Fleet
  
  # 5D Array: Sim, Stock, Year, Fleet, Area
  Hist@Misc$Closure <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Closure
    }) |> List2Array(pos = 3) # Sim, Year, Fleet, Area
  }) |> List2Array(pos = 2, "Stock") # Sim, Stock, Year, Fleet, Area
  
  # 3D Array: Sim, Year, Fleet
  Hist@Misc$Spatial_Targeting <- purrr::map(Hist@OM@Fleet[[1]], \(fleet) {
    fleet@Effort@Targeting
  }) |> List2Array(pos = 3) # Sim, Year, Fleet

  # 4D Array: Sim, Stock, Year, Fleet. Multiplier on the derived stock-targeting
  # resistance; NULL anywhere defaults to 1 (derived value unchanged)
  Hist@Misc$StockTargetingLambda <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      lam <- fleet@Effort@StockTargetingLambda
      if (is.null(lam))
        lam <- array(1, c(1, 1), dimnames = list(Sim = 1, Year = YearVec[1]))
      lam
    }) |> List2Array(pos = 3) # Sim, Year, Fleet
  }) |> List2Array(pos = 2, "Stock") # Sim, Stock, Year, Fleet
  
  ##  ---- Lists - length nStock ---- 
  Hist@Misc$WeightFleetRetainedList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@WeightFleetRetained
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet
  })

  Hist@Misc$WeightFleetSelectedList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@WeightFleetSelected
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet
  })


  Hist@Misc$SelAgeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Selectivity@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  Hist@Misc$SelSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      if (!is.null(  fleet@Selectivity@MeanAtLength))
      return(fleet@Selectivity@MeanAtLength) # Sim, Age, Year, Area
      fleet@Selectivity@MeanAtWeight
    })
  })
  
  Hist@Misc$RetAgeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Retention@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  Hist@Misc$RetSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      if (!is.null(  fleet@Retention@MeanAtLength))
        return(fleet@Retention@MeanAtLength) # Sim, Age, Year, Area
      fleet@Retention@MeanAtWeight
    })
  })
  
  Hist@Misc$DiscMortList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@DiscardMortality@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  Hist@Misc$DiscMortSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@DiscardMortality@MeanAtLength  # Sim, Class, Year, Area
    })
  })
  
  # OM-level: StockTargeting 
  if (nStock(Hist) == 1 || is.null(Hist@OM@StockTargeting@Targeting)) {
    # single stock 
    
    Hist@Misc$StockTargeting <- array(1, 
                                      dim = c(1, 1, nFleet, 1),
                                      dimnames = list(
                                        Sim   = 1,
                                        Stock = stock_names,
                                        Fleet = fleet_names,
                                        Year  = YearVec[1]
                                        
                                      ))
    Hist@Misc$StockTargetingFlag <- 0
  } else {
    Hist@Misc$StockTargeting <- Hist@OM@StockTargeting@Targeting
    Hist@Misc$StockTargetingFlag <- 1
  }

  # OM-level: Herm / stock transition
  HermResolved <- .HermResolvePairs(Hist@OM, YearVec)
  if (!length(HermResolved$From)) {
    Hist@Misc$TransitionHazard    <- list()
    Hist@Misc$TransitionToStock   <- integer(0)
    Hist@Misc$TransitionFromStock <- integer(0)
    Hist@Misc$TransitionFlag      <- 0
  } else {
    Hist@Misc$TransitionToStock   <- HermResolved$To
    Hist@Misc$TransitionFromStock <- HermResolved$From
    Hist@Misc$TransitionHazard    <- HermResolved$Hazard
    Hist@Misc$TransitionFlag      <- 1
  }

  Hist@Misc <- ExtendYears(Hist@Misc, Years=YearVec)
  .CheckHistMisc(Hist, Period)
  Hist
}


#' Restore `@Misc` Slot After a Projection
#'
#' Restores the `@Misc` slot of a `hist-class` or `mse-class` object to its
#' saved state, then re-attaches any named components that were preserved
#' separately (`Advice`, `Selectivity`, `Retention`, `DiscardMortality`).
#'
#' During a projection run, the full `@Misc` contents are stashed in
#' `@Misc$SAVE` and named components are stored alongside it. This function
#' reverses that process: it replaces `@Misc` with the stashed contents and
#' then writes back any non-`NULL` named components so they are not lost.
#'
#' @param Hist A `hist-class` or `mse-class` object whose `@Misc$SAVE` slot
#'   contains the stashed `@Misc` list.
#'
#' @return The input object with `@Misc` restored.
#' @keywords internal
.RestoreHistMisc <- function(Hist) {
  preserved_names <- c('Advice', 'Selectivity', 'Retention', 'DiscardMortality')
  saved <- purrr::map(
    c('SAVE', preserved_names),
    \(nm) Hist@Misc[[nm]]
  ) |> purrr::set_names(c('SAVE', preserved_names))
  
  Hist@Misc <- saved$SAVE %||% list()
  
  for (nm in preserved_names) {
    if (!is.null(saved[[nm]]))
      Hist@Misc[[nm]] <- saved[[nm]]
  }
  Hist
}
