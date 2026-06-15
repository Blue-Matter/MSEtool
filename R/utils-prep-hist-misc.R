# Prepares Misc slot in Hist for temp objects that appropriately structured for
# C++ code

# This prepares arrays for easy access in the C++ code
# temporary elements of Misc are removed later
PrepHistMisc <- function(Hist, Period = c('Historical', 'Projection')) {
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
    stock@SRR@SPFrom
  }) |> unlist() |> array(dim=nStock(Hist), dimnames = list(Stock=StockNames(Hist)))
  
  Hist@Misc$PlusGroup <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Ages@PlusGroup
  }) |> unlist() |> as.numeric() |> array(dim=nStock(Hist), dimnames = list(Stock=StockNames(Hist)))
  
  Hist@Misc$Mode <- purrr::map_int(Hist@OM@Fleet[[1]], \(fleet) {
    mode <- fleet@Effort@Mode
    if (mode == 'Density') return(1)
    0
    })
  
  ## ---- 2D Array ----
  
  # Sim, Year - must be the same for all stocks
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

  # Unfished Spawning Production
  Hist@Misc$SP0 <- Hist@Unfished@Equilibrium@SProduction
  
  
  R0_list <- purrr::map(Hist@OM@Stock, \(stock){
    stock@SRR@R0  
  })
  nSim_R0 <- lapply(R0_list, dim) |> lapply('[',1) |> unlist() |> max()
  Hist@Misc$R0 <- purrr::map(R0_list, \(st) ExtendSims(st, nSim_R0)) |>
    List2Array('Stock', pos=2)
  
  # Unfished distribution
  Hist@Misc$RecDist <- purrr::map(Hist@OM@Stock, \(stock) {
    abind::adrop(stock@Spatial@UnfishedDist[,,1,, drop=FALSE], 3) 
  }) |> List2Array("Stock", pos=2) |>
    aperm(c('Sim', 'Stock', 'Year', 'Area'))
  
  
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
  
  ##  ---- Lists - length nStock ---- 
  Hist@Misc$WeightFleetList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@WeightFleet
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet
  })
  
  Hist@Misc$SelAgeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Selectivity@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  # Hist@Misc$SelSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
  #   purrr::map(FleetList, \(fleet) {
  #     if (!is.null(  fleet@Selectivity@MeanAtLength)) 
  #     return(fleet@Selectivity@MeanAtLength) # Sim, Age, Year, Area
  #     fleet@Selectivity@MeanAtWeight
  #   }) 
  # })
  
  Hist@Misc$RetAgeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Retention@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  # Hist@Misc$RetSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
  #   purrr::map(FleetList, \(fleet) {
  #     if (!is.null(  fleet@Retention@MeanAtLength)) 
  #       return(fleet@Retention@MeanAtLength) # Sim, Age, Year, Area
  #     fleet@Retention@MeanAtWeight
  #   }) 
  # })
  
  Hist@Misc$DiscMortList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@DiscardMortality@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  # Hist@Misc$DiscMortSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
  #   purrr::map(FleetList, \(fleet) {
  #     fleet@DiscardMortality@MeanAtLength  # Sim, Class, Year, Area
  #   }) 
  # })
  
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
  
  Hist@Misc <- ExtendYears(Hist@Misc, Years=YearVec)
  CheckHistMisc(Hist, Period)
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
RestoreHistMisc <- function(Hist) {
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