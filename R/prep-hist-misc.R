# Prepares Misc slot in Hist for temp objects that appropriately structured for
# C++ code



# Check the dimensions are correct
CheckDims <- function(array, ndim, name=NULL) {
  if (is.list(array)) {
    return(
      purrr::map(array, \(array) {
        CheckDims(array, ndim, name)
      })
    )
  }
  
  if (length(dim(array)) == ndim) return(NULL)
  
  cli::cli_abort(c("x"="Array {.val {name}} must have {.val {ndim}} dimensions",
                   "i"="Currently dim: {.val {dim(array)}}"), .internal=TRUE
  )
  
}
CheckLength <- function(vector, length, name=NULL) {
  if (length(vector)!=length) {
    cli::cli_abort(c("x"="Vector {.val {name}} must be length {.val {length}}",
                     "i"="Currently length: {.val {length(vector)}}"), .internal=TRUE
    )
  }
}



# This prepares arrays for easy access in the C++ code
# temporary elements of Misc are removed later
PrepHistMisc <- function(Hist, Period=c('Historical', 'Projection')) {
  Period <- match.arg(Period)
  saveMisc <- Hist@Misc
  Hist@Misc <- list()
  Hist@Misc$SAVE <- saveMisc
  
  nStock <- nStock(Hist)
  
  Hist@Misc$maxF <- Hist@OM@maxF

  # ---- 2D Array ----
  
  
  # ---- Stock -----
  
  ## ---- Vector ----
  
  Hist@Misc$SPFrom <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@SRR@SPFrom
  }) |> unlist() |> array(dim=nStock(Hist), dimnames = list(Stock=StockNames(Hist)))
  
  
  CheckLength(Hist@Misc$SPFrom, nStock, 'SPFrom')
  
  Hist@Misc$PlusGroup <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Ages@PlusGroup
  }) |> unlist() |> as.numeric() |> array(dim=nStock(Hist), dimnames = list(Stock=StockNames(Hist)))
  
  CheckLength(Hist@Misc$PlusGroup, nStock, 'PlusGroup')
  
  ## ---- 2D Array ----
  
  # Sim, Year - must be the same for all stocks
  Hist@Misc$RelSize <- Hist@OM@Stock[[1]]@Spatial@RelativeSize
  CheckDims( Hist@Misc$RelSize, 2, 'RelSize')
  
  # Sim Stock
  Hist@Misc$SpawnTimeFrac <- purrr::map(Hist@OM@Stock, \(stock) {
    array(rep(stock@SRR@SpawnTimeFrac, Hist@OM@nSim)[1:Hist@OM@nSim],
          dim=Hist@OM@nSim, 
          dimnames = list(Sim=1:Hist@OM@nSim)
    )
  }) |> List2Array('Stock') |>
    ReduceDims(IncYear=FALSE)
  
  CheckDims(Hist@Misc$SpawnTimeFrac, 2, 'SpawnTimeFrac')
  
  ## ---- Stock Lists ----
  
  # Sim, Age, Year
  Hist@Misc$LengthList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Length@MeanAtAge
  })
  CheckDims(Hist@Misc$LengthList, 3, 'LengthList')
  
  
  # Sim, Age, Year
  Hist@Misc$WeightList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Weight@MeanAtAge
  })
  CheckDims(Hist@Misc$WeightList, 3, 'WeightList')
  
  # Sim, Age, Year
  Hist@Misc$NaturalMortalityList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@NaturalMortality@MeanAtAge
  })
  CheckDims(Hist@Misc$NaturalMortalityList, 3, 'NaturalMortalityList')
  
  # Sim, Age, Year
  Hist@Misc$MaturityList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Maturity@MeanAtAge
  })
  CheckDims(Hist@Misc$MaturityList, 3, 'MaturityList')
  
  # Sim, Age, Year
  Hist@Misc$SemelparousList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Maturity@Semelparous
  })
  CheckDims(Hist@Misc$SemelparousList, 3, 'SemelparousList')
  
  #  Sim, Age, Year
  Hist@Misc$FecundityList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Fecundity@MeanAtAge
  })
  CheckDims(Hist@Misc$FecundityList, 3, 'FecundityList')
  
  #  Sim, FromArea, ToArea, Age, Year
  Hist@Misc$MovementList <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@Spatial@Movement
  })
  CheckDims(Hist@Misc$MovementList, 5, 'MovementList')
  
  ## ---- SRR ----
  
  # Calculate recruitment lag 
  Hist@Misc$RecLag <- purrr::map(Hist@OM@Stock, \(stock) {
    MinAge <- min(stock@Ages@Classes)
    which(seq(0, to=max(stock@Ages@Classes), by=1/stock@Seasons) == MinAge) - 1
  }) |> unlist()
  
  if (length(Hist@Misc$RecLag) != nStock) {
    cli::cli_abort("`Hist@Misc$SRR_RecLag` should be length `nStock`", .internal=TRUE)
  }
  
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
      fleet@Catchability@Efficiency
    }) |> List2Array(pos = 3) # Sim, Year, Fleet
  }) |> List2Array(pos = 2, "Stock") # Sim, Stock, Year, Fleet
  CheckDims(Hist@Misc$Catchability, 4, 'Catchability')
  
  # 5D Array: Sim, Stock, Year, Fleet, Area
  Hist@Misc$Closure <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Closure
    }) |> List2Array(pos = 3) # Sim, Year, Fleet, Area
  }) |> List2Array(pos = 2, "Stock") # Sim, Stock, Year, Fleet, Area
  CheckDims(Hist@Misc$Closure, 5, 'Closure')
  
  # 3D Array: Sim, Year, Fleet
  Hist@Misc$Targeting <- purrr::map(Hist@OM@Fleet[[1]], \(fleet) {
    fleet@Effort@Targeting
  }) |> List2Array(pos = 3) # Sim, Year, Fleet
  CheckDims(Hist@Misc$Targeting, 3, 'Targeting')
  
  
  ##  ---- Lists - length nStock ---- 
  Hist@Misc$WeightFleetList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@WeightFleet
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet
  })
  CheckDims(Hist@Misc$WeightFleetList, 4, 'WeightFleetList')
  
  Hist@Misc$SelAgeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Selectivity@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  CheckDims(Hist@Misc$SelAgeList, 5, 'SelAgeList')
  
  Hist@Misc$SelSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Selectivity@MeanAtLength # Sim, Age, Year, Area
    }) 
  })
  CheckDims(Hist@Misc$SelSizeList, 4, 'SelSizeList')
  
  Hist@Misc$RetAgeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Retention@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  CheckDims(Hist@Misc$RetAgeList, 5, 'RetAgeList')
  
  Hist@Misc$RetSizeList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Retention@MeanAtLength # Sim, Age, Year, Area
    }) 
  })
  CheckDims(Hist@Misc$RetSizeList, 4, 'RetSizeList')
  
  Hist@Misc$DiscMortList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@DiscardMortality@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  CheckDims(Hist@Misc$DiscMortList, 5, 'DiscMortList')
  
  Hist@Misc <- ExtendYears( Hist@Misc, Years=Years(Hist,Period))
  
  Hist
}








# Restores Hist@Misc
RestoreHistMisc <- function(Hist) {
  saveMisc <- Hist@Misc$SAVE
  saveAdvice <- Hist@Misc$MPAdvice
    
  Hist@Misc <- list()
  restore <- c(saveMisc, saveAdvice)
  if (!is.null(restore))
    Hist@Misc <- restore
  Hist
}