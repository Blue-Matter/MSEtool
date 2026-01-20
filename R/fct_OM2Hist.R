#' Initialize a Hist object from an OM
#'
#' Internal constructor that converts an initialized [OM()] object into a
#' corresponding [Hist()] object by expanding all stock, fleet, and time-series
#' slots over historical years and preparing C++-friendly data structures.
#'
#' @param OM An initialized [OM()] object.
#' @param silent Logical. Suppress progress reporting.
#'
#' @return A fully initialized [Hist()] object.
#'
#' @keywords internal
OM2Hist <- function(OM, silent = FALSE) {
  if (!silent) {
    id <- cli::cli_progress_bar("Initializing `Hist` Object")
  }

  # Populate if needed
  OM <- PopulateOM(OM, silent = silent)

  Hist <- new("hist")
  Hist@OM <- OM
  HistYears <- Years(OM, "Historical")
  nYears <- length(HistYears)
  nSim <- NULL # don't extend arrays to length OM@nSim

  # Stock - expand all arrays to all historical years
  Hist@OM@Stock <- purrr::map(OM@Stock, \(Stock) {
    Stock <- ExtendStock(Stock, nSim, HistYears, silent, id)
    Stock@SRR@SPFrom <- match(Stock@SRR@SPFrom, StockNames(OM))
    Stock
  })


  # Fleet
  # Extend Fleet arrays to include historical years
  AgeClassList <- purrr::map(Hist@OM@Stock, \(Stock) Stock@Ages@Classes)
  Hist@OM@Fleet <- purrr::map2(Hist@OM@Fleet, AgeClassList, \(FleetList, AgeClasses)
                               ExtendFleet(FleetList, AgeClasses, nSim, HistYears, silent, id))


  # Create Time Series Arrays
  Hist <- InitializeTimeSeries(Hist)

  # Add values included in Misc
  # These values won't be over-written by the model
  # TODO - new feature not used or testedd
  Hist <- FillFromMisc(Hist)

  if (!silent) {
    cli::cli_progress_done()
  }


  PrepHistMisc(Hist) # add temporary lists and arrays to Hist@Misc for C++
}


InitializeTimeSeries <- function(Hist) {
  OM <- Hist@OM

  # List of Stocks - Number by Sim, Age, Year, and Area
  Hist@Number <- ListArraySimAgeTimeArea(OM, "Historical")

  # Arrays: Sim, Stock, Year
  Hist@Biomass <- ListArraySimAgeTime(OM, "Historical") |>
    lapply(DropDimension, "Age", FALSE) |>
    List2Array("Stock") |>
    aperm(c("Sim", "Stock", "Year"))
  Hist@SBiomass <- Hist@SProduction <- Hist@Biomass

  # Landings and Discards by Age and Size
  # List of Stocks - array Sim, Age, Year, Fleet, Area
  Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetArea(OM, "Historical")
  # List of Stocks - list of Fleets - array Sim, Class, Year, Area
  Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetArea(OM, "Historical")

  # Historical Fishing Effort - Total
  # Sim, Stock, Year, Fleet
  Hist@Effort <- ArraySimAgeTimeFleet(OM, "Historical") |> DropDimension("Age", FALSE)

  # Add Effort from OM
  for (fl in 1:nFleet(OM)) {
    Hist@Effort[, , fl] <- Hist@OM@Fleet[[1]][[fl]]@Effort@Effort
  }

  # Effort Distribution over Areas - effort by area
  # Sim, Year, Fleet, Area
  Hist@Distribution <- ArraySimAgeTimeFleetArea(OM, "Historical") |> DropDimension("Age", FALSE)

  # Add Distribution from OM
  for (fl in 1:nFleet(OM)) {
    Hist@Distribution[, , fl, ] <- Hist@OM@Fleet[[1]][[fl]]@Effort@Distribution
  }

  # Fishing Mortality - Dead and Retain
  # Overall
  # List of Stocks - array Sim, Age, Year, Fleet
  Hist@FDead <- Hist@FRetain <- ListArraySimAgeTimeFleet(OM, "Historical")

  # Within Area
  # List of Stocks - array Sim, Age, Year, Fleet, Area
  Hist@FDeadArea <- Hist@FRetainArea <- ListArraySimAgeTimeFleetArea(OM, "Historical")
  Hist
}

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
PrepHistMisc <- function(Hist) {
  saveMisc <- Hist@Misc
  Hist@Misc <- list()
  Hist@Misc$SAVE <- saveMisc
  
  nStock <- nStock(Hist)

  # ---- 2D Array ----

  
  # ---- Stock -----
  
  ## ---- Vector ----
  
  Hist@Misc$SPFrom <- purrr::map(Hist@OM@Stock, \(stock) {
    stock@SRR@SPFrom
  }) |> unlist() |> array(dim=nStock(Hist), dimnames = list(Stock=StockNames(Hist)))
  CheckLength(Hist@Misc$SPFrom, nStock, 'SPFrom')
  
  ## ---- 2D Array ----
  
  # Sim, Year
  Hist@Misc$RelSize <- Hist@OM@Stock[[1]]@Spatial@RelativeSize
  CheckDims( Hist@Misc$RelSize, 2, RelSize)
  
  # Sim Stock
  Hist@Misc$SpawnTimeFrac <- purrr::map(Hist@OM@Stock, \(stock) {
    array(rep(stock@SRR@SpawnTimeFrac, Hist@OM@nSim)[1:Hist@OM@nSim],
          dim=Hist@OM@nSim, 
          dimnames = list(Sim=1:Hist@OM@nSim)
    )
  }) |> List2Array('Stock')
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
  
  ## ---- SRR ----
  
  # Calculate recruitment lag 
  Hist@Misc$SRR_RecLag <- purrr::map(Hist@OM@Stock, \(stock) {
    MinAge <- min(stock@Ages@Classes)
    which(seq(0, to=max(stock@Ages@Classes), by=1/stock@Seasons) == MinAge) - 1
  }) |> unlist()
  
  if (length(Hist@Misc$SRR_RecLag) != nStock) {
    cli::cli_abort("`Hist@Misc$SRR_RecLag` should be length `nStock`", .internal=TRUE)
  }

  
  

  
  Hist@Misc$SRR_Pars <- purrr::map(Hist@OM@Stock, \(stock) stock@SRR@Pars)
 
  stock@SRR@Model
  
  Hist@Misc$SRR_Pars$Female$h
  
  
  stock <- Hist@OM@Stock$Female
  stock@SRR@Pars$h 
  
  
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
  
  Hist
}

# Restores Hist@Misc
RestoreHistMisc <- function(Hist) {
  saveMisc <- Hist@Misc$SAVE
  Hist@Misc <- list()
  Hist@Misc <- saveMisc
  Hist
}
