

# Initialize a `Hist` object for a given `OM`
OM2Hist <- function(OM, silent=FALSE) {
  
  if (!silent) {
    id <- cli::cli_progress_bar("Initializing `Hist` Object")  
  }
    
  # Populate if needed 
  OM <- PopulateOM(OM, silent=silent) 
  
  Hist <- new('hist')
  Hist@OM <- OM
  HistYears <- Years(OM, 'Historical')
  nYears <- length(HistYears)
  nSim <- OM@nSim
  
  # Stock - expand all arrays to include all Sims and all Years
  # TODO - this can be updated to only extend to all years, with Sim 
  #        dimension remaining either length or length nSim
  #        Rest of the code base should support this but currently not tested
  Hist@OM@Stock <- purrr::map(OM@Stock, \(Stock) {
    Stock <- ExtendStock(Stock, nSim, HistYears, silent, id)
    Stock@SRR@SPFrom <- match(Stock@SRR@SPFrom, StockNames(OM))
    Stock
  })
  

  # Fleet
  # Extend Fleet arrays to include all Sims and historical years
  # TODO - as above
  AgeClassList <- purrr::map(Hist@OM@Stock, \(Stock) Stock@Ages@Classes)
  Hist@OM@Fleet <- purrr::map2(Hist@OM@Fleet, AgeClassList, \(FleetList, AgeClasses)
                               ExtendFleet(FleetList, AgeClasses, nSim, HistYears, silent, id)
  )
  
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
  

InitializeTimeSeries <- function(Hist)  {
  OM <- Hist@OM
  
  # List of Stocks - Number by Sim, Age, Year, and Area
  Hist@Number <- ListArraySimAgeTimeArea(OM, 'Historical') 
  
  # Arrays: Sim, Stock, Year
  Hist@Biomass <- ListArraySimAgeTime(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> 
    aperm(c('Sim', 'Stock', 'Year'))
  Hist@SBiomass <-  Hist@SProduction <- Hist@Biomass 
  
  # Landings and Discards by Age and Size 
  # List of Stocks - array Sim, Age, Year, Fleet, Area
  Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  # List of Stocks - list of Fleets - array Sim, Class, Year, Area
  Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetArea(OM, 'Historical')
  
  # Historical Fishing Effort - Total
  # Sim, Stock, Year, Fleet
  Hist@Effort <- ArraySimAgeTimeFleet(OM, 'Historical') |> DropDimension('Age', FALSE)
    
  # Add Effort from OM
  for (fl in 1:nFleet(OM)) {
    Hist@Effort[,,fl] <- Hist@OM@Fleet[[1]][[fl]]@Effort@Effort
  }
    
  # Effort Distribution over Areas - effort by area
  # Sim, Year, Fleet, Area
  Hist@Distribution <- ArraySimAgeTimeFleetArea(OM, 'Historical') |> DropDimension('Age', FALSE)
    
  # Add Distribution from OM
  for (fl in 1:nFleet(OM)) {
    Hist@Distribution[,,fl,] <- Hist@OM@Fleet[[1]][[fl]]@Effort@Distribution 
  }
  
  # Fishing Mortality - Dead and Retain 
  # Overall
  # List of Stocks - array Sim, Age, Year, Fleet
  Hist@FDead <- Hist@FRetain <- ListArraySimAgeTimeFleet(OM, 'Historical') 
  
  # Within Area
  # List of Stocks - array Sim, Age, Year, Fleet, Area 
  Hist@FDeadArea <- Hist@FRetainArea <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist
}


# This prepares arrays for easy access in the C++ code
# temporary elements of Misc are removed later 
PrepHistMisc <- function(Hist) {
  saveMisc <- Hist@Misc
  Hist@Misc <- list()
  Hist@Misc$SAVE <- saveMisc
  
  
  # Stock Length Lists                  
  
  Hist@Misc$WeightFleetList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@WeightFleet
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet
  })
  
  
  Hist@Misc$SelList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Selectivity@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  Hist@Misc$RetList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Retention@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  Hist@Misc$DiscMortList <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@DiscardMortality@MeanAtAge
    }) |> List2Array(pos = 4) # Sim, Age, Year, Fleet, Area
  })
  
  
  # 5D Array: Sim, Stock, Year, Fleet, Area  
  Hist@Misc$Closure <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Closure
    }) |> List2Array(pos = 3) # Sim, Year, Fleet, Area
  }) |> List2Array(pos = 2, 'Stock') # Sim, Stock, Year, Fleet, Area
  
  
  # 4D Array: Sim, Stock, Year, Fleet   
  Hist@Misc$Catchability <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(fleet) {
      fleet@Catchability@Efficiency
    }) |> List2Array(pos = 3) # Sim, Year, Fleet
  }) |> List2Array(pos = 2, 'Stock') # Sim, Stock, Year, Fleet
  
  
  # 3D Array: Sim, Year, Fleet        
  Hist@Misc$Targeting <- purrr::map(Hist@OM@Fleet[[1]], \(fleet) {
    fleet@Effort@Targeting
  }) |> List2Array(pos = 3) # Sim, Year, Fleet
  
  
  # 2D Array: Sim, Year                
  Hist@Misc$RelSize <- Hist@OM@Stock[[1]]@Spatial@RelativeSize
  
  Hist
}

# Restores Hist@Misc 
RestoreHistMisc <- function(Hist) {
  saveMisc <- Hist@Misc$SAVE
  Hist@Misc <- list()
  Hist@Misc <- saveMisc
  Hist
}
 

