#' Initialize Time Series Slots
#'
#' Internal helper used to initialize time-series slots in a historical
#' or projection object based on an [OM()] specification.
#'
#' This function allocates and populates arrays for numbers, biomass,
#' fishing mortality, landings, discards, effort, and spatial distribution
#' for either the `"Historical"` or `"Projection"` period.
#'
#' @param Hist A history/projection object containing an `OM` slot.
#' @param Period Character string indicating the time period.
#'   Either `"Historical"` or `"Projection"`.
#' @param MPs Optional vector of management procedure names.
#'   Required when `Period = "Projection"`.
#'
#' @return The modified `Hist` object with initialized time-series slots.
#' @keywords internal
InitializeTimeSeries <- function(Hist,
                                 Period = c("Historical", "Projection"),
                                 MPs = NULL) {
  
  Period <- match.arg(Period)
  
  OM <- Hist@OM
  nSim <- OM@nSim
  HistYears <- Years(Hist, "H")
  Areas <- seq_len(nArea(Hist))
  
  isProj <- identical(Period, "Projection")
  if (!isProj) MPs <- 'temp'
  
  ## --- Numbers at age (Sim × Age × Year × Area) ---
  
  Hist@Number <- if (isProj) {
    ListArraySimAgeTimeAreaMP(OM, Period, MPs)
  } else {
    ListArraySimAgeTimeArea(OM, Period)
  }
  
  ## --- Biomass, SBiomass, & SProduction  (Sim × Stock × Year [+ MP]) ---
  
  Hist@Biomass <- ListArraySimAgeTime(OM, Period) |>
    lapply(DropDimension, "Age", FALSE) |>
    List2Array("Stock") |>
    aperm(c("Sim", "Stock", "Year"))
  
  if (isProj) {
    Hist@Biomass <- AddDimension(Hist@Biomass, "MP", val = MPs)
  }
  
  Hist@SBiomass <- Hist@SProduction <- Hist@Biomass
  
  ## --- Landings & Discards ---
  
  Hist@Landings <- ArraySimStockTimeFleetMP(OM, Period, MPs)
  Hist@Discards <- ArraySimStockTimeFleetMP(OM, Period, MPs)
  
  Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs)
  Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetAreaMP(OM, Period, MPs = MPs)
  
  if (!isProj) {
    Hist@Landings <- DropDimension(Hist@Landings, 'MP', FALSE)
    Hist@Discards <- DropDimension(Hist@Discards, 'MP', FALSE)
    
    Hist@LandingsAtAge <- purrr::map(Hist@LandingsAtAge, \(stock)
                                 DropDimension(stock, 'MP', FALSE))
    
    Hist@DiscardsAtAge <- purrr::map(Hist@DiscardsAtAge, \(stock)
                                     DropDimension(stock, 'MP', FALSE))
    
    
    Hist@LandingsAtSize <- purrr::map(Hist@LandingsAtSize, \(stock) {
      purrr::map(stock, \(fleet) DropDimension(fleet, 'MP', FALSE))
    })
    
    Hist@DiscardsAtSize <- purrr::map(Hist@DiscardsAtSize, \(stock) {
      purrr::map(stock, \(fleet) DropDimension(fleet, 'MP', FALSE))
    })
  }
    
  
  ## --- Effort & Distribution ---
  
  Hist@Effort <- ArraySimAgeTimeFleet(OM, Period) |>
    DropDimension("Age", FALSE)
  
  Hist@Distribution <- ArraySimAgeTimeFleetArea(OM, Period) |>
    DropDimension("Age", FALSE)
  
  if (isProj) {
    Hist@Effort <- AddDimension(Hist@Effort, "MP", MPs)
    Hist@Distribution <- AddDimension(Hist@Distribution, "MP", MPs)
  }
  
  ## Populate historical effort from OM
  
  if (!isProj) {
    for (fl in seq_len(nFleet(OM))) {
      Hist@Effort[, , fl] <-
        ExtendSims(OM@Fleet[[1]][[fl]]@Effort@Effort, nSim)
      
      Hist@Distribution[, , fl, ] <-
        Extend(OM@Fleet[[1]][[fl]]@Effort@Distribution,
               nSim, NULL, HistYears, Areas)
    }
  }
  
  ## --- Fishing Mortality ---
  Hist@FDead <-  Hist@FRetain <- ArraySimStockTimeFleetMP(OM, Period, MPs)
  Hist@FDeadArea <- Hist@FRetainArea <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs)
  
  if (!isProj) {
    Hist@FDead <- DropDimension(Hist@FDead, 'MP', FALSE)
    Hist@FRetain <- DropDimension(Hist@FRetain, 'MP', FALSE)
    
    Hist@FDeadArea <- purrr::map(Hist@FDeadArea, \(stock)
                                 DropDimension(stock, 'MP', FALSE))
      
    Hist@FRetainArea <- purrr::map(Hist@FRetainArea, \(stock)
                                 DropDimension(stock, 'MP', FALSE))
    
  } 
  
  Hist
}