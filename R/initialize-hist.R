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
  
  default <- 0
  
  ## --- Numbers at age (Sim × Age × Year × Area) ---
  
  Hist@Number <- if (isProj) {
    ListArraySimAgeTimeAreaMP(OM, Period, MPs, default=default)
  } else {
    ListArraySimAgeTimeArea(OM, Period, default=default)
  }
  
  ## --- Biomass, SBiomass, & SProduction  (Sim × Stock × Year [+ MP]) ---
  MakeBioListArray <- function(OM, Period, default, isProj, MPs) {
    l <- ListArraySimAgeTime(OM, Period, default=default) |>
      lapply(DropDimension, "Age", FALSE) |>
      List2Array("Stock") |>
      aperm(c("Sim", "Stock", "Year"))
    
    if (!isProj) 
      return(l)
    
    AddDimension(l, "MP", val = MPs)
  }
  
  Hist@Biomass <- MakeBioListArray(OM, Period, default, isProj, MPs)
  Hist@SProduction <- MakeBioListArray(OM, Period, default, isProj, MPs)
  Hist@SBiomass <- MakeBioListArray(OM, Period, default, isProj, MPs)
  
  ## --- Landings & Discards ---
  
  Hist@Interactions <- ArraySimStockTimeFleetMP(OM, Period, MPs, default=default)
  Hist@Landings <- ArraySimStockTimeFleetMP(OM, Period, MPs, default=default)
  Hist@Discards <- ArraySimStockTimeFleetMP(OM, Period, MPs, default=default)
  
  Hist@InteractAtAge  <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  Hist@LandingsAtAge <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  Hist@LandingsAtSize <- ListArraySimClassTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  Hist@DiscardsAtSize <- ListArraySimClassTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  
  if (!isProj) {
    Hist@Interactions <- DropDimension(Hist@Interactions, 'MP', FALSE)
    Hist@Landings <- DropDimension(Hist@Landings, 'MP', FALSE)
    Hist@Discards <- DropDimension(Hist@Discards, 'MP', FALSE)
    
    Hist@InteractAtAge <- purrr::map(Hist@InteractAtAge, \(stock)
                                     DropDimension(stock, 'MP', FALSE))
    
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
  
  Hist@Effort <- ArraySimAgeTimeFleet(OM, Period, default=default) |>
    DropDimension("Age", FALSE)
  
  Hist@Distribution <- ArraySimAgeTimeFleetArea(OM, Period, default=default) |>
    DropDimension("Age", FALSE)
  
  if (isProj) {
    Hist@Effort <- AddDimension(Hist@Effort, "MP", MPs)
    Hist@Distribution <- AddDimension(Hist@Distribution, "MP", MPs)
  }
  
  ## Populate historical effort from OM
  
  if (!isProj) {
    for (fl in seq_len(nFleet(OM))) {
      Hist@Effort[, , fl] <- ExtendSims(OM@Fleet[[1]][[fl]]@Effort@Effort, nSim)
      
      dist <- OM@Fleet[[1]][[fl]]@Effort@Distribution
      if (!is.null(dist)) 
        Hist@Distribution[, , fl, ] <- Extend(dist, nSim, NULL, HistYears, Areas)
    }
  }
  
  ## --- Fishing Mortality ---
  Hist@FInteract <- ArraySimStockTimeFleetMP(OM, Period, MPs, default=default)
  Hist@FDead <- ArraySimStockTimeFleetMP(OM, Period, MPs, default=default)
  Hist@FRetain <- ArraySimStockTimeFleetMP(OM, Period, MPs, default=default)
  
  
  Hist@FInteractArea <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  Hist@FDeadArea <-  ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  Hist@FRetainArea <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs = MPs, default=default)
  
  if (!isProj) {
    Hist@FInteract <- DropDimension(Hist@FInteract, 'MP', FALSE)
    Hist@FDead <- DropDimension(Hist@FDead, 'MP', FALSE)
    Hist@FRetain <- DropDimension(Hist@FRetain, 'MP', FALSE)

    Hist@FInteractArea <- purrr::map(Hist@FInteractArea, \(stock)
                                 DropDimension(stock, 'MP', FALSE))
    
    Hist@FDeadArea <- purrr::map(Hist@FDeadArea, \(stock)
                                 DropDimension(stock, 'MP', FALSE))
      
    Hist@FRetainArea <- purrr::map(Hist@FRetainArea, \(stock)
                                 DropDimension(stock, 'MP', FALSE))
    
  } 
  
  Hist
}