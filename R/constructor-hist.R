#' Create or Access a `Hist` Object
#'
#' The `Hist()` constructor creates a new historical simulation object or,
#' when applied to an [OM()] object, returns the historical results stored
#' within that object.
#'
#' @param OM A [OM()] object. If missing, an empty `Hist` object is returned.
#' @param silent Should messages be printed out to the console?
#'
#' @return A [Hist()] object.
#'
#' @seealso [OM()], [Data()]
#' @include class-hist.R
#' @rdname Hist
#' @export
Hist <- function(OM=NULL, silent = FALSE) {
  
  if (is.null(OM)) {
    return(methods::new("hist"))
  }
  
  # Create a Hist object from an OM and extend for all Sims and Years
  if (!silent) {
    id <- cli::cli_progress_bar("Initializing `Hist` Object")
  }

  # Populate if needed
  OM <- PopulateOM(OM, silent = silent)

  Hist <- new("hist")
  Hist@OM <- OM
  HistYears <- Years(OM, "Historical")

  # Create Time Series Arrays
  Hist <- InitializeTimeSeries(Hist)
  
  # Add values included in Misc
  # These values won't be over-written by the model
  # TODO - new feature not used or testedd
  Hist <- FillFromMisc(Hist)
  
  # Extend all arrays for all sims, ages, historical years, and area
  Hist <- ExtendHist(Hist, HistYears, silent, id)
  
  Hist@OM@Stock <- purrr::imap(Hist@OM@Stock, \(Stock, idx) {
    SPFrom <- Stock@SRR@SPFrom
    if (!length(SPFrom)) {
      SPFrom <- idx
    }
    if (is.character(SPFrom)) {
      Stock@SRR@SPFrom <- match(SPFrom, StockNames(OM))
    } else if (is.numeric(SPFrom)) {
      Stock@SRR@SPFrom <- SPFrom
    }
    Stock
  })
  
  if (!silent) {
    cli::cli_alert_success("Initialized `Hist` Object")
  }
  Hist 
}


InitializeTimeSeries <- function(Hist, Period=c("Historical", 'Projection'), MPs=NULL) {
  Period <- match.arg(Period, c("Historical", 'Projection'))
  
  OM <- Hist@OM
  nSim <- Hist@OM@nSim
  HistYears <- Years(Hist,'H')
  Areas <- 1:nArea(Hist)

  # List of Stocks - Number by Sim, Age, Year, and Area
  if (Period=='Historical') {
    Hist@Number <- ListArraySimAgeTimeArea(OM, Period)  
  } else {
    Hist@Number <- ListArraySimAgeTimeAreaMP(OM, Period, MPs)
  }
  
  # Arrays: Sim, Stock, Year
  Hist@Biomass <- ListArraySimAgeTime(OM,Period) |>
    lapply(DropDimension, "Age", FALSE) |>
    List2Array("Stock") |>
    aperm(c("Sim", "Stock", "Year"))
  
  if (Period=='Projection') {
    Hist@Biomass <- AddDimension(Hist@Biomass,
                                 'MP',
                                 val=MPs)
  }
  
  Hist@SBiomass <- Hist@SProduction <- Hist@Biomass

  if (Period=='Historical') {
    # Landings and Discards by Age and Size
    # List of Stocks - array Sim, Age, Year, Fleet, Area
    Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetArea(OM, Period)
    # List of Stocks - list of Fleets - array Sim, Class, Year, Area
    Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetArea(OM, Period)
  } else {
    # Landings and Discards by Age and Size
    # List of Stocks - array Sim, Age, Year, Fleet, Area
    Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs=MPs)
    # List of Stocks - list of Fleets - array Sim, Class, Year, Area
    Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetAreaMP(OM, Period, MPs=MPs)
  }


  # Historical Fishing Effort - Total
  # Sim, Stock, Year, Fleet
  Hist@Effort <- ArraySimAgeTimeFleet(OM, Period) |> DropDimension("Age", FALSE)

  # Effort Distribution over Areas - effort by area
  # Sim, Year, Fleet, Area
  Hist@Distribution <- ArraySimAgeTimeFleetArea(OM, Period) |> DropDimension("Age", FALSE)

  if (Period=='Projection')  {
    Hist@Effort <- Hist@Effort |> AddDimension('MP', MPs)
    Hist@Distribution <- Hist@Distribution |> AddDimension('MP', MPs)
  }
    
  # Add Effort & Distribution from OM
  if (Period == 'Historical') {
    for (fl in 1:nFleet(OM)) {
      Hist@Effort[, , fl] <- ExtendSims(Hist@OM@Fleet[[1]][[fl]]@Effort@Effort, nSim)
      Hist@Distribution[, , fl, ] <- Extend(Hist@OM@Fleet[[1]][[fl]]@Effort@Distribution,
                                            nSim, NULL, HistYears, Areas)
    }  
  }
  
  if (Period == 'Historical') {
    # Fishing Mortality - Dead and Retain
    # Overall
    # List of Stocks - array Sim, Age, Year, Fleet
    Hist@FDead <- Hist@FRetain <- ListArraySimAgeTimeFleet(OM, Period)
    
    # Within Area
    # List of Stocks - array Sim, Age, Year, Fleet, Area
    Hist@FDeadArea <- Hist@FRetainArea <- ListArraySimAgeTimeFleetArea(OM, Period)
    
  } else {
    # Fishing Mortality - Dead and Retain
    # Overall
    # List of Stocks - array Sim, Age, Year, Fleet
    Hist@FDead <- Hist@FRetain <- ListArraySimAgeTimeFleetMP(OM, Period, MPs=MPs)
    
    # Within Area
    # List of Stocks - array Sim, Age, Year, Fleet, Area
    Hist@FDeadArea <- Hist@FRetainArea <- ListArraySimAgeTimeFleetAreaMP(OM, Period, MPs=MPs)
  }

  
  
  Hist
}



