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
  
  Hist@OM@Stock <- purrr::map(Hist@OM@Stock, \(Stock) {
    Stock@SRR@SPFrom <- match(Stock@SRR@SPFrom, StockNames(OM))
    Stock
  })
  
  
  if (!silent) {
    cli::cli_alert_success("Initialized `Hist` Object")
  }
  Hist 
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



