#' `Data` Object
#'
#' The `data` class stores observed or simulated fishery data used by an
#' [OM()] object. Data may include life-history information, exploitation
#' patterns, catches, indices, compositions, and management advice.
#'
#' @slot Name Optional name of the data object.
#' @slot CommonName Optional common name of the stock.
#' @slot Species Optional species name.
#' @slot Agency Optional agency responsible for the data.
#' @slot Author Optional author(s) of the data.
#' @slot Email Optional email address(es) for the author(s).
#' @slot Region Optional geographic region.
#' @slot Latitude Optional latitude (decimal degrees).
#' @slot Longitude Optional longitude (decimal degrees).
#'
#' @slot Years Numeric vector of time steps represented in the data.
#' @slot YearLH Last historical time step.
#' @slot Seasons Number of seasons per year.
#' @slot nArea Number of spatial areas.
#'
#' @slot LifeHistory TODO
#' @slot Exploitation TODO
#' @slot Reference TODO
#'
#' @slot Effort TODO
#'
#' @slot Landings TODO
#' @slot Discards TODO
#'
#' @slot CPUE TODO
#' @slot Survey TODO
#'
#' @slot CAA TODO
#' @slot CAL TODO
#'
#' @slot Advice TODO
#'
#' @slot Log Internal list used for diagnostics.
#' @slot Misc Miscellaneous list for additional data.
#'
#' @seealso [Data()], [OM()]

#' @include class-data-components.R 
#'
#' @export
setClass(
  "data",
  slots = c(
    Name = "char.null",
    CommonName = "char.null",
    Species = "char.null",
    Agency = "char.null",
    Author = "char.null",
    Email = "char.null",
    Region = "char.null",
    Latitude = "num.null",
    Longitude = "num.null",
    
    Years = "num.null",
    YearLH = "num.null",
    Seasons = "num.null",
    nArea = "num.null",
    
    LifeHistory = "lifehistorydata",
    Exploitation = "exploitationdata",
    Reference = "referencedata",
    
    Effort = "effortdata",
    
    Landings = "catchdata",
    Discards = "catchdata",
    
    CPUE = "indicesdata",
    Survey = "indicesdata",
    
    CAA = "compdata",
    CAL = "compdata",
    
    Advice = "advicedata",
    Log = "list",
    Misc = "list"
  )
)
