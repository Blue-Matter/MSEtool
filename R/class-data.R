
#' The `data` S4 Class
#'
#' The `data` class stores observed or simulated fishery data used by an
#' [OM()] object. Data may include life-history information, exploitation
#' patterns, catches, indices, age and size compositions, and management advice.
#'
#' Objects should be created with the [Data()] constructor, which initialises
#' all sub-object slots to empty objects of the appropriate class when not
#' supplied.
#'
#' @slot Name Optional character string. Name of the data object.
#' @slot CommonName Optional character string. Common name of the stock.
#' @slot Species Optional character string. Scientific name of the species.
#' @slot Agency Optional character string. Name of the managing agency.
#' @slot Author Optional character string. Name(s) of the data author(s).
#' @slot Email Optional character string. Contact email(s) for the author(s).
#' @slot Region Optional character string. Geographic region of the stock.
#' @slot Latitude Optional numeric. Latitude of the stock in decimal degrees.
#' @slot Longitude Optional numeric. Longitude of the stock in decimal degrees.
#'
#' @slot Years Numeric vector of calendar years covered by the data.
#' @slot YearLH Numeric. The last historical calendar year (always a whole
#'   integer, even when `Seasons > 1` and `Years` holds sub-annual decimal
#'   steps), separating the historical period from the projection period.
#'   Defaults to `floor(max(Years))` when not supplied to [Data()].
#' @slot Seasons Positive integer. Number of seasons per year. Defaults to `1`.
#' @slot nArea Positive integer. Number of spatial areas. Defaults to `1`.
#'
#' @slot LifeHistory An object of class [lifehistorydata-class] containing
#'   biological parameters such as growth, maturity, and natural mortality.
#' @slot Exploitation An object of class [exploitationdata-class] containing
#'   selectivity, retention, and discard mortality parameters.
#' @slot Reference An object of class [referencedata-class] containing biological
#'   reference points such as unfished biomass and MSY-based quantities.
#'
#' @slot Effort An object of class [effortdata-class] containing fishing effort
#'   time series.
#'
#' @slot Landings An object of class [catchdata-class] containing landed catch
#'   time series.
#' @slot Discards An object of class [catchdata-class] containing discarded catch
#'   time series.
#'
#' @slot CPUE An object of class [indicesdata-class] containing catch-per-unit-effort
#'   indices.
#' @slot Survey An object of class [indicesdata-class] containing fishery-independent
#'   survey indices.
#'
#' @slot LandingsAtAge An object of class [compdata-class] containing age composition
#'   of landed catch.
#' @slot DiscardsAtAge An object of class [compdata-class] containing age composition
#'   of discarded catch.
#' @slot LandingsAtSize An object of class [compdata-class] containing size
#'   composition of landed catch.
#' @slot DiscardsAtSize An object of class [compdata-class] containing size
#'   composition of discarded catch.
#'
#' @slot Advice An object of class [advicedata-class] containing TAC recommendations
#'   and related management advice.
#'
#' @slot Misc A named list for any additional user-defined data. Defaults to
#'   `list()`.
#' @slot Log Internal named list used for diagnostic and audit logging.
#'
#' @seealso [Data()]
#' @include class-data-catch.R
#' @include class-data-effort.R
#' @include class-data-indices.R
#' @include class-data-comp.R
#' @include class-data-lifehistory.R
#' @include class-data-exploitation.R
#' @include class-data-reference.R
#' @include class-data-advice.R
#' @name data-class
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
    
    LandingsAtAge = "compdata",
    DiscardsAtAge = "compdata",
    
    LandingsAtSize = "compdata",
    DiscardsAtSize = 'compdata',
    
    Advice = "advicedata",
    Log = "list",
    Misc = "list"
  )
)
