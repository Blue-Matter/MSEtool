#' Constructor and Accessor for `Data`
#'
#' Creates a new [data-class] object, or extracts the `Data` or `PPD` slot from
#' an existing [om-class], [hist-class], or [mse-class] class object.
#'
#' When `Name` is an [om-class] or [hist-class] object, the function returns the
#' corresponding `@@Data` slot. When `Name` is an [mse-class] object, the `@@PPD`
#' slot is returned instead. Otherwise, a new `data` object is constructed from
#' the supplied arguments.
#'
#' All slot arguments default to `NULL`, in which case an empty sub-object of
#' the appropriate class is initialised automatically:
#'
#' - `LifeHistory` → [lifehistorydata]
#' - `Exploitation` → [exploitationdata]
#' - `Reference` → [referencedata]
#' - `Effort` → [effortdata]
#' - `Landings`, `Discards` → [catchdata]
#' - `CPUE`, `Survey` → [indicesdata]
#' - `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize` → [compdata]
#' - `Advice` → [advicedata]
#'
#' @param Name Either a character string naming the new [data-class] object, or an
#'   existing [om-class], [hist-class], or [mse-class] object from which to extract data.
#'   Defaults to `"New Data Object"`.
#' @param CommonName Optional character string. Common name of the stock.
#' @param Species Optional character string. Scientific name of the species.
#' @param Agency Optional character string. Name of the managing agency.
#' @param Author Optional character string. Name of the data author.
#' @param Email Optional character string. Contact email for the author.
#' @param Region Optional character string. Geographic region of the stock.
#' @param Latitude Optional numeric. Latitude of the stock.
#' @param Longitude Optional numeric. Longitude of the stock.
#' @param Years Vector of calendar years covered by the data. **Required**
#' @param YearLH The last historical year; separates the
#'   historical period from the projection period. Defaults to `max(Years)`.
#' @param Seasons A positive integer giving the number of seasons per year.
#'   Defaults to `1`.
#' @param nArea A positive integer giving the number of spatial areas.
#'   Defaults to `1`.
#' @param LifeHistory Optional. An object of class [lifehistorydata] . 
#' @param Exploitation Optional. An object of class [exploitationdata].
#' @param Reference Optional. An object of class [referencedata]. 
#' @param Effort Optional. An object of class [effortdata]. 
#' @param Landings Optional. An object of class [catchdata] for landed catch.
#' @param Discards Optional. An object of class [catchdata] for discarded
#'   catch. 
#' @param CPUE Optional. An object of class [indicesdata] for catch-per-unit-
#'   effort indices.
#' @param Survey Optional. An object of class [indicesdata] for fishery-
#'   independent survey indices. 
#' @param LandingsAtAge Optional. An object of class [compdata] for age
#'   composition of landings. 
#' @param DiscardsAtAge Optional. An object of class [compdata] for age
#'   composition of discards. 
#' @param LandingsAtSize Optional. An object of class [compdata] for size
#'   composition of landings. 
#' @param DiscardsAtSize Optional. An object of class [compdata] for size
#'   composition of discards.
#' @param Advice Optional. An object of class [advicedata] containing TAC and
#'   related advice. 
#' @param Misc A named list for any additional user-defined data. Defaults to
#'   `list()`.
#'
#' @return A [data-class] object, or when `Name` is an [mse-class] object, a list
#'   of [data-class] objects from the `@@PPD` slot.
#'
#' @seealso [data-class], [LastTAC()], [LastHistYearInd()], [ProjectionYear()]
#' @name Data
#' @export
Data <- function(Name = 'New Data Object', 
                 CommonName = NULL,
                 Species = NULL,
                 Agency = NULL,
                 Author = NULL,
                 Email = NULL, 
                 Region = NULL,
                 Latitude = NULL,
                 Longitude = NULL,
                 Years = NULL, 
                 YearLH  = NULL,
                 Seasons = 1,
                 nArea = 1,
                 LifeHistory = NULL,
                 Exploitation = NULL,
                 Reference = NULL,
                 Effort = NULL,
                 Landings = NULL,
                 Discards = NULL,
                 CPUE = NULL,
                 Survey = NULL,
                 LandingsAtAge = NULL, 
                 DiscardsAtAge = NULL, 
                 LandingsAtSize = NULL, 
                 DiscardsAtSize = NULL, 
                 Advice = NULL,
                 Misc = list()
) {
  
  classes <- c('om', 'hist')
  
  if (inherits(Name, classes)) 
    return(Name@Data)
  
  if (inherits(Name, 'mse')) 
    return(Name@PPD)
  
  if (is.null(LifeHistory))
    LifeHistory <- new('lifehistorydata')
 
  if (is.null(Exploitation))
    Exploitation <- new('exploitationdata')
  
  if (is.null(Reference))
    Reference <- new('referencedata')
  
  if (is.null(Effort))
    Effort <- new('effortdata')
  
  if (is.null(Landings))
    Landings <- new('catchdata')
  
  if (is.null(Discards))
    Discards <- new('catchdata')
  
  if (is.null(CPUE))
    CPUE <- new('indicesdata')
  
  if (is.null(Survey))
    Survey <- new('indicesdata')
  
  if (is.null(LandingsAtAge))
    LandingsAtAge <- new('compdata')
  
  if (is.null(DiscardsAtAge))
    DiscardsAtAge <- new('compdata')
  
  if (is.null(LandingsAtSize))
    LandingsAtSize <- new('compdata')
  
  if (is.null(DiscardsAtSize))
    DiscardsAtSize <- new('compdata')
  
  if (is.null(Advice))
    Advice <- new('advicedata')
  
  if (!is.null(Years) && is.null(YearLH))
    YearLH <- max(Years)
  
  object <- methods::new(
    "data",
    Name = Name,
    Agency = Agency,
    Author = Author,
    Email = Email, 
    Region = Region,
    Latitude = Latitude,
    Longitude = Longitude,
    Years = Years, 
    YearLH  = YearLH ,
    Seasons = Seasons,
    nArea = nArea,
    
    LifeHistory = LifeHistory,
    Exploitation = Exploitation,
    Reference = Reference,
    Effort = Effort,
    
    Landings = Landings,
    Discards = Discards,
    
    CPUE = CPUE,
    Survey = Survey,
    
    LandingsAtAge = LandingsAtAge, 
    DiscardsAtAge = DiscardsAtAge, 
    
    LandingsAtSize = LandingsAtSize, 
    DiscardsAtSize = DiscardsAtSize, 
    
    Advice = Advice,
    
    Misc = Misc,
    Log = list()
  )
  
  methods::validObject(object)
  object
}
