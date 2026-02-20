#' Constructor and Accessors for `Data`
#'
#'
#' @param object A [Data()] object.
#' @param x Leave as NULL to create a new `Data` object, or a [OM()], [Hist()], or [MSE()] object to acces the `Data` or `PPD` slots.
#' @param value Value to assign.
#'
#' @return
#'  A [data-class] object
#'
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
