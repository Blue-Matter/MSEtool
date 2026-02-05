#' Stock
#'
#' Construct a [Stock()] object defining the biological and
#' population-dynamics properties of a stock used in an operating model.
#'
#' @param Name Character string. Unique stock name.
#' @param CommonName Common name.
#' @param Species Scientific name.
#' @param Ages An [Ages()] object.
#' @param Length A [Length()] object.
#' @param Weight A [Weight()] object.
#' @param NaturalMortality A [NaturalMortality()] object.
#' @param Maturity A [Maturity()] object.
#' @param Fecundity A [Fecundity()] object.
#' @param SRR A [SRR()] object.
#' @param Spatial A [Spatial()] object.
#' @param Depletion A [Depletion()] object.
#' @param Seasons Number of seasons in a year.
#' @param Misc Miscellaneous list
#'
#' @details
#' The `Stock` class aggregates age structure, growth, mortality,
#' reproduction, recruitment, and optional spatial dynamics into a
#' single object.
#'
#' A `Stock` object can be attached to an [OM()] using [SetStock()] and
#' retrieved using [GetStock()].
#'
#' Individual components may be accessed or modified using accessor
#' and replacement functions such as [GetAges()], [SetAges(), [GetLength()], [SetLength()], etc.
#'
#' @return A [Stock()] object.
#'
#' @seealso
#' [GetStock()], [SetStock()],
#' [Ages()], [Length()], [Weight()], [NaturalMortality()],
#' [Maturity()], [Fecundity()], [SRR()], [Spatial()], [Depletion()]
#'
#'
#' @export
Stock <- function(Name,
                  CommonName = NULL,
                  Species = NULL,
                  Ages = NULL,
                  Length = NULL,
                  Weight = NULL,
                  NaturalMortality = NULL,
                  Maturity = NULL,
                  Fecundity = NULL,
                  SRR = NULL,
                  Spatial = NULL,
                  Depletion = NULL,
                  Seasons = 1,
                  Misc = list()) {
  
  ## ---- Empty constructor ----
  if (missing(Name)) {
    object <- methods::new("stock")
    methods::validObject(object)
    return(object)
  }
  
  if (!inherits(Name, 'character')) {
    if (!'Stock' %in% slotNames(Name))
      cli::cli_abort(c('x'='No slot {.val Stock} found in object class {.val {class(Name)}}'))
    stock <- Name@Stock
    if (is.numeric(CommonName))
      return(stock[[CommonName]])
    
    return(Name@Stock)
  }

  
  
  if (is.null(Ages))  
    Ages <- Ages()
  if (is.null(Length))
    Length <- Length()
  if (is.null(Weight))
    Weight <- Weight()
  if (is.null(NaturalMortality)) 
    NaturalMortality <- NaturalMortality()
  if (is.null(Maturity)) 
    Maturity <- Maturity()
  if (is.null(Fecundity)) 
    Fecundity <- Fecundity()
  if (is.null(SRR))         
    SRR <- SRR()
  if (is.null(Spatial))
    Spatial <- Spatial()
  if (is.null(Depletion))
    Depletion <- Depletion()
  
  # Defaults
  nYear <- 20
  pYear <- 30
  nSim <- 48
  
  CurrentYear <- as.numeric(format(Sys.Date(), "%Y"))
 
  object <- methods::new(
    "stock",
    Name = Name,
    CommonName = CommonName,
    Species = Species,
    Ages = Ages,
    Length = Length,
    Weight = Weight,
    NaturalMortality = NaturalMortality,
    Maturity = Maturity,
    Fecundity = Fecundity,
    SRR = SRR,
    Spatial = Spatial,
    Depletion = Depletion,
    nYear = nYear,
    pYear = pYear,
    nSim = nSim,
    CurrentYear = CurrentYear,
    Seasons = Seasons,
    Years = CalcYears(nYear, pYear, CurrentYear, Seasons),
    Misc = Misc,
    Log = list()
  )
  
  methods::validObject(object)
  object
}

#' Stock accessors and assignment functions
#'
#' Functions for accessing and modifying a [Stock()] object, and for
#' attaching or retrieving a `Stock` object from an [OM()].
#'
#' @param OM An [OM()] object.
#' @param x A [Stock()] object.
#' @param value Replacement value.
#' @param i Integer. Index for stock number
#'
#' @details
#' - `GetStock()` and `SetStock()` retrieve or assign stocks to an [OM()].
#' - Slot accessors retrieve or modify individual components of a `Stock`.
#'
#' Conceptual details and valid inputs are documented in [Stock()].
#'
#' @name Stock-accessors
NULL

#' @rdname Stock-accessors
#' @export
GetStock <- function(OM, i = NULL) {
  CheckClass(OM, "om", "OM")
  
  if (is.null(i))
    return(OM@Stock)
  
  if (!is.numeric(i) || i < 1 || i > length(OM@Stock))
    cli::cli_abort("Invalid stock index")
  
  OM@Stock[[i]]
}

#' @rdname Stock-accessors
#' @export
SetStock <- function(OM, Stock) {
  CheckClass(OM, "om", "OM")
  CheckClass(Stock, "stock", "Stock")
  
  OM@Stock <- list(Stock)
  names(OM@Stock) <- Stock@Name
  class(OM@Stock) <- "StockList"
  
  OM
}

