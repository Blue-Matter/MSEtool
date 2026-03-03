#' Stock
#'
#' Construct a [stock-class] object defining the biological and
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
#' A `Stock` object can be attached to an [OM()] using [Stock()] and
#' retrieved using [`Stock<-`].
#'
#' Individual components may be accessed or modified using accessor
#' and replacement functions such as [Ages()], [`Ages<-`], etc.
#'
#' `r TechManLink()`
#' 
#' @return A [stock-class] object.
#'
#' @seealso
#' [Ages()], [Length()], [Weight()], [NaturalMortality()],
#' [Maturity()], [Fecundity()], [SRR()], [Spatial()], [Depletion()]
#'
#'
#' @export
Stock <- function(Name = "New Stock Object",
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


#' @rdname Stock
#' @export
`Stock<-` <- function(x, value) {
  AssignSlot(x, value, 'Stock')
}

#' @rdname Stock
#' @export
CommonName <- function(x) {
  AccessSlot(x, 'CommonName')
}

#' @rdname Stock
#' @export
`CommonName<-` <- function(x, value) {
  AssignSlot(x, value, 'CommonName')
}

#' @rdname Stock
#' @export
Species <- function(x) {
  AccessSlot(x, 'Species')
}

#' @rdname Stock
#' @export
`Species<-` <- function(x, value) {
  AssignSlot(x, value, 'Species')
}
