#' Stock
#'
#' Construct and manipulate a [stock-class] object defining the biological and
#' population-dynamics properties of a stock in an operating model.
#'
#' @param Name Character. Unique stock name, or an S4 object with a `Stock`
#'   slot (e.g., an [om-class] object) for pass-through access (see `Details`).
#'   Default `"New Stock Object"`.
#' @param CommonName Character. Common name of the species. When `Name` is an
#'   S4 object and `CommonName` is numeric, it is used as a stock index for
#'   list-structured `Stock` slots (see `Details`). Default `NULL`.
#' @param Species Character. Scientific name of the species. Default `NULL`.
#' @param Ages An [ages-class] object. Required. If `NULL` (default), an
#'   empty [ages-class] object is created via [Ages()].
#' @param Length A [length-class] object. Required. If `NULL` (default), an
#'   empty [length-class] object is created via [Length()].
#' @param Weight A [weight-class] object. Required. If `NULL` (default), an
#'   empty [weight-class] object is created via [Weight()].
#' @param NaturalMortality A [naturalmortality-class] object. Required. If
#'   `NULL` (default), an empty [naturalmortality-class] object is created via
#'   [NaturalMortality()].
#' @param Maturity A [maturity-class] object. Required. If `NULL` (default),
#'   an empty [maturity-class] object is created via [Maturity()].
#' @param Fecundity A [fecundity-class] object. Optional. If `NULL`
#'   (default), an empty [fecundity-class] object is created via [Fecundity()].
#' @param SRR A [srr-class] object. Required. If `NULL` (default), an empty
#'   [srr-class] object is created via [SRR()].
#' @param Spatial A [spatial-class] object. Optional — only required for
#'   operating models with explicit spatial structure (`nArea > 1`). If `NULL`
#'   (default), an empty [spatial-class] object is created via [Spatial()].
#' @param Depletion A [depletion-class] object. If `NULL` (default), an empty
#'   [depletion-class] object is created via [Depletion()]. If  `Final` is 
#'   specified for the `Depletion` object, the model will ignore any existing
#'   catchability (`q`) values (see [catchability-class]) and optimize `q` to
#'   reach the specified depletion for each simulation. 
#' @param Seasons Integer. Number of seasons per year. Default `1`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x An S4 object with a `Stock` slot, or a [stock-class] object for
#'   slot accessors.
#' @param value For `Stock<-`: a [stock-class] object or list of [stock-class]
#'   objects. For `CommonName<-` and `Species<-`: a character string.
#'
#' @details
#' The [stock-class] object aggregates age structure, growth, natural
#' mortality, maturity, fecundity, recruitment, and optional spatial dynamics
#' into a single object for use in an operating model.
#'
#' ## Required Components
#'
#' The following components are required for all stocks: [Ages()], [Length()],
#' [Weight()], [NaturalMortality()], [Maturity()], and [SRR()]. If not
#' supplied, empty objects are created automatically with default slots. 
#'
#' [Fecundity()] and [Spatial()] are optional. If `Spatial` is not specified,
#' the model assumes a single well-mixed area.
#'
#' ## Pass-Through Access from an OM
#'
#' When `Name` is an S4 object with a `Stock` slot (e.g., an [om-class]
#' object), `Stock()` acts as an accessor rather than a constructor:
#'
#' - `Stock(om)` returns `om@Stock`.
#' - `Stock(om, i)` returns `om@Stock[[i]]` when the `Stock` slot is a list,
#'   or `om@Stock` when it is a single stock.
#'
#' where `i` is passed via the `CommonName` argument.
#'
#' ## Internal Defaults
#'
#' When constructing a new [stock-class] object, `nYear`, `pYear`, `nSim`,
#' and `CurrentYear` are set to default values (`20`, `30`, `48`, and the
#' current system year respectively). These are overridden when the stock is
#' added to an [OM()].
#'
#' ## Attaching to an OM
#'
#' A `Stock` object can be attached to an [OM()] with
#' `Stock(om) <- MyStock` and retrieved with `Stock(om)`.
#'
#' Individual slots may be accessed or modified using [Ages()], [Length()],
#' [Weight()], [NaturalMortality()], [Maturity()], [Fecundity()], [SRR()],
#' [Spatial()], [Depletion()], [CommonName()], and [Species()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Stock()` returns a [stock-class] object. If `Name` is an S4 object with
#'   a `Stock` slot, returns that slot or an indexed element of it.
#' - `Stock<-` returns `x` with the `Stock` slot replaced.
#' - `CommonName()`, `Species()` return the corresponding slot from `x`.
#' - `CommonName<-`, `Species<-` return `x` with the corresponding slot
#'   updated.
#'
#' @seealso [stock-class], [OM()], [Ages()], [Length()], [Weight()],
#'   [NaturalMortality()], [Maturity()], [Fecundity()], [SRR()], [Spatial()],
#'   [Depletion()]
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
      cli::cli_abort(c('x'='No slot {.val Stock} found in object class {.cls {class(Name)}}'))
    stock <- Name@Stock
    
    if (is.numeric(CommonName) && is.list(stock))
      return(stock[[CommonName]])
    
    if (is.numeric(CommonName) && !is.list(stock))
      return(stock)
    
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
