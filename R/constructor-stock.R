#' Stock Constructor and Accessors
#'
#' Construct a [stock-class] object defining the biological and
#' population-dynamics properties of a stock, or access and replace the `Stock`
#' slot of an enclosing S4 object (e.g., an [om-class] object).
#'
#' @param Name `character(1)`. Unique stock identifier used throughout model
#'   output. Alternatively, an S4 object that contains a `Stock` slot (e.g.,
#'   an [om-class] object), in which case `Stock()` acts as a pass-through
#'   accessor — see *Pass-Through Access*. Default `"New Stock Object"`.
#' @param CommonName `character(1)`. Common name of the species (e.g.,
#'   `"Atlantic cod"`). When `Name` is an S4 object and `CommonName` is
#'   `numeric`, it is treated as a list index for multi-stock `Stock` slots —
#'   see *Pass-Through Access*. Default `NULL`.
#' @param Species `character(1)`. Scientific (Latin) name of the species (e.g.,
#'   `"Gadus morhua"`). Default `NULL`.
#'   
#' @param Ages An [ages-class] object defining the age structure and plus-group
#'   age. If `NULL` (default), an empty [ages-class] is created via [Ages()].
#'   
#' @param Length A [length-class] object specifying the length-at-age growth
#'   schedule. If `NULL` (default), created via [Length()].
#'   
#' @param Weight A [weight-class] object specifying the weight-at-age and
#'   weight-at-length schedules. If `NULL` (default), created via [Weight()].
#'  
#' @param NaturalMortality A [naturalmortality-class] object specifying natural
#'   mortality rate(s), optionally age- or length-varying. If `NULL` (default),
#'   created via [NaturalMortality()]. 
#'   
#' @param Maturity A [maturity-class] object specifying the maturity schedule
#'   as a function of age, length, or weight. If `NULL` (default), created via
#'   [Maturity()]. 
#'   
#' @param Fecundity A [fecundity-class] object specifying egg production as a
#'   function of age or length. Optional. When `NULL` (default), spawning
#'   production (`SProduction`) equals spawning biomass (`SBiomass`, i.e.
#'   mature weight-at-age). When populated, `SProduction` is in the units of
#'   the `Fecundity` object (e.g. eggs). If `NULL` (default), created via 
#'   [Fecundity()].
#'   
#' @param SRR A [srr-class] object specifying the stock-recruitment
#'   relationship and recruitment variability. Created via [SRR()]
#'   
#' @param Spatial A [spatial-class] object specifying spatial structure and
#'   movement dynamics. Optional — leave empty for single-area (non-spatial)
#'   models. If `NULL` (default), created via [Spatial()].
#'   
#' @param Depletion A [depletion-class] object specifying depletion assumptions
#'   at the start (`Initial`) and/or end (`Final`) of the historical period,
#'   relative to a reference biomass (default `"B0"`). When `Final` is
#'   specified, `Efficiency` in [Catchability()] is optimised to achieve the
#'   target terminal depletion, overwriting any existing values. If `NULL`
#'   (default), created via [Depletion()].
#'   
#' @param Seasons `integer(1)`. Number of seasons per calendar year. Use
#'   values greater than `1` for within-year dynamics (e.g., `4` for quarterly
#'   seasons). Default `1`. Must match the time units set in [Ages()].
#'   
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [stock-class] object, or an S4 object with a `Stock` slot, for
#'   use with accessor and replacement functions.
#' @param value For `Stock<-`: a [stock-class] object, or a list of
#'   [stock-class] objects for multi-stock operating models. For `CommonName<-`
#'   and `Species<-`: a `character(1)` string.
#'
#' @details
#' ## Object Structure
#'
#' A [stock-class] object aggregates all biological components needed to
#' simulate stock dynamics: 
#' - age structure ([Ages()]); 
#' - somatic growth ([Length()], [Weight()]); 
#' - survival ([NaturalMortality()]); 
#' - reproduction ([Maturity()], [Fecundity()], [SRR()]);
#' - initial conditions ([Depletion()]);
#' - and optional spatial structure ([Spatial()]).
#'
#' ## Required vs Optional Components
#'
#' [Ages()], [Length()], [Weight()], [NaturalMortality()], [Maturity()], and
#' [SRR()] are required. If any are omitted, empty objects
#' with default slots are created automatically, but the model will not run
#' until the required parameters within each sub-object are populated.
#'
#' [Fecundity()] and [Spatial()] are optional. Omitting [Fecundity()] causes
#' `SProduction` to equal `SBiomass`. Omitting [Spatial()] implies a single
#' well-mixed area.
#'
#' [Depletion()] is also optional. Omitting it (or leaving both `Initial` and
#' `Final` as `NULL`) means the stock starts unfished and terminal depletion is
#' determined by the [Fleet()] and [Catchability()] parameters.
#'
#' ## Pass-Through Access from an Enclosing Object
#'
#' When `Name` is an S4 object with a `Stock` slot (e.g., an [om-class]),
#' `Stock()` returns the slot rather than constructing a new object:
#'
#' - `Stock(om)` returns `om@Stock`.
#' - `Stock(om, i)` returns `om@Stock[[i]]` when `om@Stock` is a list
#'   (multi-stock model), or `om@Stock` when it is a single [stock-class].
#'
#' The index `i` is passed via the `CommonName` argument.
#'
#' ## Bookkeeping Slots
#'
#' The slots `nYear`, `pYear`, `nSim`, `CurrentYear`, and `Years` are
#' initialised to working defaults (`20`, `30`, `48`, the current calendar
#' year, and the corresponding year vector). These are overwritten automatically
#' when the stock is attached to an [OM()], so they need not be set manually.
#'
#' @return
#' - `Stock()` returns a [stock-class] object. If `Name` is an S4 object with
#'   a `Stock` slot, returns that slot (or an indexed element for list-valued
#'   slots).
#' - `Stock<-` returns `x` with the `Stock` slot replaced by `value`.
#' - `CommonName()` and `Species()` return a `character(1)` string from the
#'   corresponding slot of `x`.
#' - `CommonName<-` and `Species<-` return `x` with the named slot updated.
#'
#' @seealso
#' - [stock-class] for the class definition and slot-level documentation.
#' - [OM()] for the operating model constructor.
#' - [Ages()], [Length()], [Weight()], [NaturalMortality()], [Maturity()],
#'   [Fecundity()], [SRR()], [Spatial()], [Depletion()] for the sub-object
#'   constructors.
#' - [Specifying Biological and Fleet Schedules][populating-schedules] for how
#'   `Pars`, `Model`, and `MeanAt*` arrays are specified across sub-objects.
#'
#' @family stock
#'
#' @example man-examples/class-Stock.R
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
  
  
  if (!inherits(Name, "character")) {
    if (!"Stock" %in% slotNames(Name))
      cli::cli_abort(c("x" = "No slot {.val Stock} found in object class {.cls {class(Name)}}"))
    stock <- Name@Stock
    
    if (is.numeric(CommonName) && is.list(stock))
      return(stock[[CommonName]])
    
    if (is.numeric(CommonName) && !is.list(stock))
      return(stock)
    
    return(Name@Stock)
  }

  
  if (is.null(Ages))             Ages             <- Ages()
  if (is.null(Length))           Length           <- Length()
  if (is.null(Weight))           Weight           <- Weight()
  if (is.null(NaturalMortality)) NaturalMortality <- NaturalMortality()
  if (is.null(Maturity))         Maturity         <- Maturity()
  if (is.null(Fecundity))        Fecundity        <- Fecundity()
  if (is.null(SRR))              SRR              <- SRR()
  if (is.null(Spatial))          Spatial          <- Spatial()
  if (is.null(Depletion))        Depletion        <- Depletion()
  
  # Defaults
  nYear       <- 20
  pYear       <- 30
  nSim        <- 48
  CurrentYear <- as.numeric(format(Sys.Date(), "%Y"))
 
  object <- methods::new(
    "stock",
    Name             = Name,
    CommonName       = CommonName,
    Species          = Species,
    Ages             = Ages,
    Length           = Length,
    Weight           = Weight,
    NaturalMortality = NaturalMortality,
    Maturity         = Maturity,
    Fecundity        = Fecundity,
    SRR              = SRR,
    Spatial          = Spatial,
    Depletion        = Depletion,
    nYear            = nYear,
    pYear            = pYear,
    nSim             = nSim,
    CurrentYear      = CurrentYear,
    Seasons          = Seasons,
    Years            = CalcYears(nYear, pYear, CurrentYear, Seasons),
    Misc             = Misc,
    Log              = list()
  )
  
  methods::validObject(object)
  object
}


#' @rdname Stock
#' @export
`Stock<-` <- function(x, value) {
  CheckClass(x, 'om', 'x')
  
  if (inherits(value, 'stock')) {
    l <- list(value)
    names(l) <- value@Name
    x@Stock <- l
    return(x)
  }
  
  if (inherits(value, 'list')) {
    cls <- purrr::map_chr(value, class)
    chk <- cls == 'stock'
    if (any(!chk)) 
      cli::cli_abort(c(
        'x' = 'All elements of `value` must be an {.help MSEtool::Stock} object',
        'i' = 'Current classes of `value` are: {.val {cls}}'
      ))
      
    
    nms <- purrr::map_chr(value, Name)
    if (length(unique(nms)) != length(nms)) {
      cli::cli_abort(c(
        'x' = 'Stocks must have unique names `Name(Stock)`',
        'i' = 'Current names of stocks in `value` are: {.val {nms}}'
      ))
    }
    names(value) <- nms
    x@Stock <- value
    return(x)
  }
  
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
