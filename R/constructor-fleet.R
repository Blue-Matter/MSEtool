#' Fleet
#'
#' Construct and manipulate a [fleet-class] object defining the exploitation
#' characteristics of a fleet for a stock in an operating model.
#'
#' @param Name Character or `NULL`. Fleet name, or an [om-class] object for
#'   pass-through access (see Details).
#' @param Effort An [effort-class] object defining historical fishing effort,
#'   spatial distribution, and targeting. See [Effort()] for full
#'   documentation. Default is an empty [effort-class]. When `Name` is an
#'   [om-class] object, a numeric value here indexes the stock within the OM
#'   (see Details).
#' @param Catchability A [catchability-class] object defining gear efficiency
#'   and optional projected-year stochasticity or trends. See [Catchability()]
#'   for full documentation. Default is an empty [catchability-class]. When
#'   `Name` is an [om-class] object and `Effort` is numeric, a numeric value
#'   here further indexes the fleet within the stock (see Details).
#' @param Selectivity A [selectivity-class] object defining
#'   selectivity-at-age, -at-length, or -at-weight. Required for all fleets.
#'   See [Selectivity()] for full documentation. Default is an empty
#'   [selectivity-class].
#' @param Retention A [retention-class] object defining retention-at-age,
#'   -at-length, or -at-weight. Optional. If not specified, all age and length
#'   classes are assumed fully retained during [PopulateFleet()]. See
#'   [Retention()] for full documentation. Default is an empty
#'   [retention-class].
#' @param DiscardMortality A [discardmortality-class] object defining the
#'   proportion of discarded catch that dies. Optional. If not specified,
#'   discard mortality is set to 0 (all discards survive) during
#'   [PopulateFleet()]. See [DiscardMortality()] for full documentation.
#'   Default is an empty [discardmortality-class].
#' @param Closure Numeric array or `NULL`. Spatio-temporal closure schedule
#'   with dimensions `Sim x Year x Area`. Values of 1 indicate an open area;
#'   0 indicates a closed area. The `Sim` and `Year` dimensions may be length
#'   1 (replicated internally). Default is an empty array; if not specified,
#'   [PopulateClosure()] sets all areas to open (1) across all simulations and
#'   years.
#' @param WeightFleet Numeric array or `NA`. Fleet-specific weight-at-age
#'   (`Sim x Age x Year`). Used in fleet-level biomass calculations. If `NA`
#'   (default), [PopulateFleet()] sets `WeightFleet` equal to the stock
#'   weight-at-age array.
#' @param Bioeconomic A [bioeconomic-class] object. Not currently used.
#'   Default is an empty [bioeconomic-class].
#' @param Dynamics List. Reserved for future use for fleet dynamics model
#'   parameters. Default `list()`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [fleet-class] object or an [om-class] object for `Fleet<-`.
#' @param value A [fleet-class] object or named list of [fleet-class] objects
#'   for `Fleet<-`.
#'
#' @details
#' A [fleet-class] object describes the fishing characteristics of a fleet for
#' a particular stock in an operating model, including effort dynamics,
#' catchability, selectivity, retention, discard mortality, and spatial
#' closures.
#'
#' ## Required and Optional Components
#'
#' [Selectivity()] and [Effort()] are required for all fleets. All other
#' components are optional and will use default values if not specified:
#' - [Catchability()]: defaults to efficiency = 1 across all years.
#' - [Retention()]: defaults to full retention for all age and length classes.
#' - [DiscardMortality()]: defaults to 0 (all discards survive).
#' - `Closure`: defaults to 1 (all areas open) across all simulations and years.
#' - `WeightFleet`: defaults to the stock weight-at-age array.
#'
#' ## Pass-Through Access from an OM
#'
#' When `Name` is an [om-class] object, `Fleet()` acts as an accessor rather
#' than a constructor:
#' - `Fleet(om)` returns the full fleet list (`om@Fleet`).
#' - `Fleet(om, st)` returns all fleets for stock index `st`
#'   (`om@Fleet[[st]]`), where `st` is passed via the `Effort` argument.
#' - `Fleet(om, st, fl)` returns fleet `fl` for stock `st`
#'   (`om@Fleet[[st]][[fl]]`), where `fl` is passed via the `Catchability`
#'   argument.
#'
#' ## Assigning Fleets to an OM
#'
#' [Stock()] objects must be added to the OM before fleets. A single
#' [fleet-class] object assigned with `Fleet(om) <- MyFleet` is applied to all
#' stocks using the fleet name. A list must be structured as a stock-indexed
#' list of fleet lists.
#'
#' ## Bookkeeping Slots
#'
#' The slots `nYear`, `pYear`, `nSim`, `CurrentYear`, `Years`, and `Seasons`
#' are inherited from the paired [stock-class] object automatically when
#' [PopulateFleet()] is called and do not need to be set manually.
#'
#' ## Closure Array
#'
#' `Closure` has dimensions `Sim x Year x Area`. The `Sim` and `Year`
#' dimensions may be length 1 (replicated internally by [PopulateClosure()]).
#' If the array is empty or `NULL`, all areas are set to open (1) across all
#' simulations and years. Dimension names for `Sim`, `Year`, and `Area` are
#' added automatically if missing.
#'
#' Individual slots may be accessed or modified using [Effort()],
#' [Catchability()], [Selectivity()], [Retention()], [DiscardMortality()],
#' [Closure()], [WeightFleet()], and [Bioeconomic()].
#'
#' @return
#' - `Fleet()` returns a [fleet-class] object. If `Name` is an [om-class]
#'   object, returns the fleet list or a specific fleet from the OM.
#' - `Fleet<-` returns the [om-class] object `x` with the `Fleet` slot
#'   updated.
#' - `Closure()` and `WeightFleet()` return the corresponding slot from the
#'   [fleet-class] object `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso
#' - [fleet-class] for the class definition and slot-level documentation.
#' - [OM()] for the operating model constructor.
#' - [Effort()], [Catchability()], [Selectivity()], [Retention()],
#'   [DiscardMortality()] for sub-object constructors.
#' - [PopulateFleet()] for how fleet components are expanded across
#'   simulations and years.
#'
#' @family fleet
#'
#' @examples
#' # See man-examples/class-Fleet.R
#'
#' @export
Fleet <- function(Name = NULL,
                  Effort = new("effort"),
                  Catchability = new("catchability"),
                  Selectivity = new("selectivity"),
                  Retention = new("retention"),
                  DiscardMortality = new("discardmortality"),
                  Closure = array(),
                  WeightFleet = array(),
                  Bioeconomic = new("bioeconomic"),
                  Dynamics = list(),
                  Misc = list()) {
  

  if (inherits(Name, "om")) {
    if (inherits(Effort, 'numeric')) {
      if (inherits(Catchability, 'catchability'))
        return(Name@Fleet[[Effort]])
      if (inherits(Catchability, 'numeric'))
        return(Name@Fleet[[Effort]][[Catchability]])
    }
    
    if (inherits(Effort, 'effort'))
      return(Name@Fleet)
  }

  methods::new(
    "fleet",
    Name             = Name,
    Effort           = Effort,
    Catchability     = Catchability,
    Selectivity      = Selectivity,
    Retention        = Retention,
    DiscardMortality = DiscardMortality,
    Closure          = Closure,
    WeightFleet      = WeightFleet,
    Bioeconomic      = Bioeconomic,
    Dynamics         = Dynamics,
    Misc             = Misc
  )
}

#' @rdname Fleet
#' @export
Closure <- function(x) {
  AccessSlot(x, 'Closure')
}

#' @rdname Fleet
#' @export
`Closure<-` <- function(x, value) {
  AssignSlot(x, value, 'Closure')
}

#' @rdname Fleet
#' @export
WeightFleet <- function(x) {
  AccessSlot(x, 'WeightFleet')
}

#' @rdname Fleet
#' @export
`WeightFleet<-` <- function(x,value) {
  AssignSlot(x, value, 'WeightFleet')
}

#' @rdname Fleet
#' @export
`Fleet<-` <- function(x, value) {
  CheckClass(x, "om", "x")
  
  OM <- x
  Fleet <- value
  
  if (inherits(OM@Stock, 'stock')) {
    stocknames <- OM@Stock@Name
  } else {
    stocknames <- StockNames(OM)
  }
  
  if (is.null(stocknames))
    cli::cli_abort("Add `Stock` object(s) to `OM` first")
  
  nstocks <- nStock(OM)
  
  check_and_name_fleets <- function(fleet_list) {
    cls <- purrr::map_chr(fleet_list, class)
    chk <- cls == "fleet"
    if (any(!chk))
      cli::cli_abort(c(
        'x' = 'All elements of `value` must be a {.help MSEtool::Fleet} object',
        'i' = 'Current classes of `value` are: {.val {cls}}'
      ))
    
    nms <- purrr::map_chr(fleet_list, Name)
    if (length(unique(nms)) != length(nms))
      cli::cli_abort(c(
        'x' = 'Fleets must have unique names `Name(Fleet)`',
        'i' = 'Current names of fleets in `value` are: {.val {nms}}'
      ))
    
    names(fleet_list) <- nms
    fleet_list
  }
  
  # single fleet object - replicate across all stocks
  if (inherits(Fleet, "fleet")) {
    named_fleet <- check_and_name_fleets(list(Fleet))
    OM@Fleet <- MakeNamedList(stocknames, named_fleet)
    return(OM)
  }
  
  if (inherits(Fleet, "list")) {
    is_nested <- purrr::every(Fleet, is.list)
    
    # nested list [stock][fleet]
    if (is_nested) {
      if (length(Fleet) != nstocks)
        cli::cli_abort(c(
          'x' = 'Nested `value` must have one element per stock',
          'i' = 'Expected {.val {nstocks}} stock{?s}, got {.val {length(Fleet)}}'
        ))
      
      Fleet <- purrr::map(Fleet, check_and_name_fleets)
      names(Fleet) <- stocknames
      OM@Fleet <- Fleet
      return(OM)
    }
    
    # flat list of fleet objects - apply to all stocks
    named_fleet <- check_and_name_fleets(Fleet)
    OM@Fleet <- MakeNamedList(stocknames, named_fleet)
    return(OM)
  }
  
  AssignSlot(OM, Fleet, 'Fleet')
}
