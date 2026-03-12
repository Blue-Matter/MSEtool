#' Fleet
#'
#' Construct and manipulate a [fleet-class] object defining the exploitation
#' characteristics of a fleet for a stock in an operating model.
#'
#' @param Name Character. Fleet name, or an [om-class] object for pass-through
#'   access (see `Details`).
#' @param Effort An [effort-class] object. Default is an empty [effort-class].
#' @param Catchability A [catchability-class] object. Default is an empty
#'   [catchability-class]. When `Name` is an [om-class] object and `Effort` is
#'   numeric, a numeric value here indexes the fleet within the stock (see
#'   Details).
#' @param Selectivity A [selectivity-class] object. Selectivity is required
#'   for all fleets. Default is an empty [selectivity-class].
#' @param Retention A [retention-class] object. Optional — if not specified,
#'   all age and length classes are assumed fully retained. Default is an empty
#'   [retention-class].
#' @param DiscardMortality A [discardmortality-class] object. Default is an
#'   empty [discardmortality-class].
#' @param Closure Array. Spatio-temporal closure schedule. Default is an empty
#'   array.
#' @param WeightFleet Array. Fleet-specific weight-at-age. If not specified,
#'   the stock-level weight-at-age is used. Default is an empty array.
#' @param BioEconomic A [bioeconomic-class] object. Not currently used. Default
#'   is an empty [bioeconomic-class].
#' @param Dynamics List. Reserved for future use for fleet dynamics model
#'   parameters. Default `list()`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [fleet-class] object or an [om-class] object for `Fleet<-`.
#' @param value A [fleet-class] object or named list of [fleet-class] objects
#'   for `Fleet<-`.
#'
#' @details
#' A [fleet-class] object describes the fishing characteristics of a fleet for
#' a particular stock in an operating model.
#'
#' ## Pass-Through Access from an OM
#'
#' When `Name` is an [om-class] object, `Fleet()` acts as an accessor rather
#' than a constructor:
#'
#' - `Fleet(om)` returns the full fleet list (`om@Fleet`).
#' - `Fleet(om, st)` returns all fleets for stock index `st`
#'   (`om@Fleet[[st]]`).
#' - `Fleet(om, st, fl)` returns fleet `fl` for stock `st`
#'   (`om@Fleet[[st]][[fl]]`).
#'
#' where `st` and `fl` are passed via the `Effort` and `Catchability`
#' arguments respectively.
#'
#' ## Assigning Fleets to an OM
#'
#' A [fleet-class] object or list of [fleet-class] objects can be assigned to
#' an [om-class] object with `Fleet(om) <- MyFleet`. Stock objects must be
#' added to the OM before fleets. If a single [fleet-class] object is
#' assigned, it is applied to all stocks using the fleet name. If a list is
#' assigned, it must be structured as a stock-indexed list of fleet lists.
#'
#' ## Required Components
#'
#' [Selectivity()] and [Effort()] are required for all fleets. All other components are
#' optional and will use default (empty) values if not specified.
#'
#' Individual slots may be accessed or modified using [Effort()], [Catchability()],
#' [Selectivity()], [Retention()], [DiscardMortality()], [Closure()],
#' [WeightFleet()], and [Bioeconomic()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Fleet()` returns a [fleet-class] object, or if `Name` is an [om-class]
#'   object, returns the fleet list or a specific fleet from the OM.
#' - `Fleet<-` returns the [om-class] object `x` with the `Fleet` slot
#'   updated.
#' - `Closure()`, `WeightFleet()` return the corresponding slot from the
#'   [fleet-class] object.
#' - Their replacement forms return the object with the corresponding slot
#'   updated.
#'
#' @seealso [fleet-class], [OM()], [Effort()], [Catchability()],
#'   [Selectivity()], [Retention()], [DiscardMortality()], [Bioeconomic()]
#'
#' @examples
#' f <- Fleet(Name = "Trawl")
#' Selectivity(f)
#' Effort(f)
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
                  BioEconomic = new("bioeconomic"),
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
    Name = Name,
    Effort = Effort,
    Catchability = Catchability,
    Selectivity = Selectivity,
    Retention = Retention,
    DiscardMortality = DiscardMortality,
    Closure = Closure,
    WeightFleet = WeightFleet,
    BioEconomic = BioEconomic,
    Dynamics = Dynamics,
    Misc = Misc
  )
}

#' @rdname Fleet
#' @export
Closure <- function(x) {
  AccessSlot(Fleet, 'Closure')
}

#' @rdname Fleet
#' @export
`Closure<-` <- function(x, value) {
  AssignSlot(x, value, 'Closure')
}

#' @rdname Fleet
#' @export
WeightFleet <- function(x) {
  AccessSlot(Fleet, 'WeightFleet')
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
  
  if (inherits(Fleet, "fleet")) {
    OM@Fleet <- MakeNamedList(
      stocknames,
      MakeNamedList(Fleet@Name, Fleet)
    )
    return(OM)
  }
  
  if (!inherits(Fleet, "list"))
    cli::cli_abort("`Fleet` must be a `Fleet` object or a list of Fleet objects")
  
  OM@Fleet <- Fleet
  names(OM@Fleet) <- stocknames
  class(OM@Fleet) <- "StockFleetList"
  
  OM
}
