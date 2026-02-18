#' Fleet
#'
#' Construct a [fleet-class] object defining the exploitation characteristics 
#' of a fleet for stock used in an operating model.
#'
#' @param Name Fleet name.
#' @param Effort An [Effort()] object.
#' @param Catchability A [Catchability()] object.
#' @param Selectivity A [Selectivity()] object.
#' @param Retention A [Retention()] object.
#' @param DiscardMortality A [DiscardMortality()] object.
#' @param Closure Spatio-temporal closures.
#' @param WeightFleet Fleet-specific weight-at-age schedules.
#' @param BioEconomic A [Bioeconomic()] object.
#' @param Avoidance Numeric between 0 and 1. Determines the ability of the fleet to avoid exceeding TACs.
#'   * `0` = hard fleet choke (fleet stops when TAC reached)
#'   * `1` = full avoidance disabled (fleet catches up to TAC regardless)
#'   * Intermediate values = partial avoidance.
#' @param Dexterity Numeric between 0 and 1. Determines fleet precision in targeting stocks.
#'   * `0` = perfect targeting (no excess catch)
#'   * `1` = all excess catch is retained/discarded according to TAC fraction.
#' @param Misc Miscellaneous list.
#'
#' @details
#' 
#' A `Fleet` object describes the fishing characteristics of a fleet for a particular stock 
#' in an operating model.
#' 
#' 
#' The **Avoidance** and **Dexterity** parameters are used in multi-stock OMs and 
#' control how fleet effort is adjusted relative to Total Allowable Catches (TACs):
#'
#' | Parameter   | Value | Effect                                                                 |
#' |------------|-------|------------------------------------------------------------------------|
#' | Avoidance  | 0     | Fleet stops when the most restrictive TAC is reached (hard choke)                            |
#' | Avoidance  | 1     | Fleet catches all TACs regardless of choke (no avoidance)                   |
#' | Avoidance  | 0-1   | Fleet partially avoids exceeding TACs proportionally                     |
#' | Dexterity  | 0     | Fleet perfectly targets retained catch (no excess mortality)           |
#' | Dexterity  | 1     | All excess catch is discarded       |
#' | Dexterity  | 0-1   | Partial precision in targeting; some excess catch may occur             |
#'
#'
#' A `Fleet` object can be attached to an [OM()] using [Fleet()] and
#' retrieved using [`Fleet<-`].
#'
#' Individual components may be accessed or modified using accessor
#' and replacement functions such as [Effort()], [`Effort<-`], etc.
#'
#' `r TechManLink()`
#' 
#' @return A [fleet-class] object.
#'
#' @seealso [OM()], [Effort()], [Catchability()], [Selectivity()],
#' [Retention()], [DiscardMortality()], [BioEconomic()]
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
                  Avoidance = 1,
                  Dexterity = 1,
                  Misc = list()) {
  

  if (methods::is(Name, "om"))
    return(Name@Fleet)
  
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
    Avoidance = Avoidance,
    Dexterity = Dexterity,
    Misc = Misc
  )
}

#' @rdname Fleet
#' @export
Closure <- function(Fleet) {
  AccessSlot(Fleet, 'Closure')
}

#' @rdname Fleet
#' @export
`Closure<-` <- function(x, value) {
  AssignSlot(x, value, 'Closure')
}

#' @rdname Fleet
#' @export
WeightFleet <- function(Fleet) {
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
