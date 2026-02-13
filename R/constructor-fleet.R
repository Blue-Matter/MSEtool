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
#' @param Misc Miscellaneous list.
#'
#' @details
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
