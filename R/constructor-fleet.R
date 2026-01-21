#' Fleet
#'
#' Create a `Fleet` object.
#'
#' A `Fleet` object aggregates effort, selectivity, retention,
#' discard mortality, and bioeconomic components.
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
#' @return A `Fleet` object.
#'
#' @seealso [OM()]
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
  
  ## OM pass-through
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

#' Fleet accessors and assignment functions
#'
#' Functions for accessing and modifying a [Fleet()] object, and for
#' attaching or retrieving a `Fleet` object from an [OM()].
#'
#' @param OM An [OM()] object.
#' @param x A [Fleet()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetFleet()` and `SetFleet()` retrieve or assign `Fleet` objects
#'   within an [OM()].
#' - Slot accessors retrieve individual fleet components.
#' - Replacement functions update slots and validate the object.
#'
#' Conceptual details are documented in [Fleet()].
#'
#' @name Fleet-accessors
NULL


#' @rdname Fleet-accessors
#' @export
GetFleet <- function(OM) {
  CheckClass(OM, "om", "OM")
  OM@Fleet
}

#' @rdname Fleet-accessors
#' @export
SetFleet <- function(OM, Fleet) {
  CheckClass(OM, "om", "OM")
  
  stocknames <- StockNames(OM)
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
