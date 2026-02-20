#' Populate Operating Model Components
#'
#' Generic dispatcher function to populate operating model objects and their components.
#'
#' @param object An object to populate. The class of `object` determines which
#'   class-specific `Populate*()` function is called.
#' @param ... Named arguments passed to the appropriate `Populate*()` function.
#'   Only arguments relevant to the class of `object` are used.
#'
#' @details
#' 
#' Generally, users will not need to use the `Populate*` functions as 
#' they are called internally.
#' 
#' Depending on the class of `object`, the function calls a class-specific
#' `Populate*()` function that generates stochastic values, fills derived slots,
#' and checks object structure and contents.
#' 
#' `Populate()` is a wrapper around class-specific population functions.  
#' The following methods may be called depending on the class of `object`:
#' * [PopulateOM()]
#' * [PopulateStock()]
#' * [PopulateLength()]
#' * [PopulateWeight()]
#' * [PopulateNaturalMortality()]
#' * [PopulateMaturity()]
#' * [PopulateFecundity()]
#' * [PopulateSRR()]
#' * [PopulateSpatial()]
#' * [PopulateDepletion()]
#' * [PopulateFleet()]
#' * [PopulateEffort()]
#' * [PopulateCatchability()]
#' * [PopulateSelectivity()]
#' * [PopulateRetention()]
#' * [PopulateDiscardMortality()]
#'
#' Each `Populate*()` function has its own documentation detailing which named
#' arguments it accepts. Users should consult the specific `Populate*()` help
#' page for information about required slots, default behavior, and argument
#' descriptions.
#' 
#' @return The populated object, of the same class as `object`.
#'
#' @examples
#' \dontrun{
#' # Populate a full operating model
#' OM <- Populate(OM)
#'
#' # Populate a stock with specific arguments
#' Stock <- Populate(
#'   Stock,
#'   nYear = 40,
#'   pYear = 20,
#'   nSim = 100
#' )
#'
#' # Populate a fleet using an already-populated stock
#' Fleet <- Populate(
#'   Fleet,
#'   Stock = Stock,
#'   nSim = 100
#' )
#' }
#'
#' @export
Populate <- function(object, ...) {
  
  object <- UpdateObject(object)
  
  if (inherits(object, "om")) {
    return(PopulateOM(OM = object, ...))
  }
  
  if (inherits(object, "stock")) {
    return(PopulateStock(Stock = object, ...))
  }
  
  if (inherits(object, "length")) {
    return(PopulateLength(Length = object, ...))
  }
  
  if (inherits(object, "weight")) {
    return(PopulateWeight(Weight = object, ...))
  }
  
  if (inherits(object, "naturalmortality")) {
    return(PopulateNaturalMortality(NaturalMortality = object, ...))
  }
  
  if (inherits(object, "maturity")) {
    return(PopulateMaturity(Maturity = object, ...))
  }
  
  if (inherits(object, "fecundity")) {
    return(PopulateFecundity(Fecundity = object, ...))
  }
  
  if (inherits(object, "srr")) {
    return(PopulateSRR(SRR = object, ...))
  }
  
  if (inherits(object, "spatial")) {
    return(PopulateSpatial(Spatial = object, ...))
  }
  
  if (inherits(object, "depletion")) {
    return(PopulateDepletion(Depletion = object, ...))
  }
  
  if (inherits(object, "fleet")) {
    return(PopulateFleet(Fleet = object, ...))
  }
  
  if (inherits(object, "effort")) {
    return(PopulateEffort(Effort = object, ...))
  }
  
  if (inherits(object, "catchability")) {
    return(PopulateCatchability(Catchability = object, ...))
  }
  
  if (inherits(object, "selectivity")) {
    return(PopulateSelectivity(Selectivity = object, ...))
  }
  
  if (inherits(object, "retention")) {
    return(PopulateRetention(Retention = object, ...))
  }
  
  if (inherits(object, "discardmortality")) {
    return(PopulateDiscardMortality(DiscardMortality = object, ...))
  }
  
  cli::cli_abort(
    c("x" = "Cannot populate object of class {.cls {class(object)}}.")
  )
}
