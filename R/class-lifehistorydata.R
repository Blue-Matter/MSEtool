#' `lifehistorydata` Class
#'
#' Groups the biological life-history components required to describe
#' population dynamics. Each slot holds a dedicated sub-object defining
#' the model and parameters for that process. Used in the `LifeHistory` slot
#' of a [data-class] object.
#'
#' @slot Ages An object of class [ages-class] defining the age structure of the
#'   population (minimum age, maximum age, plus-group).
#' @slot Length An object of class [length-class] defining the growth model
#'   (e.g., von Bertalanffy) and length-at-age parameters.
#' @slot Weight An object of class [weight-class] defining the weight-at-age or
#'   length–weight relationship.
#' @slot NaturalMortality An object of class [naturalmortality-class] defining
#'   natural mortality rates, which may be age-, size-, or time-varying.
#' @slot Maturity An object of class [maturity-class] defining maturity-at-age or
#'   maturity-at-length schedules.
#' @slot Fecundity An object of class [fecundity-class] defining fecundity-at-age
#'   or fecundity-at-length relationships.
#' @slot SRR An object of class [srr-class] defining the stock–recruitment
#'   relationship (e.g., Beverton–Holt, Ricker) and associated parameters.
#' @slot Spatial An object of class [spatial-class] defining the spatial structure
#'   of the population, including movement and area allocation.
#' @slot Depletion An object of class [depletion-class] specifying the initial
#'   depletion level relative to unfished biomass.
#' @slot Misc A named list for any additional life-history metadata.
#'
#' `LifeHistoryData()` creates a new `lifehistorydata` object. 
#' 
#' @return `LifeHistoryData()` returns a `lifehistorydata` object.
#' @name lifehistorydata
#' @aliases lifehistorydata-class
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' @include class-stock.R
#'
#' @export
setClass(
  "lifehistorydata",
  slots = c(
    Ages              = "ages",
    Length            = "length",
    Weight            = "weight",
    NaturalMortality  = "naturalmortality",
    Maturity          = "maturity",
    Fecundity         = "fecundity",
    SRR               = "srr",
    Spatial           = "spatial",
    Depletion         = "depletion",
    Misc              = "list"
  )
)

#' @rdname lifehistorydata
#' @export
LifeHistoryData <- function() {
  new('lifehistorydata')
}