#' Fleet Class
#'
#' The `fleet` class defines fishing fleet structure, effort dynamics,
#' selectivity, retention, discard mortality, and bioeconomic information.
#'
#' See [Fleet()] for details.
#' 
#' @slot Name Fleet name.
#' @slot Effort [Effort()] object.
#' @slot Catchability [Catchability()] object.
#' @slot Selectivity [Selectivity()] object.
#' @slot Retention [Retention()] object.
#' @slot DiscardMortality [DiscardMortality()] object.
#' @slot Closure Spatial or temporal closures.
#' @slot WeightFleet Fleet-specific weights.
#' @slot BioEconomic [Bioeconomic()] object.
#' @slot nYear Number of years.
#' @slot pYear Projection year.
#' @slot nSim Number of simulations.
#' @slot CurrentYear Current year index.
#' @slot Years Year vector.
#' @slot Seasons Season vector.
#' @slot Misc Miscellaneous list.
#' 
#' @include class-unions.R
#' @include class-effort.R
#' @include class-catchability.R
#' @include class-selectivity.R
#' @include class-retention.R
#' @include class-discardmortality.R
#' @include class-bioeconomic.R
#' @name fleet-class
setClass(
  "fleet",
  slots = c(
    Name = "char.null",
    Effort = "effort",
    Catchability = "catchability",
    Selectivity = "selectivity",
    Retention = "retention",
    DiscardMortality = "discardmortality",
    Closure = "num.array.null",
    WeightFleet = "array.null",
    BioEconomic = "bioeconomic",
    nYear = "num.null",
    pYear = "num.null",
    nSim = "num.null",
    CurrentYear = "num.null",
    Years = "num.null",
    Seasons = "num.null",
    Misc = "list"
  )
)

setValidity("fleet", function(object) {
  # TODO 
  TRUE
})
