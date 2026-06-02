#' The `fleet` S4 Class
#'
#' Defines the fishing characteristics of a fleet for use in an operating
#' model. Objects are typically created via the [Fleet()] constructor, which
#' documents all parameters in detail.
#'
#' @slot Name `character` or `NULL`. Unique fleet identifier.
#' @slot Effort An [effort-class] object defining historical fishing effort,
#'   spatial distribution, and targeting behaviour. See [Effort()].
#' @slot Catchability A [catchability-class] object defining gear efficiency
#'   and optional projected-year trends or stochasticity. See [Catchability()].
#' @slot Selectivity A [selectivity-class] object defining selectivity-at-age,
#'   -at-length, or -at-weight. Required for all fleets. See [Selectivity()].
#' @slot Retention A [retention-class] object defining retention-at-age,
#'   -at-length, or -at-weight. Optional; defaults to full retention if not
#'   supplied. See [Retention()].
#' @slot DiscardMortality A [discardmortality-class] object defining the
#'   proportion of discarded catch that dies. Optional; defaults to 0 (all
#'   discards survive) if not supplied. See [DiscardMortality()].
#' @slot Closure `numeric` array or `NULL`. Spatio-temporal closure schedule
#'   with dimensions `Sim x Year x Area`. Values of 1 indicate an open area;
#'   0 indicates a closed area. Defaults to 1 (all areas open) if not
#'   supplied. See [Fleet()].
#' @slot WeightFleet `numeric` array or `NULL`. Fleet-specific weight-at-age
#'   (`Sim x Age x Year`). If `NA`, set to the stock weight-at-age during
#'   [PopulateFleet()]. See [Fleet()].
#' @slot Bioeconomic A [bioeconomic-class] object. Not currently used.
#' @slot Dynamics List. Reserved for future use for fleet dynamics model
#'   parameters. Default `list()`. Not currently used.#'   
#' @slot nYear `numeric` or `NULL`. Number of years. Inherited from the paired
#'   [stock-class] during [PopulateFleet()].
#' @slot pYear `numeric` or `NULL`. Number of projection years. Inherited from
#'   the paired [stock-class] during [PopulateFleet()].
#' @slot nSim `numeric` or `NULL`. Number of simulation replicates. Inherited
#'   from the paired [stock-class] during [PopulateFleet()].
#' @slot CurrentYear `numeric` or `NULL`. Current calendar year. Inherited
#'   from the paired [stock-class] during [PopulateFleet()].
#' @slot Years `numeric` or `NULL`. Full year vector (historical and
#'   projected). Inherited from the paired [stock-class] during
#'   [PopulateFleet()].
#' @slot Seasons `numeric` or `NULL`. Number of seasons. Inherited from the paired
#'   [stock-class] during [PopulateFleet()].
#' @slot Misc `list`. Miscellaneous additional inputs.
#'
#' @seealso [Fleet()] for the constructor and full parameter documentation.
#'   [Effort()], [Catchability()], [Selectivity()], [Retention()],
#'   [DiscardMortality()] for sub-object constructors.
#'
#' @family fleet
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
    Name             = "char.null",
    Effort           = "effort",
    Catchability     = "catchability",
    Selectivity      = "selectivity",
    Retention        = "retention",
    DiscardMortality = "discardmortality",
    Closure          = "num.array.null",
    WeightFleet      = "array.null",
    Bioeconomic      = "bioeconomic",
    Dynamics         = "list",
    nYear            = "num.null",
    pYear            = "num.null",
    nSim             = "num.null",
    CurrentYear      = "num.null",
    Years            = "num.null",
    Seasons          = "num.null",
    Misc             = "list"
  )
)

setValidity("fleet", function(object) {
  # TODO 
  TRUE
})
