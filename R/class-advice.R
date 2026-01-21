methods::setClassUnion(
  name    = "selectivity.list",
  members = c("selectivity", "list")
)

methods::setClassUnion(
  name    = "retention.list",
  members = c("retention", "list")
)

methods::setClassUnion(
  name    = "discardmortality.list",
  members = c("discardmortality", "list")
)

#' `Advice` Object
#'
#' The `advice` class defines management advice produced by a management
#' procedure. Advice may include output controls (e.g. total allowable catch),
#' input controls (e.g. effort), spatial controls (closures), gear effects,
#' or direct fishing mortality targets.
#'
#' @slot TAC Numeric array specifying total allowable catch. 
#' 
#' @slot Effort Numeric array specifying relative or absolute fishing effort.
#' @slot EffType Character. Are effort regulations relative to last historical year (`Rel`) or
#' absolute (`Abs`; in units of [Effort()])
#'
#' @slot Closure Numeric or logical array specifying spatial or temporal
#' fishery closures.
#'
#' @slot Selectivity A [Selectivity()] object or an `nFleet` long list of [Selectivity()] objects
#' defining gear selectivity associated with the advice.
#'
#' @slot Retention A [Retention()] object or an `nFleet` long list of [Retention()] objects
#' defining retention-at-age associated with the advice.
#'
#' @slot DiscardMortality A [DiscardMortality()] object or an `nFleet` long 
#'  list of [DiscardMortality()] objects defining discard mortality rates.
#'
#' @slot apicalF Numeric array specifying target apical fishing mortality. Not Currently used
#'
#' @slot Misc Miscellaneous list
#'
#' @slot Log List used internally to store diagnostics
#'
#' @seealso [Advice()], [Selectivity()], [Retention()], [DiscardMortality()]
#'
#'
#' @include class-unions.R
#' @include class-selectivity.R
#' @include class-retention.R
#' @include class-discardmortality.R
setClass("advice",
         slots = c(TAC = "num.array.null",
                   Effort = "num.array.null",
                   EffType = 'character',
                   
                   Closure = "num.array.null",
                   Selectivity = "selectivity.list",
                   Retention = "retention.list",
                   DiscardMortality = "discardmortality.list",
                   apicalF = "num.array.null",
                   Misc = "list",
                   Log = "list")
)


setValidity("advice", function(object) {
  # TODO
  TRUE
})