methods::setClassUnion(
  name    = "selectivity.list",
  members = c("selectivity", "list", 'NULL')
)

methods::setClassUnion(
  name    = "retention.list",
  members = c("retention", "list", 'NULL')
)

methods::setClassUnion(
  name    = "discardmortality.list",
  members = c("discardmortality", "list", 'NULL')
)

#' `Advice` Object
#'
#' The `advice` class defines management advice produced by a management
#' procedure. Advice may include output controls (e.g. total allowable catch),
#' input controls (e.g. effort), spatial controls (closures), gear effects,
#' or direct fishing mortality targets. See [Advice()] for details on valid 
#' entries and options for each slot. 
#'
#' @slot TAC Numeric vector or numeric array specifying total allowable catch.
#' 
#' @slot Effort Numeric vector array specifying relative or absolute fishing effort.
#' 
#' @slot EffType Character. Are effort regulations relative to last historical year (`"Rel"`) or
#' absolute (`"Abs"`; in units of [Effort()]). Default is `"Abs"`.
#'
#' @slot Closure Numeric vector or array specifying spatial closures.
#'
#' @slot Selectivity A [Selectivity()] object or an `nFleet` long list of [Selectivity()] objects
#' defining gear selectivity prescribed by the `MP`
#'
#' @slot Retention A [Retention()] object or an `nFleet` long list of [Retention()] objects
#' defining retention prescribed by the `MP`
#'
#' @slot DiscardMortality A [DiscardMortality()] object or an `nFleet` long 
#'  list of [DiscardMortality()] objects defining discard mortality set in the `MP`
#'
#' @slot ApicalF Numeric array specifying target apical fishing mortality. Not currently used
#'
#' @slot Misc Miscellaneous list. Will be passed to `Data@Misc` in following time steps.
#'
#' @slot Log List used internally to store diagnostics
#' 
#' @seealso [Advice()], [Selectivity()], [Retention()], [DiscardMortality()]
#'
#' @name advice-class
#' @include class-unions.R
#' @include class-selectivity.R
#' @include class-retention.R
#' @include class-discardmortality.R
setClass("advice",
         slots = c(TAC = "num.array.null",
                   Effort = "num.array.null",
                   EffType = 'char.null',
                   
                   Closure = "num.array.null",
                   Selectivity = "selectivity.list",
                   Retention = "retention.list",
                   DiscardMortality = "discardmortality.list",
                   ApicalF = "num.array.null",
                   Misc = "list",
                   Log = "list")
)


setValidity("advice", function(object) {
  # TODO
  TRUE
})