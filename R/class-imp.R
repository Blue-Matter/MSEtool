

#' Implementation Error
#'
#' The [imp-class] defines implementation error associated with management
#' advice, controlling how management recommendations (TAC, effort, and size
#' limits) are imperfectly applied in the operating model.
#'
#' @slot Name `character` or `NULL`. Name of the implementation error model.
#'   See [Imp()].
#' @slot TAC An [impslot-class] object. Implementation error for total
#'   allowable catch recommendations, via `Mean`/`SD`/`Error`. See [Imp()].
#' @slot Effort An [impslot-class] object. Implementation error for
#'   effort-based controls, via `Mean`/`SD`/`Error`. See [Imp()].
#' @slot Size An [impslot-class] object. Implementation error for size-based
#'   regulations (minimum legal size etc.), via `Compliance` only. See
#'   [Imp()].
#' @slot Misc List. Miscellaneous additional objects.
#'
#' @details
#' `TAC` and `Effort` each use `Mean`/`SD`/`Error`: a multiplicative bias and
#' its stochastic realisation applied to the advised TAC/effort. `Size`
#' instead uses only `Compliance`: the
#' fraction of the fleet that adopts a newly-advised size-based regulation
#' (a change to `Advice@Retention` and/or `Advice@Selectivity`) in the year it
#' changes, with the remainder of the fleet continuing under the prior
#' (status-quo) Retention/Selectivity curve.
#' 
#' An [imp-class] object can be attached to an [om-class] object and
#' retrieved with `Imp(om)`.
#'
#' @seealso
#' - [Imp()] for the constructor and accessor.
#' - [ImpSlot()] for the sub-object constructor and slot-level accessors.
#' - [OM()] for the operating model constructor.
#' - [Advice()] for the advice object connected to implementation.
#' - [ConvertImp()] for converting legacy implementation objects.
#'
#' @family imp
#'
#' @include class-unions.R
#' @include class-imp-slot.R
#' @name imp-class
#' @export
setClass(
  "imp",
  slots = c(
    Name   = "char.null",
    TAC    = "impslot",
    Effort = "impslot",
    Size   = "impslot",
    Misc   = "list"
  )
)


setValidity("imp", function(object) {
  # TODO
  TRUE
})

