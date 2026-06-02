

#' Implementation Error
#'
#' The [imp-class] defines implementation error associated with management
#' advice, controlling how management recommendations (TAC, effort, and size
#' limits) are imperfectly applied in the operating model.
#'
#' @slot Name Character. Name of the implementation error model.
#'   See [Imp()].
#' @slot TAC An [impslot-class] object. Implementation error for total
#'   allowable catch recommendations. See [Imp()].
#' @slot Effort An [impslot-class] object. Implementation error for
#'   effort-based controls. See [Imp()].
#' @slot Size An [impslot-class] object. Implementation error for size-based
#'   regulations. See [Imp()].
#' @slot Misc List. Miscellaneous additional objects.
#'
#' @details
#' Each of the `TAC`, `Effort`, and `Size` slots is an [impslot-class] object
#' containing `Mean`, `SD`, `Compliance`, and `Error` arrays that together
#' describe how imperfectly the corresponding management control is implemented
#' across simulations.
#'
#' An [imp-class] object can be attached to an [om-class] object and
#' retrieved with `Imp(om)`.
#'
#' Direct construction via [methods::new()] is not recommended; use [Imp()]
#' instead, which initialises all sub-objects automatically.
#'
#' @note
#' The [imp-class] is currently a placeholder. The class interface is
#' subject to change.
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
    Name   = "character",
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


