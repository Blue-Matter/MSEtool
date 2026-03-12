#' Implementation Error Slot
#'
#' Internal class representing implementation uncertainty for a single
#' management control (TAC, effort, or size regulations) within an
#' [imp-class] object.
#'
#' @slot Mean Numeric array or list of arrays. Mean implemented fraction of
#'   the management recommendation (e.g. `1` = perfect compliance with TAC).
#' @slot SD Numeric array or list of arrays. Standard deviation of
#'   implementation error around `Mean`.
#' @slot Compliance Numeric array or list of arrays. Compliance rate, i.e.
#'   the proportion of the fleet that adheres to the management control.
#' @slot Error Numeric array or list of arrays. Realised implementation error,
#'   typically derived from `Mean`, `SD`, and `Compliance` during simulation.
#' @slot Misc List. Miscellaneous additional information.
#'
#' @seealso [imp-class], [Imp()]
#'
#' @include class-unions.R
#' @keywords internal
setClass(
  "impslot",
  slots = c(
    Mean       = "num.array.list",
    SD         = "num.array.list",
    Compliance = "num.array.list",
    Error      = "num.array.list",
    Misc       = "list"
  )
)

#' Implementation Error
#'
#' The [imp-class] defines implementation error associated with management
#' advice, controlling how management recommendations (TAC, effort, and size
#' limits) are imperfectly applied in the operating model.
#'
#' @slot Name Character. Name of the implementation error model.
#' @slot TAC An [impslot-class] object. Implementation error associated with
#'   total allowable catch (TAC) recommendations.
#' @slot Effort An [impslot-class] object. Implementation error associated
#'   with effort-based controls.
#' @slot Size An [impslot-class] object. Implementation error associated with
#'   size-based regulations (e.g. minimum landing size).
#' @slot Misc List. Miscellaneous additional objects.
#'
#' @details
#' Each of the `TAC`, `Effort`, and `Size` slots is an [impslot-class] object
#' containing `Mean`, `SD`, `Compliance`, and `Error` arrays that together
#' describe how imperfectly the corresponding management control is
#' implemented across simulations.
#'
#' An [imp-class] object can be attached to an [om-class] object and
#' retrieved with `Imp(om)`.
#'
#' @seealso [Imp()], [impslot-class], [OM()], [Advice()], [ConvertImp()]
#'
#' @include class-unions.R
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


#' @rdname imp-class
#' @param object An [om-class] object, or `NULL` (default) to create a new
#'   empty [imp-class] object.
#' @return
#' - If `object` is an [om-class] object, returns `object@Imp`.
#' - Otherwise returns a new empty [imp-class] object.
#' @export
Imp <- function(object = NULL) {
  if (inherits(object, "om"))
    return(object@Imp)
  
  .Object <- methods::new("imp")
  methods::validObject(.Object)
  .Object
}

setValidity("imp", function(object) {
  # TODO: structural and dimensional checks
  TRUE
})


