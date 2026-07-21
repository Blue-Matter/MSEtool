#' Implementation Error Slot
#'
#' Internal class representing implementation uncertainty for a single
#' management control (TAC, effort, or size regulations) within an
#' [imp-class] object.
#'
#' @slot Mean Numeric array or list of arrays. Mean implemented fraction of
#'   the management recommendation. See [ImpSlot()].
#' @slot SD Numeric array or list of arrays. Standard deviation of
#'   implementation error around `Mean`. See [ImpSlot()].
#' @slot Compliance Numeric array or list of arrays, populated to `[Sim x
#'   Year]` by [PopulateImpSlot()]. Meaning is context-dependent on which
#'   [imp-class] control this `impslot` belongs to: for `TAC`/`Effort`,
#'   governs multi-stock reconciliation; for `Size`, is the fraction of the fleet
#'   adopting a newly-advised size regulation (see [Imp()]). See
#'   [ImpSlot()].
#' @slot Error Numeric array or list of arrays. Realised implementation error,
#'   typically derived from `Mean`, `SD`, and `Compliance` during simulation.
#'   See [ImpSlot()].
#' @slot Misc List. Miscellaneous additional objects.
#'
#' @details
#' `impslot` objects are not intended to be constructed directly via
#' [methods::new()]. Use [ImpSlot()] to create a new object, and
#' attach it to an [imp-class] object via [TACImp()], [Effort()], or
#' [Size()].
#'
#' @seealso
#' - [ImpSlot()] for the constructor and slot-level accessors.
#' - [imp-class] for the enclosing implementation error object.
#' - [Imp()] for the top-level constructor and accessor.
#'
#' @family imp
#' 
#' @include class-unions.R
#' @keywords internal
#' @aliases impslot-class
#' @name impslot-class
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
