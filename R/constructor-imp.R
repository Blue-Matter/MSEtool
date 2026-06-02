#' Imp Constructor and Accessor
#'
#' Construct an [imp-class] object defining implementation error for TAC,
#' effort, and size-based management controls, or extract the `Imp` slot from
#' an enclosing object.
#'
#' @param Name `character(1)` or an S4 object. Unique identifier for this
#'   implementation error model. Default `NULL`.
#'
#'   If `Name` is an [om-class] object, `Imp()` returns `Name@Imp` rather than
#'   constructing a new object. See *Pass-Through Access* in Details.
#' @param TAC An [impslot-class] object, or `NULL` (default). Implementation
#'   error for total allowable catch recommendations. When `NULL`, an empty
#'   [impslot-class] is created via [ImpSlot()].
#' @param Effort An [impslot-class] object, or `NULL` (default). Implementation
#'   error for effort-based controls. When `NULL`, an empty [impslot-class] is
#'   created via [ImpSlot()].
#' @param Size An [impslot-class] object, or `NULL` (default). Implementation
#'   error for size-based regulations (e.g. minimum landing size). When `NULL`,
#'   an empty [impslot-class] is created via [ImpSlot()].
#' @param Misc List. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Pass-Through Access
#' When `Name` is an [om-class] object, `Imp()` acts as an accessor and
#' returns `Name@Imp` directly. This allows a consistent interface for both
#' construction and retrieval:
#'
#' ```r
#' imp_obj <- Imp(om)          # extract from OM
#' Imp(om) <- Imp("MyImp")    # assign to OM
#' ```
#'
#' ## Sub-object initialisation
#' When any of `TAC`, `Effort`, or `Size` is `NULL`, an empty [impslot-class]
#' object is created automatically so that the returned [imp-class] object is
#' always fully populated and valid.
#'
#' ## Placeholder status
#' The [imp-class] is currently a placeholder. The class interface is subject to change.
#'
#' @return
#' - `Imp()` returns a new [imp-class] object when `Name` is `NULL` or a
#'   character string.
#' - `Imp()` returns `Name@Imp` (an [imp-class] object) when `Name` is an
#'   [om-class] object.
#' - `Imp<-` returns `x` with the `Imp` slot replaced.
#' - `TAC()`, `EffortImp()`, `SizeImp()` return the corresponding
#'   [impslot-class] slot from an [imp-class] object `x`.
#' - `TAC<-`, `EffortImp<-`, `SizeImp<-` return `x` with the named slot
#'   replaced.
#'
#' @seealso
#' - [imp-class] for the class definition.
#' - [impslot-class] and [ImpSlot()] for the sub-object constructor and
#'   slot-level accessors.
#' - [OM()] for the operating model constructor.
#' - [Advice()] for the advice object connected to implementation.
#' - [ConvertImp()] for converting legacy implementation objects.
#'
#' @family imp
#'
#'
#' @name Imp
#' @export
Imp <- function(Name   = NULL,
                TAC    = NULL,
                Effort = NULL,
                Size   = NULL,
                Misc   = list()) {
  if (inherits(Name, "om"))
    return(Name@Imp)
  
  .Object <- methods::new("imp")
  if (!is.null(Name))   .Object@Name   <- Name
  .Object@TAC    <- if (!is.null(TAC))    TAC    else ImpSlot()
  .Object@Effort <- if (!is.null(Effort)) Effort else ImpSlot()
  .Object@Size   <- if (!is.null(Size))   Size   else ImpSlot()
  .Object@Misc   <- Misc
  
  methods::validObject(.Object)
  .Object
}