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
#'   error for size-based regulations (e.g. minimum landing size), via its
#'   `Compliance` slot only (see Details). When `NULL`, an empty
#'   [impslot-class] is created via [ImpSlot()].
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
#' ## What is consumed during simulation
#' `TAC@Mean`/`SD`/`Error` and `Effort@Mean`/`SD`/`Error` are applied as a
#' multiplicative implementation-error factor to the advised TAC/effort
#' before effort-solving (`Error` is generated from `Mean`/`SD` if not
#' supplied directly -- see [PopulateImpSlot()]). `TAC@Compliance` and
#' `Effort@Compliance` (`[Sim x Year]` within each fleet/complex's
#' `impslot`, in `[0, 1]`) govern how a
#' fleet reconciles competing TAC/effort recommendations across multiple
#' stocks or complexes -- meaningless (and not consulted) for a
#' single-stock/single-complex OM, since there is nothing to reconcile
#' against.
#'
#' For `TAC@Compliance`, a fleet that cannot satisfy every complex's TAC at
#' once must trade off falling short of one against exceeding another.
#' `Compliance` sets how heavily exceeding this complex's TAC is penalised,
#' relative to falling short of it:
#'
#' - `0`: exceeding this TAC costs nothing, so the fleet effectively ignores
#'   it and fishes to satisfy its other quotas.
#' - `0.5`: exceeding and falling short are weighted equally.
#' - `0.7` (the default when unset): exceeding is penalised about 2.3 times
#'   more heavily than falling short, so the fleet stops at whichever quota
#'   binds first rather than overshooting it to fill the others.
#' - Approaching `1`: an effective hard choke.
#'
#' The default is asymmetric because a TAC is a cap rather than a target:
#' landing under quota is routine, while exceeding it is a regulatory breach.
#' Set `0.5` explicitly if you want the symmetric treatment.
#'
#' `Size@Compliance` (in `[0, 1]`) is the fraction of the fleet that adopts a
#' newly-advised size-based regulation (a change to `Advice@Retention`
#' and/or `Advice@Selectivity`) in the year it changes; the remaining
#' `1 - Compliance` fraction continues under the prior curve. Missing/`NA`
#' defaults to `1` (full, immediate adoption). See
#' `.UpdateSelectivitySim()`. `Size@Mean`/`SD`/`Error` are not consumed.
#'
#' @param x An [om-class] object, for `Imp<-`; an [imp-class] object, for
#'   `TACImp()`/`TACImp<-`.
#' @param value For `Imp<-`: a single [imp-class] object (replicated across
#'   every complex/fleet), a flat list of [imp-class] objects of length
#'   `nFleet` (replicated across every complex), or a nested list
#'   `[[complex]][[fleet]]` of [imp-class] objects. For `TACImp<-`: an
#'   [impslot-class] object to assign to the `TAC` slot.
#'
#' @return
#' - `Imp()` returns a new [imp-class] object when `Name` is `NULL` or a
#'   character string.
#' - `Imp()` returns `Name@Imp` (an [imp-class] object) when `Name` is an
#'   [om-class] object.
#' - `Imp<-` returns `x` with the `Imp` slot replaced.
#' - `TACImp()`, `Effort()`, `Size()` return the corresponding
#'   [impslot-class] slot from an [imp-class] object `x`. (`TAC()` is not
#'   used for this purpose because it is already defined elsewhere in the
#'   package as a function for running MPs against a [data-class] object.)
#' - `TACImp<-`, `Effort<-`, `Size<-` return `x` with the named slot
#'   replaced.
#'
#' @seealso
#' - [imp-class] for the class definition.
#' - [impslot-class] and [ImpSlot()] for the sub-object constructor and
#'   slot-level accessors.
#' - [TACImp()], [Effort()], [Size()] for extracting the `TAC`, `Effort`,
#'   and `Size` sub-objects from an [imp-class] object.
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

#' @rdname Imp
#' @export
`Imp<-` <- function(x, value) {
  .CheckClass(x, "om", "x")

  OM  <- x
  Imp <- value

  Complexes    <- Complexes(OM)
  nComplex     <- length(Complexes)
  ComplexNames <- names(Complexes)

  if (is.null(ComplexNames) || nComplex < 1) {
    Complexes    <- MakeNamedList(StockNames(OM))
    for (i in seq_along(Complexes))
      Complexes[[i]] <- i
    ComplexNames <- StockNames(OM)
    nComplex     <- length(Complexes)
  }

  if (is.null(ComplexNames) || nComplex < 1)
    cli::cli_abort("Add `Stock` object(s) to `OM` first")

  FleetNames <- FleetNames(OM)
  nFleet     <- length(FleetNames)

  # validate and name a flat list of imp objects, one per fleet
  check_and_name_imp <- function(imp_list) {
    cls <- purrr::map_chr(imp_list, class)
    if (any(cls != "imp"))
      cli::cli_abort(c(
        'x' = 'All elements of `value` must be a {.help MSEtool::Imp} object',
        'i' = 'Current classes of `value` are: {.val {cls}}'
      ))

    if (length(imp_list) != nFleet)
      cli::cli_abort(c(
        'x' = 'Each complex must have exactly one `Imp` object per fleet',
        'i' = 'Expected {.val {nFleet}} fleet{?s}, got {.val {length(imp_list)}}'
      ))

    names(imp_list) <- FleetNames
    imp_list
  }

  # Case 1: single imp object — replicate across all complexes and fleets
  if (inherits(Imp, "imp")) {
    OM@Imp <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, Imp))
    return(OM)
  }

  if (inherits(Imp, "list")) {
    is_nested <- purrr::every(Imp, is.list)

    # Case 2: nested list [complex][fleet]
    if (is_nested) {
      if (length(Imp) != nComplex)
        cli::cli_abort(c(
          'x' = 'Nested `value` must have one element per complex',
          'i' = 'Expected {.val {nComplex}} complex{?es}, got {.val {length(Imp)}}'
        ))

      Imp <- purrr::map(Imp, check_and_name_imp)
      names(Imp) <- ComplexNames
      OM@Imp <- Imp
      return(OM)
    }

    # Case 3: flat list of imp objects — replicate across all complexes
    named_imp <- check_and_name_imp(Imp)
    OM@Imp <- MakeNamedList(ComplexNames, named_imp)
    return(OM)
  }

  .AssignSlot(OM, Imp, 'Imp')
}

#' @rdname Imp
#' @export
TACImp <- function(x) {
  .AccessSlot(x, 'TAC')
}

#' @rdname Imp
#' @export
`TACImp<-` <- function(x, value) {
  .AssignSlot(x, value, 'TAC')
}
