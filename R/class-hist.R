methods::setClassUnion(
  name    = "list.data",
  members = c("list", "data")
)

#' `Hist` Object
#'
#' The `hist` class stores historical time-series generated from an
#' [OM()] object. It contains realized population dynamics, exploitation,
#' reference quantities, and associated data used in the forward projections.
#'
#'
#' @slot OM The originating [OM()] object used to generate the historical
#'   time-series.
#'
#' @slot Unfished An object containing unfished equilibrium and
#'   dynamic reference trajectories.
#'
#' @slot Reference A `reference` object containing reference-point quantities
#'   calculated from historical dynamics.
#'
#' @slot Data A [Data()] object or list of such objects containing historical
#'   observations (catch, indices, compositions, etc.).
#'
#' @slot Log Internal list used to store diagnostics and bookkeeping
#'   information generated during historical simulation.
#'
#' @slot Misc A named list of additional objects carried with the historical
#'   results.
#'
#'
#' @include class-unions.R
#' @include class-data.R
#' @include class-om.R
#' @include class-internal.R
#' @include class-reference.R
#'
#' @export
setClass(
  "hist",
  slots = c(
    OM        = "om",
    Unfished  = "unfished",
    Reference = "reference",
    Data      = "list.data",
    Log       = "list",
    Misc      = "list"
  ),
  contains = "timeseries"
)


setValidity("hist", function(object) {
  # TODO: structural checks on dimensions / consistency
  TRUE
})


#' Create or Access a `Hist` Object
#'
#' The `Hist()` constructor creates a new historical simulation object or,
#' when applied to an [OM()] object, returns the historical results stored
#' within that object.
#'
#' @param OM A [OM()] object. If missing, an empty `hist` object is returned.
#' @param ... Additional arguments passed to the internal historical
#'   simulation routine.
#'
#' @return A `hist` object.
#'
#' @seealso [OM()], [Data()]
#'
#' @rdname Hist
#' @export
Hist <- function(OM = NULL, ...) {
  
  if (is.null(OM)) {
    return(methods::new("hist"))
  }
  
  if (!methods::is(OM, "om")) {
    cli::cli_abort("`OM` must be an object of class `om`")
  }
  
  OM2Hist(OM, ...)
}
