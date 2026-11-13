#' Stock Complexes and Relations
#'
#' Access and replace the `Complexes` and `Relations` slots of an [om-class]
#' object. Also accepts [hist-class] and [mse-class] objects, extracting the
#' embedded `OM` slot.
#'
#' - `Complexes`: a named list defining the stock complexes used for data
#'   aggregation and management. Each element is a vector of the stocks in
#'   the complex. When `Complexes` is empty, [Simulate()] creates the
#'   complexes, linking stocks that share fleets.
#' - `Relations`: a list of biological or ecological relationships among
#'   stocks (e.g. predator-prey). Relationships among stocks are not
#'   modelled, and [Simulate()] records a warning in the log (see [Log()])
#'   when `Relations` is not empty.
#'
#' Both slots are used to simulate the historical dynamics and can only be
#' replaced in [om-class] objects.
#'
#' @param x An [om-class], [hist-class], or [mse-class] object.
#' @param value The value to assign to the slot.
#'
#' @return
#' - `Complexes()` and `Relations()` return the value of the slot.
#' - Replacement functions return `x` with the slot updated to `value`.
#'
#' @seealso [ComplexNames()], [nComplex()], [Herm()], [StockTargeting()]
#'
#' @family om
#'
#' @name Complexes
NULL

#' @rdname Complexes
#' @export
Complexes <- function(x) .IsHist(x, "Complexes")

#' @rdname Complexes
#' @export
`Complexes<-` <- function(x, value) .AssignOMSlot(x, value, "Complexes", "struct")

#' @rdname Complexes
#' @export
Relations <- function(x) .IsHist(x, "Relations")

#' @rdname Complexes
#' @export
`Relations<-` <- function(x, value) .AssignOMSlot(x, value, "Relations", "struct")
