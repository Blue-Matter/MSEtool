#' Performance Metric Accessors
#'
#' Access and replace the slots of [pm-class] objects.
#'
#' @param x A [pm-class] object, or another S4 object with the corresponding
#'   slot. `Mean()` also applies to [impslot-class] and
#'   [stocktargeting-class] objects, and `MPs()` to [mse-class] objects.
#' @param value The value to assign to the slot.
#'
#' @return
#' - `Stat()`, `Prob()`, and `Mean()` on a [pm-class] object convert the
#'   named `Sim x Stock x MP` (or `Stock x MP`) array to a tidy data frame
#'   via [Array2DF()]. On other objects they return the slot value.
#' - `Caption()` and `MPs()` return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [PM()], [PerformanceMetric()]
#'
#' @family slot-accessors
#'
#' @name PM-accessors
NULL

#' @rdname PM-accessors
#' @export
Caption <- function(x) {
  .AccessSlot(x, 'Caption')
}

#' @rdname PM-accessors
#' @export
`Caption<-` <- function(x, value) {
  .AssignSlot(x, value, 'Caption')
}

#' @rdname PM-accessors
#' @export
Mean <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Mean')))
  .AccessSlot(x, 'Mean')
}

#' @rdname PM-accessors
#' @export
`Mean<-` <- function(x, value) {
  .AssignSlot(x, value, 'Mean')
}

#' @rdname PM-accessors
#' @export
MPs <- function(x) {
  if (!isS4(x) && is.list(x))
    return(purrr::map(x, MPs))
  .AccessSlot(x, 'MPs')
}

#' @rdname PM-accessors
#' @export
`MPs<-` <- function(x, value) {
  .AssignSlot(x, value, 'MPs')
}

#' @rdname PM-accessors
#' @export
Prob <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Prob')))
  .AccessSlot(x, 'Prob')
}

#' @rdname PM-accessors
#' @export
`Prob<-` <- function(x, value) {
  .AssignSlot(x, value, 'Prob')
}

#' @rdname PM-accessors
#' @export
Stat <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Stat')))
  .AccessSlot(x, 'Stat')
}

#' @rdname PM-accessors
#' @export
`Stat<-` <- function(x, value) {
  .AssignSlot(x, value, 'Stat')
}
