#' Observation Error Accessors
#'
#' Access and replace the observation-error settings shared by the
#' observation model components: [catchobs-class], [effortobs-class],
#' [indicesobs-class], and [compobs-class]. Functions work on any S4 object
#' that contains the corresponding slot. Settings specific to one component
#' are documented with its constructor: [IndicesObs()] and [CompObs()].
#'
#' | Function | Slot of |
#' |---|---|
#' | `Bias()` | `catchobs`, `effortobs` |
#' | `CV()` | `catchobs`, `effortobs`, `indicesobs`; also the data components |
#' | `DataCV()` | `catchobs`, `effortobs`, `indicesobs` |
#' | `Error()` | `catchobs`, `effortobs`, `indicesobs`; also [impslot-class] |
#' | `AC()` | `indicesobs`; also [srr-class] and [stocktargeting-class] |
#' | `MaxGap()` | `indicesobs`, `compobs` |
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [Obs()], [CatchObs()], [EffortObs()], [IndicesObs()],
#'   [CompObs()]
#'
#' @family slot-accessors
#'
#' @examples
#' obs <- CatchObs()
#' CV(obs) <- 0.2
#' CV(obs)
#'
#' @name Obs-accessors
NULL

#' @rdname Obs-accessors
#' @export
AC <- function(x) {
  .AccessSlot(x, 'AC')
}

#' @rdname Obs-accessors
#' @export
`AC<-` <- function(x, value) {
  .AssignSlot(x, value, 'AC')
}

#' @rdname Obs-accessors
#' @export
Bias <- function(x) {
  .AccessSlot(x, 'Bias')
}

#' @rdname Obs-accessors
#' @export
`Bias<-` <- function(x, value) {
  .AssignSlot(x, value, 'Bias')
}

#' @rdname Obs-accessors
#' @export
CV <- function(x) {
  .AccessSlot(x, 'CV')
}

#' @rdname Obs-accessors
#' @export
`CV<-` <- function(x, value) {
  .AssignSlot(x, value, 'CV')
}

#' @rdname Obs-accessors
#' @export
DataCV <- function(x) {
  .AccessSlot(x, 'DataCV')
}

#' @rdname Obs-accessors
#' @export
`DataCV<-` <- function(x, value) {
  .AssignSlot(x, value, 'DataCV')
}

#' @rdname Obs-accessors
#' @export
Error <- function(x) {
  .AccessSlot(x, 'Error')
}

#' @rdname Obs-accessors
#' @export
`Error<-` <- function(x, value) {
  .AssignSlot(x, value, 'Error')
}

#' @rdname Obs-accessors
#' @export
MaxGap <- function(x) {
  .AccessSlot(x, 'MaxGap')
}

#' @rdname Obs-accessors
#' @export
`MaxGap<-` <- function(x, value) {
  if (!is.null(value)) value <- as.numeric(value)
  .AssignSlot(x, value, 'MaxGap')
}
