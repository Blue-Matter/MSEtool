#' Stock and Fleet Model Component Accessors
#'
#' Access and replace the slots of the stock and fleet model components:
#' [ages-class], [length-class], [weight-class], [maturity-class],
#' [fecundity-class], [naturalmortality-class], [srr-class],
#' [selectivity-class], [retention-class], and [discardmortality-class].
#' Functions work on any S4 object that contains the corresponding slot.
#'
#' | Function | Slot of |
#' |---|---|
#' | `Pars()`, `Model()` | all model components except `ages` and `discardmortality` |
#' | `Units()` | `ages`, `length`, `weight`, `fecundity`, `naturalmortality`, `srr`; also observation and data components |
#' | `Classes()` | all model components except `srr`; also [compdata-class]; for a [stock-class] object returns `Ages@Classes` |
#' | `MeanAtAge()` | all model components except `ages` and `srr` |
#' | `MeanAtLength()` | all model components except `ages`, `length`, and `srr` |
#' | `MeanAtWeight()` | `maturity`, `selectivity`, `retention` |
#' | `CVatAge()`, `Dist()` | `length`, `weight` |
#' | `Random()` | `length`, `weight`, `naturalmortality` |
#' | `Timing()` | `length`, `weight`, `fecundity`; also [indicesdata-class] |
#' | `TruncSD()` | `length`, `weight`, `srr`; also [indicesobs-class] |
#' | `SD()` | `srr`; also [impslot-class] |
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @family slot-accessors
#'
#' @examples
#' len <- Length(Pars = list(Linf = 80, K = 0.2, t0 = -0.5))
#' Pars(len)
#' Model(len)
#' Units(len) <- "cm"
#'
#' @name Model-accessors
NULL

#' @rdname Model-accessors
#' @export
Classes <- function(x) {
  if (inherits(x, 'stock'))
    return(x@Ages@Classes)

  if (inherits(x, 'StockList'))
    return(purrr::map(x, Classes))

  if (isS4(x) && methods::.hasSlot(x, 'Classes'))
    return(x@Classes)

  cli::cli_abort(c("Argument {.arg x} must be a {.cls stock} or an object with a {.code Classes} slot",
                   "x" = "You've supplied an object of class {.cls {class(x)}}"), call = NULL)
}

#' @rdname Model-accessors
#' @export
`Classes<-` <- function(x, value) {
  .AssignSlot(x, value, 'Classes')
}

#' @rdname Model-accessors
#' @export
CVatAge <- function(x) {
  .AccessSlot(x, 'CVatAge')
}

#' @rdname Model-accessors
#' @export
`CVatAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'CVatAge')
}

#' @rdname Model-accessors
#' @export
Dist <- function(x) {
  .AccessSlot(x, 'Dist')
}

#' @rdname Model-accessors
#' @export
`Dist<-` <- function(x, value) {
  .AssignSlot(x, value, 'Dist')
}

#' @rdname Model-accessors
#' @export
MeanAtAge <- function(x) {
  .AccessSlot(x, 'MeanAtAge')
}

#' @rdname Model-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtAge')
}

#' @rdname Model-accessors
#' @export
MeanAtLength <- function(x) {
  .AccessSlot(x, 'MeanAtLength')
}

#' @rdname Model-accessors
#' @export
`MeanAtLength<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtLength')
}

#' @rdname Model-accessors
#' @export
MeanAtWeight <- function(x) {
  .AccessSlot(x, 'MeanAtWeight')
}

#' @rdname Model-accessors
#' @export
`MeanAtWeight<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtWeight')
}

#' @rdname Model-accessors
#' @export
Model <- function(x) {
  .AccessSlot(x, 'Model')
}

#' @rdname Model-accessors
#' @export
`Model<-` <- function(x, value) {
  .AssignSlot(x, value, 'Model')
}

#' @rdname Model-accessors
#' @export
Pars <- function(x) {
  .AccessSlot(x, 'Pars')
}

#' @rdname Model-accessors
#' @export
`Pars<-` <- function(x, value) {
  .AssignSlot(x, value, 'Pars')
}

#' @rdname Model-accessors
#' @export
Random <- function(x) {
  .AccessSlot(x, 'Random')
}

#' @rdname Model-accessors
#' @export
SD <- function(x) {
  .AccessSlot(x, 'SD')
}

#' @rdname Model-accessors
#' @export
`SD<-` <- function(x, value) {
  .AssignSlot(x, value, 'SD')
}

#' @rdname Model-accessors
#' @export
Timing <- function(x) {
  .AccessSlot(x, 'Timing')
}

#' @rdname Model-accessors
#' @export
TruncSD <- function(x) {
  .AccessSlot(x, 'TruncSD')
}

#' @rdname Model-accessors
#' @export
`TruncSD<-` <- function(x, value) {
  .AssignSlot(x, value, 'TruncSD')
}

#' @rdname Model-accessors
#' @export
Units <- function(x) {
  .AccessSlot(x, 'Units')
}

#' @rdname Model-accessors
#' @export
`Units<-` <- function(x, value) {
  .AssignSlot(x, value, 'Units')
}

#' @rdname Model-accessors
#' @export
`Random<-` <- function(x, value) {
  .AssignSlot(x, value, 'Random')
}

#' @rdname Model-accessors
#' @export
`Timing<-` <- function(x, value) {
  .AssignSlot(x, value, 'Timing')
}
