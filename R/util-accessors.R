#' Access Slots from a Compatible Object
#'
#' Generic accessor functions to retrieve named slots from a compatible S4 object.
#'
#' @param x An S4 object with the corresponding slot.
#'
#' @return The value stored in the named slot of `x`.
#'
#' @examples
#' MyLength <- Length()
#' MeanAtAge(MyLength)
#'
#' @name AccessorFunctions
NULL


#' @rdname AccessorFunctions
#' @export
Classes <- function(x) {
  AccessSlot(x,'Classes')
}

#' @rdname AccessorFunctions
#' @export
CVatAge <- function(x) {
  AccessSlot(x,'CVatAge')
}

#' @rdname AccessorFunctions
#' @export
Dist <- function(x) {
  AccessSlot(x,'Dist')
}

#' @rdname AccessorFunctions
#' @export
MeanAtAge <- function(x) {
  AccessSlot(x,'MeanAtAge')
}

#' @rdname AccessorFunctions
#' @export
MeanAtLength <- function(x) {
  AccessSlot(x,'MeanAtLength')
}

#' @rdname AccessorFunctions
#' @export
MeanAtWeight <- function(x) {
  AccessSlot(x,'MeanAtWeight')
}


#' @rdname AccessorFunctions
#' @export
Misc <- function(x) {
  AccessSlot(x,'Misc')
}

#' @rdname AccessorFunctions
#' @export
Model <- function(x) {
  AccessSlot(x,'Model')
}

#' @rdname AccessorFunctions
#' @export
Pars <- function(x) {
  AccessSlot(x,'Pars')
}

#' @rdname AccessorFunctions
#' @export
Random <- function(x) {
  AccessSlot(x,'Random')
}

#' @rdname AccessorFunctions
#' @export
Timing <- function(x) {
  AccessSlot(x,'Timing')
}


#' @rdname AccessorFunctions
#' @export
TruncSD <- function(x) {
  AccessSlot(x,'TruncSD')
}

#' @rdname AccessorFunctions
#' @export
Units <- function(x) {
  AccessSlot(x,'Units')
}

# ---- Helpers -----

AccessSlot <- function(x, slotname) {
  CheckClass(slotname, 'character', 'slotname')
  if (!isS4(x))
    cli::cli_abort("`x` is not an S4 object")
  
  if (!slotname %in% slotNames(x))
    cli::cli_abort("Slot {.val {slotname}} is not found in object class {.val {class(x)}}")
  
  slot(x, slotname)
}