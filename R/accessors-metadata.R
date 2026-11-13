#' Object Metadata
#'
#' Access and replace the descriptive metadata slots of `openMSE` objects:
#' names, species, authorship, location, and miscellaneous information.
#'
#' | Function | Slot of |
#' |---|---|
#' | `Name()` | [om-class], [stock-class], [fleet-class], [obs-class], [imp-class], [data-class], [pm-class], and the data components (e.g. [catchdata-class]) |
#' | `CommonName()`, `Species()` | [stock-class], [data-class] |
#' | `Agency()`, `Author()`, `Email()`, `Region()`, `Latitude()`, `Longitude()` | [om-class], [data-class] |
#' | `Sponsor()`, `Source()` | [om-class] |
#' | `Misc()` | most objects |
#'
#' All functions except `Misc()` act on the embedded `OM` of a [hist-class]
#' or [mse-class] object. `Misc()` returns or replaces the `Misc` slot of the
#' object itself.
#'
#' All functions accept a list of objects (e.g. a list of [data-class]
#' objects, one per stock, or a list of [stock-class] objects), and return a
#' list with the slot of each.
#'
#' @param x An S4 object with the corresponding slot, a [hist-class] or
#'   [mse-class] object, or a list of these objects.
#' @param value The value to assign to the slot. For a list `x`, a list with
#'   one value per element of `x`, or a single non-list value assigned to
#'   every element.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`, or
#'   a list of values when `x` is a list.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [Names] for the names of the stocks, fleets, indices, complexes,
#'   and management procedures in an object.
#'
#' @family slot-accessors
#'
#' @examples
#' Name(SingleStockOM)
#' Agency(SingleStockOM) <- "My Agency"
#' Agency(SingleStockOM)
#'
#' @name Metadata
NULL

#' @rdname Metadata
#' @export
Name <- function(x) .AccessMetaSlot(x, "Name", OM = TRUE)

#' @rdname Metadata
#' @export
`Name<-` <- function(x, value) .AssignMetaSlot(x, value, "Name", OM = TRUE)

#' @rdname Metadata
#' @export
CommonName <- function(x) .AccessMetaSlot(x, "CommonName", OM = TRUE)

#' @rdname Metadata
#' @export
`CommonName<-` <- function(x, value) .AssignMetaSlot(x, value, "CommonName", OM = TRUE)

#' @rdname Metadata
#' @export
Species <- function(x) .AccessMetaSlot(x, "Species", OM = TRUE)

#' @rdname Metadata
#' @export
`Species<-` <- function(x, value) .AssignMetaSlot(x, value, "Species", OM = TRUE)

#' @rdname Metadata
#' @export
Agency <- function(x) .AccessMetaSlot(x, "Agency", OM = TRUE)

#' @rdname Metadata
#' @export
`Agency<-` <- function(x, value) .AssignMetaSlot(x, value, "Agency", OM = TRUE)

#' @rdname Metadata
#' @export
Author <- function(x) .AccessMetaSlot(x, "Author", OM = TRUE)

#' @rdname Metadata
#' @export
`Author<-` <- function(x, value) .AssignMetaSlot(x, value, "Author", OM = TRUE)

#' @rdname Metadata
#' @export
Email <- function(x) .AccessMetaSlot(x, "Email", OM = TRUE)

#' @rdname Metadata
#' @export
`Email<-` <- function(x, value) .AssignMetaSlot(x, value, "Email", OM = TRUE)

#' @rdname Metadata
#' @export
Region <- function(x) .AccessMetaSlot(x, "Region", OM = TRUE)

#' @rdname Metadata
#' @export
`Region<-` <- function(x, value) .AssignMetaSlot(x, value, "Region", OM = TRUE)

#' @rdname Metadata
#' @export
Latitude <- function(x) .AccessMetaSlot(x, "Latitude", OM = TRUE)

#' @rdname Metadata
#' @export
`Latitude<-` <- function(x, value) .AssignMetaSlot(x, value, "Latitude", OM = TRUE)

#' @rdname Metadata
#' @export
Longitude <- function(x) .AccessMetaSlot(x, "Longitude", OM = TRUE)

#' @rdname Metadata
#' @export
`Longitude<-` <- function(x, value) .AssignMetaSlot(x, value, "Longitude", OM = TRUE)

#' @rdname Metadata
#' @export
Sponsor <- function(x) .AccessMetaSlot(x, "Sponsor", OM = TRUE)

#' @rdname Metadata
#' @export
`Sponsor<-` <- function(x, value) .AssignMetaSlot(x, value, "Sponsor", OM = TRUE)

#' @rdname Metadata
#' @export
Source <- function(x) .AccessMetaSlot(x, "Source", OM = TRUE)

#' @rdname Metadata
#' @export
`Source<-` <- function(x, value) .AssignMetaSlot(x, value, "Source", OM = TRUE)

#' @rdname Metadata
#' @export
Misc <- function(x) .AccessMetaSlot(x, "Misc", OM = FALSE)

#' @rdname Metadata
#' @export
`Misc<-` <- function(x, value) .AssignMetaSlot(x, value, "Misc", OM = FALSE)

.AssignMetaSlot <- function(x, value, SlotName, OM = TRUE) {
  if (OM && inherits(x, c("hist", "mse"))) {
    x@OM <- .AssignMetaSlot(x@OM, value, SlotName)
    return(x)
  }
  if (is.list(x) && !isS4(x)) {
    if (!is.list(value))
      value <- rep(list(value), length(x))
    if (length(value) != length(x))
      cli::cli_abort(c("x" = "{.arg value} must be a list of length {length(x)} or a single value",
                       "i" = "{.arg value} has length {length(value)}"), call = NULL)
    return(purrr::map2(x, value, \(xi, vi) .AssignMetaSlot(xi, vi, SlotName, OM)))
  }
  .AssignSlot(x, value, SlotName)
}

.AccessMetaSlot <- function(x, SlotName, OM = TRUE) {
  if (OM && inherits(x, c("hist", "mse")))
    x <- x@OM
  if (is.list(x) && !isS4(x))
    return(purrr::map(x, \(xi) .AccessMetaSlot(xi, SlotName, OM)))
  .AccessSlot(x, SlotName)
}
