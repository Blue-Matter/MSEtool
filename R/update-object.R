#' Update an S4 Object and Its Slots
#'
#' Recursively updates an openMSE S4 object to ensure all slots are valid and optionally
#' replaces specified slots with new values.
#'
#' The function first calls `UpdateSlots()` to recursively rebuild any invalid
#' or corrupted slots using class defaults. It then replaces slots provided via
#' `...` with the supplied objects, provided the names match existing slot names.
#'
#' * Invalid or corrupted slots are reset to their class defaults.
#' * Nested S4 objects are updated recursively.
#' * Named arguments in `...` must correspond to slot names.
#' * Unknown slot names are ignored with a warning.
#'
#' @param object An S4 object.
#' @param ... Named objects used to replace existing slots. Names must match
#'   slot names in `object`.
#'
#' @return The updated S4 object.
#'
#' @examples
#' \dontrun{
#'   obj <- UpdateObject(obj, Fleet = newFleet)
#' }
#'
#' @export
UpdateObject <- function(object, ...) {
  object <- UpdateSlots(object)
  
  DotsList <- list(...)
  names(DotsList) <- lapply(DotsList, class) |> lapply(firstup)
  if (!length(DotsList))
    return(object)
  
  slots <- slotNames(class(object))
  UpdatedSlots <- slots[slots %in% names(DotsList)]
  Invalid <- names(DotsList)[!names(DotsList) %in% slots]
  if (length(Invalid)) {
    cli::cli_alert_warning('Note: {.val {Invalid}} are not slots in object class {.cls {class(object)}}. Ignoring.')
  }
  
  for (sl in UpdatedSlots) {
    slot(object, sl) <- DotsList[[sl]]
  }
  object
}

#' @rdname UpdateObject
#' @export
UpdateSlots <- function(object) {
  if (!isS4(object))
    return(object)
  suppressWarnings(
    slots <- slotNames(object)
  )
  
  for (sl in slots) {
    chk <- try(slot(object,sl), silent=TRUE)

    if (inherits(chk, "try-error")) {
      newobject <- new(class(object))
      slot(object, sl) <- slot(newobject, sl)
      next
    }
    
    if (isS4(chk)) {
      slot(object, sl) <- Recall(chk)
    }
    
  }
  object
}




