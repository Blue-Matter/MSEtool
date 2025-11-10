#' Update an S4 object 
#' 
#' @export
UpdateObject <- function(object, ...) {
  object <- UpdateSlots(object)
  
  DotsList <- list(...)
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

UpdateSlots <- function(object) {
  if (!isS4(object))
    return(object)
  slots <- slotNames(object)
  for (sl in slots) {
    chk <- try(slot(object,sl), silent=TRUE)
    if (inherits(chk, 'try-error')) {
      newobject <- new(class(object))
      slot(object,sl) <- slot(newobject,sl)
    }
  }
  object
}




