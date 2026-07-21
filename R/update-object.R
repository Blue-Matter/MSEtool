#' Update an S4 Object and Its Slots
#'
#' Recursively updates an openMSE S4 object to ensure all slots are valid, and
#' optionally replaces specified slots with new values. [UpdateSlots()] is
#' called first to reset any inaccessible or corrupted slots to their class
#' defaults, recursing into nested S4 objects. Named arguments in `...` are
#' then used to replace slots by name.
#'
#' @param object An S4 object.
#' @param ... Named objects used to replace existing slots. Names must match
#'   slot names in `object`. Unrecognised names emit a warning and are ignored.
#'
#' @return The updated S4 object with all slots valid and any supplied
#'   replacements applied.
#'
#' @examples
#' \dontrun{
#' obj <- UpdateObject(obj, Fleet=newFleet)
#' }
#'
#' @export
UpdateObject <- function(object, ...) {
  object <- UpdateSlots(object)
  
  dots <- list(...)
  if (!length(dots))
    return(object)
  
  if (is.null(names(dots)) || any(!nzchar(names(dots))))
    cli::cli_abort("All arguments in `...` must be named with the slot name to update.")
  
  slots   <- slotNames(class(object))
  valid   <- names(dots)[names(dots) %in% slots]
  invalid <- names(dots)[!names(dots) %in% slots]
  
  if (length(invalid))
    cli::cli_alert_warning(
      "{.val {invalid}} {?is/are} not slot{?s} of {.cls {class(object)}}. Ignoring."
    )
  
  for (sl in valid)
    slot(object, sl) <- dots[[sl]]
  
  object
}



#' @rdname UpdateObject
#' @export
UpdateSlots <- function(object) {
  if (is.list(object))
    return(lapply(object, UpdateSlots))
  
  if (!isS4(object))
    return(object)
  
  has_missing <- any(vapply(slotNames(object), \(sl)
                            inherits(try(slot(object, sl), silent=TRUE), 'try-error'),
                            logical(1)
  ))
  
  if (has_missing) {
    fresh <- try(new(class(object)), silent=TRUE)
    if (!inherits(fresh, 'try-error')) {
      for (sl in slotNames(fresh)) {
        current <- try(slot(object, sl), silent=TRUE)
        if (!inherits(current, 'try-error'))
          slot(fresh, sl) <- current
      }
      object <- fresh
    }
  }
  
  # Recurse into accessible slots
  for (sl in slotNames(object)) {
    current <- try(slot(object, sl), silent=TRUE)
    if (inherits(current, 'try-error')) next
    
    updated <- if (isS4(current))    UpdateSlots(current)   else
      if (is.list(current)) .UpdateList(current)   else
        current
    
    if (!identical(current, updated))
      slot(object, sl) <- updated
  }
  object
  
 
}

.AnyMissingSlots <- function(x) {
  if (is.list(x))
    return(any(vapply(x, .AnyMissingSlots, logical(1))))
  
  if (!isS4(x))
    return(FALSE)
  
  for (sl in slotNames(x)) {
    current <- try(slot(x, sl), silent=TRUE)
    if (inherits(current, 'try-error'))
      return(TRUE)
    if (isS4(current)  && .AnyMissingSlots(current)) return(TRUE)
    if (is.list(current) && .AnyMissingSlots(current)) return(TRUE)
  }
  FALSE
}

.UpdateList <- function(lst) {
  if (!is.list(lst))         return(lst)
  if (!.AnyMissingSlots(lst)) return(lst)
  
  updated        <- lapply(lst, \(x) {
    if (is.list(x)) return(.UpdateList(x))
    if (isS4(x))    return(UpdateSlots(x))
    x
  })
  names(updated) <- names(lst)
  updated
}
