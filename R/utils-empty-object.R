#' Test Whether an Object is Empty
#'
#' Returns `TRUE` if an object contains no user-supplied information — i.e. it
#' is either a freshly constructed default object or all of its slots are
#' `NULL`, zero-length, or entirely `NA`.
#'
#' @param object An S4 object, or any R object.
#'
#' For S4 objects, `EmptyObject()` first checks whether the object is
#' identical to a freshly constructed default of the same class via
#' [isNewObject()]. If so, it returns `TRUE` immediately.
#'
#' Otherwise it inspects every slot recursively:
#'
#' * Slots containing functions are skipped.
#' * Slots containing nested S4 objects are tested recursively via
#'   `EmptyObject()`.
#' * All other slots are considered empty if they are `NULL`, zero-length, or
#'   entirely `NA`.
#'
#' `EmptyObject()` returns `TRUE` only when every slot passes the emptiness
#' check. A single non-empty slot causes the object to be considered
#' non-empty.
#'
#' For non-S4 objects, `EmptyObject()` returns `TRUE` if the object has
#' length zero or consists entirely of `NA` values.
#'
#' `EmptyObject()` is used throughout the `Populate*()` functions as an early
#' exit guard: if an object is empty, population is skipped and the object is
#' returned unchanged.
#'
#' @return A length-1 logical.
#'
#' @seealso [isNewObject()]
#'
#' @examples
#' # A freshly constructed object is empty
#' EmptyObject(CompObs())
#'
#' # An object with any slot filled is not empty
#' EmptyObject(CompObs(SampleSize = 200))
#'
#' # Non-S4 objects
#' EmptyObject(NULL)
#' EmptyObject(numeric(0))
#' EmptyObject(c(NA, NA))
#'
#' @export
EmptyObject <- function(object) {
  if (isS4(object)) {
    if (isNewObject(object))
      return(TRUE)
    
    sltnms <- slotNames(object)
    empty  <- rep(TRUE, length(sltnms))
    
    for (i in seq_along(sltnms)) {
      sl  <- sltnms[i]
      val <- slot(object, sl)
      
      if (inherits(val, "function"))
        next
      
      if (isS4(val)) {
        empty[i] <- Recall(val)
      } else {
        empty[i] <- is.null(val) || length(val) == 0 || all(is.na(val))
      }
    }
    
    return(as.logical(prod(empty)))
  }
  
  as.logical(length(object) < 1 || all(is.na(object)))
}


#' Test Whether an Object is a Default-Constructed Instance
#'
#' Returns `TRUE` if `object` is identical to a freshly constructed default
#' instance of its class — i.e. the object has not been modified since
#' construction. Used internally by [EmptyObject()].
#'
#' @param object An S4 object.
#'
#' `isNewObject()` constructs a default instance of the same class as
#' `object` and compares the two with [identical()]. Before comparing, the
#' `Name` slot (if present) is set to `NULL` on `object` so that a user-
#' supplied name alone does not cause the object to be considered non-default.
#'
#' The default instance is obtained as follows:
#'
#' * For a small set of classes whose constructors do not follow the standard
#'   `firstup(class)()` naming convention (`naturalmortality`, `srr`, `om`,
#'   `discardmortality`), the appropriate constructor is called directly.
#' * For all other classes, the constructor is looked up as
#'   `get(firstup(class))`. If no such constructor exists, `new(class)` is
#'   used as a fallback.
#'
#' @return A length-1 logical. Returns `FALSE` for non-S4 objects.
#'
#' @seealso [EmptyObject()]
#'
#' @export
isNewObject <- function(object) {
  if (!isS4(object))
    return(FALSE)
  
  cl <- class(object)
  
  # Classes whose constructors don't follow the standard firstup(class)()
  newobj <- if (inherits(object, "naturalmortality")) {
    NaturalMortality()
  } else if (inherits(object, "srr")) {
    SRR()
  } else if (inherits(object, "om")) {
    OM()
  } else if (inherits(object, "discardmortality")) {
    DiscardMortality()
  } else {
    chk <- try(get(firstup(cl)), silent = TRUE)
    if (inherits(chk, "try-error")) {
      new(cl)
    } else {
      get(firstup(cl))()
    }
  }
  
  if ("Name" %in% slotNames(object))
    object@Name <- NULL
  
  identical(object, newobj)
}