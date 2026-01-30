
#' Validate the class of an object
#'
#' Checks whether a given object inherits from one or more specified classes.
#' Throws an informative error if the object does not match any of the allowed classes.
#'
#' @param object The object to check.
#' @param class Character vector of allowed class names. Default is `"om"`.
#' @param name Name of the object, used in error messages. Default is `"OM"`.
#' @param type Character string describing the type of check (e.g., `"Argument"`). Default is `"Argument"`.
#'
#' The function tests whether `object` inherits from any class listed in `class`.
#'
#' @return
#' Invisibly returns the input `object` if it passes the class check.
#'
#' @keywords internal
CheckClass <- function(object, class='om', name='OM', type='Argument') {
  
  checkClass <- sapply(class, function(i) inherits(object, i))
  if (all(!checkClass)) {
    cli::cli_abort(c('{type} {.var {name}} must be class {.cls {class}}',
                     "x" = "You've supplied an object of class {.cls {class(object)}}"), call=NULL)
  }
  invisible(object)
}