#' Check That an Object Inherits the Required Class
#'
#' Validates that `object` inherits from `class` and throws an informative
#' error if not, directing the user to the relevant help page. Returns `NULL`
#' invisibly on success.
#'
#' @param object The object to check.
#' @param class Character string. The required S4 class name
#'   (e.g. `"length"`, `"weight"`).
#' @param argName Character string. The argument name used in the error
#'   message. If `NULL` (default), derived from `class` via `firstup`.
#'
#' @return `NULL` invisibly if `object` inherits from `class`. Otherwise
#'   throws an error via [cli::cli_abort()].
#' @keywords internal
CheckRequiredObject <- function(object, class, argName=NULL) {
  if (methods::is(object, class))
    return(invisible(NULL))
  
  if (is.null(argName))
    argName <- firstup(class)
  
  obj <- paste0('MSEtool::', argName)
  cli::cli_abort(c(
    "{.arg {argName}} must be a {.help {obj}} object.",
    "i" = "Provide a {.cls {class}} object to the {.arg {argName}} argument."
  ))
}

