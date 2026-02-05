
#' Print a `RepList` object
#'
#' Print a summary of a `RepList` object, which is a list of outputs
#' from `r4ss::SS_output`.
#'
#' @param x A `RepList` object.
#' @param ... Additional arguments (currently ignored).
#'
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.RepList <- function(x, ...) {
  n <- length(x)
  cli::cli_h2("A list of length {.val {n}} of output from `r4ss::SS_output`")
  
  if (!is.null(names(x))) {
    cli::cli_text("Elements: {.val {head(names(x), 5)}}{? ...}")
  }
  
  invisible(x)  
}

