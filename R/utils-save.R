#' Save an Object to Disk
#'
#' A wrapper for [saveRDS()] that automatically creates the required directory
#' structure and prints a informational message to the console.
#'
#' @param object Any R object to save.
#' @param path Character. File path including file name and extension (e.g.,
#'   `"results/my_om.rds"`). If `NULL` (default), a temporary file path is
#'   generated via [tempfile()].
#' @param overwrite Logical. If `FALSE` (default), an error is thrown if
#'   `path` already exists. Set to `TRUE` to overwrite an existing file.
#' @param ... Additional arguments passed to [saveRDS()].
#'
#' @return Invisibly returns the full file path of the saved object.
#'
#' @seealso [saveRDS()]
#'
#' @examples
#' x <- list(a = 1, b = 2)
#' path <- Save(x, path = tempfile(fileext = ".rds"))
#' path
#'
#' @export
Save <- function(object, path = NULL, overwrite = FALSE, ...) {
  if (is.null(path))
    path <- tempfile()
  
  CreateDir(dirname(path))
  
  if (file.exists(path) && !overwrite)
    cli::cli_abort(
      c("x" = "File {.file {path}} already exists.",
        "i" = "Use `overwrite = TRUE` to overwrite an existing file."),
      call = NULL
    )
  
  name <- deparse(substitute(object))
  cli::cli_alert_info(
    "Saving {.val {name}} of class {.cls {class(object)}} to {.val {path}}."
  )
  saveRDS(object, path, ...)
  invisible(path)
}

CreateDir <- function(path) {
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
