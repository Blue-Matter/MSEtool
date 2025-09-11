
#' Delete the Log Directory
#' 
#' @param Dir Directory where the logs are kept.
#' 
#' @return Nothing
#' @export
DeleteLogs <- function(Dir=file.path(getwd(),'Log')) {
  cli::cli_inform('Deleting all log files in: {.val {Dir}}')
  unlink(Dir)
}