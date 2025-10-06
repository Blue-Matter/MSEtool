# Misc functions

#' Save an object to disk
#' 
#' A wrapper for [saveRDS()] that automatically creates the directory structyre
#' (if needed) and prints a helpful message to the console
#' 
#' @param object Any object to save to disk
#' @param path The file path (including file name and extension) for the saved 
#' object. Defaults to [tempfile()].
#' @param overwrite Logical. Overwrite the file if it already exists?
#' 
#' @return invisibly returns the full file path of the saved object
#' @export
Save <- function(object, path=NULL, overwrite=FALSE, ...) {
  if (is.null(path))
    path <- tempfile()
  
  CreateDir(dirname(path))
  
  if (file.exists(path) && !overwrite)
    cli::cli_abort(c('File {.file {path}} already exists',
                     'i'='Use `overwrite=TRUE` to overwrite existing file'))  
  
  name <- deparse(substitute(object))
  cli::cli_alert_info('Saving {.val {name}} of class {.val {class(object)}} to {.file {path}}')
  
  saveRDS(object, path, ...)
  invisible(path)
}


CreateDir <- function(path) {
  paths <- strsplit(path, '/')[[1]]
  for (i in seq_along(paths)) {
    dir <- paste0(paths[1:i], collapse = '/')
    if (i==1) {
      dir <- paste0(dir,'/')
    }
    if (!dir.exists(dir))
      dir.create(dir)
  }
}




CheckPackage <- function(pkg, pkg.path=NULL,  version=NULL) {
  chk <- requireNamespace(pkg, quietly = TRUE)
  
  if (!chk) {
    if (is.null(pkg.path)) 
      cli::cli_abort("Package {.pkg {pkg}} is required for this function. Please install it")
    
    cli::cli_abort(c("Package {.pkg {pkg}} is required for this function",
                     "i"="Install with {.var {pkg.path}}"))
  } 
  
  if (!is.null(version))
    if (packageVersion(pkg) < version) {
      if (is.null(pkg.path)) 
        cli::cli_abort(c("Version {val {version}} is required for Package {.pkg {pkg}}. Please install latest version"))
      
      cli::cli_abort(c("Version {val {version}} is required for Package {.pkg {pkg}}. Please install latest version",
                       "i"="Install with {.var {pkg.path}}"))
      
    }
}
