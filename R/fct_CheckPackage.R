#' Check that a required package is installed
#'
#' Checks if a required package and (if applicable) package version are installed
#' and if not, provides an error with installation instructions
#'
#' @param pkg Character string with the name of the package
#' @param version Optional. Character string specifying required version number
#' @param pkg.path Optional. Character string specifying the install command. See `Examples`
#' @export
#' @examples
#' # Not installed
#' \dontrun{
#' CheckPackage("MadeUp")
#' }
#'
#' # Already installed
#' CheckPackage("MSEtool")
#'
#' # Needs updating
#' CheckPackage("MSEtool", "99")
#'
#' # Update and specify installation path
#' \dontrun{
#' CheckPackage("MSEtool", "99", "pak::pgk_install('blue-matter/MSEtool')")
#' }
#'
CheckPackage <- function(pkg, version = NULL, pkg.path = NULL) {
  PackageInstalled <- requireNamespace(pkg, quietly = TRUE)
  
  if (!PackageInstalled) {
    cli::cli_abort(c(
      "x" = "Package {.pkg {pkg}} is required for this function",
      "i" = MessageInstallPackage(pkg, pkg.path)
    ), call = NULL)
  }
  
  CorrectVersion <- CheckPackageVersion(pkg, version)
  if (PackageInstalled & CorrectVersion) {
    return(TRUE)
  }

  if (PackageInstalled & !CorrectVersion) {
    MessageUpdatePackage(pkg, version, pkg.path)
  }
}

CheckPackageVersion <- function(pkg, version = NULL) {
  if (is.null(version) || packageVersion(pkg) >= version) {
    return(TRUE)
  }
  FALSE
}

MessageInstallPackage <- function(pkg, pkg.path = NULL) {
  if (is.null(pkg.path)) {
    return("Please install it with {.code install.packages('{pkg}')}")
  }
  "Please install it with {.code {pkg.path}}"
}

MessageUpdatePackage <- function(pkg, version = NULL, pkg.path = NULL) {
  curent_version <- packageVersion(pkg)
  cli::cli_abort(c(
    "x" = "Package {.pkg {pkg}} in installed but version {.val {version}+} is required",
    "i" = MessageInstallPackage(pkg, pkg.path)
  ), call = NULL)
}
