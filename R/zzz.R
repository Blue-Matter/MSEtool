.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("openMSE.quiet"))) return(invisible())
  packageStartupMessage(.StartupMessage())
}

.StartupMessage <- function() {
  Lines <- c(
    cli::format_inline("{cli::symbol$info} {.strong openMSE} is developed and maintained by {.strong Blue Matter Science}."),
    "  We provide training, operating model development, management procedure",
    "  design and evaluation, and end-to-end support for MSE processes.",
    paste0("  ", cli::format_inline("More information and contact details: {.url https://www.bluematterscience.com}")),
    paste0("  ", cli::col_grey(cli::format_inline("Hide this message with {.code options(openMSE.quiet = TRUE)}.")))
  )
  paste(Lines, collapse = "\n")
}
