.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("openMSE.quiet"))) return(invisible())
  packageStartupMessage(.StartupMessage())
}

.StartupMessage <- function() {
  Header <- tryCatch(paste("openMSE", utils::packageVersion("openMSE")),
                     error = function(e) paste("MSEtool", utils::packageVersion("MSEtool")))
  Lines <- c(
    cli::rule(left = Header, width = 60),
    cli::format_inline("{.strong openMSE} is developed and maintained by {.strong Blue Matter Science}."),
    "",
    "We offer training, operating model development, MP design",
    "and evaluation, and end-to-end MSE support.",
    cli::format_inline("Contact us via {.url https://www.bluematterscience.com}"),
    "",
    cli::col_grey(cli::format_inline("Hide this message with {.code options(openMSE.quiet = TRUE)}."))
  )
  paste(Lines, collapse = "\n")
}
