#' Check MSE Run Status and Log Failures
#'
#' Checks whether an MP run succeeded or failed, reports results to the
#' console, and writes a log file for any simulation-level failures.
#'
#' @param Proj A `Hist` object .
#' @param MSE An MSE object (currently unused, reserved for future use).
#' @param MPName Character. Name of the management procedure.
#' @param StartTime A `POSIXct` timestamp from before the MP was run.
#' @param EndTime A `POSIXct` timestamp from after the MP was run.
#' @param Error Logical. `TRUE` if the MP threw an error.
#' @param ErrorMessage Character or condition. The error message if `Error`
#'   is `TRUE`.
#' @param silent Logical. Suppress the success message? Failures are always
#'   reported.
#' @param mp Integer. Index of the MP.
#' @param nMP Integer. Total number of MPs being projected.
#'
#' @return A list with `Proj` (the `Proj` object) and `AllFailed` (logical -
#'   `TRUE` if every simulation logged a failure at some point during the
#'   run, meaning the `MSE` object should not be updated for this MP).
#' @keywords internal
.CheckMSERun <- function(Proj, MSE, MPName, StartTime, EndTime, Error, ErrorMessage,
                        silent = FALSE, mp = 1, nMP = 1) {

  el    <- .FormatElapsed(difftime(EndTime, StartTime, units = 'secs'))
  label <- .MsgFormat("[{mp}/{nMP}] {.mp {MPName}}")
  ErrorEntries <- Proj@Log$error

  # MP-level error - report and return early
  if (Error) {
    msg <- as.character(ErrorMessage)
    cli::cli_alert_danger("{label} {msg} {.timestamp {el}}")
    .MsgErrorDetails(ErrorEntries)
    return(list(Proj = Proj, AllFailed = TRUE))
  }

  # Check for simulation-level failures via Log - entries are tagged with
  # `sim` directly, so the failed sims are whichever appear here at all
  SimsFailed <- ErrorEntries |>
    purrr::map('sim') |>
    purrr::compact() |>
    unlist() |>
    unique() |>
    sort()

  nFailed <- length(SimsFailed)

  if (!nFailed) {
    if (!silent)
      cli::cli_alert_success("{label} {.timestamp {el}}")
    return(list(Proj = Proj, AllFailed = FALSE))
  }

  nSim      <- Proj@OM@nSim
  AllFailed <- nFailed == nSim
  sims      <- .CompressRange(SimsFailed)

  if (AllFailed) {
    cli::cli_alert_danger("{label} failed for all simulations; {.cls mse} not updated for this MP {.timestamp {el}}")
  } else {
    cli::cli_alert_warning("{label} failed for {nFailed} of {nSim} simulation{?s} ({sims}); {.cls mse} not updated for {cli::qty(nFailed)}{?this/these} simulation{?s} {.timestamp {el}}")
  }
  .MsgErrorDetails(ErrorEntries)

  # Write log file
  LogDir  <- file.path(getwd(), 'Log')
  if (!dir.exists(LogDir))
    dir.create(LogDir)

  logFile <- file.path(LogDir, paste0(MPName, '_',
                                      format(Sys.time(), '%Y%m%d%H%M'),
                                      '.log'))
  file.create(logFile)

  cat(paste0('MP: ', MPName), file=logFile, sep='\n', append=TRUE)
  for (sim in SimsFailed) {
    cat(paste0('Simulation: ', sim), file=logFile, sep='\n', append=TRUE)
    SimEntries <- Filter(\(e) identical(e$sim, sim), ErrorEntries)
    for (entry in SimEntries) {
      label <- paste(Filter(nzchar, c(
        if (!is.null(entry$year)) paste0('Year: ', entry$year),
        entry$name
      )), collapse = ' | ')
      if (nzchar(label))
        cat(label, file=logFile, sep='\n', append=TRUE)
      cat(entry$message, file=logFile, sep='\n', append=TRUE)
    }
  }

  .MsgAlert('Error log written to {.file {logFile}}')

  list(Proj = Proj, AllFailed = AllFailed)
}

# distinct error messages as indented bullets under an MP failure line
.MsgErrorDetails <- function(ErrorEntries, max = 3) {
  msgs <- unique(vapply(ErrorEntries, \(e) gsub("\\s*\n\\s*", " ", trimws(.LogEntryMessage(e))),
                        character(1)))
  if (!length(msgs)) return(invisible(NULL))
  shown <- utils::head(msgs, max)
  bullets <- stats::setNames(.MsgEscape(shown), rep(" ", length(shown)))
  if (length(msgs) > max)
    bullets <- c(bullets, " " = paste0("... and ", length(msgs) - max, " more"))
  cli::cli_bullets(bullets)
  invisible(NULL)
}
