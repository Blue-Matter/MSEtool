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
#'
#' @return The `Proj` object.
#' @keywords internal
CheckMSERun <- function(Proj, MSE, MPName, StartTime, EndTime, Error, ErrorMessage,
                        silent = FALSE) {
  
  elapsed_secs <- as.numeric(round(difftime(EndTime, StartTime, units='secs'), 2))
  elapsed_auto <- format(round(difftime(EndTime, StartTime, units='auto'), 2))
  
  # MP-level error - report and return early
  if (Error) {
    cli::cli_alert_warning('{.val {MPName}} failed:')
    cli::cli_alert_danger(as.character(ErrorMessage))
    Proj@Log$error <- as.character(ErrorMessage)
    return(Proj)
  }
  
  # Check for simulation-level failures via Log
  FailedLog <- ReverseList(Proj@Log$error)

  CheckFailed <- purrr::map_int(FailedLog, length)

  nFailed <- length(CheckFailed)

  if (!nFailed) {
    # Success - include elapsed time only if run took more than 5 seconds
    if (!silent) {
      if (elapsed_secs > 5) {
        cli::cli_alert_success('{.val {MPName}} ({elapsed_auto})')
      } else {
        cli::cli_alert_success('{.val {MPName}}')
      }
    }
    return(Proj)
  }

  cli::cli_text('')
  if (nFailed == Proj@OM@nSim) {
    cli::cli_alert_danger(
      c('x'='ERROR: {.val {MPName}} failed for all simulations.
           `MSE` object not updated for this MP.')
    )
  } else {
    cli::cli_alert_warning(
      c('x'='WARNING: {.val {MPName}} failed for {.val {nFailed}}
           simulation{?s}: {.val {Failed}}.{cli::qty(nFailed)}
           `MSE` object not updated for {?this/these} simulation{?s}.')
    )
  }

  # Write log file
  LogDir  <- file.path(getwd(), 'Log')
  if (!dir.exists(LogDir))
    dir.create(LogDir)

  logFile <- file.path(LogDir, paste0(MPName, '_',
                                      format(Sys.time(), '%Y%m%d%H%M'),
                                      '.log'))
  file.create(logFile)

  for (i in seq_along(CheckFailed)) {
    cat(paste0('MP: ', MPName), file=logFile, sep='\n', append=TRUE)
    nms <- names(FailedLog[[i]])
    for (j in seq_along(nms)) {
      cat(nms[j], file=logFile, sep='\n', append=TRUE)
      msgs <- unlist(FailedLog[[i]][[j]], recursive = TRUE)
      cat(msgs, file=logFile, sep='\n', append=TRUE)
    }
  }

  cli::cli_alert_info('Writing error log to {.file {logFile}}')
  cli::cli_text('')

  Proj
}
