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
CheckMSERun <- function(Proj, MSE, MPName, StartTime, EndTime, Error, ErrorMessage) {
  
  elapsed_secs <- as.numeric(round(difftime(EndTime, StartTime, units='secs'), 2))
  elapsed_auto <- format(round(difftime(EndTime, StartTime, units='auto'), 2))
  
  # MP-level error - report and return early
  if (Error) {
    cli::cli_alert_warning('{.val {MPName}} failed:')
    cli::cli_alert_danger(as.character(ErrorMessage))
    return(Proj)
  }
  
  # Check for simulation-level failures via Log
  FailedLog  <- ReverseList(Proj@Log)
  CheckFailed <- purrr::map_lgl(FailedLog, \(sim) !is.null(unlist(sim)))
  Failed     <- which(CheckFailed)
  nFailed    <- length(Failed)
  
  if (nFailed > 0) {
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
    
    logFile <- file.path(LogDir, paste0(format(Sys.time(), '%Y%m%d%H%M'),
                                        '_', MPName, '.log'))
    file.create(logFile)
    
    for (i in Failed) {
      msgs <- unlist(FailedLog[[i]], recursive=TRUE)
      cat(msgs, file=logFile, sep='\n', append=TRUE)
    }
    
    cli::cli_alert_info('Writing error log to {.file {logFile}}')
    
  } else {
    # Success - include elapsed time only if run took more than 5 seconds
    if (elapsed_secs > 5) {
      cli::cli_alert_success('{.val {MPName}} ({elapsed_auto})')
    } else {
      cli::cli_alert_success('{.val {MPName}}')
    }
  }
  
  Proj
}
