
CheckMSERun <- function(Proj, MSE, MPName, StartTime, EndTime, Error, ErrorMessage) {
  
  elapse_secs <- round(difftime(time1 = EndTime, time2 = StartTime, units = "secs"),2) |> as.numeric()
  elapse_auto <- round(difftime(time1 = EndTime, time2 = StartTime, units = "auto"),2) |> format()
  incElapse <- ifelse(elapse_secs > 5, TRUE, FALSE)
  
  # TODO error checks and loggin 
  
  # ErrorCheck <- unlist(lapply(Proj, class))
   
  # Failed <- which(ErrorCheck=='try-error') |> as.numeric()
  # nFailed <- length(Failed)
  
  nFailed <- Error 
  if (incElapse) {
    if (nFailed<1) {
      cli::cli_alert_success('{.val {MPName}} ({elapse_auto})')  
    } else {
      cli::cli_alert_warning('{.val {MPName}} ({elapse_auto})')
      cli::cli_alert_danger("{as.character(ErrorMessage)}")
    }
  } else {
    if (nFailed<1) {
      cli::cli_alert_success('{.val {MPName}}')  
    } else {
      cli::cli_alert_warning('{.val {MPName}}')  
      cli::cli_alert_danger("{as.character(ErrorMessage)}")
    }
  }
  
  # if (nFailed) {
  #   if (nFailed == length(SimList_MP)) {
  #     cli::cli_alert_danger(c("x"="ERROR: {.val {MP}} failed for all simulations. `MSE` object not updated for this MP. See {.var MSE@Misc$Failed}"))
  #   } else {
  #     cli::cli_alert_warning(c("x"="WARNING: {.val {MP}} failed for {.val {nFailed}} simulation{?s}: {.val {Failed}}.{cli::qty(nFailed)} `MSE` object not updated for {?this/these} simulation{?s}. See {.var MSE@Misc$Failed}"))
  #   }
  #   time <- format(Sys.time(), "%Y%m%d%H%M")
  #   logFile <- paste0(time, "_", MP, '.log')
  #   LogDir <- file.path(getwd(), 'Log')
  #   if (!dir.exists(LogDir))
  #     dir.create(LogDir)
  #   logFile <- file.path(LogDir, logFile)
  #   file.create(logFile)
  #   for (i in Failed) {
  #     cat(paste0("\nSimulation ", i , '\n'), file=logFile, append=TRUE)
  #     cat(SimList_MP[[i]], file=logFile, append=TRUE)
  #     SimList_MP[[i]] <- ProjSimList[[i]]
  #     SimList_MP[[i]]@Misc$Failed <- i
  #   }
  #   cli::cli_alert_info("Writing error log to {.file {logFile}}")
  # }
  Proj
}
