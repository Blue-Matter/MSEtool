
#' Display Log Warnings
#'
#' Prints any warnings stored in the `Log` slot of an S4 object to the console
#' using `cli` formatting.
#'
#' @param object An S4 object with a `Log` slot
#'
#' @return Invisibly returns `NULL`. Called for its side effect of printing
#'   warnings to the console.
#'
#' @examples
#' \dontrun{
#'   Log(my_object)
#' }
#'
#' @export
Log <- function(object) {
  
  # if (inherits(object, 'hist') || inherits(object, 'mse')) {
  #   object@Log$warning <- c(object@Log$warning, object@OM@Log$warning)
  # }
  #   

  if (!'Log' %in% slotNames(object))
    cli::cli_abort("Object of class {.val {class(object)}} does not have slot {.val Log}")
  
  if (is.null(object@Log$warning) && is.null(object@Log$warning))
    return(invisible(NULL))
  
  # warnings 
  if (!is.null(object@Log$warning)) {
    cli::cli_text('')
    cli::cli_alert_warning(cli::col_yellow("WARNINGS:"))
    nms <- names(object@Log$warning)
    for (i in seq_along(object@Log$warning)) {
      nm <- nms[i]
      if (nchar(nm)) {
        cli::cli_text('')
        cli::cli_alert_warning("{nm}")
      }
      
      cli::cli_text(object@Log$warning[[i]])
    }
  }

  # errors 
  if (!is.null(object@Log$error)) {
    cli::cli_text('')
    cli::cli_alert_warning(cli::col_red("ERRORS:"))
    nms <- names(object@Log$error)
    for (i in seq_along(object@Log$error)) {
      nm <- nms[i]
      if (nchar(nm)) {
        cli::cli_text('')
        cli::cli_alert_danger("{nm}")
      }
      
      cli::cli_text(object@Log$error[[i]])
    }
  }
  invisible(NULL)
}


CaptureLog <- function(object, string, name = '', type = 'warning') {
  if ('Log' %in% slotNames(object)) {
    object@Log[[type]] <- c(object@Log[[type]], MakeNamedList(name, string))
  } else {
    object@Misc[[type]] <- c(object@Misc[[type]], MakeNamedList(name, string))
  }
    
  object
}

CheckLog <- function(object, cls = 'Hist') {
  if (!'Log' %in% slotNames(object))
    cli::cli_abort("Object of class {.val {class(object)}} does not have slot {.val Log}")
  
  if (!length(object@Log))
    return(
      invisible(NULL)    
    )
  
  cli::cli_alert_warning("Some warning alerts in {.val Log}. Use {.var Log({cls })} to print log messages")
  
}

JoinLog <- function(Log1, Log2) {
  out <- list()
  out$warning <- c(Log1$warning, Log2$warning)
  out$error <- c(Log1$error, Log2$error)
  out
}
