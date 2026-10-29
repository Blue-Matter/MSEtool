
# custom inline classes: {.om}, {.mp}, {.stock}, {.fleet}, {.complex}

.MSEtoolTheme <- function() {
  list(
    span.fn      = list(color = "cyan"),
    span.om      = list(color = "blue", "font-weight" = "bold"),
    span.mp      = list(color = "magenta"),
    span.stock   = list(color = "green"),
    span.fleet   = list(color = "green"),
    span.complex = list(color = "green")
  )
}

# applies the theme to all cli output (incl. progress bars) until `.envir` exits
.MsgTheme <- function(.envir = parent.frame()) {
  cli::cli_div(theme = .MSEtoolTheme(), .envir = .envir)
}

.MsgFormat <- function(msg, .envir = parent.frame()) {
  cli::cli_div(theme = .MSEtoolTheme())
  cli::format_inline(msg, .envir = .envir)
}

# formatted text passed back through a cli glue template must not be re-interpolated
.MsgEscape <- function(x) gsub("([{}])", "\\1\\1", x)

.MsgShowProgress <- function(silent = FALSE) {
  !isTRUE(silent) && cli::is_dynamic_tty()
}

.FormatElapsed <- function(secs) {
  secs <- as.numeric(secs)
  if (secs < 1)
    return(paste0(round(secs * 1000), "ms"))
  if (secs < 60)
    return(paste0(format(round(secs, 1), nsmall = 1), "s"))
  if (secs < 3600)
    return(sprintf("%dm %ds", as.integer(secs %/% 60), as.integer(secs %% 60)))
  sprintf("%dh %dm", as.integer(secs %/% 3600), as.integer((secs %% 3600) %/% 60))
}

.MsgElapsed <- function(StartTime) {
  .FormatElapsed(difftime(Sys.time(), StartTime, units = "secs"))
}

.MsgCountFleets <- function(OM) {
  fleet <- OM@Fleet
  if (is.null(fleet)) return(0L)
  if (isS4(fleet)) return(1L)
  if (is.list(fleet[[1]])) return(length(fleet[[1]]))
  length(fleet)
}

.MsgCountStocks <- function(OM) {
  if (isS4(OM@Stock)) 1L else length(OM@Stock)
}

.MsgStart <- function(fn, OM, nSim = NULL, silent = FALSE, nMP = NULL, parallel = FALSE) {
  if (silent) return(invisible(NULL))
  nSim   <- min(nSim %||% OM@nSim, OM@nSim)
  nStock <- .MsgCountStocks(OM)
  nFleet <- .MsgCountFleets(OM)
  name   <- if (length(OM@Name) && nchar(OM@Name) >= 2) OM@Name else 'Unnamed OM'
  parts  <- c("{.fn {fn}}", "OM {.om {name}}", "{nSim} sim{?s}")
  if (nStock > 1 || nFleet > 1)
    parts <- c(parts, paste("{nStock} stock{?s}", intToUtf8(215), "{nFleet} fleet{?s}"))
  if (!is.null(nMP))
    parts <- c(parts, paste0("{nMP} MP{?s}", if (parallel) " in parallel"))
  left <- .MsgFormat(paste(parts, collapse = paste0(" ", intToUtf8(183), " ")))
  cli::cli_rule(left = .MsgEscape(left))
  invisible(NULL)
}

.MsgDone <- function(fn, StartTime, silent = FALSE) {
  if (silent) return(invisible(NULL))
  done <- .MsgFormat("{.fn {fn}} completed")
  el   <- .MsgElapsed(StartTime)
  cli::cli_alert_success("{done} {.timestamp {el}}")
  invisible(NULL)
}

.MsgAlert <- function(msg, type = c('info', 'success', 'warning', 'danger'),
                      silent = FALSE, .envir = parent.frame()) {
  if (silent) return(invisible(NULL))
  type <- match.arg(type)
  m <- .MsgFormat(msg, .envir)
  switch(type,
         info    = cli::cli_alert_info("{m}"),
         success = cli::cli_alert_success("{m}"),
         warning = cli::cli_alert_warning("{m}"),
         danger  = cli::cli_alert_danger("{m}"))
  invisible(NULL)
}

# dynamic-only status line; auto-cleared when `.envir` exits
.MsgStatus <- function(msg, silent = FALSE, .envir = parent.frame()) {
  if (!.MsgShowProgress(silent)) return(invisible(NULL))
  m <- .MsgEscape(.MsgFormat(msg, .envir))
  cli::cli_progress_message(m, current = FALSE, .envir = .envir)
}

# one success/failure line when `.envir` exits (or at `.MsgStepDone()`); `msg` is a dynamic-only status line
.MsgStep <- function(msg, done, silent = FALSE, failed = NULL, .envir = parent.frame()) {
  step <- new.env(parent = emptyenv())
  step$active <- !isTRUE(silent)
  if (!step$active) return(invisible(step))

  step$start  <- Sys.time()
  step$done   <- .MsgFormat(done, .envir)
  step$failed <- .MsgFormat(failed %||% paste(msg, "failed"), .envir)
  step$id     <- NULL
  if (cli::is_dynamic_tty())
    step$id <- cli::cli_progress_message(.MsgEscape(.MsgFormat(msg, .envir)),
                                         current = FALSE, .auto_close = FALSE)

  sentinel <- new.env(parent = emptyenv())
  exit <- bquote(.MsgStepDone(.(step), ok = !identical(returnValue(.(sentinel)), .(sentinel))))
  do.call(base::on.exit, list(exit, add = TRUE), envir = .envir)
  invisible(step)
}

.MsgStepDone <- function(step, ok = TRUE) {
  if (!isTRUE(step$active)) return(invisible(NULL))
  step$active <- FALSE
  if (!is.null(step$id))
    cli::cli_progress_done(id = step$id, result = "clear")
  el <- .MsgElapsed(step$start)
  if (ok) {
    cli::cli_alert_success("{step$done} {.timestamp {el}}")
  } else {
    cli::cli_alert_danger("{step$failed} {.timestamp {el}}")
  }
  invisible(NULL)
}
