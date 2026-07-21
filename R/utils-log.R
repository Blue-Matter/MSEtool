
#' Display Log Warnings
#'
#' Prints any messages stored in the `Log` slot of an S4 object to the
#' console using `cli` formatting, grouped into assumption, warning, and
#' error sections.
#'
#' @param object An S4 object with a `Log` slot
#' @param type Character vector. Restrict the printed sections to one or
#'   more of `'assumption'`, `'warning'`, `'error'`. Default `NULL` prints
#'   all sections.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of printing
#'   warnings to the console.
#'
#' @examples
#' \dontrun{
#'   Log(my_object)
#'   Log(my_object, type = 'error')
#' }
#'
#' @export
Log <- function(object, type = NULL) {

  if (!'Log' %in% slotNames(object))
    cli::cli_abort("Object of class {.val {class(object)}} does not have slot {.val Log}")

  if (!length(object@Log))
    return(invisible(NULL))

  sections <- list(
    assumption = list(title = "ASSUMPTIONS:", colour = cli::col_blue,   header = cli::cli_alert_info,    entry = cli::cli_alert_info),
    warning    = list(title = "WARNINGS:",    colour = cli::col_yellow, header = cli::cli_alert_warning, entry = cli::cli_alert_warning),
    error      = list(title = "ERRORS:",      colour = cli::col_red,    header = cli::cli_alert_danger,  entry = cli::cli_alert_danger)
  )

  types <- names(sections)
  if (!is.null(type)) {
    type <- match.arg(type, names(sections), several.ok = TRUE)
    types <- type
  }

  for (t in types) {
    entries <- object@Log[[t]]
    if (!length(entries)) next

    section <- sections[[t]]
    cli::cli_text('')
    section$header(section$colour(section$title))
    for (entry in entries) {
      nm  <- .LogEntryName(entry)
      tag <- .LogEntryTag(entry)
      if (nchar(nm) || nchar(tag)) {
        cli::cli_text('')
        section$entry("{nm}{tag}")
      }
      cli::cli_text(.LogEntryMessage(entry))
    }
  }
  invisible(NULL)
}

# a Log entry: message text plus an optional grouping `name` label and
# optional sim/year/mp context, used for both display and filtering
.NewLogEntry <- function(message, name = '', sim = NULL, year = NULL, mp = NULL) {
  list(message = message, name = name, sim = sim, year = year, mp = mp)
}

.IsLogEntry <- function(x) is.list(x) && !is.null(x$message)

# entries created before this structured format existed (or written
# directly to e.g. `Advice@Log` by external code) are bare characters -
# tolerate both shapes throughout
.LogEntryMessage <- function(entry) {
  if (.IsLogEntry(entry)) return(entry$message)
  as.character(entry)
}

.LogEntryName <- function(entry) {
  if (.IsLogEntry(entry)) return(entry$name %||% '')
  ''
}

.LogEntryTag <- function(entry) {
  if (!.IsLogEntry(entry)) return('')
  parts <- c(
    if (!is.null(entry$mp))   cli::format_inline("MP: {.val {entry$mp}}"),
    if (!is.null(entry$year)) cli::format_inline("Year: {.val {entry$year}}"),
    if (!is.null(entry$sim))  cli::format_inline("Sim: {.val {entry$sim}}")
  )
  if (!length(parts)) return('')
  paste0(' (', paste(parts, collapse = ', '), ')')
}

# type: 'assumption', 'warning', or 'error'
.CaptureLog <- function(object, string, name = '', type = 'warning', sim = NULL, year = NULL, mp = NULL) {
  entry <- .NewLogEntry(message = string, name = name, sim = sim, year = year, mp = mp)
  if ('Log' %in% slotNames(object)) {
    object@Log[[type]] <- c(object@Log[[type]], list(entry))
  } else {
    object@Misc[[type]] <- c(object@Misc[[type]], list(entry))
  }

  object
}

.CheckLog <- function(object, cls = 'Hist') {
  if (!'Log' %in% slotNames(object))
    cli::cli_abort("Object of class {.val {class(object)}} does not have slot {.val Log}")

  if (!length(object@Log))
    return(invisible(NULL))

  nWarning <- length(object@Log$warning)
  nError   <- length(object@Log$error)

  if (nWarning || nError) {
    cli::cli_alert_warning("Some warning or error alerts in {.val Log}. Use {.var Log({cls})} to print log messages")
  } else if (length(object@Log$assumption)) {
    cli::cli_alert_info("Some assumptions were recorded in {.val Log}. Use {.var Log({cls})} to print log messages")
  }

  invisible(NULL)
}

# merges every type present in either Log (assumption/warning/error/...)
.JoinLog <- function(Log1, Log2) {
  types <- union(names(Log1), names(Log2))
  out <- stats::setNames(vector("list", length(types)), types)
  for (type in types) {
    out[[type]] <- c(Log1[[type]], Log2[[type]])
  }
  out
}
