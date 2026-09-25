
#' Display Log Messages
#'
#' Prints any messages stored in the `Log` slot of an S4 object to the
#' console using `cli` formatting. Messages are grouped into assumption,
#' warning, and error sections; within each section, entries are further
#' grouped by the name of the function that recorded them. Identical messages
#' recorded for several MPs, years, or simulations are printed once, with the
#' MPs, years, and simulations listed after the message.
#'
#' Log entries fall into three types:
#' \itemize{
#'   \item `'assumption'`: something was inferred or auto-corrected (e.g.
#'     a missing value defaulted, a selectivity schedule rescaled to a
#'     maximum of 1) and no further action is required. 
#'   \item `'warning'`: something is likely wrong with the object or its
#'     inputs and should be reviewed.
#'   \item `'error'`: a problem serious enough that results built from the
#'     object should not be trusted. 
#' }
#' 
#' @param object An S4 object with a `Log` slot. See e.g. [om-class],
#'   [hist-class], [mse-class], [stock-class], [data-class],
#'   [advice-class].
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
#' @seealso [DeleteLogs()]
#'
#' @export
Log <- function(object, type = NULL) {

  if (!'Log' %in% slotNames(object))
    cli::cli_abort("Object of class {.val {class(object)}} does not have slot {.val Log}")

  if (!length(object@Log))
    return(invisible(NULL))

  sections <- list(
    assumption = list(title = "Assumptions", alert = cli::cli_alert_info),
    warning    = list(title = "Warnings",    alert = cli::cli_alert_warning),
    error      = list(title = "Errors",      alert = cli::cli_alert_danger)
  )

  types <- names(sections)
  if (!is.null(type))
    types <- match.arg(type, names(sections), several.ok = TRUE)

  .MsgTheme()
  body_theme <- list(.logbody = list("margin-left" = 2),
                     ".logbody li" = list("text-exdent" = 2),
                     .logdetail = list("margin-left" = 2))

  first <- TRUE
  for (t in types) {
    entries <- object@Log[[t]]
    if (!length(entries)) next

    groups <- .LogGroups(entries)
    nLines <- .LogCount(entries)
    title  <- sections[[t]]$title
    if (!first) cli::cli_text('')
    first <- FALSE
    cli::cli_rule(left = "{title} ({nLines})")

    for (g in groups) {
      if (nchar(g$name)) {
        nm <- g$name
        sections[[t]]$alert("{.strong {nm}}")
      }
      body <- cli::cli_div(class = "logbody", theme = body_theme)
      if (length(g$headline)) {
        cli::cli_ul()
        for (line in g$headline) cli::cli_li("{line}")
        cli::cli_end()
      }
      if (length(g$detail)) {
        cli::cli_div(class = "logdetail")
        for (line in g$detail) cli::cli_text("{line}")
        cli::cli_end()
      }
      cli::cli_end(body)
    }
  }
  invisible(NULL)
}

# unnamed entries are detail lines of the preceding name; identical messages merge their tags
.LogGroups <- function(entries) {
  orig_nms    <- vapply(entries, .LogEntryName, character(1))
  is_headline <- nchar(orig_nms) > 0
  nms  <- orig_nms
  last <- ''
  for (i in seq_along(nms)) {
    if (nchar(nms[i])) last <- nms[i] else nms[i] <- last
  }
  msgs <- vapply(entries, \(e) gsub("\\s*\n\\s*", " ", trimws(.LogEntryMessage(e))), character(1))

  merge_lines <- function(idx) {
    keys <- unique(msgs[idx])
    vapply(keys, \(k) {
      group <- entries[idx[msgs[idx] == k]]
      paste0(k, .LogMergedTag(group))
    }, character(1), USE.NAMES = FALSE)
  }

  purrr::map(unique(nms), \(nm) {
    idx <- which(nms == nm)
    list(name     = nm,
         headline = merge_lines(idx[is_headline[idx]]),
         detail   = merge_lines(idx[!is_headline[idx]]))
  })
}

# one per named group; unnamed entries count individually
.LogCount <- function(entries) {
  if (!length(entries)) return(0L)
  sum(purrr::map_int(.LogGroups(entries), \(g)
    if (nchar(g$name)) 1L else length(g$headline) + length(g$detail)))
}

.LogMergedTag <- function(group) {
  group <- Filter(.IsLogEntry, group)
  if (!length(group)) return('')
  mps   <- unique(unlist(purrr::map(group, 'mp')))
  years <- unlist(purrr::map(group, 'year'))
  sims  <- unlist(purrr::map(group, 'sim'))
  parts <- c(
    if (length(mps))   .MsgFormat("MP{?s}: {.mp {mps}}"),
    if (length(years)) paste0(if (length(unique(years)) > 1) "Years: " else "Year: ", .CompressRange(years)),
    if (length(sims))  paste0(if (length(unique(sims)) > 1) "Sims: " else "Sim: ", .CompressRange(sims))
  )
  if (!length(parts)) return('')
  paste0(' ', cli::col_grey(paste0('[', paste(parts, collapse = paste0(' ', intToUtf8(183), ' ')), ']')))
}

# e.g. c(1,2,3,5,7,8) -> "1-3, 5, 7-8"
.CompressRange <- function(x) {
  x <- sort(unique(x))
  if (any(x != round(x)))
    return(if (length(x) > 4) paste0(x[1], '-', x[length(x)]) else paste(x, collapse = ', '))
  runs <- split(x, cumsum(c(1, diff(x) != 1)))
  out  <- vapply(runs, \(r) if (length(r) > 1) paste0(r[1], '-', r[length(r)]) else as.character(r), character(1))
  if (length(out) > 6)
    return(paste0(length(x), ' in ', x[1], '-', x[length(x)]))
  paste(out, collapse = ', ')
}


.NewLogEntry <- function(message, name = '', sim = NULL, year = NULL, mp = NULL) {
  list(message = message, name = name, sim = sim, year = year, mp = mp)
}

.IsLogEntry <- function(x) is.list(x) && !is.null(x$message)


.LogEntryMessage <- function(entry) {
  if (.IsLogEntry(entry)) return(entry$message)
  as.character(entry)
}

.LogEntryName <- function(entry) {
  if (.IsLogEntry(entry)) return(entry$name %||NA% '')
  ''
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

  nAssumption <- .LogCount(object@Log$assumption)
  nWarning    <- .LogCount(object@Log$warning)
  nError      <- .LogCount(object@Log$error)
  if (!(nAssumption || nWarning || nError))
    return(invisible(NULL))

  cls <- tolower(cls)
  counts <- c(if (nError) "{nError} error{?s}",
              if (nWarning) "{nWarning} warning{?s}",
              if (nAssumption) "{nAssumption} assumption{?s}")
  msg <- paste0(paste(counts, collapse = ", "), " recorded. Use {.code Log({cls})} to view.")
  .MsgAlert(msg, if (nWarning || nError) 'warning' else 'info')

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
