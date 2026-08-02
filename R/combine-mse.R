#' Combine Multiple `mse` Objects Across Simulations
#'
#' Combines a list of [mse-class] objects into a single `MSE` object, by
#' concatenating every simulation-indexed array along the `Sim` dimension.
#' Used internally by [PM_FFMSY()] and the other `PM_*` functions whenever
#' `object` is a `list` rather than a single `MSE`.
#'
#' All elements of `MSE_List` must share the same MPs (same names, same
#' order), the same stock names, and the same historical/projection years.
#'
#' @param MSE_List A list of [mse-class] objects.
#' @param silent Logical. Suppress the summary message. Default `FALSE`.
#'
#' @return A single [mse-class] object spanning all simulations in
#'   `MSE_List`.
#'
#' @seealso [CombineOMs()], [CombineFleets()]
#' @export
CombineMSE <- function(MSE_List, silent = FALSE) {
  .CheckClass(MSE_List, 'list', 'MSE_List')
  purrr::walk(MSE_List, .CheckClass, class = 'mse', name = 'element of `MSE_List`')
  
  n <- length(MSE_List)
  
  if (n == 1)
    return(MSE_List[[1]])
  
  extend_id <- if (!silent) {
    cli::cli_progress_bar(
      format = "Extending MSE objects {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent}",
      format_done = "{cli::col_green(cli::symbol$tick)} Extended {n} MSE object{?s} to full `nSim` [{cli::pb_elapsed}]",
      total = n,
      clear = TRUE
    )
  } else NULL
  
  MSE_List <- purrr::imap(MSE_List, \(object, i) {
    out <- ExtendSims(object, nSim(object))
    if (!silent) cli::cli_progress_update(id = extend_id)
    out
  })
  
  ref      <- MSE_List[[1]]
  RefMPs   <- names(MPs(ref))
  RefStock <- StockNames(ref)
  RefYears <- Years(ref@OM)

  purrr::walk(MSE_List[-1], \(m) {
    if (!identical(names(MPs(m)), RefMPs))
      cli::cli_abort("All `mse` objects in `MSE_List` must evaluate the same MPs, in the same order.")
    if (!identical(StockNames(m), RefStock))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same stock names.")
    if (!identical(Years(m@OM), RefYears))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same historical and projection years.")
  })

  out   <- ref
  slots <- methods::slotNames(ref)

  combine_id <- if (!silent) {
    cli::cli_progress_bar(
      format = "Combining slot {.val {cli::pb_extra$slot}} ({cli::pb_current}/{cli::pb_total}) {cli::pb_bar} {cli::pb_percent}",
      format_done = "{cli::col_green(cli::symbol$tick)} Combined {length(slots)} slot{?s} [{cli::pb_elapsed}]",
      total = length(slots),
      clear = TRUE,
      extra = list(slot = "")
    )
  } else NULL
  

  sim_offsets <- c(0, cumsum(purrr::map_dbl(MSE_List, nSim)))[seq_along(MSE_List)]

  for (sl in slots) {
    if (identical(sl, 'Log')) {
      slot(out, sl) <- .CombineLogs(purrr::map(MSE_List, methods::slot, 'Log'), sim_offsets)
    } else {
      values        <- purrr::map(MSE_List, \(m) methods::slot(m, sl))
      slot(out, sl) <- purrr::reduce(values, .CombineSimwise)
    }
    if (!silent) cli::cli_progress_update(id = combine_id, extra = list(slot = sl))
  }
  
 
  totalSim       <- sum(purrr::map_dbl(MSE_List, nSim))
  out            <- .RelabelSim(out, totalSim)
  out@OM@nSim    <- totalSim

  cli::cli_progress_done()
  
  if (!silent)
    cli::cli_alert_success("Combined {length(MSE_List)} `MSE` objects into {totalSim} total simulations")

  out
}


.CombineLogs <- function(LogList, sim_offsets) {
  types <- c('assumption', 'warning', 'error')
  out   <- stats::setNames(vector('list', length(types)), types)

  for (type in types) {
    entries <- purrr::map2(LogList, sim_offsets, \(Log, offset) {
      .CombineLogEntries(Log[[type]], offset)
    })
    combined <- purrr::list_c(entries)
    if (length(combined)) out[[type]] <- combined
  }

  out
}

.CombineLogEntries <- function(entries, offset) {
  if (is.null(entries) || !length(entries)) return(list())

  lapply(entries, \(entry) {
    if (.IsLogEntry(entry) && !is.null(entry$sim)) entry$sim <- entry$sim + offset
    entry
  })
}

# Recursively concatenate two S4/array/list structures along their `Sim` dimension.
.CombineSimwise <- function(a, b) {
 
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)

  if (isS4(a)) {
    for (sl in methods::slotNames(a))
      methods::slot(a, sl) <- .CombineSimwise(methods::slot(a, sl), methods::slot(b, sl))
    return(a)
  }

  if (is.array(a)) {
    dn <- names(dimnames(a))
    if (!is.null(dn) && 'Sim' %in% dn) {
      out <- abind::abind(a, b, along = match('Sim', dn))
      names(dimnames(out)) <- dn
      return(out)
    }
    return(a)
  }
  
  if (is.data.frame(a)) {
    if (!is.null(a$Sim)) {
      return(dplyr::bind_rows(a,b))
    } 
    return(a)
  }

  if (is.list(a)) {
    if (length(a) == length(b))
      return(purrr::map2(a, b, .CombineSimwise))
    return(a)
  }

  a
}

# Recursively relabel every `Sim` dimname to sequential integers after combining.
.RelabelSim <- function(x, totalSim) {
  if (isS4(x)) {
    for (sl in methods::slotNames(x)) {
      # OUT <<- methods::slot(x, sl)
      methods::slot(x, sl) <- .RelabelSim(methods::slot(x, sl), totalSim)
    }
      
    return(x)
  }

  if (is.array(x)) {
    dn <- dimnames(x)
    nm <- names(dn)
    if (!is.null(nm) && 'Sim' %in% nm && dim(x)[match('Sim', nm)] == totalSim)
      dn[[match('Sim', nm)]] <- as.character(seq_len(totalSim))
    dimnames(x) <- dn
    return(x)
  }
  
  if (is.data.frame(x)) {
    if (!is.null(x$Sim)) {
      n_exist <- length(x$Sim)
      x$Sim <- seq_len(totalSim)[seq_len(n_exist)]
      return(x)
    }
  }
  
  if (is.list(x))
    return(purrr::map(x, .RelabelSim, totalSim = totalSim))

  x
}
