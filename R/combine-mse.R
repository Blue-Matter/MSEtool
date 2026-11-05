#' Combine Multiple `mse` Objects Across Simulations or MPs
#'
#' @description{
#' Combines a list of [mse-class] objects into a single `MSE` object: 
#' - `CombineMSE()` concatenates every simulation-indexed array along the `Sim` dimension. All elements of
#' `MSE_List` must share the same MPs (same names, same order), the same stock names, and the same historical/projection years.
#' - `CombineMP()` concatenates every MP-indexed array along the `MP` dimension. All elements of
#' `MSE_List` must share the same OM (number of simulations), the same stock names, and the same historical/projection years.
#' }
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
      values                     <- purrr::map(MSE_List, \(m) methods::slot(m, sl))
      slot(out, sl, check = FALSE) <- purrr::reduce(values, .CombineSimwise)
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

#' @rdname CombineMSE
#' @export
CombineMP <- function(MSE_List, silent = FALSE) {
  MSEtool:::.CheckClass(MSE_List, 'list', 'MSE_List')
  purrr::walk(MSE_List, MSEtool:::.CheckClass, class = 'mse', name = 'element of `MSE_List`')
  
  n <- length(MSE_List)
  
  if (n == 1)
    return(MSE_List[[1]])
  
  ref      <- MSE_List[[1]]
  RefnSim  <- nSim(ref)
  RefStock <- StockNames(ref)
  RefYears <- Years(ref@OM)
  RefOM    <- OM(ref)
  
  purrr::walk(MSE_List[-1], \(m) {
    if (!identical(nSim(m), RefnSim))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same number of simulations.")
    if (!identical(StockNames(m), RefStock))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same stock names.")
    if (!identical(Years(m@OM), RefYears))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same historical and projection years.")
    if (!identical(OM(m), RefOM))
      cli::cli_abort("All `mse` objects in `MSE_List` must share the same operating model.")
  })
  
  out   <- ref
  slots <- methods::slotNames(ref)
  slots_ts <- methods::slotNames("timeseries")
  
  combine_id <- if (!silent) {
    cli::cli_progress_bar(
      format = "Combining slot {.val {cli::pb_extra$slot}} ({cli::pb_current}/{cli::pb_total}) {cli::pb_bar} {cli::pb_percent}",
      format_done = "{cli::col_green(cli::symbol$tick)} Combined {length(slots)} slot{?s} [{cli::pb_elapsed}]",
      total = length(slots),
      clear = TRUE,
      extra = list(slot = "")
    )
  } else NULL
  
  for (sl in slots) {
    
    if (sl %in% c("OM", "Unfished", "Reference", "Hist", "Log")) {
      # Do nothing
      #purrr::map(MSE_List, methods::slot, name = sl) |> reduce(identical)
      
    } else if (sl == "MPs") {
      # Concatenate by MP
      slot(out, sl) <- purrr::map(MSE_List, methods::slot, name = sl) |> purrr::list_c()
    } else if (sl == "PPD") {
      # Concatenate by MP
      PPDlist <- purrr::map(MSE_List, methods::slot, name = sl) 
      
      # Remove historical years in MSE_List[-1]
      ProjYears <- Years(MSE_List[[1]], "P")
      PPDlist[-1] <- purrr::map(PPDlist[-1], \(ProjDataList1) {
        purrr::map(ProjDataList1, \(ProjDataList2) {
          purrr::map(ProjDataList2, \(ProjDataList3) {
            purrr::map(ProjDataList3, \(ProjData) Subset(ProjData, Years = ProjYears))
          })
        })
      })
      slot(out, sl) <- purrr::list_c(PPDlist)
    } else if (sl == "Misc") {
      
      Misc_slots <- names(ref@Misc)
      slot(out, sl) <- purrr::map(Misc_slots, \(i) {
        purrr::map(MSE_List, \(m) methods::slot(m, "Misc")[[i]]) |> purrr::list_c()
      }) |>
        structure(names = Misc_slots)
      
    } else if (sl %in% slots_ts) {
      
      if (is.list(slot(MSE_List[[1]], sl))) { # List by stock complex
        
        slot(out, sl) <- purrr::map(RefStock, \(s) {
          slot_complex <- purrr::map(MSE_List, \(m) slot(m, sl)[[s]])
          
          if (is.list(slot_complex[[1]])) { # Nested list by fleet
            FleetNames <- names(slot_complex[[1]])
            res <- purrr::map(FleetNames, \(f) {
              slot_fleet <- purrr::map(slot_complex, getElement, f) 
              res <- abind::abind(slot_fleet)
              names(dimnames(res)) <- names(dimnames(slot_fleet[[1]]))
              return(res)
            }) |> structure(names = FleetNames)
          } else {
            res <- abind::abind(slot_complex) # Default is last dimension (perfect!)
            names(dimnames(res)) <- names(dimnames(slot_complex[[1]]))
          }
          return(res)
        }) |> structure(names = RefStock)
        
      } else if (is.array(slot(MSE_List[[1]], sl))) {
        
        slot(out, sl) <- purrr::map(MSE_List, methods::slot, name = sl) |> abind::abind()
        names(dimnames(slot(out, sl))) <- names(dimnames(slot(MSE_List[[1]], sl)))
        
      }
      
    } else {
      cli::cli_abort("Unknown slot: { sl }")
    }
    if (!silent) cli::cli_progress_update(id = combine_id, extra = list(slot = sl))
  }
  
  totalMPs <- length(MPs(out))
  cli::cli_progress_done()
  
  if (!silent)
    cli::cli_alert_success("Combined {length(MSE_List)} `MSE` objects into a single object with {totalMPs} total MPs")
  
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

.CombineSimwise <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)

  if (isS4(a)) {
    for (sl in methods::slotNames(a))
      methods::slot(a, sl, check = FALSE) <- .CombineSimwise(methods::slot(a, sl), methods::slot(b, sl))
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

.RelabelSim <- function(x, totalSim) {
  if (isS4(x)) {
    for (sl in methods::slotNames(x))
      methods::slot(x, sl, check = FALSE) <- .RelabelSim(methods::slot(x, sl), totalSim)
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
    }
    return(x)
  }
  
  if (is.list(x))
    return(purrr::map(x, .RelabelSim, totalSim = totalSim))
  
  x
}
