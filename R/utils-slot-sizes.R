#' Summarise S4 Object Memory Usage by Slot
#'
#' Recursively inspects all slots of an S4 object, including nested S4 objects
#' and lists containing S4 objects, and reports their memory footprint. Useful
#' for identifying which slots dominate memory usage in complex S4 objects.
#'
#' @param obj An S4 object.
#' @param n Integer. Number of largest slots to display in the printed summary.
#'   Default is `5`.
#' @param verbose Logical. If `TRUE` (default), prints a summary table of the
#'   top `n` slots. If `FALSE`, returns the full results silently.
#'
#' @return A `data.frame` with columns:
#'   - `path`: Full slot path using `@` notation for S4 slots and `[["name"]]`
#'     or `[[i]]` notation for list elements.
#'   - `mb`: Size of the slot or element in megabytes.
#'
#'   Returned invisibly, sorted descending by size.
#'
#' @details
#' Slot paths reflect the access pattern needed to reach each element. For
#' example, a nested S4 slot would appear as `obj@SlotA@SlotB`, and a named
#' list element inside a slot as `obj@SlotA[["item"]]`.
#'
#' Note that sizes are **not additive** — parent slots include the memory of
#' their children, so a slot containing a large nested S4 object will itself
#' appear large.
#'
#' @examples
#' \dontrun{
#' # Inspect top 10 slots of a complex S4 model object
#' S4SlotSizes(my_model, n = 10)
#'
#' # Retrieve full results without printing
#' sizes <- S4SlotSizes(my_model, verbose = FALSE)
#' sizes[sizes$mb > 1, ]  # slots larger than 1 MB
#' }
#'
#' @export
S4SlotSizes <- function(obj, n = 5, verbose = TRUE) {
  if (!isS4(obj))
    cli::cli_abort("`obj` must be an S4 object.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1L || n < 1L)
    cli::cli_abort("`n` must be a positive integer.", call. = FALSE)
  
  results <- .CollectS4Sizes(obj, prefix = "")
  results <- results[order(-results$mb), ]
  
  if (verbose) {
    total_mb <- as.numeric(utils::object.size(obj)) / 1024^2
    cat(sprintf("\nObject size: %.3f MB\n", total_mb))
    cat(sprintf("Top %d slots by size:\n\n", n))
    cat(sprintf("%-60s %10s\n", "Slot", "Size (MB)"))
    cat(strrep("-", 72), "\n")
    top_n <- utils::head(results, n)
    for (i in seq_len(nrow(top_n))) {
      cat(sprintf("%-60s %8.3f MB\n", top_n$path[i], top_n$mb[i]))
    }
    cat("\n")
  }
  
  invisible(results)
}


.CollectS4Sizes <- function(obj, prefix) {
  slots <- slotNames(obj)
  results <- lapply(slots, function(s) {
    slot_val  <- slot(obj, s)
    slot_path <- if (nzchar(prefix)) paste0(prefix, "@", s) else s
    size_mb   <- as.numeric(utils::object.size(slot_val)) / 1024^2
    
    self <- data.frame(path = slot_path, mb = size_mb, stringsAsFactors = FALSE)
    
    if (isS4(slot_val)) {
      rbind(self, .CollectS4Sizes(slot_val, prefix = slot_path))
    } else if (is.list(slot_val)) {
      rbind(self, .CollectListSizes(slot_val, prefix = slot_path))
    } else {
      self
    }
  })
  
  do.call(rbind, results)
}


.CollectListSizes <- function(lst, prefix) {
  nms <- names(lst)
  
  results <- lapply(seq_along(lst), function(i) {
    label <- if (!is.null(nms) && nzchar(nms[[i]])) {
      paste0('[["', nms[[i]], '"]]')
    } else {
      paste0("[[", i, "]]")
    }
    item_path <- paste0(prefix, label)
    item      <- lst[[i]]
    size_mb   <- as.numeric(utils::object.size(item)) / 1024^2
    
    self <- data.frame(path = item_path, mb = size_mb, stringsAsFactors = FALSE)
    
    if (isS4(item)) {
      rbind(self, .CollectS4Sizes(item, prefix = item_path))
    } else if (is.list(item)) {
      rbind(self, .CollectListSizes(item, prefix = item_path))
    } else {
      self
    }
  })
  
  do.call(rbind, results)
}