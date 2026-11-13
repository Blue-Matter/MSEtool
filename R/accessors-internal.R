# named list of stock objects from an om/hist/mse, a single stock, or a list
.StockList <- function(x) {
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (inherits(x, 'om'))
    x <- x@Stock
  if (is.null(x))
    return(list())
  if (!is.list(x))
    x <- list(x)
  x
}

# flat list of fleet objects (first stock) from an om/hist/mse, a fleet, or a flat/nested list
.FleetList <- function(x) {
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (inherits(x, 'om'))
    x <- x@Fleet
  if (is.null(x))
    return(list())
  if (!is.list(x))
    return(list(x))
  if (length(x) && is.list(x[[1]]))
    return(x[[1]])
  x
}

.IsDataList <- function(x) {
  is.list(x) && !isS4(x) && length(x) > 0 && all(purrr::map_lgl(x, \(d) inherits(d, 'data')))
}

# list by simulation of data objects or lists of data objects, e.g. `Hist@Data`
.IsSimDataList <- function(x) {
  is.list(x) && !isS4(x) && length(x) > 0 &&
    all(purrr::map_lgl(x, \(d) inherits(d, 'data') || .IsDataList(d)))
}

# `fn` applied to a data object or each of a list; a single value when all are identical
.DataValue <- function(x, fn) {
  if (inherits(x, 'data'))
    return(fn(x))
  out <- purrr::map(x, fn)
  if (length(unique(out)) == 1)
    return(out[[1]])
  out
}

.ValidStock <- function(st, Stocks) {
  if (is.numeric(st) && st > length(Stocks) || is.character(st) && !st %in% names(Stocks))
    cli::cli_abort("Stock {.val {st}} not found", call = NULL)
  st
}

.AccessSlot <- function(x, slotname) {
  if (is.null(x)) return(NULL)
  if (.IsDataList(x))
    return(purrr::map(x, \(d) .AccessSlot(d, slotname)))
  .CheckClass(slotname, 'character', 'slotname')
  if (!isS4(x))
    cli::cli_abort("{.arg x} is not an S4 object.")
  if (!slotname %in% slotNames(x))
    cli::cli_abort("Slot {.val {slotname}} not found in class {.cls {class(x)}}.")
  slot(x, slotname)
}

.AccessSlotRecursive <- function(x, SlotName) {
  if (is.list(x))
    return(purrr::map(x, \(xi) .AccessSlotRecursive(xi, SlotName)))
  .AccessSlot(x, SlotName)
}

.AssignSlot <- function(x, value, slot) {
  if (!isS4(x) || !slot %in% methods::slotNames(x))
    cli::cli_abort("Slot {.code {slot}} not found in class {.cls {class(x)}}.", call = NULL)
  slot(x, slot) <- value
  methods::validObject(x)
  x
}

.AssignSlotRecursive <- function(x, value, SlotName) {
  if (is.list(x)) {
    if (!is.list(value) || length(value) != length(x))
      cli::cli_abort(c(
        "x" = "`value` must be a list of length {length(x)}",
        "i" = "`value` has length {length(value)}"
      ))
    return(purrr::map2(x, value, \(xi, vi) .AssignSlotRecursive(xi, vi, SlotName)))
  }
  .AssignSlot(x, value, SlotName)
}

# Is `x` a non-empty list whose every element inherits from S4 class `what`?
.IsTypeList <- function(x, what) {
  is.list(x) && length(x) > 0 &&
    all(vapply(x, inherits, logical(1), what = what))
}

# object of class `what`, or a (nested) list of them
.IsDataContainer <- function(x, what) {
  if (inherits(x, what))
    return(TRUE)
  is.list(x) && !isS4(x) && length(x) > 0 &&
    all(vapply(x, .IsDataContainer, logical(1), what = what))
}

.IsStockList <- function(x) {
  .IsTypeList(x, 'stock')
}

.IsStockOrList <- function(x) {
  inherits(x, 'stock') ||
    inherits(x, 'om')    ||
    .IsStockList(x)
}

.IsFleetList <- function(x) {
  .IsTypeList(x, 'fleet')
}

.IsStockFleetList <- function(x) {
  is.list(x) && length(x) > 0 &&
    all(vapply(x, .IsFleetList, logical(1)))
}

.IsFleetOrList <- function(x) {
  inherits(x, 'fleet') ||
    inherits(x, 'om')    ||
    .IsFleetList(x)        ||
    .IsStockFleetList(x)
}

.ExtractStockSlot <- function(x, SlotName) {
  if (inherits(x, 'stock'))
    return(slot(x, SlotName))
  
  if (inherits(x, 'om')) {
    out <- purrr::map(x@Stock, slot, SlotName)
    if (!length(out))
      return(NULL)
    
    class(out) <- 'StockList'
    return(out)
  }
  
  if (.IsStockList(x)) {
    out <- purrr::map(x, slot, SlotName)
    if (!length(out))
      return(NULL)
    class(out) <- 'StockList'
    return(out)
  }
  
  NULL
}

.ExtractFleetSlot <- function(x, SlotName) {
  if (inherits(x, 'fleet'))
    return(slot(x, SlotName))
  
  if (inherits(x, 'om')) {
    out <- purrr::map(x@Fleet, \(FleetList) {
      fl <- purrr::map(FleetList, slot, SlotName)
      class(fl) <- 'FleetList'
      fl
    })
    if (!length(out))
      return(NULL)
    class(out) <- 'StockFleetList'
    return(out)
  }
  
  if (.IsStockFleetList(x)) {
    out <- purrr::map(x, \(FleetList) {
      fl <- purrr::map(FleetList, slot, SlotName)
      class(fl) <- 'FleetList'
      fl
    })
    if (!length(out))
      return(NULL)
    class(out) <- 'StockFleetList'
    return(out)
  }
  
  if (.IsFleetList(x)) {
    out <- purrr::map(x, slot, SlotName)
    if (!length(out))
      return(NULL)
    class(out) <- 'FleetList'
    return(out)
  }
  
  NULL
}

.AssignFleetSlot <- function(x, value, SlotName) {
  if (inherits(x, 'om')) {
    x@Fleet <- .AssignSlotRecursive(x@Fleet, value, SlotName)
    return(x)
  }
  .AssignSlotRecursive(x, value, SlotName)
}
