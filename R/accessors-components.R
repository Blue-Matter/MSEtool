# Stock and fleet component pass-through: Length(), Selectivity(), etc. called
# with an object that holds the component instead of constructor arguments.

.LifeHistoryClasses <- c('lifehistorydata', 'lifehistoryobs')
.ExploitationClasses <- c('exploitationdata', 'exploitationobs')

# can `x` supply stock component `SlotName`?
.IsStockComponentSource <- function(x) {
  inherits(x, c('stock', 'om', 'hist', 'mse', 'data', 'obs', .LifeHistoryClasses)) ||
    .IsStockList(x) ||
    .IsDataContainer(x, c('data', .LifeHistoryClasses))
}

.IsFleetComponentSource <- function(x, Containers = TRUE, Extra = character(0)) {
  if (inherits(x, c('fleet', 'om', 'hist', 'mse', Extra)) || .IsFleetList(x) || .IsStockFleetList(x))
    return(TRUE)
  Containers && (inherits(x, c('data', 'obs', .ExploitationClasses)) ||
                   .IsDataContainer(x, c('data', .ExploitationClasses)))
}

.GetStockSlot <- function(x, SlotName) {
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (inherits(x, c('stock', .LifeHistoryClasses)))
    return(methods::slot(x, SlotName))
  if (inherits(x, c('data', 'obs')))
    return(.GetStockSlot(x@LifeHistory, SlotName))
  if (inherits(x, 'om'))
    x <- .StockList(x)
  if (!length(x))
    return(NULL)
  out <- purrr::map(x, \(xi) .GetStockSlot(xi, SlotName))
  if (.IsStockList(x))
    class(out) <- 'StockList'
  out
}

.GetFleetSlot <- function(x, SlotName) {
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (inherits(x, 'om'))
    x <- x@Fleet
  if (inherits(x, c('data', 'obs')))
    return(.GetFleetSlot(x@Exploitation, SlotName))
  if (isS4(x))
    return(methods::slot(x, SlotName))
  if (!length(x))
    return(NULL)
  out <- purrr::map(x, \(xi) .GetFleetSlot(xi, SlotName))
  if (.IsStockFleetList(x)) {
    class(out) <- 'StockFleetList'
  } else if (.IsFleetList(x)) {
    class(out) <- 'FleetList'
  }
  out
}

.AbortSimulatedAssign <- function(x, SlotName) {
  cli::cli_abort(c(
    "{.code {SlotName}} cannot be changed on a {.cls {class(x)}} object.",
    "i" = "It is used to simulate the historical dynamics. Change it in the {.cls om} object and re-run {.fn Simulate}."
  ), call = NULL)
}

.ObjectNames <- function(x) {
  nms <- names(x)
  if (is.null(nms) || any(!nzchar(nms)))
    nms <- purrr::map_chr(x, \(xi) if (isS4(xi) && methods::.hasSlot(xi, 'Name') && length(xi@Name)) xi@Name[1] else NA_character_)
  nms
}

# assign `value` to `SlotName` of each object in list `x`, matching by name;
# a single (non-list) `value` is accepted only when `x` has one element
.AssignEachByName <- function(x, value, Label, AssignFn) {
  Names <- .ObjectNames(x)
  n <- length(x)
  if (!is.list(value) || isS4(value)) {
    if (n != 1)
      cli::cli_abort(c(
        "x" = "{.arg value} must be a list named by {Label}, because {.arg x} has {n} {Label}s.",
        "i" = "Expected names: {.val {Names}}"
      ), call = NULL)
    x[[1]] <- AssignFn(x[[1]], value)
    return(x)
  }
  if (length(value) != n)
    cli::cli_abort(c(
      "x" = "{.arg value} must have one element per {Label} ({n}).",
      "i" = "{.arg value} has length {length(value)}."
    ), call = NULL)
  if (n == 1 && is.null(names(value))) {
    x[[1]] <- AssignFn(x[[1]], value[[1]])
    return(x)
  }
  if (is.null(names(value)) || !setequal(names(value), Names))
    cli::cli_abort(c(
      "x" = "{.arg value} must be named by {Label}.",
      "i" = "Expected names: {.val {Names}}",
      "i" = "Supplied names: {.val {names(value) %||% character(0)}}"
    ), call = NULL)
  for (i in seq_len(n))
    x[[i]] <- AssignFn(x[[i]], value[[Names[i]]])
  x
}

.AssignStockSlot <- function(x, value, SlotName) {
  if (inherits(x, c('hist', 'mse')))
    .AbortSimulatedAssign(x, SlotName)
  if (inherits(x, c('stock', .LifeHistoryClasses)))
    return(.AssignSlot(x, value, SlotName))
  if (inherits(x, c('data', 'obs'))) {
    x@LifeHistory <- .AssignStockSlot(x@LifeHistory, value, SlotName)
    methods::validObject(x)
    return(x)
  }
  if (inherits(x, 'om')) {
    if (is.null(x@Stock) || (is.list(x@Stock) && !length(x@Stock)))
      cli::cli_abort("{.arg x} has no stocks. Add them with {.code Stock(x) <- value}.", call = NULL)
    if (inherits(x@Stock, 'stock')) {
      x@Stock <- .AssignStockSlot(x@Stock, value, SlotName)
    } else {
      x@Stock <- .AssignEachByName(x@Stock, value, 'stock', \(s, v) .AssignStockSlot(s, v, SlotName))
    }
    return(x)
  }
  if (is.list(x) && length(x) && all(purrr::map_lgl(x, \(xi) inherits(xi, c('stock', .LifeHistoryClasses)))))
    return(.AssignEachByName(x, value, 'stock', \(s, v) .AssignStockSlot(s, v, SlotName)))
  cli::cli_abort("{.code {SlotName}} cannot be assigned to an object of class {.cls {class(x)}}.", call = NULL)
}

.AssignFleetSlot <- function(x, value, SlotName, Extra = character(0)) {
  if (inherits(x, c('hist', 'mse')))
    .AbortSimulatedAssign(x, SlotName)
  if (inherits(x, c('fleet', .ExploitationClasses, Extra)))
    return(.AssignSlot(x, value, SlotName))
  if (inherits(x, c('data', 'obs'))) {
    x@Exploitation <- .AssignFleetSlot(x@Exploitation, value, SlotName)
    methods::validObject(x)
    return(x)
  }
  if (inherits(x, 'om')) {
    if (is.null(x@Fleet) || (is.list(x@Fleet) && !length(x@Fleet)))
      cli::cli_abort("{.arg x} has no fleets. Add them with {.code Fleet(x) <- value}.", call = NULL)
    x@Fleet <- .AssignFleetSlot(x@Fleet, value, SlotName)
    return(x)
  }
  IsFleet <- \(xi) inherits(xi, c('fleet', .ExploitationClasses))
  if (is.list(x) && length(x) && all(purrr::map_lgl(x, IsFleet)))
    return(.AssignEachByName(x, value, 'fleet', \(f, v) .AssignFleetSlot(f, v, SlotName)))
  if (is.list(x) && length(x) && all(purrr::map_lgl(x, \(xi) is.list(xi) && length(xi) && all(purrr::map_lgl(xi, IsFleet))))) {
    # a list by fleet for a single stock
    if (length(x) == 1 && is.list(value) && !isS4(value) && setequal(names(value) %||% '', .ObjectNames(x[[1]])))
      value <- stats::setNames(list(value), names(x))
    AssignStock <- \(fl, v) .AssignEachByName(fl, v, 'fleet', \(f, vv) .AssignFleetSlot(f, vv, SlotName))
    if (length(x) == 1 && (!is.list(value) || isS4(value))) {
      x[[1]] <- AssignStock(x[[1]], value)
      return(x)
    }
    if (is.null(names(x)) || any(!nzchar(names(x))))
      cli::cli_abort("The list of fleets must be named by stock.", call = NULL)
    return(.AssignEachByName(x, value, 'stock', AssignStock))
  }
  cli::cli_abort("{.code {SlotName}} cannot be assigned to an object of class {.cls {class(x)}}.", call = NULL)
}
