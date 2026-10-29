# Containers for Data@LifeHistory (list by stock) and Data@Exploitation (list by stock of lists by fleet)

.IsLifeHistoryData  <- function(x) inherits(x, "lifehistorydata")
.IsExploitationData <- function(x) inherits(x, "exploitationdata")

# Names of the elements of `x` that are not in `Valid`
.UnknownNames <- function(x, Valid) setdiff(names(x), Valid)

.CheckLifeHistoryList <- function(x, Where = "LifeHistory") {
  if (!length(x)) return(invisible(x))
  Bad <- !vapply(x, .IsLifeHistoryData, logical(1))
  if (any(Bad))
    cli::cli_abort(c(
      "x" = "{.arg {Where}} must be a {.cls lifehistorydata} object or a list of them.",
      "i" = "Element{?s} {.val {which(Bad)}} {?is/are} not {.cls lifehistorydata} object{?s}."
    ), call = NULL)
  invisible(x)
}

.CheckExploitationList <- function(x, Where = "Exploitation") {
  if (!length(x)) return(invisible(x))
  Leaf <- vapply(x, .IsExploitationData, logical(1))
  Node <- vapply(x, is.list, logical(1))
  if (!all(Leaf | Node))
    cli::cli_abort(c(
      "x" = "{.arg {Where}} must be an {.cls exploitationdata} object, a list of them, or a list of lists of them."
    ), call = NULL)
  for (el in x[Node]) {
    if (length(el) && !all(vapply(el, .IsExploitationData, logical(1))))
      cli::cli_abort(c(
        "x" = "{.arg {Where}} must be an {.cls exploitationdata} object, a list of them, or a list of lists of them."
      ), call = NULL)
  }
  invisible(x)
}

# Name an unnamed list by `Valid` if it is the right length; otherwise require names within `Valid`
.NameByLevel <- function(x, Valid, Level, Where) {
  if (is.null(names(x))) {
    if (length(x) != length(Valid))
      cli::cli_abort(c(
        "x" = "{.code {Where}} has {length(x)} element{?s} but there {cli::qty(length(Valid))}{?is/are} {length(Valid)} {Level}{?s}.",
        "i" = "Name the elements by {Level}: {.val {Valid}}."
      ), call = NULL)
    names(x) <- Valid
    return(x)
  }
  Unknown <- .UnknownNames(x, Valid)
  if (length(Unknown) || anyDuplicated(names(x)))
    cli::cli_abort(c(
      "x" = "{.code {Where}} has {Level} name{?s} {.val {names(x)}} that {cli::qty(length(Unknown))}{?is/are} not unique or not in the OM.",
      "i" = "{Level} names: {.val {Valid}}."
    ), call = NULL)
  x
}

# Data@LifeHistory as an empty lifehistorydata or a list named by the stocks of the complex
.NormalizeLifeHistoryData <- function(LH, StockNms, Where = "Data@LifeHistory") {
  Empty <- new("lifehistorydata")
  if (is.null(LH)) return(Empty)

  if (.IsLifeHistoryData(LH)) {
    if (EmptyObject(LH)) return(Empty)
    if (length(StockNms) != 1)
      cli::cli_abort(c(
        "x" = "{.code {Where}} is a single {.cls lifehistorydata} object but the stock complex has {length(StockNms)} stocks.",
        "i" = "Supply a list named by stock: {.val {StockNms}}."
      ), call = NULL)
    return(stats::setNames(list(LH), StockNms))
  }

  .CheckLifeHistoryList(LH, Where)
  if (!length(LH)) return(Empty)
  .NameByLevel(LH, StockNms, "stock", Where)
}

# Data@Exploitation as an empty exploitationdata or a list by stock of lists by fleet
.NormalizeExploitationData <- function(Ex, StockNms, FleetNms, Where = "Data@Exploitation") {
  Empty <- new("exploitationdata")
  if (is.null(Ex)) return(Empty)
  nStock <- length(StockNms)
  nFleet <- length(FleetNms)

  if (.IsExploitationData(Ex)) {
    if (EmptyObject(Ex)) return(Empty)
    if (nStock != 1 || nFleet != 1)
      cli::cli_abort(c(
        "x" = "{.code {Where}} is a single {.cls exploitationdata} object but there {cli::qty(nStock)}{?is/are} {nStock} stock{?s} and {nFleet} fleet{?s}.",
        "i" = "Supply a list named by stock of lists named by fleet."
      ), call = NULL)
    return(stats::setNames(list(stats::setNames(list(Ex), FleetNms)), StockNms))
  }

  .CheckExploitationList(Ex, Where)
  if (!length(Ex)) return(Empty)

  Leaf <- vapply(Ex, .IsExploitationData, logical(1))

  if (all(Leaf)) {
    Nms <- names(Ex)
    AsFleets <- if (is.null(Nms)) nStock == 1 && length(Ex) == nFleet else all(Nms %in% FleetNms)
    AsStocks <- if (is.null(Nms)) nFleet == 1 && length(Ex) == nStock else all(Nms %in% StockNms)

    if (AsFleets && (nStock == 1 || !AsStocks)) {
      if (nStock != 1)
        cli::cli_abort(c(
          "x" = "{.code {Where}} is a list by fleet but the stock complex has {nStock} stocks.",
          "i" = "Supply a list named by stock of lists named by fleet."
        ), call = NULL)
      Ex <- .NameByLevel(Ex, FleetNms, "fleet", Where)
      return(stats::setNames(list(Ex), StockNms))
    }
    if (AsStocks) {
      Ex <- .NameByLevel(Ex, StockNms, "stock", Where)
      return(lapply(Ex, \(el) stats::setNames(list(el), FleetNms)))
    }
    cli::cli_abort(c(
      "x" = "{.code {Where}} could not be matched to stocks or fleets.",
      "i" = "Stocks: {.val {StockNms}}. Fleets: {.val {FleetNms}}."
    ), call = NULL)
  }

  if (!all(vapply(Ex, is.list, logical(1))))
    cli::cli_abort("{.code {Where}} mixes {.cls exploitationdata} objects and lists.", call = NULL)

  Ex <- .NameByLevel(Ex, StockNms, "stock", Where)
  for (st in names(Ex))
    Ex[[st]] <- .NameByLevel(Ex[[st]], FleetNms, "fleet", paste0(Where, "[[\"", st, "\"]]"))
  Ex
}

# lifehistorydata of one stock, or NULL
.StockLifeHistory <- function(Data, Stock) {
  LH <- Data@LifeHistory
  if (.IsLifeHistoryData(LH)) return(if (EmptyObject(LH)) NULL else LH)
  LH[[Stock]]
}

# exploitationdata of one stock and fleet, or NULL
.StockFleetExploitation <- function(Data, Stock, Fleet) {
  Ex <- Data@Exploitation
  if (.IsExploitationData(Ex)) return(if (EmptyObject(Ex)) NULL else Ex)
  Ex[[Stock]][[Fleet]]
}

# First non-empty lifehistorydata / exploitationdata held by a data object
.FirstLifeHistory <- function(data) {
  LH <- data@LifeHistory
  if (.IsLifeHistoryData(LH)) return(LH)
  for (el in LH) if (!EmptyObject(el)) return(el)
  new("lifehistorydata")
}

.FirstExploitation <- function(data) {
  Ex <- data@Exploitation
  if (.IsExploitationData(Ex)) return(Ex)
  for (st in Ex) {
    if (.IsExploitationData(st) && !EmptyObject(st)) return(st)
    for (el in st) if (.IsExploitationData(el) && !EmptyObject(el)) return(el)
  }
  new("exploitationdata")
}
