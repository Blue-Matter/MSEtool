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

# Stock-specific data slots: one object, or a list named by the complex and/or its stocks

# Elements of a data or obs slot as a named list; a single object is the complex element
.SlotElements <- function(x, ComplexName) {
  if (is.list(x)) return(x)
  stats::setNames(list(x), ComplexName)
}

# A list holding only the complex element is returned as a single object
.ElementsToSlot <- function(x, ComplexName) {
  if (length(x) == 1 && identical(names(x), ComplexName)) return(x[[1]])
  x
}

# Stocks (indices) covered by element `Name` of complex `stocks`
.ElementStocks <- function(Name, ComplexName, StockNms, stocks) {
  if (identical(Name, ComplexName)) return(stocks)
  stocks[match(Name, StockNms[stocks])]
}

# Obs object of element `Name`; a single obs object applies to every element
.ElementObs <- function(x, Name) {
  if (!is.list(x)) return(x)
  x[[Name]]
}

.EmptySlot <- function(x) {
  if (is.list(x)) return(all(vapply(x, EmptyObject, logical(1))))
  EmptyObject(x)
}

.IsJointData <- function(x, Name) methods::.hasSlot(x, "Joint") && isTRUE(x@Joint[Name])

.IsJointObs <- function(x) methods::.hasSlot(x, "Joint") && isTRUE(x@Joint)

# Validates element names of a stock-specific data slot against the complex and its stocks
.CheckSlotElements <- function(x, ComplexName, StockNms, Where) {
  if (!is.list(x)) return(invisible(x))
  if (is.null(names(x)) || any(!nzchar(names(x))))
    cli::cli_abort(c(
      "x" = "{.code {Where}} is a list but its elements are not named.",
      "i" = "Name the elements by the complex ({.val {ComplexName}}) and/or its stocks ({.val {StockNms}})."
    ), call = NULL)
  .NameByLevel(x, unique(c(ComplexName, StockNms)), "complex or stock", Where)
  invisible(x)
}

.DataSlotClass <- function(type) {
  switch(type, Landings = , Discards = "catchdata", CPUE = , Survey = "indicesdata", "compdata")
}

# Element names of a data slot: those of the real data, else those of the obs
# (lists name their elements; a specified single obs object is the complex)
.SlotElementNames <- function(Current, ObsSlots, ComplexName) {
  if (!.EmptySlot(Current)) return(names(.SlotElements(Current, ComplexName)))
  Names <- unique(unlist(lapply(ObsSlots, \(o) if (is.list(o)) names(o) else if (!isNewObject(o)) ComplexName)))
  if (length(Names)) Names else ComplexName
}

# Hist (or Proj) with the `type` obs of element `El` for every fleet and index of complex `i`
.ElementHist <- function(Hist, i, type, El) {
  for (nm in names(Hist@OM@Obs[[i]])) {
    x <- slot(Hist@OM@Obs[[i]][[nm]], type)
    if (is.list(x))
      slot(Hist@OM@Obs[[i]][[nm]], type) <- x[[El]] %||% new(class(x[[1]])[1])
  }
  Hist
}

# Applies `Generate(Data, Hist, stocks)` to each element of a stock-specific data slot
.GenHistByElement <- function(Data, Hist, i, stocks, type, Generate) {
  Current  <- slot(Data, type)
  ObsSlots <- lapply(Hist@OM@Obs[[i]], slot, type)
  if (!is.list(Current) && !any(vapply(ObsSlots, is.list, logical(1))))
    return(Generate(Data, Hist, stocks))
  Complex  <- names(Hist@OM@Complexes)[i]
  Elements <- .SlotElementNames(Current, ObsSlots, Complex)
  Out <- lapply(Elements, \(el) {
    slot(Data, type) <- .SlotElements(Current, Complex)[[el]] %||% new(.DataSlotClass(type))
    Generate(Data, .ElementHist(Hist, i, type, el),
             .ElementStocks(el, Complex, StockNames(Hist@OM), stocks))
  }) |> stats::setNames(Elements)
  .ElementsToSlot(Out, Complex)
}

# Applies `Generate(Proj, stocks)`, returning a list by simulation, to each element of a data slot
.GenProjByElement <- function(Proj, i, stocks, type, Generate) {
  Slot1 <- slot(Proj@Data[[1]][[i]], type)
  if (!is.list(Slot1))
    return(Generate(Proj, stocks))
  Complex <- names(Proj@OM@Complexes)[i]
  BySim <- lapply(names(Slot1), \(el) {
    P <- .ElementHist(Proj, i, type, el)
    for (x in seq_along(P@Data))
      slot(P@Data[[x]][[i]], type) <- slot(P@Data[[x]][[i]], type)[[el]]
    if (length(P@OM@Data) >= i && is.list(slot(P@OM@Data[[i]], type)))
      slot(P@OM@Data[[i]], type) <- slot(P@OM@Data[[i]], type)[[el]] %||% new(.DataSlotClass(type))
    Generate(P, .ElementStocks(el, Complex, StockNames(Proj@OM), stocks))
  }) |> stats::setNames(names(Slot1))
  lapply(seq_along(BySim[[1]]), \(x) lapply(BySim, `[[`, x))
}

# Applies `Condition(Hist, FisheryData, stocks)` to each element of a stock-specific data slot;
# the obs of each fleet and index become lists named by element
.ConditionByElement <- function(Hist, FisheryData, i, stocks, type, Condition) {
  Current <- slot(FisheryData, type)
  if (!is.list(Current))
    return(Condition(Hist, FisheryData, stocks))
  Complex <- names(Hist@OM@Complexes)[i]
  Result  <- list()
  for (el in names(Current)) {
    slot(FisheryData, type) <- Current[[el]]
    H <- Condition(.ElementHist(Hist, i, type, el), FisheryData,
                   .ElementStocks(el, Complex, StockNames(Hist@OM), stocks))
    Hist@Log <- H@Log
    for (nm in names(H@OM@Obs[[i]]))
      Result[[nm]][[el]] <- slot(H@OM@Obs[[i]][[nm]], type)
  }
  for (nm in names(Result))
    slot(Hist@OM@Obs[[i]][[nm]], type) <- Result[[nm]]
  Hist
}

# One catchdata from a stock-specific catch slot: values summed by fleet and year
.CombineCatchElements <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, EmptyObject, logical(1))]
  if (!length(x)) return(new("catchdata"))
  if (length(x) == 1) return(x[[1]])
  Names <- unique(unlist(lapply(x, \(el) colnames(el@Value) %||% el@Name)))
  Years <- rownames(x[[1]]@Value)
  Value <- Var <- array(NA_real_, c(length(Years), length(Names)), list(Year = Years, Fleet = Names))
  Units <- stats::setNames(rep(NA_character_, length(Names)), Names)
  for (el in x) {
    nms <- colnames(el@Value) %||% el@Name
    v   <- el@Value[match(Years, rownames(el@Value)), , drop = FALSE]
    cv  <- if (is.null(el@CV)) v * NA else el@CV[match(Years, rownames(el@CV)), , drop = FALSE]
    Value[, nms] <- ifelse(is.na(Value[, nms]) & is.na(v), NA, rowSums(cbind(c(Value[, nms]), c(v)), na.rm = TRUE))
    Var[, nms]   <- ifelse(is.na(Var[, nms]) & is.na(cv), NA,
                           rowSums(cbind(c(Var[, nms]), c((cv * v)^2)), na.rm = TRUE))
    if (length(el@Units)) Units[nms][is.na(Units[nms])] <- rep(el@Units, length.out = length(nms))[is.na(Units[nms])]
  }
  Out <- new("catchdata")
  Out@Name  <- Names
  Out@Value <- Value
  Out@CV    <- if (all(is.na(Var))) NULL else sqrt(Var) / Value
  Out@Units <- if (all(is.na(Units))) NULL else unname(Units)
  Out
}

# One indicesdata from a stock-specific index slot: the indices of every element
.CombineIndexElements <- function(x) {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, EmptyObject, logical(1))]
  if (!length(x)) return(new("indicesdata"))
  if (length(x) == 1) return(x[[1]])
  Names <- unique(unlist(lapply(x, \(el) el@Name %||% colnames(el@Value))))
  Years <- rownames(x[[1]]@Value)
  Value <- CV <- array(NA_real_, c(length(Years), length(Names)), list(Year = Years, Fleet = Names))
  Out <- new("indicesdata")
  Units <- Timing <- Ref <- stats::setNames(rep(NA, length(Names)), Names)
  for (el in x) {
    nms <- el@Name %||% colnames(el@Value)
    for (k in seq_along(nms)) {
      Row <- match(Years, rownames(el@Value))
      Fill <- is.na(Value[, nms[k]])
      Value[Fill, nms[k]] <- el@Value[Row, k][Fill]
      if (!is.null(el@CV)) CV[Fill, nms[k]] <- el@CV[Row, k][Fill]
      if (is.na(Units[nms[k]]) && length(el@Units) >= k) Units[nms[k]] <- el@Units[k]
      if (is.na(Timing[nms[k]]) && length(el@Timing) >= k) Timing[nms[k]] <- el@Timing[k]
      if (is.na(Ref[nms[k]]) && length(el@Ref) >= k) Ref[nms[k]] <- el@Ref[k]
    }
  }
  Out@Name   <- Names
  Out@Value  <- Value
  Out@CV     <- if (all(is.na(CV))) NULL else CV
  Out@Units  <- if (all(is.na(Units))) NULL else unname(as.character(Units))
  Out@Timing <- as.numeric(ifelse(is.na(Timing), 0, Timing))
  if (!all(is.na(Ref))) Out@Ref <- unname(as.numeric(Ref))
  Out
}

# Catch or index values of a name and year are held for the complex or for its stocks, not both
.CheckOneLevel <- function(x, ComplexName, Where) {
  if (!is.list(x) || !ComplexName %in% names(x)) return(invisible(x))
  Keys <- function(el) {
    if (is.null(el@Value)) return(character(0))
    Obs <- which(!is.na(el@Value), arr.ind = TRUE)
    paste(rownames(el@Value)[Obs[, 1]], (colnames(el@Value) %||% el@Name)[Obs[, 2]])
  }
  Dup <- intersect(Keys(x[[ComplexName]]), unlist(lapply(x[names(x) != ComplexName], Keys)))
  if (length(Dup))
    cli::cli_abort(c(
      "x" = "{.code {Where}} has values for both the complex and its stocks for the same name and year.",
      "i" = "Hold each value for the complex or for the stocks, not both. Duplicated: {.val {utils::head(Dup, 5)}}."
    ), call = NULL)
  invisible(x)
}

# catch and index slots held by stock combined into one object each
.CombineDataSeries <- function(object) {
  for (sl in c('Landings', 'Discards'))
    slot(object, sl) <- .CombineCatchElements(slot(object, sl))
  for (sl in c('CPUE', 'Survey'))
    slot(object, sl) <- .CombineIndexElements(slot(object, sl))
  object
}
