
.CompdataClasses <- function(x, fleet = NULL) {
  classes <- x@Classes
  if (is.null(classes) || !is.list(classes))
    return(classes)

  if (is.null(fleet))
    cli::cli_abort("`fleet` must be supplied when `Classes` is a per-fleet list.", .internal = TRUE)

  if (is.numeric(fleet)) {
    if (fleet < 1 || fleet > length(classes))
      cli::cli_abort(
        "`fleet` index {fleet} out of range; {length(classes)} fleet{?s} available.",
        .internal = TRUE
      )
    return(classes[[fleet]])
  }

  if (!fleet %in% names(classes))
    cli::cli_abort(
      "Fleet {.val {fleet}} not found in `Classes`; available: {.val {names(classes)}}.",
      .internal = TRUE
    )
  classes[[fleet]]
}

.PadClassesByFleet <- function(byFleet, Years) {
  FleetNames <- names(byFleet)
  classesList <- purrr::map(byFleet, \(m) as.numeric(dimnames(m)$Class))
  nClassMax   <- max(lengths(classesList), 0)
  nYear       <- length(Years)
  nFleet      <- length(byFleet)

  Value <- array(
    NA_real_,
    dim      = c(nYear, nFleet, nClassMax),
    dimnames = list(Year = Years, Fleet = FleetNames, Class = seq_len(nClassMax))
  )

  for (fl in seq_len(nFleet)) {
    nc <- length(classesList[[fl]])
    if (!nc) next
    Value[, fl, seq_len(nc)] <- t(byFleet[[fl]])
  }

  names(classesList) <- FleetNames
  list(Value = Value, Classes = classesList)
}


.AgeCompArray <- function(compdata) {
  Value <- compdata@Value
  dn    <- dimnames(Value) %||% vector('list', 3)
  if (is.null(dn[[1]])) dn[[1]] <- seq_len(dim(Value)[1])
  if (is.null(dn[[2]])) dn[[2]] <- compdata@Name %||% paste('Fleet', seq_len(dim(Value)[2]))
  if (is.null(dn[[3]])) dn[[3]] <- compdata@Classes %||% (seq_len(dim(Value)[3]) - 1)
  names(dn) <- c('Year', 'Fleet', 'Age')
  dimnames(Value) <- dn
  Value
}

.ExtractDataCompTimeseries <- function(object, slot_name, byFleet = TRUE, byStock = FALSE) {
  x <- slot(object, slot_name)
  if (!byStock || !is.list(x))
    return(.CompDataToDF(.CombineCompElements(x, paste0("`", slot_name, "`")), slot_name, byFleet))
  x <- x[!vapply(x, EmptyObject, logical(1))]
  if (!length(x))
    return(.CompDataToDF(new("compdata"), slot_name, byFleet) |> dplyr::mutate(Stock = character(0)))
  purrr::imap(x, \(cd, el) .CompDataToDF(cd, slot_name, byFleet) |> dplyr::mutate(Stock = el)) |>
    dplyr::bind_rows()
}

.CompDataToDF <- function(compdata, slot_name, byFleet = TRUE) {
  isAtSize <- grepl('AtSize$', slot_name)
  varName  <- sub('At(Age|Size)$', '', slot_name)
  classCol <- if (isAtSize) 'Class' else 'Age'

  if (EmptyObject(compdata)) {
    empty <- tibble::tibble(Year = numeric(0), Fleet = character(0),
                            Class = numeric(0), Value = numeric(0),
                            Variable = character(0))
    names(empty)[3] <- classCol
    return(empty)
  }

  if (!isAtSize) {
    array <- .AgeCompArray(compdata)
    if (!byFleet) array <- SumOverFleet(array)
    return(Array2DF(array) |> dplyr::mutate(Variable = varName))
  }

  fleetNames <- dimnames(compdata@Value)$Fleet
  classesAll <- purrr::map(fleetNames, \(fl) .CompdataClasses(compdata, fl))

  if (!byFleet) {
    ref <- classesAll[[1]]
    if (!all(purrr::map_lgl(classesAll, identical, ref)))
      cli::cli_abort(c(
        "Cannot sum {.field {slot_name}} across fleets with `byFleet = FALSE`.",
        "x" = "Fleets do not share the same size-class grid.",
        "i" = "Set `byFleet = TRUE` to keep fleets separate instead."
      ), call = NULL)
  }

  df <- purrr::map2(fleetNames, classesAll, \(fl, classes) {
    nC <- length(classes)
    if (!nC) return(NULL)
    sub <- compdata@Value[, fl, seq_len(nC), drop = FALSE]
    dimnames(sub) <- list(Year = dimnames(compdata@Value)$Year, Fleet = fl, Class = classes)
    Array2DF(sub) |> dplyr::mutate(Fleet = as.character(Fleet))
  }) |> dplyr::bind_rows() |>
    dplyr::mutate(Variable  = varName,
                  Fleet     = factor(Fleet, levels = fleetNames, ordered = TRUE))

  if (!byFleet)
    df <- df |>
      dplyr::group_by(Year, Class, Variable) |>
      dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Fleet = 'Total')

  df
}

# One compdata from a composition slot: the counts of the complex and stock
# elements are summed by fleet. Age compositions are matched by age; a fleet's
# size compositions must have the same classes in every element.
.CombineCompElements <- function(x, Where = "composition data") {
  if (!is.list(x)) return(x)
  x <- x[!vapply(x, EmptyObject, logical(1))]
  if (!length(x)) return(new("compdata"))
  if (length(x) == 1) {
    Out <- x[[1]]
    Out@Joint <- logical(0)
    return(Out)
  }
  isAge <- !is.list(x[[1]]@Classes)
  ClassesOf <- function(cd, nm) if (is.list(cd@Classes)) cd@Classes[[nm]] else cd@Classes
  Names <- unique(unlist(lapply(x, \(cd) dimnames(cd@Value)[[2]])))
  Years <- dimnames(x[[1]]@Value)[[1]]
  Classes <- lapply(Names, \(nm) {
    Grids <- lapply(Filter(\(cd) nm %in% dimnames(cd@Value)[[2]], x), ClassesOf, nm = nm)
    if (isAge) return(sort(unique(unlist(Grids))))
    if (!all(vapply(Grids, identical, logical(1), Grids[[1]])))
      cli::cli_abort(c(
        "x" = "{.val {nm}} has different classes in the stock-specific elements of the {Where}.",
        "i" = "Use {.code byStock = TRUE} to keep the stocks separate."
      ), call = NULL)
    Grids[[1]]
  }) |> stats::setNames(Names)
  if (isAge) Classes <- lapply(Classes, \(cl) sort(unique(unlist(Classes))))
  nBin  <- max(lengths(Classes))
  Value <- array(NA_real_, c(length(Years), length(Names), nBin),
                 list(Year = Years, Fleet = Names, Class = seq_len(nBin)))
  for (cd in x) {
    for (nm in dimnames(cd@Value)[[2]]) {
      Own <- ClassesOf(cd, nm)
      b   <- match(Own, Classes[[nm]])
      Add <- cd@Value[match(Years, dimnames(cd@Value)[[1]]), nm, seq_along(Own), drop = FALSE]
      Old <- Value[, nm, b, drop = FALSE]
      Value[, nm, b] <- ifelse(is.na(Old) & is.na(Add), NA, rowSums(cbind(c(Old), c(Add)), na.rm = TRUE))
    }
  }
  if (isAge) dimnames(Value)[[3]] <- Classes[[1]]
  names(dimnames(Value))[3] <- names(dimnames(x[[1]]@Value))[3] %||% "Class"
  Out <- new("compdata")
  Out@Name    <- Names
  Out@Value   <- Value
  Out@Classes <- if (isAge) as.numeric(Classes[[1]]) else Classes
  Out@Units   <- x[[1]]@Units
  Out
}

# A data object with every stock-specific slot combined
.CombineDataElements <- function(Data) {
  for (sl in c("LandingsAtAge", "DiscardsAtAge", "LandingsAtSize", "DiscardsAtSize",
               "SurveyAtAge", "SurveyAtSize"))
    if (methods::.hasSlot(Data, sl))
      slot(Data, sl) <- .CombineCompElements(slot(Data, sl), paste0("`", sl, "`"))
  .CombineDataSeries(Data)
}
