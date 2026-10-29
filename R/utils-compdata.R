
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

.ExtractDataCompTimeseries <- function(object, slot_name, byFleet = TRUE) {
  compdata <- slot(object, slot_name)
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
    array <- compdata@Value
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
